(* arch-tag: Main entrance for CD builer
* Copyright (c) 2004 John Goerzen 
*)

open Unix;;
open Printf;;
open Str;;
open Cash;;
open Cashutil;;
open Dfsutils;;

let p = print_endline;;

let run prog args =
  p ("Running: " ^ prog ^ " " ^ (String.concat " " args));
  Cashutil.run prog args;;

let genmarker () = 
  Random.self_init ();
  Printf.sprintf "DFS CD IMAGE, ID: %f,%d,%d\n" (Unix.time()) (Random.bits())
    (Random.bits());;

let getdevices cp =
  let devlist = split_ws (cp#get "cd" "devices") in
  (String.concat "\n" devlist) ^ "\n";;

let dlmirrors cp wdir =
  let suites = split_ws (cp#get "cd" "dlsuites") in
  let mirror = (cp#get "cd" "mirror") in
  Mirror.mirror_workdir suites mirror wdir;;
  
let writestring filename s =
  let sd = Pervasives.open_out filename in
  output_string sd s;
  Pervasives.close_out sd;;

let parsecmdline () =
  let cffile = ref "" in
  let wdir = ref "" in
  let args = [(("-c":Arg.key), Arg.Set_string cffile, 
              ("Configuration file":Arg.doc));
              (("-w":Arg.key), Arg.Set_string wdir,
              ("Work directory (WILL BE CLOBBERED IF EXISTS)":Arg.doc))] in
  let usage = "Usage: buildcd -c cf -w dir" in
  Arg.parse args (fun x -> ()) usage;
  if (!cffile = "") || (!wdir = "") then begin
   Arg.usage args usage;
   exit 1;
   (new ConfigParser.rawConfigParser, "");
  end else (
    let cp = new ConfigParser.rawConfigParser in
    cp#readfile !cffile;
    (cp, !wdir));;

let cdebootstrap cp target wdir =
  p ("Bootstrapping into " ^ target);
  run "cdebootstrap" [cp#get "cd" "suite"; target; "file://" ^ wdir ^ "/mirror"];
  writestring (target ^ "/etc/apt/sources.list") 
    ("deb " ^ (cp#get "cd" "mirror") ^ " " ^ (cp#get "cd" "suite") ^ " main\n");
;;

let installpkgs cp target =
  p ("Installing packages.");
  run "chroot" [target; "/bin/bash"; "-c";
    "echo \"debconf\tdebconf/priority\tselect\tcritical\" | debconf-set-selections"];
  run "cp" ["/etc/resolv.conf"; target ^ "/etc" ];

  run "chroot" [target; "apt-get"; "update"];
  let pkgstr = Strutil.strip (cp#get "cd" "packages") in
  let pkgs = split_ws pkgstr in
  run "chroot" (target :: "apt-get" :: "-y" :: "install" :: pkgs) ;
  rm (target ^ "/etc/resolv.conf");
  run "chroot" [target; "apt-get"; "clean" ];
  (*
  run "chroot" [target; "/bin/bash"; "-c"; ". /etc/updatedb.conf; updatedb"];
  *)
  (* 
  run "chroot" [target; "rm -rvf"; "/var/lib/apt/lists/*";
    "/var/cache/apt/*.bin"; "/var/cache/debconf/*"; "/etc/X11" ]; *)
  ;;

let installrd cp libdir target =
  p "Preparing ramdisk...";
  let chr args = run "chroot" (target :: args) in
  mkdir (target ^ "/opt/dfsruntime") 0o755;
  mkdir (target ^ "/opt/initrd") 0o755;
  List.iter (fun x -> mkdir (target ^ "/opt/initrd/" ^ x) 0o755) 
    ["bin"; "lib"; "sbin"; "proc"; "usr"; "usr/sbin"; "usr/bin"; "realroot"; ];
  (*
  chr ["sh"; "-c"; "cp -v /lib/libc.so* /lib/libm.so* /lib/libdl.so* /lib/ld-linux.so* /opt/initrd/lib"];
  *)
  chr ["sh"; "-c"; "cp -v /lib/libc.so* /lib/ld-linux.so* /opt/initrd/lib"];
  chr ["cp"; "-v"; "/bin/busybox"; "/opt/initrd/bin"];
  chr ["cp"; "-v"; "/usr/sbin/chroot"; "/opt/initrd/usr/sbin/"];
  chr ["cp"; "-v"; "/sbin/pivot_root"; "/opt/initrd/sbin/"];
  chr ["cp"; "-r"; "/dev"; "/opt/initrd/"];
  (*
  run "mount" ["-t"; "proc"; "none"; target ^ "/opt/initrd/proc"];
  run "chroot" [target ^ "/opt/initrd"; "/bin/busybox"; "--install"];
  run "umount" [target ^ "/opt/initrd/proc"]; *)
  if (is_file_existing_fn (target ^ "/opt/initrd/linuxrc")) then begin
    unlink (target ^ "/opt/initrd/linuxrc")
  end;
  run "cp" [libdir ^ "/linuxrc"; target ^ "/opt/initrd/sbin/init"];
  let marker = genmarker () in
  writestring (target ^ "/opt/dfsruntime/marker") marker;
  writestring (target ^ "/opt/initrd/marker") marker;
  writestring (target ^ "/opt/initrd/devices") (getdevices cp);
  run "mkcramfs" [target ^ "/opt/initrd"; target ^
    "/opt/dfsruntime/initrd.dfs"];
  rm ~recursive:true (target ^ "/opt/initrd");;

let installkernels cp target =
  p "Installing kernels...";
  let kernlist = glob (split_ws (cp#get "cd" "kernels")) in
  let modlist = glob (split_ws (cp#get "cd" "modules")) in
  run "cp" ("-v" :: (kernlist @ [(target ^ "/boot")]));
  run "cp" ("-r" :: (modlist @ [target ^ "/lib/modules"]));
  mkdir (target ^ "/boot/grub") 0o755;
  run "cp" ("-rv" :: glob ["/usr/lib/grub/*/*"] @ [target ^ "/boot/grub/"]);
  let sd = open_out (target ^ "/boot/grub/menu.lst") in
  output_string sd "color cyan/blue white/blue\n";
  let newkerns = glob ["/boot/vmlinu*"] in
  List.iter (fun x ->
    let os s = output_string sd (s ^ "\n") in
    os ("title  Boot " ^ x);
    os ("kernel /boot/" ^ (Filename.basename x) ^ " root=/dev/ram0 single");
    os ("initrd /opt/dfsruntime/initrd.dfs");
    os ("boot\n");
  ) newkerns;
  Pervasives.close_out sd;
;;

let preprd cp imageroot =
  p "Preparing run-time rd"; 
  mkdir (imageroot ^ "/opt/dfsruntime/runtimemnt") 0o755 ;
  let rdpath = imageroot ^ "/opt/dfsruntime/runtimerd" in
  mkdir rdpath 0o755;
  let file2rd f =
    let src = imageroot ^ f in
    let dest = rdpath ^ f in
    let destdir = Filename.dirname dest in
    if not (is_file_existing_fn destdir) then ( run "mkdir" ["-p"; destdir]);
    if is_file_existing_fn src then (rename_file src dest);
    create_symlink ("/opt/dfsruntime/runtimemnt" ^ f) src;
  in
  List.iter file2rd (split_ws (cp#get "cd" "ramdisk_files"));
;;

let installlib libdir imageroot =
  p "Installing runtime library files.";
  List.iter (fun x -> 
    run "cp" [libdir ^ "/" ^ x; imageroot ^ "/opt/dfsruntime/"])
    ["startup"];;

let mkiso wdir imageroot =
  p "Preparing ISO image";
  let isofile = wdir ^ "/image.iso" in
  run "mkisofs" ["-R"; "-b"; "boot/grub/stage2_eltorito"; "-no-emul-boot";
    "-boot-load-size"; "1"; "-boot-info-table"; "-o"; isofile; imageroot];;

let _ = 
  let cp, wdir = parsecmdline () in
  p ("Using working directory: " ^ wdir);
  let libdir = resolve_file_name ~dir:(Unix.getcwd ()) (cp#get "cd" "libdir") in

  p ("Using library directory: " ^ libdir);
  rm ~recursive:true ~force:true wdir;
  mkdir wdir 0o755; 
  Unix.chdir wdir;
  let wdir = getcwd () in
  let imageroot = wdir ^ "/image" in
  mkdir imageroot 0o755;
  dlmirrors cp wdir;
  cdebootstrap cp imageroot wdir;
  installpkgs cp imageroot;
  installrd cp libdir imageroot;
  installkernels cp imageroot; 
  installlib libdir imageroot;
  preprd cp imageroot;
  mkiso wdir imageroot;
;;

