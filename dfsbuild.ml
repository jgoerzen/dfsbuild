(* arch-tag: Main entrance for CD builer
* Copyright (c) 2004 John Goerzen 
*)

open Unix;;
open Printf;;
open Str;;
open Unixutil;;
open Dfsutils;;
open Shellutil;;
open Archsupport;;

let p = print_endline;;

let run prog args =
  p ("Running: " ^ prog ^ " " ^ (String.concat " " args));
  Shellutil.run prog args;;

let genmarker () = 
  Random.self_init ();
  Printf.sprintf "DFS CD IMAGE, ID: %f,%d,%d\n" (Unix.time()) (Random.bits())
    (Random.bits());;

let getdevices cp =
  let devlist = split_ws (get cp "devices") in
  (String.concat "\n" devlist) ^ "\n";;

let dlmirrors cp wdir =
  let suites = split_ws (get cp "dlrepos") in
  Mirror.mirror_workdir cp suites wdir;
;;
  
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
  let usage = "Usage: dfsbuild -c cf -w dir" in
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
  run "cdebootstrap" [get cp "suite"; target; "file://" ^ wdir ^ "/mirror"];
  writestring (target ^ "/etc/apt/sources.list") 
    ("deb " ^ (get cp "mirror") ^ " " ^ (get cp "suite") ^ " main\n");
  Unix.rename (wdir ^ "/mirror") (target ^ "/opt/packages");
;;

let installpkgs cp target =
  p ("Installing packages.");
  run "chroot" [target; "/bin/bash"; "-c";
    "echo \"debconf\tdebconf/priority\tselect\tcritical\" | debconf-set-selections"];
  run "cp" ["/etc/resolv.conf"; target ^ "/etc" ];

  run "chroot" [target; "apt-get"; "update"];
  let allpkgstr = Strutil.strip (get cp "allpackages") in
  let archpkgstr = if cp#has_option (getarch()) "archpackages" then begin
    get cp "archpackages"
  end else "" in
  let pkgs = (split_ws allpkgstr) @ (split_ws archpkgstr) in
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

let compress cp wdir target =
  if (cp#getbool (getarch()) "compress") then begin
    p "Compressing image...";
    let noncom = wdir ^ "/noncom" in
    Unix.mkdir noncom 0o755;
    let noncomfiles = if cp#has_option (getarch()) "dontcompress" then 
      List.filter (fun x -> exists (target ^ x)) 
        (split_ws (get cp "dontcompress")) 
       else [] in
    let noncommap = let rec m l c = match l with
       [] -> [] | x :: xs -> (x, string_of_int c) :: m xs (c + 1) in
       m noncomfiles 0 in
    List.iter (fun (orig, tmp) -> print_endline ("Preserving " ^ orig)
      Unix.rename (target ^ orig) (noncom ^ "/" ^ tmp)) noncommap;
    run "mkzftree" [target; wdir ^ "/zftree"];
    rm ~recursive:true target;
    Unix.rename (wdir ^ "/zftree") target;
    List.iter (fun (orig, tmp) ->
      Unix.rename (noncom ^ "/" ^ tmp) (target ^ orig)) noncommap;
  end;
;;

let preprd cp libdir target =
  p "Preparing ramdisk...";
  let chr args = run "chroot" (target :: args) in
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
  if (exists (target ^ "/opt/initrd/linuxrc")) then begin
    unlink (target ^ "/opt/initrd/linuxrc")
  end;
  run "cp" [libdir ^ "/linuxrc"; target ^ "/opt/initrd/sbin/init"];
  Unix.chmod (target ^ "/opt/initrd/sbin/init") 0o755;
  let marker = genmarker () in
  writestring (target ^ "/opt/dfsruntime/marker") marker;
  writestring (target ^ "/opt/initrd/marker") marker;
  writestring (target ^ "/opt/initrd/devices") (getdevices cp);
;;

let installdebs cp imageroot =
  p "Installing .debs...";
  let rootopt = sprintf "--root=%s" imageroot in
  if cp#has_option (getarch()) "installdebs" then begin
    run "dpkg" (rootopt :: "-i" :: (split_ws (get cp "installdebs")));
  end;
  if cp#has_option (getarch()) "unpackdebs" then begin
    run "dpkg" (rootopt :: "--force-depends" :: "--force-conflicts" :: 
         "--force-overwrite" :: "--force-architecture" :: "--unpack" 
         :: (split_ws (get cp "unpackdebs")));
  end;
;;

let installkernels cp target =
  p "Installing kernels...";
  if cp#has_option (getarch()) "kernels" then begin
    let kernlist = glob (split_ws (get cp "kernels")) in
    run "cp" ("-v" :: (kernlist @ [(target ^ "/boot")]));
  end;
  if cp#has_option (getarch()) "modules" then begin
    let modlist = glob (split_ws (get cp "modules")) in
    run "cp" ("-r" :: (modlist @ [target ^ "/lib/modules"]));
  end;
;;

let preprtrd cp imageroot =
  p "Preparing run-time rd"; 
  mkdir (imageroot ^ "/opt/dfsruntime/runtimemnt") 0o755 ;
  let rdpath = imageroot ^ "/opt/dfsruntime/runtimerd" in
  mkdir rdpath 0o755;
  let file2rd f =
    let src = imageroot ^ f in
    let dest = rdpath ^ f in
    let destdir = Filename.dirname dest in
    if not (exists destdir) then ( run "mkdir" ["-p"; destdir]);
    if exists src then (rename src dest);
    symlink ("/opt/dfsruntime/runtimemnt" ^ f) src;
  in
  List.iter file2rd (glob (split_ws (get cp "ramdisk_files")));
;;

let installlib docdir libdir imageroot =
  p "Installing runtime library files.";
  List.iter (fun x -> 
    run "cp" ["-rL"; libdir ^ "/" ^ x; imageroot ^ "/opt/dfsruntime/"])
    ["startup"; "home.html"];
  Unix.chmod (imageroot ^ "/opt/dfsruntime/startup") 0o755;
  Unix.mkdir (imageroot ^ "/opt/dfsruntime/doc") 0o755;
  List.iter (fun x ->
    run "cp" ["-r"; docdir ^ "/" ^ x; imageroot ^ "/opt/dfsruntime/doc/"])
    ["dfs.txt.gz"; "html/"];
  List.iter (fun x ->
    run "cp" ["-r"; libdir ^ "/" ^ x; imageroot ^ "/usr/local/bin/"];
    Unix.chmod (imageroot ^ "/usr/local/bin/" ^ x) 0o755)
    ["dfshelp"; "dfshints"; "dfsbuildinfo"];
;;

let mkiso cp wdir imageroot isoargs =
  p "Preparing ISO image";
  let isofile = wdir ^ "/image.iso" in
  let compressopts = if cp#getbool (getarch()) "compress" then ["-z"] else [] in
  run "mkisofs" (compressopts @ isoargs @ 
    ["-pad"; "-R"; "-o"; isofile; imageroot]);
  isofile;;

let _ = 
  let cp, wdir = parsecmdline () in
  p ("Using working directory: " ^ wdir);
  let libdir = (get cp "libdir") in 
  (*
  let libdir = resolve_file_name ~dir:(Unix.getcwd ()) (get cp "libdir") in
  *)

  p ("Using library directory: " ^ libdir);
  rm ~recursive:true ~force:true wdir;
  mkdir wdir 0o755; 
  (* Unix.chdir wdir;
  let wdir = getcwd () in *)
  let imageroot = wdir ^ "/image" in
  mkdir imageroot 0o755;
  mkdir (imageroot ^ "/opt") 0o755;
  mkdir (imageroot ^ "/opt/dfsruntime") 0o755;
  dlmirrors cp wdir;
  cdebootstrap cp imageroot wdir;
  installpkgs cp imageroot;
  installlib (get cp "docdir") libdir imageroot;
  installdebs cp imageroot;
  Configfiles.writecfgfiles cp imageroot;
  Configfiles.fixrc imageroot;
  Configfiles.writebuildinfo cp imageroot;
  preprd cp libdir imageroot;
  installkernels cp imageroot; 
  let isoargs, postisofunc = Bootloader.install cp wdir imageroot in
  preprtrd cp imageroot;
  compress cp wdir imageroot;
  let isoname = mkiso cp wdir imageroot isoargs in
  postisofunc cp wdir imageroot isoname;
;;

