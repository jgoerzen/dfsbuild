(* arch-tag: Main entrance for CD builer
* Copyright (c) 2004 John Goerzen 
*)

open Shell;;
open Unix;;

let p = print_endline;;

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

let cdebootstrap cp target =
  p ("Bootstrapping into " ^ target);
  call [ cmd "cdebootstrap" ["-v"; cp#get "cd" "suite"; target; 
                             cp#get "cd" "mirror"]];
  let sl = open_out (target ^ "/etc/apt/sources.list") in
  output_string sl ("deb " ^ (cp#get "cd" "mirror") ^ " " ^ 
                    (cp#get "cd" "suite") ^ " main\n");
  close_out sl;
  call [ cmd "chroot" [target; "apt-get"; "update"]];;

let installpkgs cp target =
  p ("Installing packages.");
  let pkgstr = Strutil.strip (cp#get "cd" "packages") in
  let pkgs = Str.split (Str.regexp "[ \n\t]") pkgstr in
  call [ cmd "chroot" (target :: "apt-get" :: "-y" :: "install" :: pkgs)
  ];
  ;;

let _ = 
  let cp, wdir = parsecmdline () in
  p ("Using working directory: " ^ wdir);
  call [cmd "rm" ["-rf"; wdir]];
  mkdir wdir 0o755;
  chdir wdir;
  let wdir = getcwd () in
  let imageroot = wdir ^ "/image" in
  mkdir imageroot 0o755;
  cdebootstrap cp imageroot;
  installpkgs cp imageroot;;

