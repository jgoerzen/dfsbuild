(* arch-tag: write config files
*)
open Dfsutils;;
open Shellutil;;
open Unixutil;;
open Strutil;;
open Archsupport;;

let datestr = strip (run_getstring "date" []);;

let getbuildinfo cp = 
  "Name: " ^ (get cp "name") ^ 
  "\nVersion: " ^ (get cp "version") ^
  "\nBuilder: " ^ (get cp "builder") ^
  "\nPreparation Date: " ^ datestr ^ "\n";;

let writebuildinfo cp target =
  let fd = open_out (target ^ "/opt/dfsruntime/buildinfo") in
  output_string fd (getbuildinfo cp);
  Pervasives.close_out fd;;

let getidstring cp =
  Printf.sprintf "DFS image: %s %s (%s)" (get cp "name") (get cp "version") datestr;;

let writecfgfiles cp basedir =
  let w fn s =
    let outfd = open_out (basedir ^ fn) in
    output_string outfd s;
    output_string outfd "\n";
    close_out outfd
  in
  let a fn s =
    let outfd = open_out_gen [Open_append; Open_wronly; Open_creat] 0o644
    (basedir ^ fn) in
    output_string outfd s;
    output_string outfd "\n";
    close_out outfd
  in

  a "/etc/issue" ("\n" ^ (getbuildinfo cp) ^ "\n");
  
  if cp#has_section "appendfiles" then begin
    List.iter (fun fn -> a fn (cp#get "appendfiles" fn))
     (cp#options "appendfiles");
  end;
  if cp#has_section "createfiles" then begin
    List.iter (fun fn -> w fn (cp#get "createfiles" fn))
      (cp#options "createfiles");
  end;
  if cp#has_section "symlinks" then begin
    List.iter (fun from -> p("Symlinking " ^ from); Unix.symlink (cp#get "symlinks" from) (basedir ^
    from))
      (cp#options "symlinks");
  end;
      List.iter (fun x -> p("Deleting " ^ x); rm ~force:true (basedir ^ x))
        (glob (split_ws (get ~default:"" cp "deletefiles")));
  p("Looking for dirs to make...");
  p (get cp "makedirs");
      List.iter (fun x -> p("Making dir " ^ x); Unix.mkdir (basedir ^ x) 0o755)
        (split_ws (get ~default:"" cp "makedirs"));
;;

let fixrc target =
  rm ~recursive:true (target ^ "/etc/rc2.d");
  run "cp" ["-r"; (target ^ "/etc/rc1.d"); (target ^ "/etc/rc2.d")];
  run "cp" ("-r" :: glob [target ^ "/etc/rc3.d/*logd*"] @
    [target ^ "/etc/rc2.d/"]);
  List.iter (fun x -> rm x) (glob [target ^ "/etc/rc2.d/S*single"]);
;;

