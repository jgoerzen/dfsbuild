(* arch-tag: write config files
*)

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
  if cp#has_section "appendfiles" then begin
    List.iter (fun fn -> a fn (cp#get "appendfiles" fn))
     (cp#options "appendfiles");
  end;
  if cp#has_section "createfiles" then begin
    List.iter (fun fn -> a fn (cp#get "createfiles" fn))
      (cp#options "createfiles");
  end;

;;

let fixrc target =
  Cashutil.rm ~recursive:true (target ^ "/etc/rc2.d");
  Cashutil.run "cp" ["-r"; (target ^ "/etc/rc1.d"); (target ^ "/etc/rc2.d")];
  List.iter (fun x -> Cashutil.rm x) (Cash.glob [target ^ "/etc/rc2.d/S*single"]);
;;
