(* arch-tag: Alpha aboot support
* Copyright (c) 2004 John Goerzen
*)

open Unix;;
open Shellutil;;
open Dfsutils;;
open Archsupport;;

let aboot cp target =
  run "cp" ["/boot/bootlx"; target ^ "/boot/"];
  installrd_cramfs target;
  let sd = open_out (target ^ "/etc/aboot.conf") in
  let newkerns = glob [target ^ "/boot/vmlinu*"] in
  ignore (List.fold_left (fun count x ->
    Printf.fprintf sd 
      "%d:boot/%s initrd=opt/dfsruntime/initrd.dfs root=/dev/ram0\n"
      count (Filename.basename x);
    count + 1) 0 newkerns);
  Pervasives.close_out sd;
  ([], fun cp wdir target isoname -> 
    run "isomarkboot" [isoname; "/boot/bootlx"; "/opt/dfsruntime/initrd.dfs"];
  );;
