(* arch-tag: Shell utilities
* Copyright (c) 2004 John Goerzen
*)

open Shell;;
open Unixutil;;
open Unix;;

let run prog args =
  print_endline ("Running: " ^ prog ^ " " ^ (String.concat " " args));
  call [cmd prog args];;

let runnoout prog args =
  let stdoutbuf = Buffer.create 10 in
  let stderrbuf = Buffer.create 10 in
  call ~stdout:(to_buffer stdoutbuf) ~stderr:(to_buffer stderrbuf)
   [cmd prog args];;

let run_getstring prog args =
  let b = Buffer.create 50 in
  call ~stdout:(to_buffer b) [cmd prog args];
  Buffer.contents b;;

let glob patlist =
  Strutil.split_ws (run_getstring "sh" ("-c" :: ["echo " ^ (String.concat " "
  patlist)]));;

let installrd_cramfs target =
  run "mkcramfs" [target ^ "/opt/initrd"; target ^
    "/opt/dfsruntime/initrd.dfs"];
  rm ~recursive:true (target ^ "/opt/initrd");;

