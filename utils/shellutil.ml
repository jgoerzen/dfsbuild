(* arch-tag: Shell utilities
* Copyright (c) 2004 John Goerzen
*)

open Shell;;
open Unixutil;;
open Unix;;

let run prog args =
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
  Dfsutils.split_ws (run_getstring "sh" ("-c" :: ["echo " ^ (String.concat " "
  patlist)]));;

let rec recurse_cmd_do f startname =
  let info = Unix.lstat startname in
  match info.st_kind with
    S_DIR -> ignore (fold_directory (recurse_cmd_dir f) startname startname);
             f info startname;
  | _ -> f info startname;
and recurse_cmd_dir f startname curname =
  let thisname = startname ^ "/" ^ curname in
  recurse_cmd_do f thisname;
  startname;;

let rec recurse_cmd f startname = recurse_cmd_do f startname;;

exception RMError of string;;
let rm ?(recursive=false) ?(force=false) filename =
  let recunl info name = 
    try
      if info.st_kind = S_DIR then 
        Unix.rmdir name
      else
        Unix.unlink name
    with (Unix.Unix_error _) as exc ->
      if not force then raise exc
  in
  if recursive then
    recurse_cmd recunl filename
  else
    try
      recunl (Unix.lstat filename) filename
    with (Unix.Unix_error _) as exc ->
      if not force then raise exc;;

let installrd_cramfs target =
  run "mkcramfs" [target ^ "/opt/initrd"; target ^
    "/opt/dfsruntime/initrd.dfs"];
  rm ~recursive:true (target ^ "/opt/initrd");;

