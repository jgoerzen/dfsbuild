(* arch-tag: Cash utilities
* Copyright (c) 2004 John Goerzen
*)

open Cash;;

exception Execution_Failure of string;;

let swait failmsg proc =
  match (wait proc) with
  WEXITED 0 -> ()
  | _ -> raise (Execution_Failure ("Failed: " ^ failmsg));;

let run prog args =
  swait (prog ^ " " ^ (String.concat " " args)) 
    (fork_child (fun () -> exec_path prog args));;

let abspath name = resolve_file_name ~dir:(Unix.getcwd ()) name;;

let rec recurse_cmd_do f startname =
  let info = file_info_fn ~chase:false startname in
  match info.st_kind with
    S_DIR -> ignore (fold_directory (recurse_cmd_dir f) startname startname);
             f info startname;
  | _ -> f info startname;
and recurse_cmd_dir f startname curname =
  let thisname = startname ^ "/" ^ curname in
  recurse_cmd_do f thisname;
  startname;;

let rec recurse_cmd f startname = recurse_cmd_do f (abspath startname);;

exception RMError of string;;
let rm ?(recursive=false) ?(force=false) filename =
  let recunl info name = 
    try
      if info.st_kind = S_DIR then 
        Unix.rmdir name
      else
        Unix.unlink name
    with (Unix.Unix_error _) as exc ->
      if not force; then raise exc;
  in
  if recursive then
    recurse_cmd recunl filename
  else
    recunl (file_info_fn ~chase:false filename) filename;;

