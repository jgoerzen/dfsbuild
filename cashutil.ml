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
