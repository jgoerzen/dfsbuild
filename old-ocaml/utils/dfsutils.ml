(* arch-tag: generic utilities
*)

let p = print_endline;;                                                         
let pn s = print_string s; flush stdout;;                                       

let print_pid () =
  p ("Current PID is: " ^ (string_of_int (Unix.getpid () )));;

let exec_passing_args filename =
  let args = Array.copy Sys.argv in
  Array.set args 0 filename;
  Unix.execvp filename args;;

  
