(* arch-tag: generic utilities
*)

let wsregexp = Str.regexp "[ \n\t]+";;

let split_ws instr =
  Str.split wsregexp (Strutil.strip instr);;

let p = print_endline;;                                                         
let pn s = print_string s; flush stdout;;                                       

let print_pid () =
  p ("Current PID is: " ^ (string_of_int (Unix.getpid () )));;

let getfirstline filename =
  let fd = open_in filename in
  let line = input_line fd in
  close_in fd;
  line;;

let getlines filename =
  let fd = open_in filename in
  let retval = ref [] in
  begin
    try
      while true do
        retval := (input_line fd) :: !retval
      done
    with End_of_file -> ();
  end;
  close_in fd;
  !retval;
;;

let exec_passing_args filename =
  let args = Array.copy Sys.argv in
  Array.set args 0 filename;
  Unix.execvp filename args;;

  