(* arch-tag: generic utilities
*)

let wsregexp = Str.regexp "[ \n\t]+";;

let split_ws instr =
  Str.split wsregexp (Strutil.strip instr);;

