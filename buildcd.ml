(* arch-tag: Main entrance for CD builer
* Copyright (c) 2004 John Goerzen 
*)

open Shell;;

let opencf () =
  let cffile = ref "" in
  let args = [(("-c":Arg.key), Arg.Set_string cffile, 
              ("Configuration file":Arg.doc))] in
  let usage = "Usage: buildcd -c cf" in
  Arg.parse args (fun x -> ()) usage;
  if !cffile = "" then begin
   Arg.usage args usage;
   exit 1;
   (new ConfigParser.rawConfigParser);
  end else (
    let cp = new ConfigParser.rawConfigParser in
    cp#readfile !cffile;
    cp);;
   
let cp = opencf ();;

print_endline (cp#to_string);;

