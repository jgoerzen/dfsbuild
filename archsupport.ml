(* arch-tag: Architecture-specific support
* Copyright (c) 2004 John Goerzen *)

open Shellutil;;
open ConfigParser;;
open Strutil;;

let defaultarch = ref "";;

let getarch () =
  if !defaultarch = "" then begin
    defaultarch := strip (run_getstring "dpkg" ["--print-architecture"]);
  end;
  !defaultarch;;

let get (cp:rawConfigParser) = cp#get (getarch ());;

