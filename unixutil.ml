(* arch-tag: Unix module utilities
* Copyright (c) 2004 John Goerzen
*)

open Unix;;
open Str;;

let exists fn = try ignore (lstat fn); true with error -> false;;

let list_of_dir dirname =
  let dirh = opendir dirname in
  let rec readit () =
    match (try Some (readdir dirh) with End_of_file -> None) with
      Some "." -> readit ()
    | Some ".." -> readit ()
    | Some x -> x :: readit ()
    | None -> []
  in 
  let result = readit () in
  closedir dirh;
  result;;

let fold_directory func firstval dirname =
  List.fold_left func firstval (list_of_dir dirname);;

