(* arch-tag: generic testing
*)
open Shellutil;;

List.iter (fun x -> print_endline x) (glob
["/home/jgoerzen/no-backup/foo/target/var/lib/apt/lists/*"]);;

