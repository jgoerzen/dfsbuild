(* arch-tag: Bootload setup
* Copyright (c) 2004 John Goerzen
*)

open Unix;;
open Shellutil;;
open Dfsutils;;
open Archsupport;;

(** Returns a tuple of (mkisofsargs, post-mkisofsfunc) *)
let install cp workdir target =
  match get cp "bootloader" with
  "grub-no-emul" -> Grub.grub_eltorito cp target
  | "grub-hd" -> Grub.grub_hd cp workdir target
  | "aboot" -> Aboot.aboot cp target
  | "yaboot" -> Yaboot.yaboot cp workdir target
  | _ -> ( p("Invalid bootloader specified"); exit 2; ([], fun _ _ _ _ -> ()))
;;
