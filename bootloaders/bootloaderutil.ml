(* arch-tag: boot load utilities
*  Copyright (c) 2004 John Goerzen *)

open Unix;;
open Shellutil;;
open Dfsutils;;
open Archsupport;;

let getrdsize_kb target =
  let bytes = (Unix.stat (target ^ "/opt/dfsruntime/initrd.dfs")).st_size
  in
  bytes / 1024 + 1;; 

let getrdparam target =
  let kb = getrdsize_kb target in
    if kb < 4096 then " " else
            " ramdisk_size=" ^ (string_of_int kb) ^ " ";;

