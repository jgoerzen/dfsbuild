(* arch-tag: Mirroring support
* Copyright (c) 2004 John Goerzen 
*)

open Unix;;
open Printf;;
open Str;;
open Cash;;
open Dfsutils;;
open Cashutil;;
open Printf;;

let mirror_data suite target mirrordir mirror workdir =
  if is_file_existing_fn target then (rm ~recursive:true target);
  if not (is_file_existing_fn mirrordir) then 
    (mkdir mirrordir 0o755);
  run "cdebootstrap" ["-d"; suite; target; mirror];
  let cfgfilename = workdir ^ "/apt-move.conf" in
  let cfd = open_out cfgfilename in
  fprintf cfd "LOCALDIR=%s\n" mirrordir;
  fprintf cfd "FILECACHE=%s/var/cache/apt/archives\n" target;
  fprintf cfd "LISTSTATE=%s/var/lib/apt/lists\n" target;
  fprintf cfd "DIST=%s\n" suite;
  output_string cfd "CONTENTS=yes\nAPTSITES=/all/\nPKGCOMP=gzip\n";
  close_out cfd;
  run "apt-move" ["-c"; cfgfilename; "update"];
  rm cfgfilename;
  rm ~recursive:true target;
;;

let mirror_workdir suite mirror workdir =
  mirror_data suite (workdir ^ "/target") (workdir ^ "/mirror") mirror workdir;;


