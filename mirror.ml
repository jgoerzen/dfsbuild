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

let find_codename target suite =
  p "Scanning for codename...";
  let filename = sprintf
  "%s/var/lib/apt/lists/debootstrap.invalid_dists_%s_Release" target suite in
  let ifd = open_in filename in
  let r = regexp "^Codename: \\([a-zA-Z]+\\)" in
  let retval = ref None in
  try
    while !retval = None do
      let line = input_line ifd in
      if string_match r line 0 then 
        retval := Some (matched_group 1 line)
    done;
    !retval;
  with End_of_file -> !retval;;

let mirror_data suites target mirrordir mirror workdir =
  if is_file_existing_fn target then (rm ~recursive:true target);
  if not (is_file_existing_fn mirrordir) then 
    (mkdir mirrordir 0o755);
  let procsuite suite =
    p "procsuite";
    run "cdebootstrap" ["-d"; suite; target; mirror];
    let cfgfilename = workdir ^ "/apt-move.conf" in
    let cfd = open_out cfgfilename in
    fprintf cfd "LOCALDIR=%s\n" mirrordir;
    fprintf cfd "FILECACHE=%s/var/cache/apt/archives\n" target;
    fprintf cfd "LISTSTATE=%s/var/lib/apt/lists\n" target;
    fprintf cfd "DIST=%s\n" suite;
    output_string cfd "COPYONLY=yes\nCONTENTS=yes\nAPTSITES=/all/\nPKGCOMP=gzip\n";
    Pervasives.close_out cfd;
    run "apt-move" ["-c"; cfgfilename; "update"];
    match find_codename target suite with
      None -> () 
    | Some codename -> begin
      p ("Got codename: " ^ codename);
      run "mkdir" ["-p"; sprintf "%s/dists/%s" mirrordir suite];
      let cfd = open_out (sprintf "%s/dists/%s/.codename" mirrordir suite) in
      output_string cfd (codename ^ "\n");
      Pervasives.close_out cfd;
      run "apt-move" ["-c"; cfgfilename; "update"];
      if not (is_file_existing_fn (sprintf "%s/dists/%s" mirrordir codename))
      then 
        create_symlink suite (sprintf "%s/dists/%s" mirrordir codename);
    end;

  in
  p "mirror_data";
  List.iter procsuite suites;
  (*
  rm cfgfilename;
  rm ~recursive:true target;
  *)
;;

let mirror_workdir suites mirror workdir =
  mirror_data suites (workdir ^ "/target") (workdir ^ "/mirror") mirror workdir;;


