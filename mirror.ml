(* arch-tag: Mirroring support
* Copyright (c) 2004 John Goerzen 
*)

open Unix;;
open Printf;;
open Str;;
open Dfsutils;;
open Shellutil;;
open Unixutil;;
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

let mirror_data cp repos target mirrordir  workdir =
  if exists target then (rm ~recursive:true target);
  if not (exists mirrordir) then 
    (mkdir mirrordir 0o755);
  let procrepo repo =
    let sect = "repo " ^ repo in
    let suite = cp#get sect "suite" in
    let mirror = cp#get sect "mirror" in
    let archargs = try ["-a"; cp#get sect "arch"] with Not_found -> [] in
    run "cdebootstrap" (archargs @ ["--debug"; "-v"; "-d"; suite; target; mirror]);
    let cfgfilename = workdir ^ "/apt-move.conf" in
    let cfd = open_out cfgfilename in
    fprintf cfd "LOCALDIR=%s\n" mirrordir;
    fprintf cfd "FILECACHE=%s/var/cache/apt/archives\n" target;
    fprintf cfd "LISTSTATE=%s/var/lib/apt/lists\n" target;
    fprintf cfd "DIST=%s\n" suite;
    try fprintf cfd "ARCH=%s\n" (cp#get sect "arch") with Not_found -> ();
    output_string cfd
    "COPYONLY=yes\nCONTENTS=yes\nAPTSITES=/all/\nPKGCOMP=\"none gzip\"\n";
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
      (*

      let filename = sprintf                                                             "%s/var/lib/apt/lists/debootstrap.invalid_dists_%s_Release" target
          suite in   
      run "cp" ["-v"; filename; sprintf "%s/dists/%s/Release" mirrordir suite];
      *)
      
      if not (exists (sprintf "%s/dists/%s" mirrordir codename))
      then 
        symlink suite (sprintf "%s/dists/%s" mirrordir codename);
    end;
    run "rm" ("-v" :: glob [sprintf "%s/var/lib/apt/lists/*" target]);
    (*
    rm cfgfilename;
    *)

  in
  List.iter procrepo repos;
  (*
  rm ~recursive:true target;
  *)
;;

let mirror_workdir cp suites workdir =
  mirror_data cp suites (workdir ^ "/target") (workdir ^ "/mirror") workdir;;


