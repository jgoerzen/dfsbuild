(* arch-tag: boot program for initrd
*)

open Shellutil;;
open Unix;;
open Dfsutils;;
open Strutil;;
open Str;;

putenv "PATH" "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";;
let mountloc = "/realroot";;

let getcddev () =
  let marker = strip (getfirstline "/marker") in

  let iscd () =
    try
      let testmarker = strip (getfirstline (mountloc ^ "/opt/dfsruntime/marker"))
      in
      marker = testmarker
    with Sys_error x ->
      false
  in

  let canmount loc =
    try
      runnoout "busybox" ["mount"; "-n"; "-t"; "iso9660"; "-o"; "ro"; loc; mountloc];
      true;
    with Shell.Subprocess_error x ->
      false;
  in

  let scandev dev =
    pn ("Scanning " ^ dev ^ ": ");
    if canmount dev then begin
      if iscd () then begin
        p "Found DFS CD.";
        true
      end else begin
        p "Found a CD, but not proper DFS CD.";
        run "busybox" ["umount"; mountloc];
        false;
      end
    end else begin
      p "Invalid device, no media, or not a CD.";
      false;
    end
  in

  let getcdcmdline () =
    run "busybox" ["mount"; "-n"; "-t"; "proc"; "none"; "/proc"];
    let cline = getfirstline "/proc/cmdline" in
    run "busybox" ["umount"; "/proc"];
    let r = regexp "dfscd=\\([^ ]+\\)" in
    if string_match r cline 0 then begin
      let dev = matched_group 1 cline in
      p ("Scanning user-specified CD device " ^ dev ^ "...");
      if scandev dev then Some dev else None;
    end else None
  in

  let rec finddev dl = match dl with
    [] -> p "Could not find a CD.  Terminating."; exit 2; ""
  | x::xs -> if scandev ("/dev/" ^ x) then "/dev/" ^ x else finddev xs in

  p "Locating DFS CD...";
  if getcdcmdline () = None then begin
    p "Scanning for DFS CD.  The dfscd kernel parameter can override";
    p "this scan if there is trouble.  Scanning...";
    let devices = List.map (fun x -> strip x) (getlines "/devices") in
    ignore (finddev devices);
  end;
;;

let _ =
  p "Welcome to Debian From Scratch.";
  p "Initial RAM disk booting.";
  let cddev = getcddev () in

  Unix.chdir mountloc;
  run "pivot_root" ["."; "initrd"];
  Unix.chroot ".";
  p "Passing control to DFS CD...";
  exec_passing_args "/opt/dfsruntime/startup";
;;


