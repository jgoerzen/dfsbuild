(* arch-tag: boot program for initrd
*)

open Shellutil;;
open Unix;;
open Dfsutils;;
open Strutil;;

putenv "PATH" "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin";;
let mountloc = "/realroot";;

let getcddev () =
  let marker = strip (getfirstline "/marker") in
  let scandev x =
    let dev = "/dev/" ^ x in
    let canmount loc =
      try
        runnoout "busybox" ["mount"; "-n"; "-t"; "iso9660"; "-o"; "ro"; loc; mountloc];
        true;
      with Shell.Subprocess_error x ->
        false;
    in
    let iscd () =
      try
        let testmarker = strip (getfirstline (mountloc ^ "/opt/dfsruntime/marker"))
        in
        marker = testmarker
      with Sys_error x ->
        false
    in
    pn ("Scanning " ^ dev ^ ": ");
    if canmount dev then begin
      if iscd () then begin
        p "Found DFS CD.";
        true
      end else begin
        p "Found a CD, but not proper DFS CD.";
        run "busybox" ["umount"; "-n"; mountloc];
        false;
      end
    end else begin
      p "Invalid device, no media, or not a CD.";
      false;
    end
  in
  let rec finddev dl = match dl with
    [] -> p "Could not find a CD.  Terminating."; exit 2; ""
  | x::xs -> if scandev x then "/dev/" ^ x else finddev xs in
  let devices = List.map (fun x -> strip x) (getlines "/devices") in
  finddev devices;;

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


