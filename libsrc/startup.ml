(* arch-tag: boot program, CD post-initrd
*)

open Shellutil;;
open Unix;;
open Dfsutils;;
open Strutil;;

let initrdloc = "/initrd";;
let newrdloc = "/opt/dfsruntime/runtimemnt";;
let newrdfiles = "/opt/dfsruntime/runtimerd";;

let initruntimerd () =
  pn "Creating new runtime ramdisk: ";
  run "mount" ["-t"; "tmpfs"; "none"; newrdloc];
  p newrdloc;

  pn "Populating runtime ramdisk: ";
  run "/bin/bash" ["-c"; "cp -a " ^ newrdfiles ^ "/* " ^ newrdloc ^ "/"];
  p "done.";
;;

let initcfgfiles () =
  pn "Initializing configuration files: /etc/fstab";
  let fd = open_out "/etc/fstab" in
  output_string fd "proc  /proc   proc    defaults        0 0\n";
  close_out fd;
  p "";
;;

let _ =
  p "";
  p " *** Debian From Scratch CD initializing ***";
  print_pid () ;
  Unix.chdir "/";
  p "Umounting bootup initrd.";
  try
    run "umount" ["-n"; initrdloc];
  with Shell.Subprocess_error x -> ();
  initruntimerd ();
  initcfgfiles ();
  p "";
  p " *** Now booting system ***";

  exec_passing_args "/sbin/init";
;;


