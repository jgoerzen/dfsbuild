(* arch-tag: Grub support
* Copyright (c) 2004 John Goerzen
*)

open Unix;;
open Shellutil;;
open Dfsutils;;
open Archsupport;;

let grub_generic cp target entryline =
  mkdir (target ^ "/boot/grub") 0o755;
  run "cp" ("-rv" :: glob ["/usr/lib/grub/*/*"] @ [target ^ "/boot/grub/"]);
  let sd = open_out (target ^ "/boot/grub/menu.lst") in
  if cp#has_option (getarch()) "grubconfig" then begin
    output_string sd (get cp "grubconfig");
    output_string sd "\n";
  end;
  output_string sd "color cyan/blue blue/light-gray\n";
  let newkerns = glob [target ^ "/boot/vmlinu*"] in
  let os s = output_string sd (s ^ "\n") in
  let fake s = os ("title " ^ s ^ "\ncolor cyan/blue blue/light-gray") in
  List.iter (fun x ->
    os ("title  Boot " ^ (Filename.basename x));
    os ("kernel /boot/" ^ (Filename.basename x) ^ " root=/dev/ram0 " ^ \
      (Bootloaderutil.getrdparam target));
    os (entryline);
    os ("boot\n");
  ) newkerns;

  (*
  fake ".";
  os ("title Help/Information Menu\nconfigfile /boot/grub/help.lst\n");
  *)
  fake ".";
  fake (Configfiles.getidstring cp);

  Pervasives.close_out sd;

  let sd2 = open_out (target ^ "/boot/grub/help.lst") in
  output_string sd2 "color cyan/black blue/light-gray
pager on
title Basic Booting Info
cat /opt/dfsruntime/dfs.html/booting.html.txt

title Selecting CD-ROM device
cat /opt/dfsruntime/dfs.html/dfsbood-selcd.html.txt

title About This CD
cat /opt/dfsruntime/buildinfo

title .
color cyan/black blue/light-gray

title Return to main menu...
configfile /boot/grub/menu.lst
";
  Pervasives.close_out sd2;;
  
let grub_eltorito cp target =
  grub_generic cp target "initrd /opt/dfsruntime/initrd.dfs";
  installrd_cramfs target;
  (["-b"; "boot/grub/stage2_eltorito"; "-no-emul-boot";
   "-boot-load-size"; "1"; "-boot-info-table"],
   fun cp wdir target isoname -> ());;

let grub_hd cp workdir target =
  grub_generic cp target "initrd /boot/initrd.dfs";
  installrd_cramfs target;
  let workbootdir = workdir ^ "/boot" in
  let workboottar = workdir ^ "/boot.tar.gz" in
  run "cp" ["-r"; target ^ "/boot"; workbootdir];
  rm ~force:true (workbootdir ^ "/grub/stage2_eltorito");
  run "cp" [target ^ "/opt/dfsruntime/initrd.dfs"; workbootdir];
  run "sh" ["-c"; "cd " ^ workdir ^ "; tar -zcpf boot.tar.gz boot"];
  run "mkbimage" ["-f"; workboottar; "-t"; "hd"; "-s"; "ext2"; "-d"; workdir];
  Unix.rename "hd.image" (target ^ "/boot/hd.image");
  (["-b"; "boot/hd.image"; "-hard-disk-boot"; "-c"; "boot/boot.catalog"],
   fun cp wdir target isoname -> ());;
