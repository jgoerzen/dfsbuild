(* arch-tag: Powerpc yaboot support
* Copyright (c) 2004 John Goerzen, Robert Jordens
*)

open Unix;;
open Shellutil;;
open Dfsutils;;
open Archsupport;;
open Bootloaderutil;;

let yaboot cp workdir target =
  run "cp" ["/usr/lib/yaboot/yaboot"; target ^ "/boot/"];
  installrd_cramfs target;
  let sd = open_out (target ^ "/boot/yaboot.conf") in
  let hfsmap = open_out (workdir ^ "/hfs.map") in
  let ofboot = open_out (target ^ "/boot/ofboot.b") in
  let newkerns = glob [target ^ "/boot/vmlinu*"] in
  output_string hfsmap 
"# ext.  xlate  creator  type    comment
.b      Raw    'UNIX'   'tbxi'  \"bootstrap\"
yaboot  Raw    'UNIX'   'boot'  \"bootstrap\"
.conf   Raw    'UNIX'   'conf'  \"bootstrap\"
*       Ascii  '????'   '????'  \"Text file\"
";
  Pervasives.close_out hfsmap;
  output_string ofboot
"<CHRP-BOOT>
<COMPATIBLE>
MacRISC MacRISC3 MacRISC4
</COMPATIBLE>
<DESCRIPTION>
GNU/Linux PPC bootloader
</DESCRIPTION>
<BOOT-SCRIPT>
\" screen\" output
load-base release-load-area
boot cd:,\\boot\\yaboot
</BOOT-SCRIPT>
<OS-BADGE-ICONS>
1010
000000000000F8FEACF6000000000000
0000000000F5FFFFFEFEF50000000000
00000000002BFAFEFAFCF70000000000
0000000000F65D5857812B0000000000
0000000000F5350B2F88560000000000
0000000000F6335708F8FE0000000000
00000000005600F600F5FD8100000000
00000000F9F8000000F5FAFFF8000000
000000008100F5F50000F6FEFE000000
000000F8F700F500F50000FCFFF70000
00000088F70000F50000F5FCFF2B0000
0000002F582A00F5000008ADE02C0000
00090B0A35A62B0000002D3B350A0000
000A0A0B0B3BF60000505E0B0A0B0A00
002E350B0B2F87FAFCF45F0B2E090000
00000007335FF82BF72B575907000000
000000000000ACFFFF81000000000000
000000000081FFFFFFFF810000000000
0000000000FBFFFFFFFFAC0000000000
000000000081DFDFDFFFFB0000000000
000000000081DD5F83FFFD0000000000
000000000081DDDF5EACFF0000000000
0000000000FDF981F981FFFF00000000
00000000FFACF9F9F981FFFFAC000000
00000000FFF98181F9F981FFFF000000
000000ACACF981F981F9F9FFFFAC0000
000000FFACF9F981F9F981FFFFFB0000
00000083DFFBF981F9F95EFFFFFC0000
005F5F5FDDFFFBF9F9F983DDDD5F0000
005F5F5F5FDD81F9F9E7DF5F5F5F5F00
0083DD5F5F83FFFFFFFFDF5F835F0000
000000FBDDDFACFBACFBDFDFFB000000
000000000000FFFFFFFF000000000000
0000000000FFFFFFFFFFFF0000000000
0000000000FFFFFFFFFFFF0000000000
0000000000FFFFFFFFFFFF0000000000
0000000000FFFFFFFFFFFF0000000000
0000000000FFFFFFFFFFFF0000000000
0000000000FFFFFFFFFFFFFF00000000
00000000FFFFFFFFFFFFFFFFFF000000
00000000FFFFFFFFFFFFFFFFFF000000
000000FFFFFFFFFFFFFFFFFFFFFF0000
000000FFFFFFFFFFFFFFFFFFFFFF0000
000000FFFFFFFFFFFFFFFFFFFFFF0000
00FFFFFFFFFFFFFFFFFFFFFFFFFF0000
00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00
00FFFFFFFFFFFFFFFFFFFFFFFFFF0000
000000FFFFFFFFFFFFFFFFFFFF000000
</OS-BADGE-ICONS>
</CHRP-BOOT>
";
  Pervasives.close_out ofboot;
  let os s = output_string sd (s ^ "\n") in
  os 
"## This yaboot.conf is for CD booting only, do not use as reference.
device=cd:
";
  List.iter (fun kern ->
    os ("image=/boot/" ^ (Filename.basename kern));
    Printf.fprintf sd 
"	label=%s
	initrd=/opt/dfsruntime/initrd.dfs
	initrd-size=%d
	append=\"initrd=/opt/dfsruntime/initrd.dfs root=/dev/ram0 %s\"
	read-only

" (Filename.basename kern) (getrdsize_kb target) (getrdparam target);
    os ("# wonky fb\nimage=/boot/" ^ (Filename.basename kern));
    Printf.fprintf sd 
"	label=%s-safe
	initrd=/opt/dfsruntime/initrd.dfs
	initrd-size=%d
	append=\"video=ofonly initrd=/opt/dfsruntime/initrd.dfs root=/dev/ram0 %s\"
	read-only

" (Filename.basename kern) (getrdsize_kb target) (getrdparam target);
    (* run "mkvmlinuz" ["-v"; "-a"; "chrp";
      "-o"; target ^ "/boot/" ^ (Filename.basename kern) ^ "-chrp";
      "-k"; kern;
      "-d"; target ^ "/usr/lib/kernel-image-" ^ version;
      "-i"; "/opt/dfsruntime/initrd.dfs"]; 
    *)
    ) newkerns;
  Pervasives.close_out sd;
  (["--netatalk"; "-hfs"; "-probe"; "-hfs-unlock"; "-part"; "-no-desktop";
   "-map"; workdir ^ "/hfs.map"; 
   "-hfs-bless"; target ^ "/boot"; 
   "-hfs-volid"; "DFS/PPC"], 
   (* fail-safe blessing: mkisofs doesnt seem to bless sucessfully,
   leaving the image --hfs-unlock'ed and the blessing it with hfsutils *)
   (fun cp wdir target isoname ->
     run "hmount" [isoname];
     run "hattrib" ["-b"; ":boot"];
     run "humount" [];)
   );;
