(* arch-tag: write config files
*)

let writecfgfiles basedir =
  let w fn s =
    let outfd = open_out (basedir ^ fn) in
    output_string outfd s;
    close_out outfd
  in
  let a fn s =
    let outfd = open_out_gen [Open_append; Open_wronly; Open_creat] 0o644 fn in
    output_string outfd s;
    close_out outfd
  in
  a "/etc/issue" "
Welcome to Debian From Scratch (DFS)

To login, supply username \"root\" and just press Enter if asked for a
password.
";

  a "/root/.bashrc" "
cat <<EOF
Welcome to Debian From Scratch (DFS)

Here are some useful commands:

Command                     Description
--------------------------- -------------------------------------------------
dfshelp                     Access primary DFS documentation.  Describes
                            network configuration, Debian installation,
                            more system usage.
reboot                      Reboot the system
/etc/init.d/lvm2 start      Initialize LVM subsystem
/etc/init.d/discover start  Autodetect hardware
/etc/init.d/hotplug start   Initialize support for USB devices
/etc/init.d/pcmcia start    Initialize PCMCIA subsystem
nano, vim, joe, or emacs    Text editors
EOF
"
  i "/etc/hostname" "dfs\n";
;;

