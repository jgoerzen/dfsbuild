{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Bootloader.Yaboot where
import Utils
import MissingH.Cmd
import MissingH.Path
import System.Posix.Files
import System.Posix.Directory
import MissingH.Path.Glob
import MissingH.ConfigParser
import MissingH.Path.FilePath

yaboot env =
    do safeSystem "cp" ["/usr/lib/yaboot/yaboot", targetdir env ++ "/boot/"]
       writeFile (wdir env ++ "/hfs.map") hfsmap
       writeFile (targetdir env ++ "/boot/ofboot.b") ofboot
       newkerns <- glob $ targetdir env ++ "/boot/vmlinu*"
       rdparam <- getrdparam env
       rdsize <- getrdsize_kb env
       writeFile (targetdir env ++ "/boot/yaboot.conf") (yabootconf newkerns rdsize rdparam)
       return (["--netatalk", "-hfs", "-probe", "-hfs-unlock", "-part",
                "-no-desktop", "-map", wdir env ++ "/hfs.map",
                "-hfs-bless", targetdir env ++ "/boot",
                "-hfs-volid", "DFS/PPC"],
               postbuild)
    where postbuild isoname =
              do safeSystem "hmount" [isoname]
                 safeSystem "hattrib" ["-b", ":boot"]
                 safeSystem "humount" []
          yabootconf klist rdsize rdparam =
              "## This yaboot.conf is for CD booting only.  Do not use as reference.\n" ++
              "debice=cd:\n" ++
              concat (map (yabootitem rdsize rdparam . fst . splitFileName) klist)
          yabootitem rdsize rdparam kern =
              "image=/boot/" ++ kern ++ "\n" ++
              " label=" ++ kern ++ "\n" ++
              " initrd=/boot/initrd.dfs\n" ++
              " initrd-size=" ++ show rdsize ++ "\n" ++
              " append=\"initrd=/boot/initrd.dfs root=/dev/ram0 " ++ rdparam ++ "\"\n" ++
              " read-only\n\n" ++
              "# wonky fb\nimage=/boot/" ++ kern ++ "\n" ++ 
              " label=" ++ kern ++ "-safe\n" ++
              " initrd=/boot/initrd.dfs\n" ++
              " initrd-size=" ++ show rdsize ++ "\n" ++
              " append=\"video=ofonly initrd=/boot/initrd.dfs root=/dev/ram0 " ++ rdparam ++ "\"\n" ++
              " read-only\n\n"
              


hfsmap = "# ext.  xlate  creator  type    comment\n\
\.b      Raw    'UNIX'   'tbxi'  \"bootstrap\"\n\
\yaboot  Raw    'UNIX'   'boot'  \"bootstrap\"\n\
\.conf   Raw    'UNIX'   'conf'  \"bootstrap\"\n\
\*       Ascii  '????'   '????'  \"Text file\"\n";

ofboot = "<CHRP-BOOT>\n\
\<COMPATIBLE>\n\
\MacRISC MacRISC3 MacRISC4\n\
\</COMPATIBLE>\n\
\<DESCRIPTION>\n\
\GNU/Linux PPC bootloader\n\
\</DESCRIPTION>\n\
\<BOOT-SCRIPT>\n\
\\" screen\" output\n\
\load-base release-load-area\n\
\boot cd:,\\boot\\yaboot\n\
\</BOOT-SCRIPT>\n\
\<OS-BADGE-ICONS>\n\
\1010\n\
\000000000000F8FEACF6000000000000\n\
\0000000000F5FFFFFEFEF50000000000\n\
\00000000002BFAFEFAFCF70000000000\n\
\0000000000F65D5857812B0000000000\n\
\0000000000F5350B2F88560000000000\n\
\0000000000F6335708F8FE0000000000\n\
\00000000005600F600F5FD8100000000\n\
\00000000F9F8000000F5FAFFF8000000\n\
\000000008100F5F50000F6FEFE000000\n\
\000000F8F700F500F50000FCFFF70000\n\
\00000088F70000F50000F5FCFF2B0000\n\
\0000002F582A00F5000008ADE02C0000\n\
\00090B0A35A62B0000002D3B350A0000\n\
\000A0A0B0B3BF60000505E0B0A0B0A00\n\
\002E350B0B2F87FAFCF45F0B2E090000\n\
\00000007335FF82BF72B575907000000\n\
\000000000000ACFFFF81000000000000\n\
\000000000081FFFFFFFF810000000000\n\
\0000000000FBFFFFFFFFAC0000000000\n\
\000000000081DFDFDFFFFB0000000000\n\
\000000000081DD5F83FFFD0000000000\n\
\000000000081DDDF5EACFF0000000000\n\
\0000000000FDF981F981FFFF00000000\n\
\00000000FFACF9F9F981FFFFAC000000\n\
\00000000FFF98181F9F981FFFF000000\n\
\000000ACACF981F981F9F9FFFFAC0000\n\
\000000FFACF9F981F9F981FFFFFB0000\n\
\00000083DFFBF981F9F95EFFFFFC0000\n\
\005F5F5FDDFFFBF9F9F983DDDD5F0000\n\
\005F5F5F5FDD81F9F9E7DF5F5F5F5F00\n\
\0083DD5F5F83FFFFFFFFDF5F835F0000\n\
\000000FBDDDFACFBACFBDFDFFB000000\n\
\000000000000FFFFFFFF000000000000\n\
\0000000000FFFFFFFFFFFF0000000000\n\
\0000000000FFFFFFFFFFFF0000000000\n\
\0000000000FFFFFFFFFFFF0000000000\n\
\0000000000FFFFFFFFFFFF0000000000\n\
\0000000000FFFFFFFFFFFF0000000000\n\
\0000000000FFFFFFFFFFFFFF00000000\n\
\00000000FFFFFFFFFFFFFFFFFF000000\n\
\00000000FFFFFFFFFFFFFFFFFF000000\n\
\000000FFFFFFFFFFFFFFFFFFFFFF0000\n\
\000000FFFFFFFFFFFFFFFFFFFFFF0000\n\
\000000FFFFFFFFFFFFFFFFFFFFFF0000\n\
\00FFFFFFFFFFFFFFFFFFFFFFFFFF0000\n\
\00FFFFFFFFFFFFFFFFFFFFFFFFFFFF00\n\
\00FFFFFFFFFFFFFFFFFFFFFFFFFF0000\n\
\000000FFFFFFFFFFFFFFFFFFFF000000\n\
\</OS-BADGE-ICONS>\n\
\</CHRP-BOOT>\n"


grub_eltorito env =
    do im "Installing bootloader: Grub raw eltorito (no HD emulation)"
       grub_generic env "initrd /opt/dfsruntime/initrd.dfs"
       return (["-b", "boot/grub/stage2_eltorito", "-no-emul-boot",
                "-boot-load-size", "1", "-boot-info-table"],
               (\_ -> return ()))

grub_hd env =
    do im "Installing bootloader: Grub with eltorito HD emulation"
       grub_generic env "initrd /boot/initrd.dfs"
       safeSystem "cp" ["-r", (targetdir env) ++ "/boot", workbootdir]
       safeSystem "rm" ["-f", workbootdir ++ "/grub/stage2_eltorito"]
       safeSystem "cp" [targetdir env ++ "/opt/dfsruntime/initrd.dfs",
                        workbootdir]
       bracketCWD (wdir env) $
          safeSystem "tar" ["-zcpf", "boot.tar.gz", "boot"]
       safeSystem "mkbimage" ["-f", workboottar, "-t", "hd", "-s", "ext2",
                              "-d", wdir env]
       rename "hd.image" $ (targetdir env) ++ "/boot/hd.image"
       return (["-b", "boot/hd.image", "-hard-disk-boot", "-c",
                "boot/boot.catalog"],
               (\_ -> return ()))
    where workbootdir = (wdir env) ++ "/boot"
          workboottar = (wdir env) ++ "/boot.tar.gz"

grub_generic env entryline =
    do createDirectory (targetdir env ++ "/boot/grub") 0o755
       grubfiles <- glob "/lib/grub/*/*"
       safeSystem "cp" $ ["-rv"] ++ grubfiles ++ [targetdir env ++ "/boot/grub/"]
       menuText <- grubMenu env entryline
       writeFile (targetdir env ++ "/boot/grub/menu.lst") menuText

       -- Help text not presently references
       writeFile (targetdir env ++ "/boot/grub/help.lst") helpText
       

grubMenu env entryline =
    do newkerns <- glob $ targetdir env ++ "/boot/vmlinu*"
       rd <- getrdparam env
       return $ 
          case get (cp env) (defaultArch env) "grubconfig" of
            Left _ -> ""
            Right line -> line ++ "\n"
          ++ "color cyan/blue blue/light-gray\n"
          ++ (concat . map (kern rd) $ (reverse newkerns))
          ++ fake "."
          ++ fake (marker env)
    where fake s = "title " ++ s ++ "\ncolor cyan/blue blue/light-gray\n"
          kern rd x = "title  Boot " ++ (snd . splitFileName $ x) ++ "\n"
                   ++ "kernel /boot/" ++ (snd . splitFileName $ x) ++ " root=/dev/ram0 " ++
                   rd ++ "\n"
                   ++ entryline ++ "\n"
                   ++ "boot\n"

helpText = "pager on\n\
\title Basic Booting Info\n\
\cat /opt/dfsruntime/dfs.html/booting.html.txt\n\
\ \n\
\title Selecting CD-ROM device\n\
\cat /opt/dfsruntime/dfs.html/dfsboot-selcd.html.txt\n\
\ \n\
\title About This CD\n\
\cat /opt/dfsruntime/buildinfo\n\
\ \n\
\title .\n\
\color cyan/black blue/light-gray\n\
\ \n\
\title Return to main menu...\n\
\configfile /boot/grub/menu.lst\n"