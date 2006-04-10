{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Bootloader.Grub where
import Utils
import MissingH.Cmd
import MissingH.Path
import System.Posix.Files
import System.Posix.Directory
import MissingH.Path.Glob
import MissingH.ConfigParser
import MissingH.Path.FilePath

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