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
import Data.List
import Actions.ConfigFiles

grub_eltorito env =
    do im "Installing bootloader: Grub raw eltorito (no HD emulation)"
       grub_generic env
       return (["-b", "boot/grub/stage2_eltorito", "-no-emul-boot",
                "-boot-load-size", "1", "-boot-info-table"],
               (\_ -> return ()))

grub_hd env =
    do im "Installing bootloader: Grub with eltorito HD emulation"
       grub_generic env
       safeSystem "cp" ["-r", (targetdir env) ++ "/boot", workbootdir]
       safeSystem "rm" ["-f", workbootdir ++ "/grub/stage2_eltorito"]
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

grub_generic env =
    do createDirectory (targetdir env ++ "/boot/grub") 0o755
	   -- since etch (Debian 4.0) grub files are located in /usr/lib instead of /lib
       grubfiles_pre_etch <- glob "/lib/grub/*/*"
       grubfiles_since_etch <- glob "/usr/lib/grub/*/*"
       safeSystem "cp" $ ["-rv"] ++ grubfiles_pre_etch ++ grubfiles_since_etch ++
         [targetdir env ++ "/boot/grub/"]
       menuText <- grubMenu env
       writeFile (targetdir env ++ "/boot/grub/menu.lst") menuText

       -- Help text not presently references
       writeFile (targetdir env ++ "/boot/grub/help.lst") helpText
       

grubMenu env  =
    do newkerns <- glob $ targetdir env ++ "/boot/vmlinu*"
       kerntext <- mapM kern (sort $ newkerns)
       return $ 
          case get (cp env) (defaultArch env) "grubconfig" of
            Left _ -> ""
            Right line -> line ++ "\n"
          ++ "color cyan/blue blue/light-gray\n"
          ++ (concat kerntext)
          ++ fake "."
          ++ fake (getidstring env)
    where fake s = "title " ++ s ++ "\ncolor cyan/blue blue/light-gray\n"
          kern x = do initrd <- getinitrdname env x
                      rootdev <- getrootdevname env x
                      return $ 
                        "title  Boot " ++ (snd . splitFileName $ x) ++ "\n"
                        ++ "kernel /boot/" ++ (snd . splitFileName $ x) ++ 
                               " root=" ++ rootdev ++ "\n"
                        ++ "initrd /boot/" ++ initrd ++ "\n"
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