{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Bootloader.Grub where

grub_eltorito env =
    do im "Installing bootloader: Grub raw eltorito (no HD emulation)"
       grub_generic env "initrd /opt/dfsruntime/initrd.dfs"
       return (["-b", "boot/grub/stage2_eltorito", "-no-emul-boot",
                "-boot-load-size", "1", "-boot-info-table"],
               (\_ _ -> return ()))

grub_hd env =
    do im "Installing bootloader: Grub with eltorito HD emulation"
       grub_generic env "initrd /boot/initrd.dfs"
       safeSystem "cp" ["-r", (targetdir env) ++ "/boot", workbootdir]
       safeSystem "rm" ["-f", workbootdir ++ "/grub/stage2_eltorito"]
       safeSystem "cp" [targetdir env ++ "/opt/dfsruntime/initrd.dfs",
                        workbootdir]
       bracketCWD (workdir env) $
          safeSystem "tar" ["-zcpf", "boot.tar.gz", "boot"]
       safeSystem "mkbimage" ["-f", workboottar, "-t", "hd", "-s", "ext2",
                              "-d", workdir]
       rename "hd.image" $ (targetdir env) ++ "/boot/hd.image"
       return (["-b", "boot/hd.image", "-hard-disk-boot", "-c",
                "boot/boot.catalog"],
               (\_ _ -> return ()))
    where workbootdir = (workdir env) ++ "/boot"
          workboottar = (workdir env) ++ "/boot.tar.gz"

grub_generic env entryline =
    do createDirectory (targetdir env ++ "/boot/grub") 0o755
       safeSystem 