{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

import System.IO
import System.Posix.Env
import System.Exit
import System.Cmd
import System.Posix.Directory
import System.Environment
import System.Posix.Process
import Control.Monad

initrdloc = "/initrd"
newrdloc = "/opt/dfsruntime/runtimemnt"
newrdfiles = "/opt/dfsruntime/runtimerd"

im msg = putStrLn msg
fm msg = do putStr msg
            hFlush stdout


initruntimerd =
    do fm "Creating new runtime ramdisk: "
       rawSystem "mount" ["-n", "-t", "tmpfs", "none", newrdloc]
       im newrdloc
       fm "Populating runtime ramdisk: "
       rawSystem "/bin/bash" ["-c", "cp -a " ++ newrdfiles ++ "/* " ++
                                  newrdloc ++ "/"]
       im "done."

initcfgfiles =
    do fm "Initializing configuration files: /etc/fstab"
       writeFile "/etc/fstab" "proc /proc proc defaults 0 0\n"
       im "."


main = 
    do im "\n *** Debian From Scratch CD, format 2, initializing ***"
       changeWorkingDirectory "/"
       initruntimerd
       initcfgfiles
       im "\n *** Now booting system ***"
       args <- getArgs
       executeFile "/sbin/init" False args Nothing
