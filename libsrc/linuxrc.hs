{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

import System.IO
import System.Posix.Env
import System.Exit
import Text.Regex
import System.Cmd
import System.Posix.Directory
import System.Environment
import System.Posix.Process
import Control.Monad

mountloc = "/realroot"
im msg = putStrLn msg
fm msg = do putStr msg
            hFlush stdout

main = 
    do setEnv "PATH" "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" True
       im "\n *** Welcome to Debian From Scratch (DFS) ***"
       im "Initial RAM disk (format 2) booting."
       cddev <- getcddev

       changeWorkingDirectory mountloc
       rawSystem "pivot_root" [".", "initrd"]
       changeWorkingDirectory"."
       im "Passing control to DFS CD..."
       args <- getArgs
       executeFile "/opt/dfsruntime/startup" False args Nothing
       
getcddev  =
    do marker <- readFile "/marker"
       im "Locaing DFS CD..."
       cmdl <- getcmdline marker
       case cmdl of
                Just x -> return x
                Nothing -> do im "Scanning for DFS CD.  The dfscd kernel"
                              im "parameter can override this scan if there"
                              im "is trouble.  Scanning..."
                              devicefile <- readFile "/devices"
                              finddev marker (lines devicefile)

iscd shouldbe = 
              do testmarker <- readFile (mountloc ++ "/opt/dfsruntime/marker")
                 return (shouldbe == testmarker)

canmount loc =
              do ec <- rawSystem "busybox" ["mount", "-n", "-t", "iso9660",
                                            "-o", "ro", loc, mountloc]
                 return $ ec == ExitSuccess

scandev shouldbe dev =
              do fm $ "Scanning " ++ dev ++ ": "
                 cm <- canmount dev
                 if cm
                    then do ic <- iscd shouldbe
                            if ic
                               then do im "Found DFS CD."
                                       return True
                               else do im "Found a CD, but not proper DFS CD."
                                       ec <- rawSystem "busybox" ["umount", mountloc]
                                       when (ec /= ExitSuccess)
                                                (im (show ec))
                                       return False
                    else do im "Invalid device, no media, or not a CD."
                            return False

getcmdline shouldbe =
    do im "Sleeping for 5 seconds to wait for any USB devices."
       rawSystem "busybox" ["sleep", "5"]
       rawSystem "busybox" ["mount", "-n", "-t", "proc", "none", "/proc"]
       clineraw <- readFile "/proc/cmdline"
       let cline = head . lines $ clineraw
       return $ seq cline cline
       rawSystem "busybox" ["umount", "/proc"]
       case matchRegex (mkRegex "dfscd=([^ ]+)") cline of
         Nothing -> return Nothing
         Just [x] -> do fm $ "Scanning user-specified CD device " ++ x ++ "..."
                        sd <- scandev shouldbe x
                        if sd
                           then return (Just x)
                           else return Nothing

finddev _ [] = fail "Could not find a CD.  Terminating."
finddev shouldbe (x:xs) = 
    do sd <- scandev shouldbe ("/dev/" ++ x)
       if sd
          then return $ "/dev/" ++ x
          else finddev shouldbe xs
