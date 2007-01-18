{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Bootloader.Aboot where
import Utils
import System.Cmd.Utils
import System.Path
import System.Posix.Files
import System.Posix.Directory
import System.Path.Glob
import Data.ConfigFile
import System.FilePath
import Text.Printf

aboot env =
    do safeSystem "cp" ["/boot/bootlx", targetdir env ++ "/boot/"]
       newkerns <- glob $ targetdir env ++ "/boot/vmlinu*"
       rdparam <- getrdparam env
       writeFile (targetdir env ++ "/etc/aboot.conf") 
                 (concat . map (kline rdparam) $ zip newkerns [(0::Int)..])
       return ([],
               \isoname -> safeSystem "isomarkboot" 
                             [isoname, "/boot/bootlx", "/boot/initrd.dfs"]
              )
    where kline rdparam (kname, count) =
              printf "%d:boot/%s initrd=boot/initrd.dfs root=/dev/ram0 ide=nodma %s\n" count (fst . splitFileName $ kname) rdparam

                                                     
