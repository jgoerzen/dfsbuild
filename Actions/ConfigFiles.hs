{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions.ConfigFiles where
import Utils
import qualified Actions.Mirror
import System.Posix.Directory
import System.Posix.Files
import Data.String.Utils
import System.Cmd.Utils
import System.Path
import System.Path.Glob
import System.FilePath
import Control.Monad
import Data.ConfigFile
import System.IO.HVFS
import System.Time
import Text.Printf

getDate =
    getClockTime >>= toCalendarTime >>= (return . calendarTimeToString)

buildinfo env =
    return $ "This is a Debian From Scratch (DFS) live CD made by dfsbuild" ++
                "\n\nName: " ++ eget env "name" ++
                "\nVersion: " ++ eget env "version" ++
                "\nBuilder: " ++ eget env "builder" ++
                "\nPreparation Date: " ++ datestr env ++ "\n"

writeBuildInfo env = 
    buildinfo env >>= 
                  (writeFile ((targetdir env) ++ "/opt/dfsruntime/buildinfo"))

getidstring :: DFSEnv -> String
getidstring env = 
    printf "DFS: %s %s (%s)" (eget env "name") (eget env "version")
           (datestr env)
    
                     
writeCfgFiles env =
 do bi <- buildinfo env
    writeit appendFile "/etc/issue" ("\n" ++ bi ++ "\n")
    case options (cp env) "appendfiles" of
      Left _ -> return ()
      Right files ->
           mapM_ (\fn -> writeit appendFile fn (esget env "appendfiles" fn))
                 files
    case options (cp env) "createfiles" of
      Left _ -> return ()
      Right files ->
          mapM_ (\fn -> writeit writeFile fn (esget env "createfiles" fn))
                files
    case get (cp env) (defaultArch env) "deletefiles" of
      Left _ -> return ()
      Right files ->
          do delfiles <- mapM glob (map ((targetdir env) ++) (splitWs files))
             mapM_ deleteit $ concat delfiles
    case options (cp env) "symlinks" of
      Left _ -> return ()
      Right files ->
          mapM_ (\from -> do dm $ "Symlinking " ++ from
                             createSymbolicLink (esget env "symlinks" from)
                                                ((targetdir env) ++ from)
                ) files
    case get (cp env) (defaultArch env) "makedirs" of
      Left _ -> return ()
      Right files ->
          mapM_ (\fn -> createDirectory ((targetdir env) ++ fn) 0o755)
                (splitWs files)

 where writeit func fn info =
           func ((targetdir env) ++ fn) (info ++ "\n")

fixRc env =
    do recursiveRemove SystemFS ((targetdir env) ++ "/etc/rc2.d")
       safeSystem "cp" ["-r", targetdir env ++ "/etc/rc1.d",
                        targetdir env ++ "/etc/rc2.d"]
       cpfiles <- glob $ targetdir env ++ "/etc/rc3.d/*logd*"
       safeSystem "cp" $ ["-r"] ++  cpfiles ++ [targetdir env ++ "/etc/rc2.d/"]
       rmfiles <- glob $ targetdir env ++ "/etc/rc2.d/S*single"
       mapM_ deleteit rmfiles


kernelimgconf =
    "do_symlinks = no\n\
\do_bootloader = no\n\
\do_bootfloppy = no\n\
\do_initrd = yes\n\
\warn_initrd = yes\n"
