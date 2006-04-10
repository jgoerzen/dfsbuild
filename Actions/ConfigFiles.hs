{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions.ConfigFiles where
import Utils
import qualified Actions.Mirror
import System.Posix.Directory
import System.Posix.Files
import MissingH.Str
import MissingH.Cmd
import MissingH.Path
import MissingH.Path.FilePath
import Control.Monad
import MissingH.ConfigParser
import MissingH.IO.HVFS
import System.Time
import Text.Printf

getDate =
    getClockTime >>= toCalendarTime >>= (return . calendarTimeToString)

buildinfo env =
    do datestr <- getDate
       return $ "Item: Debian From Scratch (DFS) live CD made by dfsbuild" ++
                "\nName: " ++ eget env "name" ++
                "\nVersion: " ++ eget env "version" ++
                "\nBuilder: " ++ eget env "builder" ++
                "\nPreparation Date: " ++ datestr ++ "\n"

writeBuildInfo env = 
    buildinfo env >>= 
                  (writeFile ((targetdir env) ++ "/opt/dfsruntime/buildinfo"))

getidstring :: DFSEnv -> IO String
getidstring env = 
    getDate >>= 
    (printf "DFS image: %s %s (%s)" (eget env "name") (eget env "version"))
    
                     
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
    case options (cp env) "symlinks" of
      Left _ -> return ()
      Right files ->
          mapM_ (\from -> do dm $ "Symlinking " ++ from
                             createSymbolicLink (esget env "symlinks" from)
                                                ((targetdir env) ++ from)
                ) files
    case get (cp env) (defaultArch env) "deletefiles" of
      Left _ -> return ()
      Right files ->
          do let delfiles <- mapM glob (splitWs files)
             mapM_ deleteit (map ((targetdir env) ++) $ concat delfiles)
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
       safeSystem "cp" ["-r", cpfiles, targetdir env ++ "/etc/rc2.d/"]
       rmfiles <- glob $ targetdir env ++ "/etc/rc2.d/S*single"
       mapM_ deleteit rmfiles

