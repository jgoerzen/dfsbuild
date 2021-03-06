{- dfsbuilt Utilities
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Utils where
import System.Random
import System.Log.Logger
import System.Time
import System.Directory
import Control.Monad
import Text.Printf
import Control.Exception
import System.Posix.Files
import Data.String.Utils
import Data.List.Utils
import Data.Either.Utils
import Data.ConfigFile
import System.Cmd.Utils
import System.IO.Unsafe
import System.IO.Error
import Text.Regex
import System.FilePath
import System.Directory(doesFileExist)

data DFSEnv = DFSEnv 
    {wdir :: String,
     libdir :: String,
     cp :: ConfigParser,
     isDebugging :: Bool,
     defaultArch :: String,
     targetdir :: String,
     marker :: String,
     datestr :: String}

data DFSState = Fresh | Initialized | Mirrored | Bootstrapped | EnvironmentPrepared
              | Installed | LibsInstalled | DebsInstalled | CfgHandled | InitPrepped 
              | RDPrepped
              | KernelsInstalled | EnvironmentCleaned | RamdiskBuilt | BootloaderInstalled
              deriving (Eq, Show, Read, Ord)

im = infoM "dfs"
wm = warningM "dfs"
dm = debugM "dfs"

getUniqueCDID :: IO String
getUniqueCDID = 
    do t <- getClockTime
       random1 <- randomIO
       random2 <- randomIO
       return $ printf "DFS CD IMAGE, format 2, ID: %d,%d,%d\n"
                ((\(TOD x _) -> x) t) (random1::Int) (random2::Int)

getDefaultArch = 
    do (ph, iarchstr) <- pipeFrom "dpkg" ["--print-architecture"]
       let archstr = (seqList (strip iarchstr))
       forceSuccess ph
       return archstr

eget :: DFSEnv -> String -> String
eget env opt = forceEither $ get (cp env) (defaultArch env) opt

esget :: DFSEnv -> String -> String -> String
esget env s o = forceEither $ get (cp env) s o

egetbool :: DFSEnv -> String -> Bool
egetbool env opt = forceEither $ get (cp env) (defaultArch env) opt

saveState :: DFSEnv -> DFSState -> IO ()
saveState env state =
    writeFile ((wdir env) ++ "/state") (show state)

getState :: DFSEnv -> IO DFSState
getState env =
    do st <- readFile ((wdir env) ++ "/state")
       return (read st)

getCodeName :: FilePath -> IO String
getCodeName fp =
    do c_old <- System.IO.Error.catch (readFile (fp ++ "Release"))
	       (\e -> if System.IO.Error.isDoesNotExistError e then return "" else ioError e)
       c_new <- System.IO.Error.catch (readFile (fp ++ "_dists_._Release"))
	       (\e -> if System.IO.Error.isDoesNotExistError e then return "" else ioError e)
       c <- if length(c_old) > 0 then return c_old else return c_new   
       let cr = mkRegex "Codename: ([a-z]+)"
       case matchRegex cr c of
         Just [cn] -> return cn
         x -> fail $ "Error finding Codename: " ++ show x

deleteit :: FilePath -> IO ()
deleteit fn =
    do dm $ "Deleting: " ++ fn
       handle delfile
              (removeLink fn)
    where 
      delfile ex =
       handle deldirrec
              (removeFile fn)
      deldirrec ex =
       handle (\e -> wm ("Delete failed: " ++ show e)) 
        (do
         files <- getDirectoryContents fn
         (foldM doDelete () files)
         )
      doDelete _ "." = return ()
      doDelete _ ".." = return ()
      doDelete _ fp = deleteit (fn ++ "/" ++ fp)


enumfiles :: FilePath -> IO [FilePath]
enumfiles fn =
    do
      children <- getDirectoryContents fn
      erest children
  where erest ch =
            do
              stat <- mapM (\x -> System.IO.Error.try (getSymbolicLinkStatus x)) children2
              let directories = filter directoryAndNotSymlink (zip children2 stat)
              (liftM (++ children2)) ((liftM (foldr (++) [])) (mapM enumfiles 
                                                (fst (unzip directories))))
                  where
                    children2 = [fn ++ "/" ++ x | x <- ch, not (x == ".." || x==".")]
                    directoryAndNotSymlink (_, Right stat) = 
                        (isDirectory stat && not (isSymbolicLink stat))
                    directoryAndNotSymlink (_, _) = False
      
      
getrdsize_kb env =
    do st <- getFileStatus $ targetdir env ++ "/boot/initrd.dfs"
       return $ ((fileSize st) `div` 1024) + 1

getrdparam env =
    do kb <- getrdsize_kb env
       return $ if kb < 4096
                   then " "
                   else " ramdisk_size=" ++ show kb ++ " "

getinitrdname env kernpath =
    do dfe <- doesFileExist (targetdir env ++ "/boot/" ++ rdname)
       if dfe
          then return rdname
          else return "initrd.dfs"
    where kname = snd . splitFileName $ kernpath
          rdname = subRegex (mkRegex "vmlinu.") kname "initrd.img"

getrootdevname env kernpath =
    do initrd <- getinitrdname env kernpath
       if initrd == "initrd.dfs"
          then return "/dev/ram0"
          else return "/dev/root"
