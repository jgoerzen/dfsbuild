{- dfsbuilt Utilities
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Utils where
import System.Random
import MissingH.Logging.Logger
import System.Time
import Text.Printf
import Control.Exception
import System.Posix.Files
import MissingH.Str
import MissingH.List
import MissingH.Either
import MissingH.ConfigParser
import MissingH.Cmd
import System.IO.Unsafe
import Text.Regex
import MissingH.Path.FilePath
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

data DFSState = Fresh | Initialized | Mirrored | Bootstrapped | Installed
              | LibsInstalled | DebsInstalled | CfgHandled | InitPrepped 
              | RDPrepped
              | KernelsInstalled | RamdiskBuilt | BootloaderInstalled
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
    do c <- readFile fp
       let cr = mkRegex "Codename: ([a-z]+)"
       case matchRegex cr c of
         Just [cn] -> return cn
         x -> fail $ "Error finding Codename: " ++ show x

deleteit fn =
    do dm $ "Deleting: " ++ fn
       handle (\e -> wm ("Delete failed: " ++ show e)) 
              (removeLink fn)

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
