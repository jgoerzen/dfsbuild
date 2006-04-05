{- dfsbuilt Utilities
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Utils where
import System.Random
import MissingH.Logging.Logger
import System.Time
import Text.Printf
import MissingH.Str
import MissingH.Either
import MissingH.ConfigParser

data DFSEnv = DFSEnv 
    {wdir :: String,
     libdir :: String,
     imagedir :: String,
     cp :: ConfigParser}

im = infoM "dfsbuild.main"
wm = warningM "dfsbuild.main"
dm = debugM "dfsbuild.main"

setLogLevel prio =
    mapM_ (\x -> updateGlobalLogger x (setLevel prio)) 
          [rootLoggerName, "dfsbuild.main"]

getUniqueCDID :: IO String
getUniqueCDID = 
    do t <- getClockTime
       random1 <- randomIO
       random2 <- randomIO
       return $ printf "DFS CD IMAGE, format 2, ID: %d,%d,%d\n"
                ((\(TOD x _) -> x) t) (random1::Int) (random2::Int)

{- | Take a ConfigParser and return a list of devices given, separated by
"\n" -}
getDevices :: ConfigParser -> String
getDevices cp = 
    (++ "\n") . join "\n" . splitWs $ dget cp "devices"

dget cp opt = forceEither $ get cp "dfs" opt
eget env opt = dget (cp env) opt