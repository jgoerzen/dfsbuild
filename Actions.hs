{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions where
import Utils
import qualified Actions.Mirror
import System.Posix.Directory
import MissingH.Str

run env = 
    do im "Running."
       mapM_ (createDirectory `flip` 0o755) 
             [imagedir env, imagedir env ++ "/opt",
              imagedir env ++ "/opt/dfsruntime"]

       -- Download all suites that we'll be using
       dlMirrors env

       -- Now bootstrap the CD image
       cdebootstrap env
       
dlMirrors env = 
    do let suites = splitWs $ eget env "dlrepos"
       Actions.Mirror.mirrorToWorkdir env suites

cdebootstrap env =
    do im $ "Bootstrapping into " ++ targetdir
       safeSystem "cdebootstrap" [eget env "suite", target,
                                  "file://" ++ (wdir env) ++ "/mirror"]
       dm $ "Saving sources.list"
       writeFile (targetdir ++ "/etc/apt/sources.list") $
                 "deb " ++ (eget env "mirror") ++ " " ++ (eget env "suite")
                 ++ " main\n"
       dm $ "Moving mirror to /opt/packages on target"
       renameFile ((wdir env) ++ "/mirror") (targetdir ++ "/opt/packages")
    where targetdir = (wdir env) ++ "/target"