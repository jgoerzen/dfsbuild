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
       dlMirrors env
       
dlMirrors env = 
    do let suites = splitWs $ eget env "dlrepos"
       Actions.Mirror.mirrorToWorkdir env suites
