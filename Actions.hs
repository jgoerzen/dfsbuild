{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions where
import Utils

run env = 
    do im "Running."
       mapM_ (createDirectory `flip` 0o755) 
             [imagedir env, imagedir env ++ "/opt",
              imagedir env ++ "/opt/dfsruntime"]
       createDirectory (imagedir env) 0o755
       dlmirrors env
       
dlMirrors env = 
    do let suites = splitWs $ eget cp "dlrepos"
       Mirror.mirror_workdir env suites
