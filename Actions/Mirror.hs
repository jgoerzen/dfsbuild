{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions.Mirror where
import Utils
import MissingH.Cmd
import MissingH.ConfigParser
import System.Posix.Directory

mirrorToWorkdir env repos =
    do im "Mirroring process starting."
       createDirectory mirrordir 0o755
       mapM_ (procrepo env) repos
    where
      mirrordir = (wdir env) ++ "/mirror"

procrepo env repo =
    do im $ "Running cdebootstrap for " ++ repo
       safeSystem "cdebootstrap" $
                  archargs ++ debugargs ++ ["-d", suite, targetdir, mirror]
    where
      targetdir = (wdir env) ++ "/target"
      mirrordir = (wdir env) ++ "/mirror"
      sect = "repo " ++ repo
      suite = esget env sect "suite"
      mirror = esget env sect "mirror"
      archargs = case get (cp env) sect "arch"
                   of Left _ -> []
                      Right a -> ["-a", a]
      debugargs = if isDebugging env
                     then ["--debug", "-v"]
                     else []
      