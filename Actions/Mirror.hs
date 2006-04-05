{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions.Mirror where
import Utils
import MissingH.Cmd
import MissingH.ConfigParser
import System.Posix.Directory
import System.IO
import Text.Printf

mirrorToWorkdir env repos =
    do im "Mirroring process starting."
       createDirectory mirrordir 0o755
       mapM_ (procrepo env) repos
    where
      mirrordir = (wdir env) ++ "/mirror"

procrepo env repo =
    do im $ "Running cdebootstrap for " ++ repo
       -- First, download the packages.
       safeSystem "cdebootstrap" $
                  archargs ++ debugargs ++ ["-d", suite, targetdir, mirror]
       -- Next, copy them into the mirror.
       cfg <- openFile aptmovecfg WriteMode
       hPrintf cfg "LOCALDIR=%s\n" mirrordir
       hPrintf cfg "FILECACHE=%s/var/cache/bootstrap\n" targetdir
       hPrintf cfg "LISTSTATE=%s/var/cache/bootstrap\n" targetdir
       hPrintf cfg "DIST=%s\n" repo
       case get (cp env) sect "arch" of
         Left _ -> return ()
         Right a -> hPrintf cfg "ARCH=%s\n" (a::String)
       hPutStr cfg "COPYONLY=yes\nCONTENTS=yes\nAPTSITES=/all/\nPKGCOMP=\"none gzip\"\n"
       hClose cfg
       im $ "Running apt-move for " ++ repo
       safeSystem "apt-move" ["-c", aptmovecfg, "update"]
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
      aptmovecfg = (wdir env) ++ "/apt-move.conf"
      