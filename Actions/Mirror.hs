{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions.Mirror where
import Utils
import MissingH.Cmd
import MissingH.ConfigParser
import System.Posix.Directory
import Control.Exception
import Data.List
import MissingH.Path
import MissingH.Path.Glob
import System.IO
import Text.Printf
import MissingH.Path
import MissingH.IO.HVFS

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
                  archargs ++ debugargs ++ ["-d", repo, targetdir env, mirror]
       -- Next, copy them into the mirror.
       codename <- getCodeName 
                   (targetdir env ++ "/var/cache/bootstrap/Release")
       dm $ "Codename for this is " ++ codename
       mapM_ (\x -> handle (\_ -> return ()) (createDirectory x 0o755))
                 [mirrordir, mirrordir ++ "/conf"]
       safeSystem "touch" [mirrordir ++ "/conf/distributions"]
       appendFile (mirrordir ++ "/conf/distributions") $ concat $ intersperse "\n" $
           ["Origin: Debian",
            "Label: Debian",
            "Suite: " ++ repo,
            "Version: 0.dfs",
            "Codename: " ++ codename,
            "Architectures: alpha amd64 arm hppa i386 ia64 m68k mips mipsel powerpc s390 sparc",
            "Description: Debian From Scratch cache of " ++ repo,
            "Components: main non-free contrib",
            "\n\n\n"]
       im $ "Running reprepro for " ++ codename
       debs <- glob (targetdir env ++ "/var/cache/bootstrap/*.deb")
       bracketCWD mirrordir $ 
                  mapM_ (\x -> safeSystem "reprepro" 
                               (repdebugargs ++ ["-b", ".", "includedeb", 
                                                 codename, x])) debs

       -- Delete the cdebootstrap cache so the next run has a clean dir
       recursiveRemove SystemFS $ targetdir env ++ "/var/cache/bootstrap"
       safeSystem "ln" ["-sf", codename, mirrordir ++ "/dists/" ++ repo]
    where
      mirrordir = (wdir env) ++ "/mirror"
      sect = "repo " ++ repo
      mirror = esget env sect "mirror"
      archargs = case get (cp env) sect "arch"
                   of Left _ -> []
                      Right a -> ["-a", a]
      debugargs = if isDebugging env
                     then ["--debug", "-v"]
                     else ["-q"]
      repdebugargs = if isDebugging env
                        then ["-V"]
                        else []
      aptmovecfg = (wdir env) ++ "/apt-move.conf"
      