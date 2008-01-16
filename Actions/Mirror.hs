{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions.Mirror where
import Utils
import System.Cmd.Utils
import Data.ConfigFile
import System.Posix.Directory
import Control.Monad
import Control.Exception
import Data.List
import System.Path
import Data.String.Utils
import System.Path.Glob
import System.IO
import Text.Printf
import System.Path
import System.IO.HVFS
import HSH hiding (glob)

mirrorToWorkdir env repos =
    do im "Mirroring process starting."
       mkdir mirrordir 0o755
       foldM_ (procrepo env) [] repos
    where
      mirrordir = (wdir env) ++ "/mirror"

procrepo env priorcodenames repo =
    do im $ "Running cdebootstrap for " ++ repo
       -- First, download the packages.
       runIO ("cdebootstrap",
              archargs ++ debugargs ++ ["-d", suite, targetdir env, mirror])

       -- Next, copy them into the mirror.
       codename <- getCodeName 
                   (targetdir env ++ "/var/cache/bootstrap/")
       dm $ "Codename for this is " ++ codename
       mapM_ (\x -> handle (\_ -> return ()) (createDirectory x 0o755))
                 [mirrordir, mirrordir ++ "/conf"]
       
       runIO ("touch", [mirrordir ++ "/conf/distributions"])

       unless (codename `elem` priorcodenames) $
         appendFile (mirrordir ++ "/conf/distributions") $ concat $ intersperse "\n" $
           ["Origin: Debian",
            "Label: Debian",
            "Suite: " ++ suite,
            "Version: 0.dfs",
            "Codename: " ++ codename,
            "Architectures: alpha amd64 arm hppa i386 ia64 m68k mips mipsel powerpc s390 sparc",
            "Description: Debian From Scratch cache of " ++ suite,
            "Components: main non-free contrib",
            "\n\n\n"]

       im $ "Running reprepro for " ++ codename
       debs <- glob (targetdir env ++ "/var/cache/bootstrap/*.deb")
       bracketCWD mirrordir $ 
                  mapM_ (\x -> runIO ("reprepro",
                               repdebugargs ++ ["-b", ".", "includedeb", 
                                                 codename, x])) debs

       -- Delete the cdebootstrap cache so the next run has a clean dir
       recursiveRemove SystemFS $ targetdir env ++ "/var/cache/bootstrap"
       runIO ("ln", ["-sf", codename, mirrordir ++ "/dists/" ++ suite])
       return $ priorcodenames ++ [codename]
    where
      mirrordir = (wdir env) ++ "/mirror"
      sect = "repo " ++ repo
      suite = case get (cp env) sect "dlsuite" of
                Left _ -> repo
                Right x -> strip x
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
      