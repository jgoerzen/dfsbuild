{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions where
import Utils
import qualified Actions.Mirror
import System.Posix.Directory
import System.Posix.Files
import MissingH.Str
import MissingH.Cmd
import Control.Monad
import System.Directory hiding (createDirectory)

run env = 
    do im "Running."
       mainRunner env

mainRunner env =
    do state <- getState env
       dm $ "Processing at state " ++ show state
       shouldContinue <- case state of
         Fresh -> do mapM_ (createDirectory `flip` 0o755) 
                               [targetdir env, targetdir env ++ "/opt",
                                targetdir env ++ "/opt/dfsruntime"]
                     next Initialized
         Initialized ->         -- Now download all suites we'll be using
             do dlMirrors env
                next Mirrored
         Mirrored ->            -- Bootstrap the CD image
             do cdebootstrap env
                next Bootstrapped
         Bootstrapped ->        -- Install additional packages
             do installpkgs env
                next Installed
         Installed ->           -- Time to install shared files
             do installlib env
                saveState env LibsInstalled
                return False
       if shouldContinue
          then mainRunner env
          else dm $ "mainRunner finished."
    where next state = do saveState env state
                          return True
             
dlMirrors env = 
    do let suites = splitWs $ eget env "dlrepos"
       Actions.Mirror.mirrorToWorkdir env suites

cdebootstrap env =
    do im $ "Bootstrapping into " ++ targetdir env
       -- cdebootstrap has issues when Release.gpg isn't there.  Sigh.
       safeSystem "find" [wdir env ++ "/mirror/dists", "-name", "Release",
                          "-exec", "touch", "{}.gpg", ";"]
                  
       safeSystem "cdebootstrap" $ debugargs ++
                      [eget env "suite", (targetdir env),
                       "file://" ++ (wdir env) ++ "/mirror"]
       dm $ "Saving sources.list"
       writeFile ((targetdir env) ++ "/etc/apt/sources.list") $
                 "deb " ++ (eget env "mirror") ++ " " ++ (eget env "suite")
                 ++ " main\n"
       dm $ "Moving mirror to /opt/packages on target"
       rename ((wdir env) ++ "/mirror") 
                      ((targetdir env) ++ "/opt/packages")
    where debugargs = if isDebugging env
                          then ["--debug", "-v"]
                          else ["-q"]


installpkgs env =
    do im "Installing additional packages."

       -- Set debconf priority to critical so that user doesn't get prompted
       safeSystem "chroot" [targetdir env, "/bin/bash", "-c",
         "echo \"debconf\tdebconf/priority\tselect\tcritical\" | debconf-set-selections"]
    
       -- Copy resolv.conf so apt-get update/install works
       safeSystem "cp" ["/etc/resolv.conf", targetdir env ++ "/etc"]

       -- Now do apt-get
       safeSystem "chroot" [targetdir env, "apt-get", "update"]
       safeSystem "chroot" $ 
                      [targetdir env, "apt-get", "-y", "install"] ++ pkgs

       -- And remove the resolv.conf again
       removeFile (targetdir env ++ "/etc/resolv.conf")

       safeSystem "chroot" [targetdir env, "apt-get", "clean"]

    where pkgs = splitWs (eget env "packages")

installlib env =
    do im "Installing runtime library files."
       -- Copy the runtime boot files
       mapM_ (\x ->
              safeSystem "cp" ["-rL", libdir ++ "/" ++ x, 
                               (targetdir env) ++ "/opt/dfsruntime/"])
         ["startup", "home.html"]
       -- Set modes
       setFileMode ((targetdir env) ++ "/opt/dfsruntime/startup") 0o755
       createDirectory ((targetdir env) ++ "/opt/dfsruntime/doc") 0o755
       mapM_ (\x ->
              safeSystem "cp" ["-r", docdir ++ "/" ++ x,
                               (targetdir env) ++ "/opt/dfsruntime/doc/"])
             ["dfs.txt.gz", "html/"]
       mapM_ (\x -> do safeSystem "cp" ["-r", libdir ++ "/" ++ x,
                                        (targetdir env) ++ "/usr/local/bin/"]
                       setFileMode ((targetdir env) ++ "/usr/local/bin/" ++ x)
                                   0o755
             ) ["dfshelp", "dfshints", "dfsbuildinfo"]
                  
    where docdir = eget env "docdir"
          libdir = eget env "libdir"