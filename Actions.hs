{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions where
import Utils
import qualified Actions.Mirror
import System.Posix.Directory
import MissingH.Str
import MissingH.Cmd
import System.Directory hiding (createDirectory)

run env = 
    do im "Running."
       mapM_ (createDirectory `flip` 0o755) 
             [imagedir env, imagedir env ++ "/opt",
              imagedir env ++ "/opt/dfsruntime"]

       -- Download all suites that we'll be using
       dlMirrors env

       -- Now bootstrap the CD image
       cdebootstrap env

       -- Install additional packages
       installpkgs env
       
dlMirrors env = 
    do let suites = splitWs $ eget env "dlrepos"
       Actions.Mirror.mirrorToWorkdir env suites

cdebootstrap env =
    do im $ "Bootstrapping into " ++ targetdir env
       safeSystem "cdebootstrap" [eget env "suite", (targetdir env),
                                  "file://" ++ (wdir env) ++ "/mirror"]
       dm $ "Saving sources.list"
       writeFile ((targetdir env) ++ "/etc/apt/sources.list") $
                 "deb " ++ (eget env "mirror") ++ " " ++ (eget env "suite")
                 ++ " main\n"
       dm $ "Moving mirror to /opt/packages on target"
       renameFile ((wdir env) ++ "/mirror") 
                      ((targetdir env) ++ "/opt/packages")

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