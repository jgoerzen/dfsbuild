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
import MissingH.Path
import MissingH.Path.FilePath
import MissingH.Path.Glob
import Control.Monad
import Control.Exception
import Data.List
import MissingH.ConfigParser
import MissingH.IO.HVFS
import System.Directory hiding (createDirectory)
import qualified Actions.ConfigFiles
import qualified Bootloader
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
                     finished Initialized
         Initialized ->         -- Now download all suites we'll be using
             do dlMirrors env
                finished Mirrored
         Mirrored ->            -- Bootstrap the CD image
             do cdebootstrap env
                finished Bootstrapped
         Bootstrapped ->        -- Install additional packages
             do installpkgs env
                finished Installed
         Installed ->           -- Time to install shared files
             do installlib env
                finished LibsInstalled
         LibsInstalled ->       -- Time to install debs
             do installdebs env
                finished DebsInstalled
         DebsInstalled ->       -- Handle config files
             do Actions.ConfigFiles.writeCfgFiles env
                Actions.ConfigFiles.fixRc env
                Actions.ConfigFiles.writeBuildInfo env
                finished CfgHandled
         CfgHandled ->          -- Prepare the ramdisk
             do preprd env
                finished RDPrepped
         RDPrepped ->           -- Install kernels
             do installKernels env
                finished KernelsInstalled
         KernelsInstalled ->    -- Make the ramdisk
             do safeSystem "mkcramfs" [(targetdir env) ++ "/opt/initrd",
                                       (targetdir env) ++ "/opt/dfsruntime/initrd.dfs"]
                recursiveRemove SystemFS $ (targetdir env) ++ "/opt/initrd"
                finished RamdiskBuilt
         RamdiskBuilt ->        -- Install the bootloader
             do (isoargs, blfunc) <- Bootloader.install env
                preprtrd env
                compress env
                isoname <- mkiso env isoargs
                blfunc isoname
                -- FINISHED!
                return False
       if shouldContinue
          then mainRunner env
          else dm $ "mainRunner finished."
    where finished state = do saveState env state
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
                      [targetdir env, "apt-get", "-y", "--allow-unauthenticated", "install"] ++ pkgs

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

installdebs env =
 do case get (cp env) (defaultArch env) "installdebs" of
      Left _ -> return ()
      Right debs ->
          do im "Installing debs..."
             safeSystem "dpkg" $ ["--root=" ++ (targetdir env), "-i"] ++
                     splitWs debs
                                       
    case get (cp env) (defaultArch env) "unpackdebs" of
      Left _ -> return () 
      Right debs -> 
          do let deblist = splitWs debs
             if deblist /= []
                then do im "Unpacking .debs..."
                        createDirectory realtmpdir 0o755
                        mapM_ (\x -> safeSystem "cp" ["-v", x, 
                                                      realtmpdir ++ "/"])
                              deblist
                        let debnames = map (\x -> chroottmpdir ++ "/" ++ 
                                       (snd . splitFileName $ x)) deblist
                        safeSystem "chroot" $
                            [(targetdir env), "dpkg", "--force-depends",
                             "--force-conflicts", "--force-overwrite",
                             "--force-architecture", "--unpack"] ++ debnames
                        recursiveRemove SystemFS realtmpdir
                else im "Not unpacking .debs since none listed in unpackdebs option"
    where
      chroottmpdir = "/insttmp"
      realtmpdir = (targetdir env) ++ chroottmpdir

preprd env =
    do im "Preparing directory for ramdisk..."
       createDirectory ((targetdir env) ++ "/opt/initrd") 0o755
       mapM_ (\x -> createDirectory ((targetdir env) ++ "/opt/initrd/" ++ x) 0o755)
             ["bin", "lib", "sbin", "proc", "usr", "usr/sbin", "usr/bin",
              "realroot"]
       chr ["sh", "-c", "cp -dv /lib/ld-* /opt/initrd/lib/"]
       chr ["sh", "-c", "cp -v /lib/libc.so* /opt/initrd/lib/"]
       chr ["cp", "-v", "/bin/busybox", "/opt/initrd/bin/"]
       chr ["cp", "-v", "/usr/sbin/chroot", "/opt/initrd/usr/sbin/"]
       chr ["cp", "-v", "/sbin/pivot_root", "/opt/initrd/sbin/"]
       chr ["cp", "-r", "/dev", "/opt/initrd/"]
       handle (\_ -> return ()) (removeLink ((targetdir env) ++ "/opt/initrd/linuxrc"))
       safeSystem "cp" [(eget env "libdir") ++ "/linuxrc",
                        (targetdir env) ++ "/opt/initrd/sbin/init"]
       setFileMode ((targetdir env) ++ "/opt/initrd/sbin/init") 0o755
       writeFile ((targetdir env) ++ "/opt/dfsruntime/marker") (marker env)
       writeFile ((targetdir env) ++ "/opt/initrd/marker") (marker env)
       writeFile ((targetdir env) ++ "/opt/initrd/devices") getdevices
    where chr args = safeSystem "chroot" $ ((targetdir env) : args)
          getdevices = (++ "\n") . concat . intersperse "\n" . 
                       splitWs . eget env $ "devices"

installKernels env =
    do dm "Installing kernels..."
       case get (cp env) (defaultArch env) "kernels" of
         Left _ -> return ()
         Right k -> 
              do kernfiles <- mapM glob (splitWs k)
                 mapM_ (\x -> safeSystem "cp" ["-v", x, targetdir env ++ "/boot/"]) (concat kernfiles)
       case get (cp env) (defaultArch env) "modules" of
         Left _ -> return ()
         Right m -> -- FIXME: this too
            do modfiles <- mapM glob (splitWs m)
               mapM_ (\x -> safeSystem "cp" ["-v", x, targetdir env ++ "/lib/modules/"]) (concat modfiles)
            
       
preprtrd env =
    do im "Preparing run-time ramdisk"
       createDirectory (targetdir env ++ "/opt/dfsruntime/runtimemnt") 0o755
       let rdpath = targetdir env ++ "/opt/dfsruntime/runtimerd"
       createDirectory rdpath 0o755
       rdfiles <- mapM glob (splitWs . eget env $ "ramdisk_files")
       mapM_ (cpfile2rd rdpath) (concat rdfiles)
    where cpfile2rd rdpath f =
              do let src = targetdir env ++ f
                 let dest = rdpath ++ f
                 let destdir = fst . splitFileName $ dest
                 dde <- doesDirectoryExist destdir
                 unless (dde)
                            (safeSystem "mkdir" ["-p", destdir])
                 handle (\_ -> return ())
                            (rename src dest)
                 createSymbolicLink ("/opt/dfsruntime/runtimemnt" ++ f) src

compress env =
    case egetbool env "compress" of
      False -> im "No image compression requested"
      True -> reallycompress env

reallycompress env = 
    do im "Compressing image.  This may take some time..."
       let noncom = wdir env ++ "/noncom"
       createDirectory noncom 0o755
       noncomfiles <- filterM (\x -> vDoesExist SystemFS (targetdir env ++ x))
                              (splitWs (eget env "dontcompress"))
       let noncommap = zip noncomfiles (map show [(0::Int)..])
       mapM_ (preserve noncom) noncommap
       safeSystem "mkzftree" [targetdir env, wdir env ++ "/zftree"]
       recursiveRemove SystemFS (targetdir env)
       rename (wdir env ++ "/zftree") (targetdir env)
       mapM_ (restore noncom) noncommap
    where preserve noncom (orig, tmp) =
              do im $ "Not compressing " ++ orig
                 rename (targetdir env ++ orig) (noncom ++ "/" ++ tmp)
          restore noncom (orig, tmp) =
              rename (noncom ++ "/" ++ tmp) (targetdir env ++ orig)

mkiso env isoargs = 
    do im "Preparing ISO image"
       let isofile = wdir env ++ "/image.iso"
       let compressopts = if egetbool env "compress" then ["-z"] else []
       safeSystem "mkisofs" $ compressopts ++ isoargs ++
                  ["-pad", "-R", "-o", isofile, targetdir env]
       return isofile
