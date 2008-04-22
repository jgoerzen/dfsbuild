{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Actions where
import Actions.ConfigFiles
import Utils
import qualified Actions.Mirror
import System.Posix.Directory
import System.Posix.Files
import Data.String.Utils
import System.Cmd.Utils
import System.Path
import System.FilePath
import System.Path.Glob
import Control.Monad
import Control.Exception
import Data.List
import Data.ConfigFile
import System.IO.HVFS
import System.Directory hiding (createDirectory)
import HSH hiding (glob)
import Text.Regex.Posix

import qualified Actions.ConfigFiles
import qualified Bootloader
runIt env = 
    do im "Running."
       mainRunner env

mainRunner env =
    do state <- getState env
       dm $ "Processing at state " ++ show state
       shouldContinue <- case state of
         Fresh -> do mapM_ (mkdir `flip` 0o755) 
                               [targetdir env, targetdir env ++ "/opt",
                                targetdir env ++ "/opt/dfsruntime"]
                     writeFile ((targetdir env) ++ "/opt/dfsruntime/marker") (marker env)
                     finished Initialized
         Initialized ->         -- Now download all suites we'll be using
             do dlMirrors env
                finished Mirrored
         Mirrored ->            -- Bootstrap the CD image
             do cdebootstrap env
                finished Bootstrapped
         Bootstrapped ->           -- Time to install shared files
             do installlib env
                finished EnvironmentPrepared
         EnvironmentPrepared ->        -- execute configurable hook scripts
             case get (cp env) (defaultArch env) "preparescripts" of
               Left _ -> finished LibsInstalled
               Right ps -> 
                   do im $ "Executing preparation scripts"
                      mapM_ (safeSystem `flip` [ targetdir env ]) (splitWs ps)
                      finished LibsInstalled
         LibsInstalled ->        -- Install additional packages
             do installpkgs env
                finished Installed
         Installed ->       -- Time to install debs
             do installdebs env
                finished DebsInstalled
         DebsInstalled ->       -- Handle config files
             do Actions.ConfigFiles.writeCfgFiles env
                Actions.ConfigFiles.fixRc env
                Actions.ConfigFiles.writeBuildInfo env
                finished CfgHandled
         CfgHandled ->          -- Prepare init
             do prepinit env
                finished InitPrepped
         InitPrepped ->          -- Prepare the ramdisk
             do preprd env
                finished RDPrepped
         RDPrepped ->           -- Install kernels
             do installKernels env
                finished KernelsInstalled
         KernelsInstalled ->    -- Make the ramdisk
             do safeSystem "mkcramfs" [(targetdir env) ++ "/opt/initrd",
                                       (targetdir env) ++ "/boot/initrd.dfs"]
                recursiveRemove SystemFS $ (targetdir env) ++ "/opt/initrd"
                finished EnvironmentCleaned
         EnvironmentCleaned ->        -- execute configurable hook scripts
             case get (cp env) (defaultArch env) "cleanupscripts" of
               Left _ -> finished RamdiskBuilt
               Right ps -> 
                   do im $ "Executing preparation scripts"
                      mapM_ (safeSystem `flip` [ targetdir env ])
                            (splitWs ps)
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
                  
       runIO ("cdebootstrap", debugargs ++
                      ["--allow-unauthenticated",
                       repo2suite bsuite, (targetdir env),
                       (esget env bsuite "mirror")])
       dm $ "Saving sources.list"
       writeFile ((targetdir env) ++ "/etc/apt/sources.list") $
                 foldr (\repo rest-> "deb " ++ (esget env ("repo "++repo) "mirror") ++ " " ++ repo2suite repo
                 ++ " main contrib non-free\n" ++ rest) "" (splitWs (eget env "installrepos"))
       dm $ "Moving mirror to /opt/packages on target"
       haveMirror <- doesDirectoryExist ((wdir env) ++ "/mirror")
       if haveMirror
          then rename ((wdir env) ++ "/mirror") 
                      ((targetdir env) ++ "/opt/packages")
          else return ()
    where debugargs = if isDebugging env
                          then ["--debug", "-v"]
                          else ["-q"]
          repo2suite repo = case get (cp env) ("repo "++repo) "dlsuite" of
                              Left _ -> repo
                              Right x -> strip x -- copied from Mirrors.hs
          bsuite = eget env "bootstraprepo"



installpkgs env =
    do im "Installing additional packages."

       -- Set debconf priority to critical so that user doesn't get prompted
       runIO $ echo "debconf\tdebconf/priority\tselect\tcritical" -|-
           ("chroot", [targetdir env, "debconf-set-selections"])
    
       -- Copy resolv.conf so apt-get update/install works
       runIO ("cp", ["/etc/resolv.conf", targetdir env ++ "/etc"])

       -- Update the cache
       runIO ("chroot", [targetdir env, "apt-get", "update"])

       -- Prepare for kernel images
       writeFile (targetdir env ++ "/etc/kernel-img.conf") kernelimgconf
       -- Install the requisite initramfs tools
       runIO ("chroot", [targetdir env, "apt-get", "-y",
                       "--allow-unauthenticated", "install", 
                       "initramfs-tools"])

       -- And the ramfs support
       runIO ("cp", [libdir env ++ "/dfs-initramfs-hook",
                   targetdir env ++ "/usr/share/initramfs-tools/hooks/dfs"])
       runIO ("cp", [libdir env ++ "/dfs-initramfs-script",
                   targetdir env ++ "/usr/share/initramfs-tools/scripts/local-top/dfs"])

       mapM_ (\x -> setFileMode ((targetdir env) ++ x) 0o755)
             ["/usr/share/initramfs-tools/hooks/dfs",
              "/usr/share/initramfs-tools/scripts/local-top/dfs"]

       -- Mount proc
       runIO("chroot", [targetdir env, "invoke-rc.d", "mountkernfs.sh", "start"])
       runIO("chroot", [targetdir env, "invoke-rc.d", "mountdevsubfs.sh", "start"])

       writeFile (targetdir env++"/usr/sbin/policy-rc.d") "#!/bin/sh\nexit 101\n"
       setFileMode (targetdir env ++ "/usr/sbin/policy-rc.d") 0o755

       -- Now do apt-get
       runIO ("chroot", [targetdir env, "apt-get", "-y", 
                       "--allow-unauthenticated", "install"] ++ pkgs)

       createDirectory ((targetdir env) ++ "/tmp/partial") 0o755
       runIO ("chroot", [targetdir env, "apt-get", "-d", "-y", "-o", "Dir::Cache::archives=/tmp", "install", "busybox-static" ])
       runIO ("chroot", [targetdir env, "apt-get", "clean"])

       runIO ("chroot", [targetdir env, "sh", "-c",
                            "for FILE in /etc/pam.d/*; do grep -v securetty $FILE > $FILE.tmp; mv $FILE.tmp $FILE; done"])

       -- And remove the resolv.conf again
       removeFile (targetdir env ++ "/etc/resolv.conf")

    where pkgs = splitWs (eget env "packages")

installlib env =
    do im "Installing runtime library files."
       -- Copy the runtime boot files
       mapM_ (\x ->
              runIO ("cp", ["-rL", libdir ++ "/" ++ x, 
                               (targetdir env) ++ "/opt/dfsruntime/"]))
         ["home.html", "dfs_startup_funcs"]

       -- Set modes
       mkdir ((targetdir env) ++ "/opt/dfsruntime/doc") 0o755
       mapM_ (\x ->
              runIO ("cp", ["-r", docdir ++ "/" ++ x,
                               (targetdir env) ++ "/opt/dfsruntime/doc/"]))
             ["dfs.txt.gz", "html/"]
       mapM_ (\x -> do runIO ("cp", ["-r", libdir ++ "/" ++ x,
                                        (targetdir env) ++ "/usr/local/bin/"])
                       setFileMode ((targetdir env) ++ "/usr/local/bin/" ++ x)
                                   0o755
             ) ["dfshelp", "dfshints", "dfsbuildinfo"]
                  
    where docdir = eget env "docdir"
          libdir = eget env "libdir"

installdebs env =
 do
 
   case get (cp env) (defaultArch env) "installdebs" of
      Left _ -> return ()
      Right debs ->
          do im "Installing debs..."
             runIO ("dpkg", ["--root=" ++ (targetdir env), "-i"] ++
                        splitWs debs)
                                       
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
                        runIO ("chroot",
                            [(targetdir env), "dpkg", "--force-depends",
                             "--force-conflicts", "--force-overwrite",
                             "--force-architecture", "--unpack"] ++ debnames)
                        recursiveRemove SystemFS realtmpdir
                else im "Not unpacking .debs since none listed in unpackdebs option"
   runIO ("sh", ["-c", "chroot " ++ (targetdir env) ++ " dpkg -l > " ++
                        (wdir env) ++ "/pkglist.txt"])

   runIO ("rm", [(targetdir env)++"/usr/sbin/policy-rc.d"])

   handle (\_->return ()) (runIO("chroot", [targetdir env, "umount", "-a"]))
   runIO("chroot", [targetdir env, "umount", "/proc"])
   runIO("chroot", [targetdir env, "umount", "/sys"])
   runIO("chroot", [targetdir env, "umount", "/dev/pts"])

 where
      chroottmpdir = "/insttmp"
      realtmpdir = (targetdir env) ++ chroottmpdir

prepinit env =
    do im "Configuring init..."
       rename (targetdir env ++ "/sbin/init")
              (targetdir env ++ "/sbin/init.real")
       runIO ("cp", ["-v", libdir env ++ "/startup",
                        targetdir env ++ "/sbin/init"])
       setFileMode (targetdir env ++ "/sbin/init") 0o755

preprd env =
    do im "Preparing directory for ramdisk..."
       createDirectory ((targetdir env) ++ "/tmp/busybox") 0o755
       chr ["sh", "-c", "dpkg -x /tmp/busybox-static*.deb /tmp/busybox"]
       createDirectory ((targetdir env) ++ "/opt/initrd") 0o755
       mapM_ (\x -> createDirectory ((targetdir env) ++ "/opt/initrd/" ++ x) 0o755)
             ["bin", "lib", "sbin", "proc", "usr", "usr/sbin", "usr/bin",
              "realroot", "sys"]
       chr ["sh", "-c", "cp -dv /lib/ld-* /opt/initrd/lib/"]
       chr ["sh", "-c", "cp -v /lib/libc.so* /opt/initrd/lib/"]
       chr ["cp", "-v", "/tmp/busybox/bin/busybox", "/opt/initrd/bin/"]
       chr ["sh", "-c", "rm -rf /tmp/busybox /tmp/busybox-static*.deb /tmp/partial"]
       chr ["cp", "-v", "/usr/sbin/chroot", "/opt/initrd/usr/sbin/"]
       chr ["cp", "-v", "/sbin/pivot_root", "/opt/initrd/sbin/"]
       chr ["cp", "-r", "/dev", "/opt/initrd/"]
       handle (\_ -> return ()) (removeLink ((targetdir env) ++ "/opt/initrd/linuxrc"))
       runIO ("cp", [(eget env "libdir") ++ "/linuxrc",
                        (targetdir env) ++ "/opt/initrd/sbin/init"])
       setFileMode ((targetdir env) ++ "/opt/initrd/sbin/init") 0o755
       runIO ("cp", [eget env "libdir" ++ "/dfs_startup_funcs",
                        targetdir env ++ "/opt/initrd"])
       writeFile ((targetdir env) ++ "/opt/initrd/marker") (marker env)
       createSymbolicLink "busybox" (targetdir env ++ "/opt/initrd/bin/sh")
    where chr args = runIO ("chroot", ((targetdir env) : args))

installKernels env =
    do dm "Installing kernels..."
       case get (cp env) (defaultArch env) "kernels" of
         Left _ -> return ()
         Right k -> 
              do kernfiles <- mapM glob (splitWs k)
                 mapM_ (\x -> runIO ("cp", ["-v", x, targetdir env ++ "/boot/"])) (concat kernfiles)
       case get (cp env) (defaultArch env) "modules" of
         Left _ -> return ()
         Right m ->
            do modfiles <- mapM glob (splitWs m)
               mapM_ (\x -> runIO ("cp", ["-vr", x, targetdir env ++ "/lib/modules/"])) (concat modfiles)
            
       
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
       noncomfilesM <- (liftM (filterM my_filter)) (enumfiles (targetdir env))
       noncomfiles <- noncomfilesM
--       noncomfiles <- filterM (\x -> vDoesExist SystemFS (targetdir env ++ x))
--                              (splitWs (eget env "dontcompress"))
       let noncommap = zip noncomfiles (map show [(0::Int)..])
       mapM_ (preserve noncom) noncommap
       safeSystem "mkzftree" [targetdir env, wdir env ++ "/zftree"]
       recursiveRemove SystemFS (targetdir env)
       rename (wdir env ++ "/zftree") (targetdir env)
       mapM_ (restore noncom) (reverse noncommap)
    where preserve noncom (orig, tmp) =
              do im $ "Not compressing " ++ orig
                 handle (\_->return ()) (rename orig (noncom ++ "/" ++ tmp))
          restore noncom (orig, tmp) =
              handle (\_->return ()) (rename (noncom ++ "/" ++ tmp) orig)
          my_filter fn = liftM ((||) ((foldl (\x y -> x || fn =~ y) False nocompats )
                                     || (fn =~ "(.*\\.gz)|(.*\\.Z)|(.*\\.zip)|(.*\\.bz2)|(.*\\.tgz)|(.*\\.jar)|(.*\\.png)|(.*\\.jpg)|(.*\\.gif)"::Bool)))
                         (handle (\_ -> return False) ((liftM (1024 >)) ((liftM fileSize) (getSymbolicLinkStatus fn))))
          nocompats = splitWs (eget env "dontcompress")

mkiso env isoargs = 
    do im "Preparing ISO image"
       let isofile = wdir env ++ "/image.iso"
       let compressopts = if egetbool env "compress" then ["-z"] else []
       safeSystem "genisoimage" $ compressopts ++ isoargs ++
                  ["-pad", "-R", "-o", isofile, targetdir env]
       return isofile
