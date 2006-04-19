{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

import Text.Printf
import MissingH.ConfigParser
import MissingH.Either
import MissingH.Str
import Utils
import MissingH.Logging.Logger
import MissingH.Logging.Handler.Simple
import Control.Monad
import MissingH.GetOpt
import MissingH.Maybe
import System.IO
import System.Posix.Directory
import System.Posix.User
import System.Console.GetOpt
import MissingH.Path
import Actions.ConfigFiles
import qualified Actions(run)
  
procCmdLine :: IO (Bool, Bool, ConfigParser, String, String)
procCmdLine =
    do (args, _) <- validateCmdLine RequireOrder options header validate
       let debugmode = (lookup "V" args == Just "")
       when (debugmode || lookup "v" args == Just "") 
                (updateGlobalLogger rootLoggerName (setLevel DEBUG))
       dm "VERBOSE MODE (DEBUG) engaged."
       dm $ "Command line parsed, results: " ++ (show args)
       val <- readfile (emptyCP {accessfunc = interpolatingAccess 5})
              (forceMaybeMsg "arg c" $ lookup "c" args)
       let cp = forceEither val
       dm $ "Config file parsed: " ++ show (content cp)
       let wdir = forceMaybeMsg "working dir" $ lookup "w" args
       dm $ "Working dir is " ++ wdir
       dm $ "Working dir created"
       cwd <- getWorkingDirectory
       dm $ "Initial cwd is " ++ cwd
       da <- getDefaultArch
       let defaultArch = case lookup "a" args of
                           Nothing -> da
                           Just x -> x
       return (debugmode, lookup "R" args == Just "", cp, 
               forceMaybeMsg "absNormPath" $ absNormPath cwd wdir,
               defaultArch)
    where options = [Option "c" [] (ReqArg (stdRequired "c") "FILE")
                            "Configuration file (required)",
                     Option "w" [] (ReqArg (stdRequired "w") "DIR")
                            "Work directory (required) (MUST NOT EXIST)",
                     Option "a" [] (ReqArg (stdRequired "a") "ARCH")
                            "Force architecture of target image to ARCH",
                     Option "R" [] (NoArg ("R", "")) "Resume an existing build (EXPERIMENTAL)",
                     Option "v" [] (NoArg ("v", "")) "Show dfsbuild debugging",
                     Option "V" [] (NoArg ("V", "")) "Show both dfsbuild AND external program debugging"
                    ]
          validate (arglist, []) =
              if (lookup "c" arglist /= Nothing &&
                  lookup "w" arglist /= Nothing)
                  then Nothing
                  else Just "Required arguments missing"
          validate (_, _) = Just "Unrecognized options appended"
          header = "Usage: dfsbuild [-v | -V] [-R] -c CONFIGFILE -w WORKDIR\n"

main =
    do loghandler <- verboseStreamHandler stderr DEBUG
       updateGlobalLogger (rootLoggerName)
                          (setLevel INFO . setHandlers [loghandler])
       traplogging "dfs" CRITICAL "Exception" runMain

runMain =
    do (debugmode, resumemode, incp, workdir, da) <- procCmdLine 
       im $ "Welcome to dfsbuild.  Image architecture: " ++ show da
       checkUID

       -- If this is a fresh run, need to create that work dir.
       unless (resumemode) (createDirectory workdir 0o755)
       changeWorkingDirectory workdir
       im $ "Using working directory " ++ workdir
       let cplibdir = forceMaybe $ 
                      absNormPath workdir 
                                      (forceEither $ get incp da "libdir")
       im $ "Using library directory " ++ cplibdir
       cdmarker <- if resumemode
                      then readFile (workdir ++ "/target/opt/dfsruntime/markst")
                      else getUniqueCDID
       date <- getDate
       let env = DFSEnv {wdir = workdir,
                         libdir = cplibdir,
                         cp = incp,
                         isDebugging = debugmode,
                         defaultArch = da,
                         targetdir = workdir ++ "/target",
                         marker = cdmarker,
                         datestr = date}
       -- Fresh run: initialize the state file.
       unless (resumemode) (saveState env Fresh)

       Actions.run env

checkUID =
    do uid <- getEffectiveUserID
       if uid /= 0
          then fail $ "dfsbuild must be run as root."
          else dm $ "dfsbuild is running as root."
