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
import System.Console.GetOpt
import MissingH.Path
import qualified Actions(run)
  
procCmdLine :: IO (Bool, ConfigParser, String)
procCmdLine =
    do (args, _) <- validateCmdLine RequireOrder options header validate
       let debugmode = (lookup "v" args == Just "")
       when debugmode (updateGlobalLogger rootLoggerName (setLevel DEBUG))
       dm "VERBOSE MODE (DEBUG) engaged."
       dm $ "Command line parsed, results: " ++ (show args)
       val <- readfile (emptyCP {accessfunc = interpolatingAccess 5})
              (forceMaybeMsg "arg c" $ lookup "c" args)
       let cp = forceEither val
       dm $ "Config file parsed: " ++ show (content cp)
       let wdir = forceMaybeMsg "working dir" $ lookup "w" args
       dm $ "Working dir is " ++ wdir
       createDirectory wdir 0o755
       dm $ "Working dir created"
       cwd <- getWorkingDirectory
       dm $ "Initial cwd is " ++ cwd
       return (debugmode, cp, 
               forceMaybeMsg "absNormPath" $ absNormPath cwd wdir)
    where options = [Option "c" [] (ReqArg (stdRequired "c") "FILE")
                            "Configuration file (required)",
                     Option "w" [] (ReqArg (stdRequired "w") "DIR")
                            "Work directory (required) (MUST NOT EXIST)",
                     Option "v" [] (NoArg ("v", "")) "Be verbose"
                    ]
          validate (arglist, []) =
              if (lookup "c" arglist /= Nothing &&
                  lookup "w" arglist /= Nothing)
                  then Nothing
                  else Just "Required arguments missing"
          validate (_, _) = Just "Unrecognized options appended"
          header = "Usage: dfsbuild [-v] -c CONFIGFILE -w WORKDIR\n"

main =
    do loghandler <- verboseStreamHandler stderr DEBUG
       updateGlobalLogger (rootLoggerName)
                          (setLevel INFO . setHandlers [loghandler])
       traplogging "dfs" CRITICAL "Exception" runMain

runMain =
    do (debugmode, incp, workdir) <- procCmdLine 
       da <- getDefaultArch
       im $ "Welcome to dfsbuild.  Host architecture: " ++ show da
       changeWorkingDirectory workdir
       im $ "Using working directory " ++ workdir
       let cplibdir = forceMaybe $ 
                      absNormPath workdir 
                                      (forceEither $ get incp da "libdir")
       im $ "Using library directory " ++ cplibdir
       let env = DFSEnv {wdir = workdir,
                         libdir = cplibdir,
                         imagedir = workdir ++ "/image",
                         cp = incp,
                         isDebugging = debugmode,
                         defaultArch = da}
       Actions.run env
