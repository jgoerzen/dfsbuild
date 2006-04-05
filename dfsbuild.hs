{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Main where

import Text.Printf
import MissingH.ConfigParser
import MissingH.Either
import MissingH.Str
import Utils
import MissingH.Logging.Logger
import Control.Monad
import MissingH.GetOpt
import MissingH.Maybe
import System.Posix.Directory
import System.Console.GetOpt
import MissingH.Path

{-
dlMirrors cp workdir = 
    do let suites = splitWs (forceEither $ get cp "dlrepos")
       Mirror.mirror_workdir cp suites workdir
-}
  
procCmdLine :: IO (ConfigParser, String)
procCmdLine =
    do (args, _) <- validateCmdLine RequireOrder options header validate
       when (lookup "v" args == Just "")
            (updateGlobalLogger "dfsbuild" (setLevel DEBUG))
       dm "VERBOSE MODE (DEBUG) engaged."
       dm $ "Command line parsed, results: " ++ (show args)
       val <- readfile (emptyCP {accessfunc = interpolatingAccess 5})
              (forceMaybe $ lookup "c" args)
       let cp = forceEither val
       dm $ "Config file parsed: " ++ show (content cp)
       let wdir = forceMaybeMsg "working dir" $ lookup "w" args
       dm $ "Working dir is " ++ wdir
       createDirectory wdir 0o755
       dm $ "Working dir created"
       cwd <- getWorkingDirectory
       dm $ "Initial cwd is " ++ cwd
       return (cp, forceMaybeMsg "absNormPath" $ absNormPath cwd wdir)
    where options = [Option "i" [] (ReqArg (stdRequired "i") "FILE")
                            "Configuration file (required)",
                     Option "w" [] (ReqArg (stdRequired "w") "DIR")
                            "Work directory (required) (MUST NOT EXIST)",
                     Option "v" [] (NoArg ("v", "")) "Be verbose"
                    ]
          validate (_, []) = Nothing
          validate (_, _) = Just "Unrecognized options appended"
          header = "Usage: dfsbuild -c CONFIGFILE -w WORKDIR\n"

main =
    do updateGlobalLogger "dfsbuild" (setLevel INFO)
       (cp, workdir) <- procCmdLine 
       changeWorkingDirectory workdir
       dm "Changed cwd to workdir"
       