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

dlMirrors cp workdir = 
    do let suites = splitWs (forceEither $ get cp "dlrepos")
       Mirror.mirror_workdir cp suites workdir
  
parseCmdLine :: IO (ConfigParser, String)
parseCmdLine =
    do (args, _) <- validateCmdLine RequireOrder options header validate
       when (lookup "v" args == Just "")
            (updateGlobalLogger "dfsbuild" (setLevel DEBUG))
       dm "VERBOSE MODE (DEBUG) engaged."
       dm $, "Command line parsed, results: " ++ (show args)
       val <- readfile (emptyCP {accessFunc = interpolatingAccess 5})
              (forceMaybe $ lookup args "c")
       dm $ "Config file parsed: " ++ show val
       let cp = forceEither val
       let wdir = lookup args "w"
       dm $ "Working dir is " ++ wdir
       createDirectory wdir 0o755
       dm $ "Working dir created"
       cwd <- getCurrentDirectory
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
       (cp, workdir) <- parseCmdLine 
       setCurrentDirectory workdir
       dm "Changed cwd to workdir"
       