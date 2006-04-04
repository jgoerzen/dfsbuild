{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Main where

import Text.Printf
import MissingH.ConfigParser
import MissingH.Either
import MissingH.Str

{- | Take a ConfigParser and return a list of devices given, separated by
"\n" -}
getDevices :: ConfigParser -> String
getDevices cp = 
    (++ "\n") . join "\n" . splitWs . forceEither . get cp "devices"

dlMirrors cp workdir = 
    do let suites = splitWs (forceEither $ get cp "dlrepos")
       Mirror.mirror_workdir cp suites workdir
  
parseCmdLine :: IO (ConfigParser, String)
parseCmdLine =
    do (args, _) <- validateCmdLine RequireOrder options header validate
       val <- readfile (emptyCP {accessFunc = interpolatingAccess 5})
              (forceMaybe $ lookup args "c")
       let cp = forceEither val
       let wdir = lookup args "w"
       createDirectory wdir 0o755
       cwd <- getCurrentDirectory
       return (cp, forceMaybeMsg "absNormPath" $ absNormPath cwd wdir)
    where options = [Option "i" [] (ReqArg (stdRequired "i") "FILE")
                            "Configuration file (required)",
                     Option "w" [] (ReqArg (stdRequired "w") "DIR")
                            "Work directory (required) (MUST NOT EXIST)"
                    ]
          validate (_, []) = Nothing
          validate (_, _) = Just "Unrecognized options appended"
          header = "Usage: dfsbuild -c CONFIGFILE -w WORKDIR\n"

