{- dfsbuild: CD image builder
Copyright (c) 2006 John Goerzen
Please see COPYRIGHT for more details
-}

module Bootloader where
import qualified Bootloader.Grub
import Utils
--import qualified Bootloader.Aboot
--import qualified Bootloader.Yaboot

install :: DFSEnv -> IO ([String], DFSEnv -> IO ())
install env =
    case eget env "bootloader" of
      "grub-no-emul" -> Bootloader.Grub.grub_eltorito env
      "grub-hd" -> Bootloader.Grub.grub_hd env
--      "aboot" -> Bootloader.Aboot.aboot env
--      "yaboot" -> Bootloader.Yaboot.yaboot env
      x -> fail $ "Invalid bootloader: " ++ x
