{-# LANGUAGE Rank2Types #-}

module System.DRM (withDrm, module System.DRM.Types) where

import System.Posix
import Control.Exception(bracket)
import Data.Reflection
import Data.Proxy

import System.DRM.Types

withDrm ∷ FilePath →
           (∀drm. (drm `Reifies` Drm) ⇒ Proxy drm → IO α) → IO α
withDrm drmPath f = bracket
                    (openFd drmPath ReadWrite Nothing defaultFileFlags)
                    closeFd
                    (\drm → reify (Drm drm) f)
