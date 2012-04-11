{-# LANGUAGE Rank2Types #-}

module Graphics.KMS.Utils where

import System.Posix
import Graphics.KMS.Types
import Control.Exception(bracket)
import Data.Reflection
import Data.Proxy

withDrm ∷ FilePath →
           (∀drm. (drm `Reifies` Drm) ⇒ Proxy drm → IO α) → IO α
withDrm drmPath f = bracket
                    (openFd drmPath ReadWrite Nothing defaultFileFlags)
                    closeFd
                    (\drm → reify (Drm drm) f)
