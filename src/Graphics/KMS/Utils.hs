{-# LANGUAGE Rank2Types #-}

module Graphics.KMS.Utils where

import System.Posix
import Graphics.KMS.Types
import Foreign
import Control.Monad
import Data.Maybe(fromMaybe)
import Control.Exception(bracket)
import Data.Reflection
import Data.Proxy

withDrm ∷ FilePath →
           (∀drm. (drm `Reifies` Drm) ⇒ Proxy drm → IO α) → IO α
withDrm drmPath f = bracket
                    (openFd drmPath ReadWrite Nothing defaultFileFlags)
                    closeFd
                    (\drm → reify (Drm drm) f)

lPeekArray ∷ Storable β ⇒
              Ptr α → (Ptr α → IO Int) → (Ptr α → IO (Ptr β)) →
              IO [β]
lPeekArray ptr count arr = join $ liftM2 peekArray (count ptr) (arr ptr)

peekEnum ∷ (Eq k, Storable k) ⇒ [(k,α)] →
            Ptr α → IO α
peekEnum table ptr = do
  cint ← peek $ castPtr ptr
  return $ fromMaybe (error "Wrong drmModeConnection value") $
    lookup cint table

showBits ∷ (Bits n, Bounded n) ⇒ n → String
showBits x = foldr (\b → (:) $ if testBit x b then '1' else '0' ) "" $ reverse [0..(bitSize x)-1]
