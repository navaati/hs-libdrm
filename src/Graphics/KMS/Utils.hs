{-# LANGUAGE Rank2Types #-}

module Graphics.KMS.Utils where

import System.Posix
import Graphics.KMS.Types
import Foreign
import Control.Monad
import Data.Maybe(fromMaybe)
import Control.Exception(bracket)

withDrm :: ((?drm :: Drm) ⇒ IO a) → IO a
withDrm f = bracket
            (openFd drm_device ReadWrite Nothing defaultFileFlags)
            closeFd
            (\drm -> let ?drm = Drm drm in f)
  where drm_device = "/dev/dri/card0"

lPeekArray :: Storable b =>
              Ptr a -> (Ptr a -> IO Int) -> (Ptr a -> IO (Ptr b)) ->
              IO [b]
lPeekArray ptr count arr = join $ liftM2 peekArray (count ptr) (arr ptr)

peekEnum :: (Eq k, Storable k) => [(k,a)] ->
            Ptr a -> IO a
peekEnum table ptr = do
  cint <- peek $ castPtr ptr
  return $ fromMaybe (error "Wrong drmModeConnection value") $
    lookup cint table

showBits :: (Bits n, Bounded n) => n -> String
showBits x = foldr (\b -> (:) $ if testBit x b then '1' else '0' ) "" $ reverse [0..(bitSize x)-1]
