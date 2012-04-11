module Graphics.KMS.Internals.Utils where

import Foreign
import Control.Monad
import Data.Maybe(fromMaybe)

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

