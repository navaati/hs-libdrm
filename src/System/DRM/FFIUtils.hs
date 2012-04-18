module System.DRM.FFIUtils where

import Prelude.Unicode
import Foreign
import Control.Monad
import Data.Maybe(fromJust)

lPeekArray ∷ Storable β ⇒
              Ptr α → (Ptr α → IO Int) → (Ptr α → IO (Ptr β)) →
              IO [β]
lPeekArray ptr count arr = join $ liftM2 peekArray (count ptr) (arr ptr)

peekEnum ∷ (Eq k, Storable k) ⇒ [(k,α)] →
            Ptr α → IO α
peekEnum table ptr = do
  cint ← peek $ castPtr ptr
  return $ fromJust $ lookup cint table

cToFlags ∷ (Bits f, Eq e) ⇒ [(e,f)] → f → [e]
cToFlags table cword = filter (isFlagSet ∘ eToF) $ map fst table
  where eToF = fromJust ∘ flip lookup table
        isFlagSet f = (f .&. cword) ≡ f
