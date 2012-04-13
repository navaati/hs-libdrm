module Graphics.KMS.Internals.Utils where

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

peekFlags ∷ (Bits f, Storable f, Eq e) ⇒ [(e,f)] → Ptr f → IO [e]
peekFlags table ptr = do
  cword ← peek ptr
  let eToF = fromJust ∘ flip lookup table
      isFlagSet f = (f .&. cword) ≡ f
  return $ filter (isFlagSet ∘ eToF) $ map fst table
