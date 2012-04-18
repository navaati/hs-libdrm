{-# LANGUAGE RecordWildCards #-}

module System.DRM.KMS.Encoder (Encoder(..),EncoderType(..),getEncoder) where

import Foreign
import Foreign.C.Error
import Data.Maybe(fromJust)

import System.DRM.C.KMS.Encoder
import System.DRM.Types

data Encoder drm = Encoder
                   { encoderId ∷ EncoderId drm
                   , encoderType ∷ EncoderType
                   , encoderCrtcId ∷ CrtcId drm
                   , encoderPossibleCrtcs ∷ Word32
                   , encoderPossibleClones ∷ Word32
                   } deriving (Show)

cToEncoder ∷ C'drmModeEncoder → Encoder drm
cToEncoder (C'drmModeEncoder{..}) =
  Encoder
  (EncoderId c'drmModeEncoder'encoder_id)
  (fromJust $ lookup c'drmModeEncoder'encoder_type encoderTypeEnum)
  (CrtcId c'drmModeEncoder'crtc_id)
  c'drmModeEncoder'possible_crtcs
  c'drmModeEncoder'possible_clones

getEncoder ∷ (RDrm drm) ⇒
  EncoderId drm → IO (Encoder drm)
getEncoder eId = do
  ptr ← throwErrnoIfNull "drmModeGetEncoder" $
         applyDrm c'drmModeGetEncoder eId
  encoder ← fmap cToEncoder $ peek ptr
  c'drmModeFreeEncoder ptr
  return encoder


data EncoderType = EncoderNone | DAC | TMDS | EncoderLVDS | TVDAC deriving (Show, Eq)

encoderTypeEnum ∷ [(Word32,EncoderType)]
encoderTypeEnum = [
  (c'DRM_MODE_ENCODER_NONE, EncoderNone),
  (c'DRM_MODE_ENCODER_DAC, DAC),
  (c'DRM_MODE_ENCODER_TMDS, TMDS),
  (c'DRM_MODE_ENCODER_LVDS, EncoderLVDS),
  (c'DRM_MODE_ENCODER_TVDAC, TVDAC)]
