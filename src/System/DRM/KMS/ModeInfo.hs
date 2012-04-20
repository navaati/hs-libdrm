module System.DRM.KMS.ModeInfo where

import Data.Word

data ModeInfo = ModeInfo
                { modeClock ∷ Word32
                , modeDisplay, modeTotal ∷ (Word16, Word16)
                , modeSync ∷ ((Word16, Word16),(Word16, Word16))
                , modeHSkew, modeVScan ∷ Word16
                , modeVRefresh ∷ Word32
                , modeFlags ∷ ModeFlags
                , modeType ∷ ModeType
                , modeName ∷ String
                } deriving (Show)

type ModeFlags = [ModeFlag]
data ModeFlag = PHSync | NHSync | PVSync | NVSync | Interlace | DBLScan | CSync | PCSync | NCSync | HSkew | BCast | PixMux | DBLClk | ClkDiv2 deriving (Show, Eq)

type ModeType = [ModeTypeFlag]
data ModeTypeFlag = Builtin | ClockC | CrtcC | Preferred | Default | UserDef | Driver deriving (Show, Eq)
