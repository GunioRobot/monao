module Sounds (
	SoundType(..), soundTypes, soundFn
	, BGMType(..), bgmFn
) where
import Data.Maybe (fromJust)

data SoundType =
		SndJump
	|	SndShot
	|	SndPunch
	|	SndBreak
	|	SndCoin
	deriving (Eq, Show)

soundTypes = [SndJump, SndShot, SndPunch, SndBreak, SndCoin]

soundFn SndJump = "hoyo.wav"
soundFn SndShot = "suiteki.wav"
soundFn SndPunch = "po2.wav"
soundFn SndBreak = "cha.wav"
soundFn SndCoin = "puni.wav"


data BGMType =
		BGMMain
	deriving (Eq, Show)

bgmFn BGMMain = "bgm.mp3"
