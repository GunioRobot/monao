module Mixer (
	createMixer,
	playSE,
	SoundType(..),
	SoundResource,
	Mixer,
	loadSoundResource,
	soundTypes
) where

import Graphics.UI.SDL
import Graphics.UI.SDL.Mixer
import Data.IORef
import Data.Maybe (catMaybes)
import Util (pair)

type AudioData = ()
type Mixer = IORef [AudioData]

freq = 22050
format = AudioS16Sys
channel = 2
samples = 4096

createMixer = do
	mixer <- newIORef []
	openAudio freq format channel samples
	return mixer

playSE mixer (Just Nothing) = return ()
playSE mixer (Just (Just ad)) = do
	playChannel (-1) ad 1
	return ()


data SoundType =
		SndJump
	|	SndShot
	|	SndPunch
	deriving (Eq, Show)

soundFn SndJump = "jump.ogg"
soundFn SndShot = "jump.ogg"
soundFn SndPunch = "jump.ogg"

soundTypes = [SndJump, SndShot, SndPunch]

type SoundResource = [(SoundType, Maybe Chunk)]

loadSoundResource :: [SoundType] -> IO SoundResource
loadSoundResource sndtypes = mapM load sndtypes
	where
		load :: SoundType -> IO (SoundType, Maybe Chunk)
		load sndtype = Prelude.flip catch err $ do
			dat <- loadWAV $ ("data/snd/" ++) $ soundFn sndtype
			return (sndtype, Just dat)
			where
				err _ = return (sndtype, Nothing)
