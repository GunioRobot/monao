module Mixer (
	createMixer,
	playSE,
	SoundType(..),
	SoundResource,
	Mixer,
	loadSoundResource,
	soundTypes,
	BGMType(..),
	bgmFn,
	playBGM,
	stopBGM
) where

import Graphics.UI.SDL.Mixer
import Data.IORef
import Data.Maybe (catMaybes)
import System.IO.Unsafe (unsafePerformIO)

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
playSE mixer (Just (Just ad)) = flip catch (\_ -> return ()) $ do
	playChannel (-1) ad 0
	return ()

playingBGM :: IORef (Maybe Music)
playingBGM = unsafePerformIO $ newIORef Nothing

stopBGM mixer = do
	m <- readIORef playingBGM
	case m of
		Nothing -> return ()
		Just mus -> do
			freeMusic mus
			writeIORef playingBGM Nothing

playBGM :: Mixer -> String -> IO ()
playBGM mixer bgmfn = do
	stopBGM mixer
	music <- loadMUS bgmfn
	playMusic music (-1)
	writeIORef playingBGM $ Just music

data SoundType =
		SndJump
	|	SndShot
	|	SndPunch
	deriving (Eq, Show)

data BGMType =
		BGMMain
	deriving (Eq, Show)

soundPath = "data/snd/"

soundFn SndJump = "jump.ogg"
soundFn SndShot = "jump.ogg"
soundFn SndPunch = "jump.ogg"

bgmFn BGMMain = soundPath ++ "bgm.mp3"

soundTypes = [SndJump, SndShot, SndPunch]

type SoundResource = [(SoundType, Maybe Chunk)]

loadSoundResource :: [SoundType] -> IO SoundResource
loadSoundResource sndtypes = mapM load sndtypes
	where
		load :: SoundType -> IO (SoundType, Maybe Chunk)
		load sndtype = flip catch err $ do
			dat <- loadWAV $ (soundPath ++) $ soundFn sndtype
			return (sndtype, Just dat)
			where
				err _ = return (sndtype, Nothing)
