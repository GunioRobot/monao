module Mixer (
	initMixer,
	playSE,
	SoundType(..),
	SoundResource,
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

freq = 22050
format = AudioS16Sys
channel = 2
samples = 4096

initMixer = do
	openAudio freq format channel samples

playSE (Just Nothing) = return ()
playSE (Just (Just ad)) = flip catch (\_ -> return ()) $ do
	playChannel (-1) ad 0
	return ()

playingBGM :: IORef (Maybe Music)
playingBGM = unsafePerformIO $ newIORef Nothing

stopBGM = do
	m <- readIORef playingBGM
	case m of
		Nothing -> return ()
		Just mus -> do
			freeMusic mus
			writeIORef playingBGM Nothing

playBGM :: String -> IO ()
playBGM bgmfn = do
	stopBGM
	maybeMusic <- tryLoadMUS bgmfn
	case maybeMusic of
		Just music -> do
			playMusic music (-1)
			writeIORef playingBGM $ Just music
		Nothing -> return ()

data SoundType =
		SndJump
	|	SndShot
	|	SndPunch
	|	SndBreak
	|	SndCoin
	deriving (Eq, Show)

data BGMType =
		BGMMain
	deriving (Eq, Show)

soundPath = "data/snd/"

soundFn SndJump = "hoyo.wav"
soundFn SndShot = "suiteki.wav"
soundFn SndPunch = "po2.wav"
soundFn SndBreak = "cha.wav"
soundFn SndCoin = "puni.wav"

bgmFn BGMMain = soundPath ++ "bgm.mp3"

soundTypes = [SndJump, SndShot, SndPunch, SndBreak, SndCoin]

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
