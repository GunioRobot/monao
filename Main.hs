{-# LANGUAGE ForeignFunctionInterface #-}
-- Monao

module Main where

import Graphics.UI.SDL hiding (Event)

import Player
import Field
import Util
import AppUtil
import Pad
import Const
import Images
import Sounds
import Font
import Event
import Actor
import Actor.AnimBlock
import Actor.Kuribo
import Actor.Nokonoko
import Actor.Kinoko
import Actor.Flower
import Actor.BrokenBlock
import Actor.CoinGet
import Actor.ScoreAdd
import Mixer

foreign export ccall "hs_main" main :: IO ()

-- Background color
backColor = Pixel 0x5080FF

-- Display command
type Scr = Surface -> IO ()

type Resources = (ImageResource, SoundResource)

-- Program etrny point
main :: IO ()
main = do
	Graphics.UI.SDL.init [InitVideo]
	setCaption wndTitle wndTitle
	sur <- setVideoMode screenWidth screenHeight wndBpp [HWSurface, DoubleBuf, AnyFormat]
	initMixer
	strm <- delayedStream (1000000 `div` frameRate) fetch
	scrs <- process $ map snd $ takeWhile notQuit strm
	mapM_ (\scr -> scr sur) scrs
	quit

	where
		-- fetch for environment
		fetch = do
			quit <- procSDLEvent
			ks <- getKeyState
			return (quit, ks)
		notQuit = not . fst

-- State of Game
data GameGame =
	GameGame {
		pl :: Player,
		fld :: Field,
		actors :: [ActorWrapper],
		time :: Int,
		snds :: [SoundType]
	}

-- Process whole key input and return display command list
process :: [KeyProc] -> IO [Scr]
process kss = do
	imgres <- loadImageResource imageTypes
	sndres <- loadSoundResource soundTypes
	fldmap <- loadField 0

	let tmpscrs = doTitle fldmap kss
	let scrs = zipWith (action (imgres,sndres)) tmpscrs kss
	return $ scrs ++ [final imgres sndres]

	where
		-- Common Action
		action resources scr ks sur = do
			scr resources sur
			if ks SDLK_s
				then saveBMP sur "ss.bmp" >> return ()
				else return ()
			Graphics.UI.SDL.flip sur
			return ()
		-- Finalize
		final imgres sndres sur = releaseImageResource imgres

-- Title
doTitle :: Field -> [KeyProc] -> [Resources -> Scr]
doTitle fldmap kss = loop kss
	where
		loop :: [KeyProc] -> [Resources -> Scr]
		loop (ks:kss) = res : left ks kss

		res resources@(imgres,_) sur = do
			fillRect sur Nothing backColor
			renderTitle imgres sur

		left ks kss
			| ks SDLK_SPACE	= doGame fldmap kss
			| otherwise		= loop kss


-- Scroll event
scrollEvent :: Field -> Int -> (Field, [Event])
scrollEvent fld cx
	| cx < length (head fld)	= foldl proc (fld, []) $ zip [0..] cols
	| otherwise					= (fld, [])
	where
		proc (f, e) (cy, c) =
			case event cy c of
				Just ev	-> (fieldSet f cx cy ' ', ev : e)
				Nothing	-> (f, e)
		cols = map (!! cx) fld
		event cy c
			| c `elem` "kn"	= Just $ EvAddActor $ genActor c
			| otherwise		= Nothing
			where
				genActor c = case c of
					'k'	-> ActorWrapper $ newKuribo cx cy
					'n'	-> ActorWrapper $ newNokonoko cx cy



-- Collision detection and response
hitcheck :: Player -> [ActorWrapper] -> (Player, [ActorWrapper], [Event])
hitcheck player actors = foldl proc (player, [], []) actors
	where
		proc (pl, ac, ev) (ActorWrapper a) = case getHitRect a of
			Nothing	-> nothingHappened
			Just rc	->
				if not $ ishit plrc rc
					then nothingHappened
					else (pl', ac', ev')
			where
				nothingHappened = (pl, ac ++ [ActorWrapper a], ev)
				plrc = getPlayerHitRect player
				(pl', a', evtmp) = onHit pl a
				ac' = case a' of
					Just a''	-> ac ++ [a'']
					Nothing		-> ac
				ev' = ev ++ evtmp

-- Game
doGame :: Field -> [KeyProc] -> [Resources -> Scr]
doGame fldmap kss = start : loop initialPad initialState (tail kss)
	where
		start resources sur = do
			playBGM $ bgmPath ++ bgmFn BGMMain

		loop :: Pad -> GameGame -> [KeyProc] -> [Resources -> Scr]
		loop opad gs (ks:kss) = scr' : left ks kss
			where
				pad = updatePad opad $ key2btn ks
				(scr', gs') = updateProc pad gs
				isPlayerDead = getPlayerY (pl gs') >= (screenHeight + chrSize * 2) * one
				timeOver = time gs' <= 0

				left ks kss
					| isPlayerDead || timeOver	= doGameOver fldmap kss
					| otherwise					= loop pad gs' kss

		-- Update
		updateProc :: Pad -> GameGame -> (Resources -> Scr, GameGame)
		updateProc pad gs = (scr', gs')
			where
				time' = max 0 (time gs - 1)
				(fld', screv') = scrollEvent (fld gs) $ getScrollPos (pl gs) `div` chrSize + 18

				(pl', plev) = updatePlayer pad fld' (pl gs)
				actors_updates = updateActors (fld gs) (actors gs)
				actors' = filterActors $ map fst actors_updates
				ev' = concatMap snd actors_updates

				(pl'', actors'', ev'') = hitcheck pl' actors'

				gstmp = gs { pl = pl'', fld = fld', actors = actors'', time = time' }
				allEvent = plev ++ ev' ++ screv' ++ ev''
				gs' = procEvent gstmp allEvent
				scr' resources@(_, sndres) sur = do
					mapM_ (\ev -> case ev of
							EvSound sndtype	->	play sndtype
							otherwise		->	return ()
						) allEvent
					renderProc gs' resources sur

					where
						play sndtype = do
							if True
								then do
									playSE $ lookup sndtype sndres
								else do
									-- Instead of play wav, print message
									putStrLn $ "play " ++ show sndtype
									return ()

		initialState = GameGame { pl = newPlayer, fld = fldmap, actors = [], time = 400 * timeBase, snds = [] }


-- Game over
doGameOver fldmap kss = end : doTitle fldmap (tail kss)
	where
		end resources sur = do
			stopBGM


-- Process events
procEvent :: GameGame -> [Event] -> GameGame
procEvent gs ev = foldl proc gs ev
	where
		proc gs (EvHitBlock imgtype cx cy bSuper)
			| hardBlock c			= gs
			| bSuper && breakable c	= breakBlock gs cx cy
			| c == 'K'				= genKinoko
			| c == '?'				= getCoin
			| otherwise				= gs'
			where
				c = fieldRef (fld gs) cx cy
				breakable c = c == 'O'

				gs' = gs { fld = fld', actors = actors' }
				actors' = actors gs ++ [ActorWrapper $ newAnimBlock cx cy $ fieldRef (fld gs) cx cy]
				fld' = fieldSet (fld gs) cx cy '*'

				breakBlock gs cx cy =
					gs {
						fld = fieldSet (fld gs) cx cy ' ',
						actors = actors gs ++ map ActorWrapper (newBrokenBlock cx cy),
						pl = addScore pointBreakBlock $ pl gs
						}
				genKinoko = gs' { actors = actors gs' ++ [a] }
					where a = if not bSuper then ActorWrapper $ newKinoko cx cy else ActorWrapper $ newFlower cx cy
				getCoin = gs' { actors = actors gs' ++ [ActorWrapper a], pl = addScore pointGetCoin $ playerGetCoin $ pl gs' }
					where a = newCoinGet cx cy

		proc gs (EvSetField cx cy c) = gs { fld = fieldSet (fld gs) cx cy c }
		proc gs (EvAddActor act) = gs { actors = actors gs ++ [act] }
		proc gs (EvScoreAddEfe sx sy pnt) = gs { actors = actors gs ++ [ActorWrapper $ newScoreAdd sx sy pnt] }
		proc gs (EvSound sndtype) = gs

-- Render
renderProc :: GameGame -> Resources -> Scr
renderProc gs (imgres,sndres) sur = do
	fillRect sur Nothing backColor

	let scrx = getScrollPos (pl gs)

	renderField sur imgres scrx (fld gs)
	renderInfo gs imgres sur
	renderActors imgres scrx sur (actors gs)
	renderPlayer sur imgres scrx (pl gs)

	return ()

-- Render information
renderInfo gs imgres sur = do
	puts  3 1 "MONAO"
	puts  3 2 $ deciWide 6 '0' $ getPlayerScore (pl gs)
	puts 11 2 ("?*" ++ deciWide 2 '0' (getPlayerCoin $ pl gs))
	puts 18 1 "WORLD"
	puts 19 2 "1-1"
	puts 25 1 "TIME"
	puts 26 2 $ deciWide 3 '0' $ (time gs + timeBase-1) `div` timeBase
	where
		puts = fontPut font sur
		font = Font (getImageSurface imgres ImgFont) 8 8 16

-- Render title screen
renderTitle imgres sur = do
	putimg sur imgres ImgTitle (5*8) (3*8)
--	puts 13 14 "@1985 NINTENDO"
	puts  9 17 "> 1 PLAYER GAME"
--	puts  9 19 "  2 PLAYER GAME"
	puts 12 22 "TOP- 000000"
	where
		puts = fontPut font sur
		font = Font (getImageSurface imgres ImgFont) 8 8 16
