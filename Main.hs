{-# LANGUAGE ForeignFunctionInterface #-}
-- Monao

module Main where

import Data.Maybe (fromJust)
import Graphics.UI.SDL hiding (Event)
import System.Environment (getArgs)

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

-- Background color
backColor :: Pixel
backColor = Pixel 0x5080FF

-- Display command
type Scr = Surface -> IO ()

type Resources = (ImageResource, SoundResource)

sdlInitFlags :: [SurfaceFlag]
sdlInitFlags = [HWSurface, DoubleBuf, AnyFormat]

-- Program etrny point
foreign export ccall "hs_main" main :: IO ()
main :: IO ()
main = do
	args <- getArgs
	let flags = if not (null args) && head args == "--fullscreen" then Fullscreen : sdlInitFlags else sdlInitFlags

	Graphics.UI.SDL.init [InitVideo]
	setCaption wndTitle wndTitle
	sur <- setVideoMode screenWidth screenHeight wndBpp flags
	initMixer
	strm <- delayedStream (1000000 `div` frameRate) fetch
	scrs <- process $ map snd $ takeWhile notQuit strm
	mapM_ (\scr -> scr sur) scrs
	quit

	where
		-- fetch for environment
		fetch = do
			bQuit <- procSDLEvent
			ks <- getKeyState
			return (bQuit, ks)
		notQuit = not . fst

-- State of Game
data GameGame =
	GameGame {
		pl_of :: Player,
		fld_of :: Field,
		actors_of :: [ActorWrapper],
		time_of :: Int,
		snds_of :: [SoundType]
	}

-- Process whole key input and return display command list
process :: [KeyProc] -> IO [Scr]
process kss = do
	imgres <- loadImageResource imageTypes
	sndres <- loadSoundResource soundTypes
	fldmap <- loadField 0

	let tmpscrs = doTitle fldmap kss
	let scrs = zipWith (action (imgres,sndres)) tmpscrs kss
	return $ scrs ++ [final (imgres,sndres)]

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
		final (imgres,_) _ = releaseImageResource imgres

-- Title
doTitle :: Field -> [KeyProc] -> [Resources -> Scr]
doTitle fldmap keyprocs = loop keyprocs
	where
		loop :: [KeyProc] -> [Resources -> Scr]
		loop (ks:kss) = res : left ks kss
		loop [] = undefined

		res (imgres,_) sur = do
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
			| c `elem` "kn"	= Just $ EvAddActor $ genActor
			| otherwise		= Nothing
			where
				genActor = case c of
					'k'	-> ActorWrapper $ newKuribo cx cy
					'n'	-> ActorWrapper $ newNokonoko cx cy
					_	-> undefined



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
doGame fldmap keyprocs = start : loop initialPad initialState (tail keyprocs)
	where
		start _ _ = do
			playBGM $ bgmPath ++ bgmFn BGMMain

		loop :: Pad -> GameGame -> [KeyProc] -> [Resources -> Scr]
		loop opad gs (ks:kss) = scr' : left
			where
				pad = updatePad opad $ key2btn ks
				(scr', gs') = updateProc pad gs
				isPlayerDead = getPlayerY (pl_of gs') >= (screenHeight + chrSize * 2) * one
				timeOver = time_of gs' <= 0

				left
					| isPlayerDead || timeOver	= doGameOver fldmap kss
					| otherwise					= loop pad gs' kss
		loop _ _ [] = undefined

		-- Update
		updateProc :: Pad -> GameGame -> (Resources -> Scr, GameGame)
		updateProc pad gs = (scr', gs')
			where
				time' = max 0 (time_of gs - 1)
				(fld', screv') = scrollEvent (fld_of gs) $ getScrollPos (pl_of gs) `div` chrSize + 18

				(pl', plev) = updatePlayer pad fld' (pl_of gs)
				actors_updates = updateActors (fld_of gs) (actors_of gs)
				actors' = filterActors $ map fst actors_updates
				ev' = concatMap snd actors_updates

				(pl'', actors'', ev'') = hitcheck pl' actors'

				gstmp = gs { pl_of = pl'', fld_of = fld', actors_of = actors'', time_of = time' }
				allEvent = plev ++ ev' ++ screv' ++ ev''
				gs' = procEvent gstmp allEvent
				scr' resources@(_, sndres) sur = do
					mapM_ (\ev -> case ev of
							EvSound sndtype	->	play sndtype
							_				->	return ()
						) allEvent
					renderProc gs' resources sur

					where
						play sndtype = do
							if True
								then do
									playSE $ fromJust $ lookup sndtype sndres
								else do
									-- Instead of play wav, print message
									putStrLn $ "play " ++ show sndtype
									return ()

		initialState = GameGame { pl_of = newPlayer, fld_of = fldmap, actors_of = [], time_of = 400 * timeBase, snds_of = [] }


-- Game over
doGameOver :: Field -> [KeyProc] -> [Resources -> Scr]
doGameOver fldmap kss = end : doTitle fldmap (tail kss)
	where
		end _ _ = do
			stopBGM


-- Process events
procEvent :: GameGame -> [Event] -> GameGame
procEvent gs ev = foldl proc gs ev
	where
		proc gs' (EvHitBlock _ cx cy bSuper)
			| hardBlock c			= gs'
			| bSuper && breakable	= breakBlock
			| c == 'K'				= genKinoko
			| c == '?'				= getCoin
			| otherwise				= gs''
			where
				c = fieldRef (fld_of gs') cx cy
				breakable = c == 'O'

				gs'' = gs' { fld_of = fld', actors_of = actors' }
				actors' = actors_of gs' ++ [ActorWrapper $ newAnimBlock cx cy $ fieldRef (fld_of gs') cx cy]
				fld' = fieldSet (fld_of gs') cx cy '*'

				breakBlock =
					gs' {
						fld_of = fieldSet (fld_of gs') cx cy ' ',
						actors_of = actors_of gs' ++ map ActorWrapper (newBrokenBlock cx cy),
						pl_of = addScore pointBreakBlock $ pl_of gs'
						}
				genKinoko = gs'' { actors_of = actors_of gs'' ++ [a] }
					where a = if not bSuper then ActorWrapper $ newKinoko cx cy else ActorWrapper $ newFlower cx cy
				getCoin = gs'' { actors_of = actors_of gs'' ++ [ActorWrapper a], pl_of = addScore pointGetCoin $ playerGetCoin $ pl_of gs'' }
					where a = newCoinGet cx cy

		proc gs' (EvSetField cx cy c) = gs' { fld_of = fieldSet (fld_of gs') cx cy c }
		proc gs' (EvAddActor act) = gs' { actors_of = actors_of gs' ++ [act] }
		proc gs' (EvScoreAddEfe sx sy pnt) = gs' { actors_of = actors_of gs' ++ [ActorWrapper $ newScoreAdd sx sy pnt] }
		proc gs' (EvSound _) = gs'

-- Render
renderProc :: GameGame -> Resources -> Scr
renderProc gs (imgres,_) sur = do
	fillRect sur Nothing backColor

	let scrx = getScrollPos (pl_of gs)

	renderField sur imgres scrx (fld_of gs)
	renderInfo gs imgres sur
	renderActors imgres scrx sur (actors_of gs)
	renderPlayer sur imgres scrx (pl_of gs)

	return ()

-- Render information
renderInfo :: GameGame -> ImageResource -> Scr
renderInfo gs imgres sur = do
	puts  3 1 "MONAO"
	puts  3 2 $ deciWide 6 '0' $ getPlayerScore (pl_of gs)
	puts 11 2 ("?*" ++ deciWide 2 '0' (getPlayerCoin $ pl_of gs))
	puts 18 1 "WORLD"
	puts 19 2 "1-1"
	puts 25 1 "TIME"
	puts 26 2 $ deciWide 3 '0' $ (time_of gs + timeBase-1) `div` timeBase
	where
		puts = fontPut font sur
		font = Font (getImageSurface imgres ImgFont) 8 8 16

-- Render title screen
renderTitle :: ImageResource -> Scr
renderTitle imgres sur = do
	putimg sur imgres ImgTitle (5*8) (3*8)
--	puts 13 14 "@1985 NINTENDO"
	puts  9 17 "> 1 PLAYER GAME"
--	puts  9 19 "  2 PLAYER GAME"
	puts 12 22 "TOP- 000000"
	where
		puts = fontPut font sur
		font = Font (getImageSurface imgres ImgFont) 8 8 16
