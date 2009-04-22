{-# LANGUAGE ForeignFunctionInterface #-}
module AppUtil (
	getKeyState,
	key2btn,

	ImageResource,
	loadImageResource,
	releaseImageResource,
	getImageSurface,
	putimg,

	cellCrd,
	Rect(..),
	ishit
) where

import Graphics.UI.SDL
import Graphics.UI.SDL.Utilities
import Data.Maybe (fromJust)
import Data.Bits ((.|.))
import Foreign
import Foreign.C.Types
import Pad
import Const
import Images

-- `SDL_GetKeyState' is not defined in Graphic.UI.SDL
foreign import ccall unsafe "SDL_GetKeyState" sdlGetKeyState :: Ptr CInt -> IO (Ptr Word8)

-- Get keyboard state and return function
getKeyState :: IO (SDLKey -> Bool)
getKeyState = alloca $ \nkp -> do
	kp <- sdlGetKeyState nkp
	let f = \k -> (/= 0) $ unsafePerformIO $ (peekByteOff kp $ fromIntegral $ Graphics.UI.SDL.Utilities.fromEnum k :: IO Int8)
	return f

key2btn :: (SDLKey -> Bool) -> Int
key2btn ks = u .|. d .|. l .|. r .|. a .|. b
	where
		u = press padU [SDLK_UP, SDLK_i]
		d = press padD [SDLK_DOWN, SDLK_k]
		l = press padL [SDLK_LEFT, SDLK_j]
		r = press padR [SDLK_RIGHT, SDLK_l]
		a = press padA [SDLK_SPACE, SDLK_z]
		b = press padB [SDLK_LSHIFT, SDLK_RSHIFT]
		press v ls = if any ks ls then v else 0

-- Image resource
type ImageResource = [(ImageType, Surface)]

-- Load image resource
loadImageResource :: [ImageType] -> IO ImageResource
loadImageResource = mapM load
	where
		load imgtype = do
			sur <- loadBMP $ ("data/img/" ++) $ imageFn imgtype
			setNuki sur
			converted <- displayFormat sur
			freeSurface sur
			return (imgtype, converted)

		setNuki sur = setColorKey sur [SrcColorKey] (Pixel 0) >> return ()		-- Set color key to palet 0

releaseImageResource :: ImageResource -> IO ()
releaseImageResource = mapM_ (\(t, sur) -> freeSurface sur)

getImageSurface :: ImageResource -> ImageType -> Surface
getImageSurface imgres = fromJust . (`lookup` imgres)

putimg :: Surface -> ImageResource -> ImageType -> Int -> Int -> IO ()
putimg sur imgres imgtype x y = do
	blitSurface (getImageSurface imgres imgtype) Nothing sur (Just $ Rect x y 0 0)
	return ()


-- From fixed point integer to cell coordinate
cellCrd :: Int -> Int
cellCrd x = x `div` (chrSize * one)



-- ========
--data Rect = Rect Int Int Int Int


ishit :: Rect -> Rect -> Bool
ishit (Rect l1 t1 r1 b1) (Rect l2 t2 r2 b2) =
	l1 < r2 && t1 < b2 && l2 < r1 && t2 < b1
