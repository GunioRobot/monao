module AppUtil (
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
import Data.Maybe (fromJust)
import Data.Bits ((.&.), (.|.), complement, shiftL)
import Const
import Images

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
