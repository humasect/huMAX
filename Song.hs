module Song where
import Element
import Common

import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.BeginEnd
--import Graphics.UI.SDL.Mixer as Mixer
import Data.Maybe
import Data.List (partition)

import Debug.Trace


type Time = Scalar      -- 1.0 = one second
type TimeWindow = (Time,Time)

type Instrument = (Either FilePath Mixer.Chunk)
type Note = (Element,Time,Instrument)

data Song = Song {
  bpm :: Int,
  incoming :: [Note],
  hits :: [Note],
  misses :: [Note]
}

instance Show (Song) where
  show s = showSong s

showSong :: Song -> String
showSong s =
    "< bpm=" ++ (show $ bpm s) ++ "; >"


initialSong :: Song
initialSong =
  Song {
    bpm = 100,
    incoming = [],
    hits = [],
    misses = []
  }

loadSong :: (Int,[Note]) -> IO Song
loadSong (bpm, ns)= do
  return initialSong
{-
  ns' <- loadNotes ns
  return initialSong {
    incoming = ns'
  }
-}

loadNotes :: [Note] -> IO [Note]
loadNotes [] = return []
loadNotes (n@(ne,nt,ni):ns) = do
  c <- chunk ni
  ns' <- loadNotes ns
  return $ (ne,nt,Right c) : ns'
  where
  chunk =
    either
      (\l -> Mixer.loadWAV l)
      (\r -> return $ r)

noteBlock :: String -> [(Element,Time)] -> [Note]
noteBlock name [] = []
noteBlock name ns =
  map (\((e,t), i) -> (e,t,mosh i)) $ zip ns [1,2..]
  where
  mosh i = Left ("./Blink/e_" ++ name ++ (show i) ++ ".wav")    -- fixme.

---------------

timeToPixel :: Song -> Time -> Scalar
timeToPixel s t = t * (fromIntegral $ bpm s)

pixelToTime :: Song -> Scalar -> Time
pixelToTime s p = p / (fromIntegral $ bpm s)

--------------

noteHitTest :: (Element,Time) -> Note -> Bool
noteHitTest (e,t) (ne,nt,_) =
  e == ne &&
  nt >= (t - h) && nt <= (t + h)
  where h = 0.15 :: Time  -- hit fuzz/epsilon, mod by bpm/speed... monad.

hitNoteAt :: Song -> (Element,Time) -> (Song,Maybe Note)
hitNoteAt s@(Song {incoming=ins, hits=hns}) (e,t) =
  (s {
    incoming = snd hitSplit,
    hits = hns ++ fst hitSplit
  }, hitn)
  where
  hitn = case fst hitSplit of
    [] -> Nothing
    ns -> Just $ head ns
  hitSplit = partition (noteHitTest (e,t)) ins

missNotesAt :: Song -> Time -> Song
missNotesAt s@(Song {incoming=ins, misses=mns}) t =
  s {
    incoming = snd missSplit,
    misses = mns ++ fst missSplit
  }
  where
  missSplit = partition (\(_,nt,_) -> nt < t) ins

noteAt :: Song -> (Element,Time) -> Maybe Note
noteAt s (e,t) =
  case ns of
    [] -> Nothing
    _ -> Just $ head ns
  where
  ns = filter (noteHitTest (e,t)) $ incoming s

playNote :: Song -> Note -> IO ()
playNote s (e,_,c) = do
  return ()
{-
  playChunk c
  return ()
  where
  playChunk = either
    (\l -> fail ("Note '" ++ (show l) ++ "' not loaded."))
    (\r -> Mixer.playChannel (fromEnum e) r 0)
-}