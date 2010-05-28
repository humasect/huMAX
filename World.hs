module World where
import Element
import Song
import Common

--import HumaDeRumba (humaDeRumba)
import Blink (blinkEasy)--,blinkHard)

import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.BeginEnd

type PaddleStates = ElementValues Bool

data World = World {
  endReached :: Bool,

  --paddleStates :: ElementValues Bool,   -- really key states.
  paddleStates :: ElementValues (Maybe Note),
  paddleFade :: ElementValues Scalar,

  timeWindow :: TimeWindow,
  song :: Song,
  speed :: Scalar,

  hp :: Int
}

instance Show (World) where
  show w = showWorld w

showWorld :: World -> String
showWorld w =
  "< endReached=" ++ (show (endReached w)) ++ "; "
  ++"paddleStates=" ++ (show (paddleStates w)) ++ "; "
  ++"timeWindow=" ++ (show (timeWindow w)) ++ "; "
  ++"song=" ++ (show (song w)) ++ "> "

initialWorld :: World
initialWorld =
  World {
    endReached = False,

    paddleStates = initialElementValues Nothing,
    paddleFade = initialElementValues 0.0,

    timeWindow = (-1, 4),    -- start with some "intro"
    song = initialSong,
    speed = 1.0,

    hp = 5
  }

loadWorld :: World -> IO World
loadWorld w = do
  s <- loadSong blinkEasy
  return $ w { song = s }

applyElement :: Element -> Bool -> World -> World
applyElement e True w@(World {timeWindow=(st,_), song=s,
                           paddleFade=pf, paddleStates=ps}) =
  w {
    paddleStates = (setElementValue e n ps),
    song = s',
    paddleFade = setElementValue e 1.0 pf
  }
  where
  (s',n) = s `hitNoteAt` (e, (pixelToTime s paddleOffset) + st)

applyElement e False w@(World {paddleStates=ps}) =
  w { paddleStates = setElementValue e Nothing ps }

-- should be.. in.. ?? Engine () nad?
updateWorld :: Time -> World -> World
updateWorld t w@(World {timeWindow = tw@(st,et), song=s,
                        paddleStates=ps, paddleFade=pf }) =
  w {
    timeWindow = (st + scroll, et + scroll),
    song = s `missNotesAt` st,
    paddleFade = mapElementValues (\f -> f - fade) pf,
    paddleStates = initialElementValues Nothing
  }
  where
  scroll = t * (speed w)
  fade = t * 3

--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------

-- related to hitEpsilon, a bit.
paddleOffset = 60 :: Scalar

playPaddle :: World -> Element -> IO ()
playPaddle (World {paddleStates=ps, song=s}) e = do
  maybe
    (return ())
    (\n -> s `playNote` n)
    (elementValue e ps)

renderWorld :: World -> IO ()
renderWorld w@(World {song=s, paddleStates=ps, timeWindow=tw}) = do
  --mapElementValues (\mn -> maybe (return ()) (\n -> s `playNote` n) mn) ps
  mapM_ (playPaddle w) allElements

  unsafePreservingMatrix $ do
    translate $ Vector3 (-200) (-300) (0 :: Scalar)
    renderNotes s tw (incoming s) Nothing
    renderPaddles w

  unsafePreservingMatrix $ do
    translate $ Vector3 200 300 (0 :: Scalar)
    renderNotes s tw (misses s) $ Just 0.33
    renderNotes s tw (hits s) $ Just 1.0

renderNotes :: Song -> TimeWindow -> [Note] -> Maybe Scalar -> IO ()
renderNotes s (st,_) ns c' =
  unsafePreservingMatrix $ do
    translate $ Vector3 0 (timeToPixel s (-st)) 0
    mapM_ note ns  --(s `notesInWindow` tw)
  where
  note (e,t,_) = do
    maybe (color $ elementColor3 e) (\c -> color $ Color4 c c c 1.0) c'
    renderBrick s LineLoop e $ timeToPixel s t

renderPaddles :: World -> IO ()
renderPaddles w@(World {song=s, paddleFade=pf}) =
  mapM_ paddle $ filter (\e -> elementValue e pf >= 0.0) allElements
  where
  paddle e = do
    color $ elementColor4 e (elementValue e pf)
    renderBrick s Quads e paddleOffset

renderBrick :: Song -> PrimitiveMode -> Element -> Scalar -> IO ()
renderBrick s mode e ofy = do
  renderPrimitive mode $ mapM_ (\v -> vertex v) brickVertices
  where
  ofx = case e of
    Water -> -120
    Metal -> -60
    Earth -> 0
    Fire -> 60
    Wood -> 120
  brickVertices =
    vertices2 $ map (\(x,y) -> (x + ofx, y + ofy))
      [(-30, -10), (30, -10), (30, 10), (-30, 10)]
