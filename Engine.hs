module Engine where
import World
import Common
import Song
import Element

import qualified Graphics.UI.SDL as SDL
--import qualified Graphics.UI.SDL.Mixer as Mixer
import Graphics.Rendering.OpenGL.GL.BasicTypes
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.PerFragment
import Graphics.Rendering.OpenGL.GL.Framebuffer
import Graphics.Rendering.OpenGL.GL.StateVar hiding (get)
import Graphics.Rendering.OpenGL.GL.BeginEnd

import Control.Monad.State
import Data.Word

----------

data GameState = GameState {
  isRunning :: Bool,
  ticks :: Time,
  world :: World
}

type Engine a = StateT GameState IO a

instance Show (GameState) where
  show gs = showGameState gs

showGameState :: GameState -> String
showGameState gs =
  "< isRunning=" ++ (show (isRunning gs)) ++ "; "
  ++"ticks=" ++ (show (ticks gs)) ++ "; "
  ++"world=" ++ (show (world gs)) ++ " >"

sync :: (GameState -> GameState) -> Engine ()
sync f = do
  gs <- get
  put $ f gs
  return ()

syncio :: (GameState -> IO GameState) -> Engine ()
syncio f = do
  gs <- get
  rgs <- liftIO $ f gs
  put rgs
  return ()

stop :: Engine ()
--stop = sync (\gs -> gs { isRunning = False })
stop = do
  gs <- get
  put (gs { isRunning = False })

loop :: Time -> (Time -> Engine a) -> Engine ()
loop ticks m = do
  gs <- get
  when (isRunning gs) $ do
    start <- liftIO $ SDL.getTicks
    m ticks
    end <- liftIO $ SDL.getTicks

    loop ((fromIntegral $ end - start) / 1000) m

guts :: Engine ()
guts = loop 0 (\ticks -> update ticks >> process >> render)

execute :: IO ()
execute = do
  gs <- execStateT
    (initEngine >> guts >> finalizeEngine)
    GameState { isRunning = True, ticks = 0, world = initialWorld }
  return ()

----------------

initEngine, finalizeEngine :: Engine ()
initEngine = do
  liftIO $ do
    SDL.init [SDL.InitVideo]--, SDL.InitAudio]
    --Mixer.openAudio 44100 SDL.AudioS16LSB 2 2048
  initWorld
finalizeEngine = do
  finalizeWorld
  liftIO $ do
    --Mixer.closeAudio
    SDL.quit

initWorld, finalizeWorld :: Engine ()
initWorld = do
  liftIO $ do
    SDL.glSetAttribute SDL.glRedSize 5
    SDL.glSetAttribute SDL.glGreenSize 5
    SDL.glSetAttribute SDL.glBlueSize 5
    SDL.glSetAttribute SDL.glDepthSize 16
    SDL.glSetAttribute SDL.glDoubleBuffer 1
    SDL.setVideoMode width height 32 [SDL.HWSurface, SDL.DoubleBuf, SDL.OpenGL]
    SDL.rawSetCaption (Just "huMAX") Nothing
    depthFunc $= Just Less
    clearColor $= Color4 0 0 0 1
    let (w,h) = (fromIntegral width, fromIntegral height)
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  syncio (\gs -> do
    w <- loadWorld $ world gs
    return $ gs { world = w })

finalizeWorld = do
  return ()

process :: Engine ()
process = do
  handleEvents
  where
    enableElement e = do
      gs <- get
      put gs { world = applyElement e True $ world gs }
      handleEvents
    disableElement e = do
      gs <- get
      put gs { world = applyElement e False $ world gs }
      handleEvents
    handleEvents = do
      event <- liftIO $ SDL.pollEvent
      case event of
        SDL.NoEvent -> return ()
        SDL.KeyDown keysym ->
          case SDL.symKey keysym of
            SDL.SDLK_ESCAPE -> stop
            SDL.SDLK_z -> enableElement Water
            SDL.SDLK_s -> enableElement Metal
            SDL.SDLK_x -> enableElement Earth
            SDL.SDLK_d -> enableElement Fire
            SDL.SDLK_c -> enableElement Wood
            _ -> handleEvents
        SDL.KeyUp keysym ->
          case SDL.symKey keysym of
            SDL.SDLK_ESCAPE -> stop
            SDL.SDLK_z -> disableElement Water
            SDL.SDLK_s -> disableElement Metal
            SDL.SDLK_x -> disableElement Earth
            SDL.SDLK_d -> disableElement Fire
            SDL.SDLK_c -> disableElement Wood
            _ -> handleEvents
        SDL.Quit -> stop
        _ -> handleEvents

update :: Time -> Engine ()
update ticks = do
  gs <- get
  put gs { world = updateWorld ticks $ world gs }
  when (endReached $ world gs) stop

render :: Engine ()
render = do
  clearScreen >> renderOutline >> renderWorld' >> swapBuffers
  where
    (w,h) = bounds
    clearScreen = liftIO $ do
      clear [ColorBuffer, DepthBuffer]
      matrixMode $= Projection
      loadIdentity
      ortho (-w) w (-h) h (-10) 10
      matrixMode $= Modelview 0
      loadIdentity
    renderOutline = liftIO $ do
      color $ Color3 (1 :: Scalar) 1 1
      renderPrimitive Lines $ mapM_ (\v -> vertex v) $ floorLines ++ wallLines
      where
        space = 50
        floorLines = vertices2 [((-w) + space, -250), (-space, -250),
                                ((-w) + space, -230), (-space, -230)]
        wallLines = vertices2 [(-space, -h), (-space, h),
                               (space, -h), (space, h),
                               ((-w) + space, h), ((-w) + space, -h),
                               (w - space, h), (w - space, -h)]
    renderWorld' = do
      gs <- get
      liftIO $ renderWorld (world gs)
    swapBuffers = liftIO $ do
      SDL.glSwapBuffers
