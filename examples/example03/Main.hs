{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Affection

import SDL (($=))
import qualified SDL

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU

import Physics.Bullet.Raw

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Linear as L

import System.Random (randomRIO)

import SpatialMath

import Options.Applicative
import Data.Semigroup ((<>))

import Init
import Types
import Foreign.C.Types (CInt(..))

opts :: Parser Opts
opts = Opts
  <$> option auto
    (  long "width"
    <> short 'w'
    <> value 1600
    <> showDefault
    <> metavar "NUM"
    <> help "Screen width"
    )
  <*> option auto
    (  long "height"
    <> short 'h'
    <> value 900
    <> showDefault
    <> metavar "NUM"
    <> help "Screen height"
    )

main :: IO ()
main = do
  o <- execParser $ info opts
    (  fullDesc
    <> progDesc "sample for hw game"
    )
  withAffection AffectionConfig
    { initComponents = All
    , windowTitle    = "hw"
    , windowConfigs   = [(0, SDL.defaultWindow
      { SDL.windowInitialSize = SDL.V2
        (CInt $ fromIntegral $ width o)
        (CInt $ fromIntegral $ height o)
      , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Normal 3 2
        }
      })]
    , initScreenMode = SDL.Fullscreen
    , preLoop = return ()
    , eventLoop = mapM_ handle
    , updateLoop = update
    , drawLoop = draw (width o) (height o)
    , loadState = load (width o) (height o)
    , cleanUp = const (return ())
    , canvasSize = Nothing
    }

update :: Double -> Affection StateData ()
update dt = do
  sd <- getAffection
  let phys = physics sd
      physos = physicsObjects sd
  liftIO $ stepSimulation (pWorld phys) dt 10 Nothing
  nvhs <- mapM (\(smallBall, vh) -> do
    ms1 <- liftIO $ getMotionState (bodyRigidBody smallBall)
    r1 <- liftIO $ return . fmap realToFrac =<< getPosition ms1
    return vh
      { shipPos = r1
      }
    ) (zip (poBalls physos) (vertHandles sd))
  -- (pos, rot) <- do
  --   ms <- liftIO $ getMotionState (bodyRigidBody $ poBall physos)
  --   npos <- liftIO $ return . fmap realToFrac =<< getPosition ms
  --   nrot <- liftIO $ return . fmap realToFrac =<< getRotation ms
  --   return (npos, nrot)
  -- let nship =
  --       (ship sd)
  --         { shipRot = rot
  --         , shipPos = pos
  --         }
  putAffection sd
    { -- ship = nship
      vertHandles = nvhs
    }

draw :: Word -> Word -> Affection StateData ()
draw w h =
  do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    StateData{..} <- getAffection
    let view = lookAt
          (cameraFocus camera +
            rotVecByEulerB2A
            (cameraRot camera)
            (V3 0 0 (-cameraDist camera)))
          (cameraFocus camera)
          (V3 0 1 0)
    drawShip shipProgram view ship
    mapM_ (drawShip handleProgram view) vertHandles
  where
    drawShip program view (Ship{..}) = do
      StateData{..} <- getAffection
      GL.currentProgram $= (Just . GLU.program $ program)
      let model = mkTransformation shipRot shipPos
          pvm = proj !*! view !*! model
      liftIO $ GLU.setUniform program "mvp" pvm
      GL.bindVertexArrayObject $= Just shipVao
      GL.bindBuffer GL.ArrayBuffer $= shipUVs
      liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral shipVaoLen)
      GL.currentProgram $= Nothing
      GL.bindBuffer GL.ArrayBuffer $= Nothing
      GL.bindVertexArrayObject $= Nothing

handle :: SDL.EventPayload -> Affection StateData ()
handle (SDL.WindowClosedEvent _) = quit

handle (SDL.KeyboardEvent dat) = do
  let key = SDL.keysymKeycode (SDL.keyboardEventKeysym dat)
  when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $
    handleKey key
handle (SDL.MouseMotionEvent dat) = do
  sd <- getAffection
  curMode <- SDL.getMouseLocationMode
  let (V2 rx ry) = fromIntegral <$> SDL.mouseMotionEventRelMotion dat
      c = camera sd
  putAffection sd
    { camera =
      case SDL.mouseMotionEventState dat of
        [SDL.ButtonRight] ->
          let (V3 sx sy _sz) = rotVecByEuler (cameraRot c) (V3 (rx / 10) 0 (ry / 10))
          in  c {cameraFocus = cameraFocus c + V3 sx 0 sy}
        [] ->
          if curMode == SDL.RelativeLocation
          then
            let dphi = pi / 4 / 45 / 10
                (Euler yaw pitch roll) = cameraRot c
                nangle
                  | nangle' >= qc = qc - mu
                  | nangle' <= -qc = -qc + mu
                  | otherwise     = nangle'
                  where
                    nangle' = (dphi * ry) + roll
                    qc = pi / 2
                    mu = 0.01
                nrot =
                  Euler
                    yaw
                    (pitch + (rx * dphi))
                    nangle
            in c
              { cameraRot = nrot
              }
          else
            c
        _ ->
          c
    }

handle _ = return ()

handleKey :: SDL.Keycode -> Affection StateData ()
handleKey code
  | code == SDL.KeycodeR = do
    curMode <- SDL.getMouseLocationMode
    if curMode == SDL.AbsoluteLocation
    then do
      _ <- SDL.setMouseLocationMode SDL.RelativeLocation
      return ()
    else do
      _ <- SDL.setMouseLocationMode SDL.AbsoluteLocation
      return ()
  | code == SDL.KeycodeP = do
    r <- liftIO $ randomRIO (0, 1)
    g <- liftIO $ randomRIO (0, 1)
    b <- liftIO $ randomRIO (0, 1)
    a <- liftIO $ randomRIO (0, 1)
    GL.clearColor $= GL.Color4 r g b a
  | code == SDL.KeycodeEscape =
    quit
  | code == SDL.KeycodeF = do
    dt <- deltaTime <$> get
    liftIO $ putStrLn $ show (1 / dt) ++ " FPS"
  | code == SDL.KeycodeT =
    toggleScreen
  -- | code `elem`
  --   [ SDL.KeycodeW
  --   , SDL.KeycodeS
  --   , SDL.KeycodeA
  --   , SDL.KeycodeD
  --   , SDL.KeycodeQ
  --   , SDL.KeycodeE
  --   ]
  --   = do
  --     sd <- getAffection
  --     let body = bodyRigidBody $ poBall $ physicsObjects sd
  --     ms <- liftIO $ getMotionState body
  --     rot <- liftIO $ return . fmap realToFrac =<< getRotation ms
  --     let tor = 5
  --         torqueimp = case code of
  --           SDL.KeycodeW -> rotate rot (V3 (-tor) 0 0) -- (-dphi)
  --           SDL.KeycodeS -> rotate rot (V3 tor 0 0) -- dphi
  --           SDL.KeycodeA -> rotate rot (V3 0 (-tor) 0) -- (-dphi)
  --           SDL.KeycodeD -> rotate rot (V3 0 tor 0) -- dphi
  --           SDL.KeycodeE -> rotate rot (V3 0 0 (-tor)) -- (-dphi)
  --           SDL.KeycodeQ -> rotate rot (V3 0 0 tor) -- dphi
  --           _            -> V3 0 0 0
  --     liftIO $ applyTorqueImpulse
  --       (bodyRigidBody $ poBall $ physicsObjects sd)
  --       torqueimp
  | otherwise =
    return ()
