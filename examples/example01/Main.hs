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

import Init
import Types

import Debug.Trace as T

main :: IO ()
main =
  withAffection AffectionConfig
    { initComponents = All
    , windowTitle    = "hw"
    , windowConfig   = SDL.defaultWindow
      { SDL.windowInitialSize = SDL.V2 1600 900
      , SDL.windowOpenGL = Just SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Normal 3 2
        }
      }
    , initScreenMode = SDL.Fullscreen
    , preLoop = return ()
    , eventLoop = mapM_ handle
    , updateLoop = update
    , drawLoop = draw
    , loadState = load
    , cleanUp = const (return ())
    , canvasSize = Nothing
    }

update :: Double -> Affection StateData ()
update dt = do
  sd <- getAffection
  let phys = physics sd
      physos = physicsObjects sd
  liftIO $ stepSimulation (pWorld phys) dt 10 Nothing
  (pos, rot) <- do
    ms <- liftIO $ getMotionState (bodyRigidBody $ poBall physos)
    npos <- liftIO $ return . fmap realToFrac =<< getPosition ms
    nrot <- liftIO $ return . fmap realToFrac =<< getRotation ms
    return (npos, nrot)
  let nship =
        (ship sd)
          { shipRot = rot
          , shipPos = pos
          }
  putAffection sd
    { ship = nship
    }

draw :: Affection StateData ()
draw = do
  GL.viewport $= (GL.Position 0 0, GL.Size 1600 900)
  StateData{..} <- getAffection
  GL.currentProgram $= (Just . GLU.program $ program)
  (\Ship{..} -> do
    let view = lookAt
          (cameraFocus camera +
            rotVecByEulerB2A
            (cameraRot camera)
            (V3 0 0 (-cameraDist camera)))
          (cameraFocus camera)
          (V3 0 1 0)
        model = mkTransformation shipRot shipPos
        pvm = proj !*! view !*! model
    liftIO $ GLU.setUniform program "mvp" pvm
    GL.bindVertexArrayObject $= Just shipVao
    liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral shipVaoLen)
    ) ship

handle :: SDL.EventPayload -> Affection StateData ()
handle (SDL.WindowClosedEvent _) = quit

handle (SDL.KeyboardEvent dat) = do
  let key = SDL.keysymKeycode (SDL.keyboardEventKeysym dat)
  when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $
    handleKey key
handle (SDL.MouseMotionEvent dat) = do
  sd <- getAffection
  let (V2 rx ry) = fromIntegral <$> SDL.mouseMotionEventRelMotion dat
      c = camera sd
  putAffection sd
    { camera =
      case SDL.mouseMotionEventState dat of
        [SDL.ButtonRight] ->
          let (V3 sx sy sz) = rotVecByEuler (cameraRot c) (V3 (rx / 10) 0 (ry / 10))
          in  c {cameraFocus = cameraFocus c + V3 sx 0 sy}
        [] ->
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
          in  c
            { cameraRot = nrot
            }
        _ ->
          c
    }

handle _ = return ()

handleKey :: SDL.Keycode -> Affection StateData ()
handleKey code
  | code == SDL.KeycodeR =
    GL.clearColor $= GL.Color4 1 0 0 1
  | code == SDL.KeycodeG =
    GL.clearColor $= GL.Color4 0 1 0 1
  | code == SDL.KeycodeB =
    GL.clearColor $= GL.Color4 0 0 1 1
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
  | code `elem`
    [ SDL.KeycodeW
    , SDL.KeycodeS
    , SDL.KeycodeA
    , SDL.KeycodeD
    , SDL.KeycodeQ
    , SDL.KeycodeE
    ]
    = do
      sd <- getAffection
      let body = bodyRigidBody $ poBall $ physicsObjects sd
      ms <- liftIO $ getMotionState body
      rot <- liftIO $ return . fmap realToFrac =<< getRotation ms
      let tor = 5
          torqueimp = case code of
            SDL.KeycodeW -> rotate rot (V3 (-tor) 0 0) -- (-dphi)
            SDL.KeycodeS -> rotate rot (V3 tor 0 0) -- dphi
            SDL.KeycodeA -> rotate rot (V3 0 (-tor) 0) -- (-dphi)
            SDL.KeycodeD -> rotate rot (V3 0 tor 0) -- dphi
            SDL.KeycodeE -> rotate rot (V3 0 0 (-tor)) -- (-dphi)
            SDL.KeycodeQ -> rotate rot (V3 0 0 tor) -- dphi
            _            -> V3 0 0 0
      liftIO $ applyTorqueImpulse
        (bodyRigidBody $ poBall $ physicsObjects sd)
        torqueimp
  | otherwise =
    return ()
