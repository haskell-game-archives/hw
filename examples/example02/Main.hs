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

main :: IO ()
main =
  withAffection AffectionConfig
    { initComponents = All
    , windowTitle    = "hw - example 02"
    , windowConfigs  =
      [ ( 0
        , SDL.defaultWindow
          { SDL.windowInitialSize = SDL.V2 1920 1080
          , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
            { SDL.glProfile = SDL.Core SDL.Normal 3 2
            }
          }
        )
      ]
    , initScreenMode = SDL.FullscreenDesktop
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
      g = 0.0667300
      -- g = 0.0000000000667300

  mapM_ (\smallBall -> do
    ms1 <- liftIO $ getMotionState (bodyRigidBody smallBall)
    ms2 <- liftIO $ getMotionState (bodyRigidBody $ poBigBall physos)
    r1 <- liftIO $ return . fmap realToFrac =<< getPosition ms1
    r2 <- liftIO $ return . fmap realToFrac =<< getPosition ms2
    let m1 = bodyMass smallBall
        -- m2 = bodyMass (poBigBall physos)
        -- m2 = 1000000000000000
        m2 = 1000000
        eta_sq = 0.1 ^ 2
        force = (g * m2 * m1 *^ (r2 - r1)) ^/
          ((sqrt (((r2 - r1) `dot` (r2 - r1)) + eta_sq)) ^ 3)
    liftIO $ applyCentralForce (bodyRigidBody smallBall) force
    ) (poSmallBalls physos ++ poBigBalls physos)

  mapM_ (\(bb1, bb2) -> do
    ms1 <- liftIO $ getMotionState (bodyRigidBody bb1)
    ms2 <- liftIO $ getMotionState (bodyRigidBody bb2)
    r1 <- liftIO $ return . fmap realToFrac =<< getPosition ms1
    r2 <- liftIO $ return . fmap realToFrac =<< getPosition ms2
    let m1 = bodyMass bb1
        -- m2 = bodyMass (poBigBall physos)
        m2 = bodyMass bb2
        eta_sq = 0.1 ^ 2
        force = (g * m2 * m1 *^ (r2 - r1)) ^/
          ((sqrt (((r2 - r1) `dot` (r2 - r1)) + eta_sq)) ^ 3)
    liftIO $ applyCentralForce (bodyRigidBody bb1) force
    ) ((,) <$> (poBigBalls physos) <*> (poBigBalls physos))

  mapM_ (\(bb1, bb2) -> do
    ms1 <- liftIO $ getMotionState (bodyRigidBody bb1)
    ms2 <- liftIO $ getMotionState (bodyRigidBody bb2)
    r1 <- liftIO $ return . fmap realToFrac =<< getPosition ms1
    r2 <- liftIO $ return . fmap realToFrac =<< getPosition ms2
    let m1 = bodyMass bb1
        -- m2 = bodyMass (poBigBall physos)
        m2 = bodyMass bb2
        eta_sq = 0.1 ^ 2
        force = (g * m2 * m1 *^ (r2 - r1)) ^/
          ((sqrt (((r2 - r1) `dot` (r2 - r1)) + eta_sq)) ^ 3)
    liftIO $ applyCentralForce (bodyRigidBody bb1) force
    ) ((,) <$> (poSmallBalls physos) <*> (poBigBalls physos))

  liftIO $ stepSimulation (pWorld phys) dt 10 Nothing
  posrots <- mapM ((\ball -> do
    ms <- liftIO $ getMotionState ball
    npos <- liftIO $ return . fmap realToFrac =<< getPosition ms
    nrot <- liftIO $ return . fmap realToFrac =<< getRotation ms
    return (npos, nrot))
    . bodyRigidBody) (poSmallBalls physos)
  posrots2 <- mapM ((\ball -> do
    ms <- liftIO $ getMotionState ball
    npos <- liftIO $ return . fmap realToFrac =<< getPosition ms
    nrot <- liftIO $ return . fmap realToFrac =<< getRotation ms
    return (npos, nrot))
    . bodyRigidBody) (poBigBalls physos)
  let nships = map (\(ship, (pos, rot)) ->
        ship
          { shipRot = rot
          , shipPos = pos
          }
        ) (zip (ships sd) posrots)
      nplanets = map (\(ball, (pos, rot)) ->
        ball
          { shipRot = rot
          , shipPos = pos
          }
        ) (zip (oplanets sd) posrots2)
  putAffection sd
    { ships = nships
    , oplanets = nplanets
    , camera = (camera sd)
      { cameraFocus = shipPos ((planet sd : nplanets) !! focusIndex sd)
      }
    }

draw :: Affection StateData ()
draw = do
  GL.viewport $= (GL.Position 0 0, GL.Size 1920 1080)
  StateData{..} <- getAffection
  drawThings program (planet : ships)
  -- drawThings program (ships)
  drawThings program2 oplanets
  where
  drawThings prog ts = do
    StateData{..} <- getAffection
    GL.currentProgram $= (Just . GLU.program $ prog)
    mapM_ (\Ship{..} -> do
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
      ) ts

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
        -- [SDL.ButtonRight] ->
        --   let (V3 sx sy sz) = rotVecByEuler (cameraRot c) (V3 (rx / 10) 0 (ry / 10))
        --   in  c {cameraFocus = cameraFocus c + V3 sx 0 sy}
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
  | code == SDL.KeycodeTab = do
      ud <- getAffection
      let ind = focusIndex ud
          ps  = planet ud : oplanets ud
          -- ps  = oplanets ud
      if ind + 1 < length ps
      then putAffection ud
        { focusIndex = ind + 1
        }
      else putAffection ud
        { focusIndex = 0
        }
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
      let ship = head (ships sd)
          rot = shipRot ship
          dphi = pi / 2 / 45
          nquat = case code of
            SDL.KeycodeW -> rot * axisAngle (V3 1 0 0) (-dphi)
            SDL.KeycodeS -> rot * axisAngle (V3 1 0 0) dphi
            SDL.KeycodeA -> rot * axisAngle (V3 0 1 0) (-dphi)
            SDL.KeycodeD -> rot * axisAngle (V3 0 1 0) dphi
            SDL.KeycodeE -> rot * axisAngle (V3 0 0 1) (-dphi)
            SDL.KeycodeQ -> rot * axisAngle (V3 0 0 1) dphi
            _            -> rot
      putAffection sd
        { ships = ship
          { shipRot = nquat
          } : tail (ships sd)
        }
  | otherwise =
    return ()
