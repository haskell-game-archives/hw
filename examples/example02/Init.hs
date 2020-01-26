{-# LANGUAGE OverloadedStrings #-}

module Init where

import SDL (($=))
import qualified SDL

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU

import qualified Data.ByteString as BS

import System.Random (randomRIO)

import Physics.Bullet.Raw

import Codec.Wavefront

import Linear as L

import SpatialMath

import Foreign

import Util
import Types

genVertBufObject :: FilePath -> IO (GL.VertexArrayObject, Int)
genVertBufObject path = do
  eobj <- fromFile path
  let obj = case eobj of
        Right o -> o
        Left err -> error err
  let lobj = loadObj obj

  shipBO <- GL.genObjectName
  GL.bindVertexArrayObject $= Just shipBO

  verts <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just verts
  withArray (loTriangles lobj) $ \ptr ->
    GL.bufferData GL.ArrayBuffer $=
      ( fromIntegral $ length (loTriangles lobj) * sizeOf (0 :: Float)
      , ptr
      , GL.StaticDraw
      )
  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 4 GL.Float 0 (plusPtr nullPtr 0)
    )
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  GL.bindVertexArrayObject $= Nothing
  return (shipBO, length (loTriangles lobj))

load :: IO StateData
load = do
  _ <- SDL.setMouseLocationMode SDL.RelativeLocation
  GL.depthFunc $= Just GL.Less

  (shipBO, stl) <- genVertBufObject "assets/spheres/smallsphere.obj"

  (planetBO, ptl) <- genVertBufObject "assets/spheres/bigsphere.obj"

  let vertexShader = foldl BS.append BS.empty
        [ "attribute vec3 coord3d;"
        , "attribute vec2 texcoord;"
        , "uniform mat4 mvp;"
        , "varying vec2 f_texcoord;"
        , "void main(void) {"
        , "  float zoom = 1.0;"
        , "  gl_Position = mvp * vec4(coord3d, 1.0);"
        -- , "  gl_Position.z = 0.0;"
        -- , "  gl_Position.w += 10.0;"
        , "  f_texcoord = texcoord;"
        , "}"
        ]
      fragmentShaderSmall = foldl BS.append BS.empty
        [ "varying vec2 f_texcoord;"
        , "void main(void) {"
        , "  gl_FragColor = vec4(1.0,1.0,1.0,1.0);"
        , "}"
        ]
      fragmentShaderBig = foldl BS.append BS.empty
        [ "varying vec2 f_texcoord;"
        , "void main(void) {"
        , "  gl_FragColor = vec4(1.0,0,0,1.0);"
        , "}"
        ]
  p <- GLU.simpleShaderProgramBS vertexShader fragmentShaderSmall
  p2 <- GLU.simpleShaderProgramBS vertexShader fragmentShaderBig

  poss <- mapM (\_ -> do
    x <- randomRIO (-50, 50)
    y <- randomRIO (-50, 50)
    z <- randomRIO (-50, 50)
    return (V3 x y z)
    ) ([0..1999] :: [Int])

  poss2 <- mapM (\_ -> do
    x <- randomRIO (-100, 100)
    y <- randomRIO (-100, 100)
    z <- randomRIO (-100, 100)
    return (V3 x y z)
    ) [] -- [0..9]

  let shipList = zipWith (Ship shipBO stl)
        poss
        (repeat $ Quaternion 1 (V3 0 0 0))
      planet = Ship planetBO ptl (V3 0 0 0) (Quaternion 1 (V3 0 0 0))
      otherPlanets = zipWith (Ship planetBO ptl)
        poss2
        (repeat $ Quaternion 1 (V3 0 0 0))

  phys <- initPhysics

  po <- initPhysicsObjects poss poss2

  mapM_ (addRigidBody (pWorld phys) . bodyRigidBody) (poSmallBalls po)
  mapM_ (addRigidBody (pWorld phys) . bodyRigidBody) (poBigBalls po)
  addRigidBody (pWorld phys) (bodyRigidBody $ poBigBall po)

  return StateData
    { ships = shipList
    , planet = planet
    , oplanets = otherPlanets
    , proj = infinitePerspective (pi/2) (1600 / 900) 1
    , camera = Camera
      { cameraFocus = V3 0 0 0
      , cameraRot = Euler 0 0 0
      , cameraDist = -100
      }
    , program = p
    , program2 = p2
    , physics = phys
    , physicsObjects = po
    , focusIndex = 0
    }

loadTex :: FilePath -> IO GL.TextureObject
loadTex f = do
  t <- either error id <$> GLU.readTexture f
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  GLU.texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  return t

initPhysics :: IO Physics
initPhysics = do
  bp <- newDbvtBroadphase
  config <- newDefaultCollisionConfiguration
  disp <- newCollisionDispatcher config
  solver <- newSequentialImpulseConstraintSolver
  world <- newDiscreteDynamicsWorld disp bp solver config
  setGravity world (V3 0 0 0)
  return $ Physics bp config disp solver world

initPhysicsObjects :: [V3 Float] -> [V3 Float] -> IO PhysicsObjects
initPhysicsObjects poss poss2 = do
  -- ground <- newStaticPlaneShape (V3 0 1 0) 1
  smallBall <- newSphereShape 1
  bigBall <- newSphereShape 5

  -- groundMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0)) (V3 0 (-51) 0)
  -- groundBody <- newRigidBody 0 groundMotionState 0.9 0.5 ground (V3 0 0 0)

  smallBallPOs <- mapM (\pos -> do
    -- fx <- randomRIO (-1000, 1000)
    -- fy <- randomRIO (-1000, 1000)
    -- fz <- randomRIO (-1000, 1000)
    let m = 100
    smallBallMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0))
      (fmap realToFrac pos)
    localInertia <- calculateLocalInertia smallBall m (V3 0 0 0)
    smallBallBody <- newRigidBody m smallBallMotionState 0.9 0.5 smallBall localInertia
    -- applyCentralForce smallBallBody (V3 fx fy fz)
    return $ PhysBody smallBall smallBallMotionState smallBallBody m
    ) poss

  bigBallsPOs <- mapM (\pos -> do
    let m = 1000000
    -- fx <- randomRIO (-1000, 1000)
    -- fy <- randomRIO (-1000, 1000)
    -- fz <- randomRIO (-1000, 1000)
    bigBallMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0))
      (fmap realToFrac pos)
    localInertia <- calculateLocalInertia bigBall m (V3 0 0 0)
    bigBallBody <- newRigidBody m bigBallMotionState 0.9 0.5 bigBall localInertia
    -- applyCentralForce bigBallBody (V3 fx fy fz)
    return $ PhysBody bigBall bigBallMotionState bigBallBody m
    ) poss2

  bigBallPO <- do
    bigBallMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0))
      (V3 0 0 0)
    localInertia <- calculateLocalInertia bigBall 1 (V3 0 0 0)
    bigBallBody <- newRigidBody 0 bigBallMotionState 0.9 0.5 bigBall localInertia
    return $ PhysBody bigBall bigBallMotionState bigBallBody 0

  return PhysicsObjects
    { poBigBall = bigBallPO
    , poSmallBalls  = smallBallPOs
    , poBigBalls = bigBallsPOs
    -- , poBigBalls = []
    }
