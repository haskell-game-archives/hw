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
      ( fromIntegral $ length (loTriangles lobj) * 3 * sizeOf (0 :: Double)
      , ptr
      , GL.StaticDraw
      )
  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 4 GL.Float 0 (plusPtr nullPtr 0)
    )
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
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
        , "  gl_Position = mvp * vec4(coord3d, 1.0);"
        , "  f_texcoord = texcoord;"
        , "}"
        ]
      fragmentShader = foldl BS.append BS.empty
        [ "varying vec2 f_texcoord;"
        , "void main(void) {"
        , "  gl_FragColor = vec4(1,1,1,0.5);"
        , "}"
        ]
  p <- GLU.simpleShaderProgramBS vertexShader fragmentShader

  poss <- mapM (\_ -> do
    x <- randomRIO (-50, 50)
    y <- randomRIO (-50, 50)
    z <- randomRIO (-50, 50)
    return (V3 x y z)
    ) [0..2000]

  let shipList = zipWith (Ship shipBO stl)
        poss
        (repeat $ Quaternion 1 (V3 0 0 0))
      planet = Ship planetBO ptl (V3 0 0 0) (Quaternion 1 (V3 0 0 0))

  phys <- initPhysics

  po <- initPhysicsObjects poss

  mapM_ (addRigidBody (pWorld phys) . bodyRigidBody) (poSmallBalls po)
  addRigidBody (pWorld phys) (bodyRigidBody $ poBigBall po)

  return StateData
    { ships = shipList
    , planet = planet
    , proj = perspective (pi/2) (1600 / 900) 1 (-1)
    , camera = Camera
      { cameraFocus = V3 0 0 0
      , cameraRot = Euler 0 0 0
      , cameraDist = -50
      }
    , program = p
    , physics = phys
    , physicsObjects = po
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
  setGravity world (V3 0 (-10) 0)
  return $ Physics bp config disp solver world

initPhysicsObjects :: [V3 Float] -> IO PhysicsObjects
initPhysicsObjects poss = do
  -- ground <- newStaticPlaneShape (V3 0 1 0) 1
  smallBall <- newSphereShape 1
  bigBall <- newSphereShape 5

  -- groundMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0)) (V3 0 (-51) 0)
  -- groundBody <- newRigidBody 0 groundMotionState 0.9 0.5 ground (V3 0 0 0)

  smallBallPOs <- mapM (\pos -> do
    smallBallMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0))
      (fmap realToFrac pos)
    localInertia <- calculateLocalInertia smallBall 1 (V3 0 0 0)
    smallBallBody <- newRigidBody 1 smallBallMotionState 0.9 0.5 smallBall localInertia
    return $ PhysBody smallBall smallBallMotionState smallBallBody 1
    ) poss

  bigBallPO <- do
    bigBallMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0))
      (V3 0 0 0)
    localInertia <- calculateLocalInertia bigBall 1 (V3 0 0 0)
    bigBallBody <- newRigidBody 0 bigBallMotionState 0.9 0.5 bigBall localInertia
    return $ PhysBody bigBall bigBallMotionState bigBallBody 0

  return PhysicsObjects
    { poBigBall = bigBallPO
    , poSmallBalls  = smallBallPOs
    }
