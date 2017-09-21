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

load :: IO StateData
load = do
  _ <- SDL.setMouseLocationMode SDL.RelativeLocation
  GL.depthFunc $= Just GL.Less
  eobj <- fromFile "assets/ships/jaeger/jaeger.obj"
  let obj = case eobj of
        Right o -> o
        Left err -> error err
  -- (ptr, len) <- objLocsToPtr obj
  -- (tptr, tlen) <- objUVsToPtr obj
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

  texture <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just texture
  maybe (return ()) (\a -> withArray a $ \ptr ->
    GL.bufferData GL.ArrayBuffer $=
      ( fromIntegral $ length a * 2 * sizeOf (0 :: Double)
      , ptr
      , GL.StaticDraw
      )) (loTexTri lobj)
  GL.vertexAttribPointer (GL.AttribLocation 1) $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 (plusPtr nullPtr 0)
    )
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

  GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit 0
  t <- loadTex "assets/ships/jaeger/jaeger.texture.tga"
  GL.textureBinding GL.Texture2D $= Just t

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
        , "uniform sampler2D texture;"
        , "void main(void) {"
        , "  vec2 flip = vec2(f_texcoord.x, 1.0 - f_texcoord.y);"
        , "  gl_FragColor = texture2D(texture, flip);"
        , "}"
        ]
  p <- GLU.simpleShaderProgramBS vertexShader fragmentShader

  poss <- mapM (\_ -> do
    x <- randomRIO (-50, 50)
    y <- randomRIO (-50, 50)
    z <- randomRIO (-50, 50)
    return (V3 x y z)
    ) [0..2000]

  let shipList = zipWith (uncurry $ Ship shipBO (length $ loTriangles lobj))
        poss
        (repeat $ Quaternion 1 (V3 0 0 0))

  phys <- initPhysics

  po <- initPhysicsObjects poss

  mapM_ (addRigidBody (pWorld phys)) . bodyRigidBody) (poBalls po)
  addRigidBody (pWorld phys) (bodyRigidBody $ poGround po)

  return StateData
    { ships = shipList
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
  ground <- newStaticPlaneShape (V3 0 1 0) 1
  ball <- newSphereShape 3

  groundMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0)) (V3 0 (-51) 0)
  groundBody <- newRigidBody 0 groundMotionState 0.9 0.5 ground (V3 0 0 0)

  balls <- mapM (\pos -> do
    ballMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0))
      (fmap realToFrac pos)
    localInertia <- calculateLocalInertia ball 1 (V3 0 0 0)
    ballBody <- newRigidBody 1 ballMotionState 0.9 0.5 ball localInertia
    return $ PhysBody ball ballMotionState ballBody
    ) poss

  return PhysicsObjects
    { poGround = PhysBody ground groundMotionState groundBody
    , poBalls  = balls
    }
