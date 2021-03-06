{-# LANGUAGE OverloadedStrings #-}

module Init where

import SDL (($=))
import qualified SDL

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU

import Data.List.Split (chunksOf)
import qualified Data.ByteString as BS

import Physics.Bullet.Raw

import Codec.Wavefront

import Linear as L

import SpatialMath

import Foreign

import Util
import Types
import Logging as LL

load :: Word -> Word -> IO StateData
load w h = do
  _ <- SDL.setMouseLocationMode SDL.RelativeLocation
  GL.depthFunc $= Just GL.Less

  svao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just svao
  (shipBO, sobj, stl) <- genVertBufObject "assets/ships/jaeger/jaeger.obj"

  texture <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just texture
  maybe (return ()) (\a -> withArray a $ \ptr ->
    GL.bufferData GL.ArrayBuffer $=
      ( fromIntegral $ length a * 2 * sizeOf (0 :: Double)
      , ptr
      , GL.StaticDraw
      )) (loTexTri sobj)
  GL.vertexAttribPointer (GL.AttribLocation 1) $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float 0 (plusPtr nullPtr 0)
    )
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
  GL.bindBuffer GL.ArrayBuffer $= Nothing

  GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit 0
  t <- loadTex "assets/ships/jaeger/jaeger.texture.tga"
  GL.textureBinding GL.Texture2D $= Just t

  GL.bindVertexArrayObject $= Nothing

  hvao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just hvao
  (vectHandleBO, hobj, vhtl) <- genVertBufObject "assets/spheres/vertHandle.obj"
  GL.bindVertexArrayObject $= Nothing

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
      fragmentShaderShip = foldl BS.append BS.empty
        [ "varying vec2 f_texcoord;"
        , "uniform sampler2D texture;"
        , "void main(void) {"
        , "  vec2 flip = vec2(f_texcoord.x, 1.0 - f_texcoord.y);"
        , "  gl_FragColor = texture2D(texture, flip);"
        , "}"
        ]
      fragmentShaderHandle = foldl BS.append BS.empty
        [ "varying vec2 f_texcoord;"
        , "void main(void) {"
        , "  gl_FragColor = vec4(1,0,1,0.5);"
        , "}"
        ]
      fragmentShaderSelHandle = foldl BS.append BS.empty
        [ "varying vec2 f_texcoord;"
        , "void main(void) {"
        , "  gl_FragColor = vec4(1,1,0,0.5);"
        , "}"
        ]

  hProgram <- GLU.simpleShaderProgramBS vertexShader fragmentShaderHandle
  sProgram <- GLU.simpleShaderProgramBS vertexShader fragmentShaderShip
  cProgram <- GLU.simpleShaderProgramBS vertexShader fragmentShaderSelHandle

  phys <- initPhysics

  po <- initPhysicsObjects (map listToPos $ chunksOf 4 $ loLocations sobj)

  -- traceIO $ show $ loLines sobj

  -- mapM_ (addRigidBody (pWorld phys)) (map bodyRigidBody (poBalls po))
  mapM_ (\b -> addRigidBody (pWorld phys) (bodyRigidBody b)) (poBalls po)

  logIO Debug "Hello world"

  return StateData
    { ship = (Ship svao stl
      (V3 0 0 0)
      (Quaternion 1 (V3 0 0 0))
      (Just t)
      (Just texture))
    , vertHandles = createHandles hvao vhtl (loLocations sobj)
    , proj = perspective (pi/2) (fromIntegral w / fromIntegral h) 1 (-1)
    , camera = Camera
      { cameraFocus = V3 0 0 0
      , cameraRot = Euler 0 0 0
      , cameraDist = -6
      }
    , physics = phys
    , physicsObjects = po
    , shipProgram = sProgram
    , handleProgram = hProgram
    , selHandleProgram = cProgram
    }

initPhysics :: IO Physics
initPhysics = do
  bp <- newDbvtBroadphase
  config <- newDefaultCollisionConfiguration
  disp <- newCollisionDispatcher config
  solver <- newSequentialImpulseConstraintSolver
  world <- newDiscreteDynamicsWorld disp bp solver config
  setGravity world (V3 0 0 0)
  return $ Physics bp config disp solver world

initPhysicsObjects :: [V3 Float] -> IO PhysicsObjects
initPhysicsObjects poss = do
  -- ground <- newStaticPlaneShape (V3 0 1 0) 1
  ball <- newSphereShape 0.05

  -- groundMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0)) (V3 0 (-51) 0)
  -- groundBody <- newRigidBody 0 groundMotionState 0.9 0.5 ground (V3 0 0 0)

  balls <- mapM (\pos -> do
    ballMotionState <- newDefaultMotionState (Quaternion 1 (V3 0 0 0))
      (fmap realToFrac pos)
    localInertia <- calculateLocalInertia ball 1 (V3 0 0 0)
    ballBody <- newRigidBody 0 ballMotionState 0 0 ball localInertia
    setSleepingThresholds ballBody 0 0
    return $ PhysBody ball ballMotionState ballBody
    ) poss

  return PhysicsObjects
    -- { poGround = PhysBody ground groundMotionState groundBody
    { poBalls = balls
    }

genVertBufObject :: FilePath -> IO (GL.BufferObject, LoadedObject, Int)
genVertBufObject path = do
  eobj <- fromFile path
  let obj = case eobj of
        Right o -> o
        Left err -> error err
      lobj = loadObj obj

  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
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
  return (vbo, lobj, length (loTriangles lobj))

createHandles :: GL.VertexArrayObject -> Int -> [Float] -> [Ship]
createHandles bo len ps =
  map (\p -> Ship bo len (listToPos p) (Quaternion 1 (V3 0 0 0)) Nothing Nothing) tris
  where
    tris               = chunksOf 4 ps

listToPos :: [Float] -> V3 Float
listToPos [x, y, z, _] = V3 x y z
listToPos _            = error "listToPos: not triangular coordinates encountered"
