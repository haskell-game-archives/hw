{-# LANGUAGE OverloadedStrings #-}

module Main where

import Affection

import SDL (($=))
import qualified SDL

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU
import qualified Graphics.GLUtil.Camera3D as GLU (projectionMatrix)

import Control.Monad (when)

import qualified Data.ByteString as BS

import Codec.Wavefront

import Linear as L

import System.Random (randomRIO)

import Foreign

import Util

import Debug.Trace

main :: IO ()
main =
  withAffection AffectionConfig
    { initComponents = All
    , windowTitle    = "hw"
    , windowConfig   = SDL.defaultWindow
      { windowInitialSize = SDL.V2 800 600
      , windowOpenGL = Just SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Normal 3 2
        }
      }
    , initScreenMode = SDL.Windowed
    , preLoop = return ()
    , eventLoop = handle
    , updateLoop = update
    , drawLoop = draw
    , loadState = load
    , cleanUp = const (return ())
    , canvasSize = Nothing
    }

data StateData = StateData
  { ship :: (GL.VertexArrayObject, Int)
  , proj :: M44 Float
  , view :: M44 Float
  , model :: V3 Float
  , program :: GLU.ShaderProgram
  , mrot :: Quaternion Float
  }

load :: IO StateData
load = do
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
        -- , "  gl_FragColor = vec4(color, 1.0);"
        -- , "  gl_FragColor = vec4(1.0);"
        , "  vec2 flip = vec2(f_texcoord.x, 1.0 - f_texcoord.y);"
        , "  gl_FragColor = texture2D(texture, flip);"
        , "}"
        ]
  p <- GLU.simpleShaderProgramBS vertexShader fragmentShader

  return StateData
    { ship = (shipBO, length $ loTriangles lobj)
    , proj = GLU.projectionMatrix (pi/2) (800 / 600) 1 (-1)
    , view = lookAt (V3 0 2 0) (V3 0 0 (-4)) (V3 0 1 0)
    , model = V3 0 0 (-5)
    , program = p
    , mrot = Quaternion 1 (V3 0 0 0)
    }

loadTex :: FilePath -> IO GL.TextureObject
loadTex f = do
  t <- either error id <$> GLU.readTexture f
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  GLU.texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  return t

update :: Double -> Affection StateData ()
update _ = return ()

draw :: Affection StateData ()
draw = do
  GL.viewport $= (GL.Position 0 0, GL.Size 800 600)
  (StateData (shipbo, slen) p v m s rot) <- getAffection
  let pvm = p !*! v !*! mkTransformation rot m
  liftIO $ GLU.setUniform s "mvp" pvm
  GL.currentProgram $= (Just . GLU.program $ s)
  GL.bindVertexArrayObject $= Just shipbo
  liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral slen)

handle :: SDL.EventPayload -> Affection StateData ()
handle (SDL.WindowClosedEvent _) = quit

handle (SDL.KeyboardEvent dat) = do
  let key = SDL.keysymKeycode (SDL.keyboardEventKeysym dat)
  when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $
    handleKey key

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
      let rot = mrot sd
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
        { mrot = nquat
        }
  | otherwise =
    return ()
