{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Affection

import SDL (($=))
import qualified SDL

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU

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
      { windowInitialSize = SDL.V2 1600 900
      , windowOpenGL = Just SDL.defaultOpenGL
        { SDL.glProfile = SDL.Core SDL.Normal 3 2
        }
      }
    , initScreenMode = SDL.Fullscreen
    , preLoop = return ()
    , eventLoop = handle
    , updateLoop = update
    , drawLoop = draw
    , loadState = load
    , cleanUp = const (return ())
    , canvasSize = Nothing
    }

data StateData = StateData
  { ships :: [Ship]
  , camera :: Camera
  , proj :: M44 Float
  -- , look :: V3 Float
  -- , view :: M44 Float
  , program :: GLU.ShaderProgram
  }

data Ship = Ship
  { shipVao    :: GL.VertexArrayObject
  , shipVaoLen :: Int
  , shipPos    :: V3 Float
  , shipRot    :: Quaternion Float
  }

data Camera = Camera
  { cameraFocus :: V3 Float
  , cameraRot :: Quaternion Float
  , cameraDist :: Float
  }

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
        -- , "  gl_FragColor = vec4(color, 1.0);"
        -- , "  gl_FragColor = vec4(1.0);"
        , "  vec2 flip = vec2(f_texcoord.x, 1.0 - f_texcoord.y);"
        , "  gl_FragColor = texture2D(texture, flip);"
        , "}"
        ]
  p <- GLU.simpleShaderProgramBS vertexShader fragmentShader

  let shipList = map (uncurry $ Ship shipBO (length $ loTriangles lobj))
        [ (V3 (-3) 0 0, Quaternion 1 (V3 0 0 0))
        , (V3 3 0 0, Quaternion 1 (V3 0 0 0))
        ]

  return StateData
    { ships = shipList
    , proj = perspective (pi/2) (1600 / 900) 1 (-1)
    , camera = Camera
      { cameraFocus = V3 0 0 0
      , cameraRot = Quaternion (-1) (V3 0 0 0)
      , cameraDist = (-10)
      }
    , program = p
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
  GL.viewport $= (GL.Position 0 0, GL.Size 1600 900)
  (StateData{..}) <- getAffection
  GL.currentProgram $= (Just . GLU.program $ program)
  mapM_ (\(Ship{..}) -> do
    let view = lookAt
          (cameraFocus camera +
            (L.rotate
            (cameraRot camera)
            (V3 0 0 (-cameraDist camera))))
          (cameraFocus camera)
          (V3 0 1 0)
        model = mkTransformation shipRot shipPos
        pvm = proj !*! view !*! model
    liftIO $ GLU.setUniform program "mvp" pvm
    GL.bindVertexArrayObject $= Just shipVao
    liftIO $ GL.drawArrays GL.Triangles 0 (fromIntegral shipVaoLen)
    ) ships

handle :: SDL.EventPayload -> Affection StateData ()
handle (SDL.WindowClosedEvent _) = quit

handle (SDL.KeyboardEvent dat) = do
  let key = SDL.keysymKeycode (SDL.keyboardEventKeysym dat)
  when (SDL.keyboardEventKeyMotion dat == SDL.Pressed) $
    handleKey key

handle (SDL.MouseMotionEvent dat) = do
  sd <- getAffection
  let (V2 rx ry) = fmap fromIntegral $ SDL.mouseMotionEventRelMotion dat
      c = camera sd
  putAffection sd
    { camera =
      case SDL.mouseMotionEventState dat of
        [SDL.ButtonRight] ->
          c {cameraFocus = cameraFocus c + V3 (rx / 10) 0 (ry / 10)}
        [] ->
          let dphi = pi / 2 / 45 / 2
          in  c
            { cameraRot =
              cameraRot c * axisAngle
                (normalize $ V3 (- ry) (- rx) 0)
                dphi
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
      let ship = ships sd !! 0
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
