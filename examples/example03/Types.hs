module Types where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU

import Linear as L

import SpatialMath

import Physics.Bullet.Raw as Bullet

data Opts = Opts
  { width :: Word
  , height :: Word
  }

data StateData = StateData
  { ship :: Ship
  , vertHandles :: [Ship]
  , camera :: Camera
  , proj :: M44 Float
  , physics :: Physics
  , physicsObjects :: PhysicsObjects
  , shipProgram :: GLU.ShaderProgram
  , handleProgram :: GLU.ShaderProgram
  , selHandleProgram :: GLU.ShaderProgram
  }

data Ship = Ship
  { shipVao     :: GL.VertexArrayObject
  , shipVaoLen  :: Int
  , shipPos     :: V3 Float
  , shipRot     :: Quaternion Float
  , shipTexture :: Maybe GL.TextureObject
  , shipUVs     :: Maybe GL.BufferObject
  } deriving (Eq)

data Camera = Camera
  { cameraFocus :: V3 Float
  , cameraRot :: Euler Float
  , cameraDist :: Float
  }

data Physics = Physics
  { pBroadphase :: DbvtBroadphase
  , pConfig     :: DefaultCollisionConfiguration
  , pDispatcher :: CollisionDispatcher
  , pSolver     :: SequentialImpulseConstraintSolver
  , pWorld      :: DiscreteDynamicsWorld
  }

data PhysicsObjects = PhysicsObjects
  -- { poGround :: PhysBody StaticPlaneShape
  { poBalls   :: [PhysBody SphereShape]
  }

data PhysBody a = PhysBody
  { bodyShape       :: a
  , bodyMotionState :: MotionState
  , bodyRigidBody   :: RigidBody
  }
