module Types where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU

import Linear as L

import SpatialMath

import Physics.Bullet.Raw as Bullet

data StateData = StateData
  { ships :: [Ship]
  , camera :: Camera
  , proj :: M44 Float
  , program :: GLU.ShaderProgram
  , physics :: Physics
  , physicsObjects :: PhysicsObjects
  }

data Ship = Ship
  { shipVao    :: GL.VertexArrayObject
  , shipVaoLen :: Int
  , shipPos    :: V3 Float
  , shipRot    :: Quaternion Float
  }

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
  { poGround :: PhysBody StaticPlaneShape
  , poBalls  :: [PhysBody SphereShape]
  }

data PhysBody a = PhysBody
  { bodyShape       :: a
  , bodyMotionState :: MotionState
  , bodyRigidBody   :: RigidBody
  }
