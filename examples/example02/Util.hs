module Util where

import Codec.Wavefront

import qualified Data.Vector as V

data LoadedObject = LoadedObject
  { loTriangles :: [Float]
  , loLines     :: [Float]
  , loPoints    :: [Float]
  , loTexTri    :: Maybe [Float]
  }

loadObj :: WavefrontOBJ -> LoadedObject
loadObj obj =
  LoadedObject ts ls ps tritex
  where
    inter = objLocations obj
    interTex = objTexCoords obj
    faces = map elValue (V.toList $ objFaces obj)
    lns = map elValue (V.toList $ objLines obj)
    points = map elValue (V.toList $ objPoints obj)
    deface (Face a b c []) =
      map (\i -> inter V.! (faceLocIndex i - 1)) [a, b, c]
    deface _ =
      error "loadObj: obj with quads encountered"
    deline (Line a b) =
      map (\i -> inter V.! (lineLocIndex i -1)) [a, b]
    depoint (Point i) = inter V.! (i - 1)
    tsLocs = concatMap deface faces
    lsLocs = concatMap deline lns
    psLocs = map depoint points
    deLoc (Location x y z w) = [x, y, z, w]
    deTex (TexCoord r s _) = [r, s]
    ts = concatMap deLoc tsLocs
    ls = concatMap deLoc lsLocs
    ps = concatMap deLoc psLocs
    defaceTex :: Face -> Maybe [TexCoord]
    defaceTex (Face a b c []) =
      mapM
      (fmap (\x -> interTex V.! (x - 1)) . faceTexCoordIndex)
      [a, b, c]
    defaceTex _ =
      error "loadObj: obj with quads encountered"
    tritex :: Maybe [Float]
    tritex = concatMap deTex <$> mftxs
    mftxs :: Maybe [TexCoord]
    mftxs = fmap concat (mapM defaceTex faces)

-- objLocsToPtr :: WavefrontOBJ -> IO (Ptr Float, Int)
-- objLocsToPtr obj = do
--   let ivs = objLocations obj
--       faces = map elValue $ V.toList $ objFaces obj
--       vs = concatMap
--         (\(Face a b c []) ->
--           map (\i -> ivs V.! ((faceLocIndex i) - 1)) [a, b, c])
--         faces
--   ptr <- newArray $ concatMap (\(Location x y z w) -> [x, y, z, w]) vs
--   return (ptr, length vs)
-- 
-- objUVsToPtr :: WavefrontOBJ -> IO (Ptr Float, Int)
-- objUVsToPtr obj = do
--   let uvs= V.toList $ objTexCoords obj
--   ptr <- newArray $ concatMap (\(TexCoord r s t) -> [r, s, t]) uvs
--   return (ptr, length uvs)
