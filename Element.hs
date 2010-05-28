module Element where
import Common

import Graphics.Rendering.OpenGL.GL.VertexSpec

data Element = Water | Metal | Earth | Fire | Wood
  deriving (Eq, Enum, Show)

allElements :: [Element]
allElements = [Water,Metal,Earth,Fire,Wood]

elementColor4 :: Element -> Scalar -> Color4 Scalar
elementColor4 e a =
  Color4 r g b a
  where
    (r,g,b) = case e of
      Water -> (0, 0, 1)
      Metal -> (1, 0, 1)
      Earth -> (1, 0, 0)
      Fire -> (1, 1, 0)
      Wood -> (0, 1, 0)

elementColor3 :: Element -> Color3 Scalar
elementColor3 e =
  Color3 r g b
  where
    (r,g,b) = case e of
      Water -> (0, 0, 1)
      Metal -> (1, 0, 1)
      Earth -> (1, 0, 0)
      Fire -> (1, 1, 0)
      Wood -> (0, 1, 0)

{-
elementIndex :: Num a => Element -> a
elementIndex e =
  case e of
    Water -> 0
    Metal -> 1
    Earth -> 2
    Fire -> 3
    Wood -> 4
-}

-------------------------------------------------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------

data ElementValues a = ElementValues {
  water :: a,
  metal :: a,
  earth :: a,
  fire :: a,
  wood :: a
}

type ElementValueAccessor a = (ElementValues a -> a)

instance Show a => Show (ElementValues a) where
  show ev =
    "< wa=" ++ (show $ water ev) ++ "; "
    ++"me=" ++ (show $ metal ev) ++ "; "
    ++"ea=" ++ (show $ earth ev) ++ "; "
    ++"fi=" ++ (show $ fire ev) ++ "; "
    ++"wo=" ++ (show $ wood ev) ++ ">"

allElementFields :: [ElementValueAccessor a]
allElementFields = [water,metal,earth,fire,wood]

-- score.
elementField :: Element -> ElementValueAccessor a
elementField e =
  case e of
    Water -> water
    Metal -> metal
    Earth -> earth
    Fire -> fire
    Wood -> wood

initialElementValues :: a -> ElementValues a
initialElementValues v =
  ElementValues {
    water = v,
    metal = v,
    earth = v,
    fire = v,
    wood = v }

modElementValues :: ((a,b) -> c) -> ElementValues a -> ElementValues b -> ElementValues c
modElementValues f a b =
  ElementValues {
    water = f (water a, water b),
    metal = f (metal a, metal b),
    earth = f (earth a, earth b),
    fire = f (fire a, fire b),
    wood = f (wood a, wood b)
  }

mapElementValues :: (a -> b) -> ElementValues a -> ElementValues b
mapElementValues f a =
  ElementValues {
    water = f (water a),
    metal = f (metal a),
    earth = f (earth a),
    fire = f (fire a),
    wood = f (wood a)
  }

makeElementValues :: (a -> b) -> a -> a -> a -> a -> a -> ElementValues b
makeElementValues pre wa me ea fi wo =
  ElementValues {
    water = pre wa,
    metal = pre me,
    earth = pre ea,
    fire = pre fi,
    wood = pre wo }

allElementValues :: ElementValues a -> (a,a,a,a,a)
allElementValues ev =
  (water ev, metal ev, earth ev, fire ev, wood ev)

{-
allElementValues :: (a, a, a, a, a) -> ElementValues a
allElementValues (wa,me,ea,fi,wo) =
  ElementValues {
    water = wa,
    metal = me,
    earth = ea,
    fire = fi,
    wood = wo }
-}


elementValue :: Element -> ElementValues a -> a
elementValue e es =
  case e of
    Water -> water es
    Metal -> metal es
    Earth -> earth es
    Fire -> fire es
    Wood -> wood es

setElementValue :: Element -> a -> ElementValues a -> ElementValues a
setElementValue e value ev =
  case e of
    Water -> ev { water = value }
    Metal -> ev { metal = value }
    Earth -> ev { earth = value }
    Fire -> ev { fire = value }
    Wood -> ev { wood = value }
