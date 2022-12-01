import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"

module vec3 = mk_vspace_3d f32


def vec3_full (k : f32) : vec3.vector = { x=k, y=k, z=k }

def vec3_from_i64 x y z = 
  { 
    x = f32.i64 x, 
    y = f32.i64 y, 
    z = f32.i64 z 
  }

-- Minimum coordinate of a vector
def vec3_min (v : vec3.vector) = 
  f32.min v.x (f32.min v.y v.z)

-- Maximum coordinate of a vector
def vec3_max (v : vec3.vector) = 
  f32.max v.x (f32.max v.y v.z)
  
def i64_clamp (x : i64) min max = 
  if x <= min then min 
  else if x >= max then max 
  else x

def f32_min4 x0 x1 x2 x3 = 
  f32.min (f32.min x0 x1) (f32.min x2 x3)

def f32_max4 x0 x1 x2 x3 = 
  f32.max (f32.max x0 x1) (f32.max x2 x3)

-- Is there an integer in the interval [low, high] ?
-- This should also work for inputs of the type nan/inf.
def contains_int (low : f32) (high : f32) =
  if low == f32.inf then false 
  else if high == -f32.inf then false 
  else low <= f32.floor high

-- Emulate a 4D scatter by flattening along a dimension and using the builtin 3D scatter.
--def scatter_4d 't [n] [k1] [k2] [k3] [k4] (dest : *[k1][k2][k3][k4]t) (is : [n](i64, i64, i64, i64))
--  (vs : [n]t) : *[k1][k2][k3][k4]t =
--  let res_flat = scatter_3d 
--    (flatten dest) 
--    (map (\(i1, i2, i3, i4) -> (i1 * k2 + i2, i3, i4)) is) 
--    vs
--  in 
--    unflatten k1 k2 res_flat :> [k1][k2][k3][k4]t
