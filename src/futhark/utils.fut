import "lib/github.com/athas/vector/vspace"
import "lib/github.com/athas/matte/colour"

module vec3 = mk_vspace_3d f32

def EPS : f32 = 0.000001

def vec3_full (k : f32) : vec3.vector = { x=k, y=k, z=k }

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
