import "lib/github.com/athas/vector/vspace"
module vec3 = mk_vspace_3d f32

def EPS : f32 = 0.00001

def vec3_full (k : f32) : vec3.vector = { x=k, y=k, z=k }

-- Minimum coordinate of a vector
def vec3_min (v : vec3.vector) = 
  f32.min v.x (f32.min v.y v.z)

-- Maximum coordinate of a vector
def vec3_max (v : vec3.vector) = 
  f32.max v.x (f32.max v.y v.z)
  
def make_grid_2d dx dy : [dx][dy](i64, i64) =
  map (\x -> map (\y -> (x, y)) (iota dy)) (iota dx)

def make_grid_3d dx dy dz : [dx][dy][dz](i64, i64, i64) =
  map (\x -> map (\y -> map (\z -> (x, y, z)) (iota dz)) (iota dy)) (iota dx)

def i64_clamp (x : i64) min max = 
  if x < min then min 
  else if x > max then max 
  else x