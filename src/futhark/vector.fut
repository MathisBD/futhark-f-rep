-- My custom 3d vector modules, for booleans, integers and floats.


-- This will be included by more sophisticated vector module.
module mk_basic_vec3 (T : { type t }) = {
  type scalar = T.t
  type t = { x : scalar, y : scalar, z : scalar }

  def full k : t = { x = k, y = k, z = k }

  def rot_left  (a : t) : t = { x = a.y, y = a.z, z = a.x }
  def rot_right (a : t) : t = { x = a.z, y = a.x, z = a.y }

  -- The map and map2 functions work for any scalar type,
  -- and can thus be used to convert between different vector types.
  -- Doing it this way is very flexible : any vector module can convert between any two types
  -- (especially useful when using vec3.(...long expression...)).  
  -- However this might create annoying-to-read function types : we'll have to see...
  def map 't1 't2 (f : t1 -> t2) (a : { x : t1, y : t1, z : t1 }) = 
    { x = f a.x, y = f a.y, z = f a.z }
  def map2 't1 't2 (f : t1 -> t1 -> t2) (a : { x : t1, y : t1, z : t1 }) (b : { x : t1, y : t1, z : t1 }) = 
    { x = f a.x b.x, y = f a.y b.y, z = f a.z b.z }

  -- Convert a vector with another scalar type (example a bool vector)
  -- to a vector, using the passed convertion function.
  -- Note that this uses the convenient structural typing of Futhark : 
  -- we only request that the input has three fields of the correct type.
  def map_from 't2 (f : t2 -> scalar) (a : { x : t2, y : t2, z : t2 }) : t = 
    { x = f a.x, y = f a.y, z = f a.z }

  -- A conditional expression coordinate-wise.
  def cond (c : { x : bool, y : bool, z : bool }) (u : t) (v : t) : t = {
    x = if c.x then u.x else v.x,
    y = if c.y then u.y else v.y,
    z = if c.z then u.z else v.z
  }
}

-- A module for vectors of booleans.
module bvec3 = {
  open mk_basic_vec3 bool
  
  -- Logical operations coordinate-wise.
  def not : t -> t = map (\b -> !b)
  def and : t -> t -> t = map2 (&&)
  def or  : t -> t -> t = map2 (||)
  
  -- Comparison functions coordinate-wise.
  def (==) : t -> t -> t = map2 (==)
  def (!=) : t -> t -> t = map2 (!=)
  
  -- Standard boolean functions.
  def all (a : t) = a.x && a.y && a.z
  def any (a : t) = a.x || a.y || a.z
}

-- A module for vectors of 'numeric' objects (think integers).
module mk_numeric_vec3 (T : numeric) = {
  open mk_basic_vec3 T

  -- Shorthands for common vectors.
  def zeros : t = full (T.i32 0)
  def ones : t = full (T.i32 1)

  -- Arithmetic operations coordinate-wise
  def neg : t -> t = map T.neg
  def (+) : t -> t -> t = map2 (T.+)
  def (-) : t -> t -> t = map2 (T.-)
  def (*) : t -> t -> t = map2 (T.*)
  def (/) : t -> t -> t = map2 (T./)  
  def min : t -> t -> t = map2 T.min
  def max : t -> t -> t = map2 T.max

  -- Comparison functions coordinate-wise.
  def (==) : t -> t -> bvec3.t = map2 (T.==)
  def (!=) : t -> t -> bvec3.t = map2 (T.!=)
  def (<)  : t -> t -> bvec3.t = map2 (T.<)
  def (<=) : t -> t -> bvec3.t = map2 (T.<=)
  def (>)  : t -> t -> bvec3.t = map2 (T.>)
  def (>=) : t -> t -> bvec3.t = map2 (T.>=)

  -- Multiplication by a scalar
  def scale (k : scalar) : t -> t = map (k T.*)

  -- Dot product.
  def dot (a : t) (b : t) : scalar = 
    T.(a.x * b.x + a.y * b.y + a.z * b.z)

  -- Minimum and maximum coordinate.
  def min_coord (a : t) : scalar = T.min a.x (T.min a.y a.z)
  def max_coord (a : t) : scalar = T.max a.x (T.max a.y a.z)
  -- Sum and product of coordinates.
  def sum_coords (a : t) : scalar = T.(a.x + a.y + a.z)
  def prod_coords (a : t) : scalar = T.(a.x * a.y * a.z)
}

-- A module for vectors of 'real' objects (think floats).
module mk_real_vec3 (T : real) = {
  open mk_numeric_vec3 T

  def norm (a : t) : scalar = T.sqrt (dot a a)
  def normalize (a : t) : t = scale T.(i32 1 / norm a) a
}

module i8vec3  = mk_numeric_vec3 i8
module i16vec3 = mk_numeric_vec3 i16
module i32vec3 = mk_numeric_vec3 i32
module i64vec3 = mk_numeric_vec3 i64

module f32vec3 = mk_real_vec3 f32 
module f64vec3 = mk_real_vec3 f64
  