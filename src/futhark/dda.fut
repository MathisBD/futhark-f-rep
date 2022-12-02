import "lib/github.com/athas/matte/colour"
import "vector"
import "tape"
import "voxelizer"


-- This is used for raycasting.
-- DO NOT make it too small, or the program will crash !
def EPS : f32 = 0.00001


type frame = {
  pos : f32vec3.t,
  size : f32
}

type ray = { orig : f32vec3.t, dir : f32vec3.t }

type hit = #miss | #hit { t : f32 }

-- Get the point on the ray corresponding to the parameter t
def ray_eval (r : ray) (t : f32) = 
  f32vec3.(r.orig + scale t r.dir)

type camera = {
  pos : f32vec3.t, -- the position in world space of the camera center
  -- the local axes of the camera :
  forward : f32vec3.t,
  right : f32vec3.t,
  up : f32vec3.t,
  fov_rad : f32, -- the horizontal field of view of the camera, in radians
  pixel_width : i64, -- the pixel width of the screen
  pixel_height : i64 -- the pixel height of the screen
}


-- This returns a ray passing starting at the camera center 
-- and corresponding to the direction of the pixel (x, y).
def camera_make_ray (cam : camera) pixel_x pixel_y : ray =
  -- The world size of the screen at a unit distance from the camera, along the camera's local axes
  let screen_world_size_x = 2.0 * f32.tan (cam.fov_rad / 2.0)
  let screen_world_size_y = screen_world_size_x * (f32.i64 cam.pixel_height / f32.i64 cam.pixel_width) 
  -- dx and dy are in the range [-1...1]
  let dx = 2.0 * (f32.i64 pixel_x / f32.i64 cam.pixel_width) - 1.0
  let dy = 2.0 * (f32.i64 pixel_y / f32.i64 cam.pixel_height) - 1.0
  let dir =
    cam.forward f32vec3.+
    f32vec3.scale (dx * screen_world_size_x) cam.right f32vec3.+
    f32vec3.scale (dy * screen_world_size_y) cam.up
  in { orig = cam.pos, dir = f32vec3.normalize dir }

-- hit is true iff the ray intersects the grid
-- t_enter is the time that the ray enters the grid (undefined when hit = false)
-- t_leave is the time at which the ray leaves the grid (undefined when hit = false)
def rayframe_intersect (r : ray) (fram : frame) : { hit : bool, t_enter : f32, t_leave : f32 } =
  let t_0 = f32vec3.((fram.pos - r.orig) / r.dir)
  let t_1 = f32vec3.((fram.pos + full fram.size - r.orig) / r.dir)
  -- The (unique) time at which the ray enters/leaves the grid along each axis.
  let t_enter = f32vec3.(cond (r.dir >= zeros) t_0 t_1)
  let t_leave = f32vec3.(cond (r.dir >= zeros) t_1 t_0)
  -- The time at which the ray enters/leaves the grid (globally).
  let t_enter = f32vec3.max_coord t_enter
  let t_leave = f32vec3.min_coord t_leave
  -- Determine if the ray hit the grid
  let hit = t_enter <= t_leave && 0.0 <= t_leave
  in { hit = hit, t_enter, t_leave }

def raytrace [d] (fram : frame) (vxls : [d][d][d]bool) (r : ray) : hit =
  let { hit, t_enter, t_leave=_ } = rayframe_intersect r fram in 
  if !hit then #miss else 
  let cell_size = fram.size / f32.i64 d
  let norm_pos = f32vec3.scale (1.0 / cell_size) (ray_eval r (t_enter + EPS) f32vec3.- fram.pos)
  -- The coordinates of the cell we are in.
  let cell = f32vec3.map (f32.floor >-> i32.f32) norm_pos
  -- The time it takes to move one unit along a given axis.
  let t_delta = f32vec3.map (\x -> f32.abs (1.0 / x)) r.dir
  -- The time until we enter a new cell along a given axis.
  let t_cross = f32vec3.(
    full (t_enter f32.+ EPS) + scale cell_size (map f32.i32 cell + map f32.bool (r.dir >= zeros) - norm_pos) / r.dir)
  let step = i32vec3.(cond f32vec3.(r.dir >= zeros) (full 1) (full (-1)))
  let (_, _, hit, _) =
  loop (cell, t_cross, _, t) = (cell, t_cross, #miss, t_enter + EPS) 
  while bvec3.(all (map (\x -> 0 <= x && x < i32.i64 d) cell)) do
    -- We hit something.
    if vxls[cell.x, cell.y, cell.z] 
    then ({ x = -1, y = -1, z = -1 }, t_cross, #hit { t }, t)
    -- Step one cell forward.
    -- This is a vector of bool with exactly one 'true' where t_cross is minimal 
    else 
    let mask = f32vec3.(t_cross <= min (rot_left t_cross) (rot_right t_cross))
    --let mask = assert (bvec3.any mask) mask
    let t_cross = f32vec3.(t_cross + cond mask t_delta zeros)
    let cell = i32vec3.(cell + cond mask step zeros)
    let t = t + f32vec3.(sum_coords (cond mask t_delta zeros))
    in (cell, t_cross, #miss, t)
  in hit

--def raytrace [d] (fram : frame) (voxels : [d][d][d]bool) (r : ray) : hit =
--  let { hit, t_enter, t_leave } = rayframe_intersect r fram in
--  if !hit then #miss else
--  let cell_size = fram.size / f32.i64 d
--  let (_, hit) = 
--    -- t_curr is at the proximal boundary of the current voxel : 
--    -- testing which voxel the corresponding position is in is thus unstable.
--    -- We add EPS to t_curr to get a position slightly inside the current voxel.
--    loop (t_curr, _) = (f32.max 0.0 t_enter, #miss) while t_curr + EPS < t_leave do
--      -- This is the voxel with float coordinates that t_curr is in.
--      let norm_pos = f32vec3.scale (1.0 / cell_size) ((ray_eval r (t_curr + EPS)) f32vec3.- fram.pos)
--      -- This is the voxel with integer coordinates that t_curr is in.
--      let cell = i32vec3.map (f32.floor >-> i32.f32) norm_pos
--      in 
--        if bvec3.(all (map (\x -> 0 <= x && x < i32.i64 d) cell)) 
--          && voxels[cell.x, cell.y, cell.z]
--        -- We hit a voxel.
--        then (t_leave, #hit { t = t_curr }) 
--        -- Otherwise step forward
--        -- These are the first times at which we cross the x/y/z axis after t_curr. 
--        -- We have to use the same t as in norm_pos.
--        else let t_next = f32vec3.(
--          (full (t_curr f32.+ EPS)) + scale cell_size (map f32.i32 cell + map f32.bool (r.dir >= zeros) - norm_pos) / r.dir)
--        in (f32vec3.min_coord t_next, #miss)
--  in hit

def shade (tap : tape) (r : ray) (h : hit) : argb.colour = 
  match h 
  case #miss -> argb.black
  case #hit h -> argb.red
    --let pos = ray_eval r h.t
    --let grad = gradient_tape_evaluator.eval tap 
    --  { v = pos.x, dx = 1.0, dy = 0.0, dz = 0.0 }
    --  { v = pos.y, dx = 0.0, dy = 1.0, dz = 0.0 }
    --  { v = pos.z, dx = 0.0, dy = 0.0, dz = 1.0 }
    --  (gradient.constant 0.0)
    --let normal = f32vec3.normalize { x = grad.dx, y = grad.dy, z = grad.dz }
    --let color = f32vec3.(full 0.5 + scale 0.5 normal)
    --in argb.from_rgba color.x color.y color.z 1.0

-- Assumes that the camera axis vectors are normalized, orthogonal and correctly oriented.
-- The camera field of view is the horizontal field of view in radians.
-- This returns a matrix of 32-bit ARGB colours.
entry main 
  -- The pixel sizes are passed in as 64bits integers because 
  -- we use them as the size of the returned array.
  -- We immediately convert them to i32 afterwards.
  (pixel_width : i64) 
  (pixel_height : i64) 
  (cam_pos_x : f32) (cam_pos_y : f32) (cam_pos_z : f32) 
  (cam_forward_x : f32) (cam_forward_y : f32) (cam_forward_z : f32) 
  (cam_right_x : f32) (cam_right_y : f32) (cam_right_z : f32) 
  (cam_up_x : f32) (cam_up_y : f32) (cam_up_z : f32)  
  (cam_fov_rad : f32)
  (tape_instrs : []u32)
  (tape_constants : []f32)
  (tape_slot_count : i64)
    : [pixel_width][pixel_height]argb.colour =
  let cam : camera = { 
    pos = { x=cam_pos_x, y=cam_pos_y, z=cam_pos_z },
    forward = { x=cam_forward_x, y=cam_forward_y, z=cam_forward_z }, 
    right = { x=cam_right_x, y=cam_right_y, z=cam_right_z }, 
    up = { x=cam_up_x, y=cam_up_y, z=cam_up_z }, 
    fov_rad = cam_fov_rad,
    pixel_width = pixel_width,
    pixel_height = pixel_height
  }
  let fram : frame = { 
    pos = { x = -10, y = -10, z = -10 }, 
    size = 20.0
  }
  let tap : tape = { 
    instrs = map decode_instruction tape_instrs, 
    slot_count = tape_slot_count, 
    constants = tape_constants 
  }
  let d = 256
  --let voxels = 
  --  tabulate_3d d d d (\x y z -> 
  --    let p = fram.pos vec3.+ vec3.scale cell_size (vec3_from_i64 x y z)
  --    in scalar_tape_evaluator.eval tap p.x p.y p.z 0.0 <= 0.0)
  let { L0 } = build_voxels d tap fram
  let vxls = L0[0].leaf_mask
  in 
    tabulate_2d pixel_width pixel_height (\x y -> 
      let r = camera_make_ray cam x y
      let h = raytrace fram vxls r
      in shade tap r h)
