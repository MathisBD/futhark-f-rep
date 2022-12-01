import "lib/github.com/athas/matte/colour"
import "utils"
import "tape"
import "voxelizer"

-- This is used for raycasting.
-- DO NOT make it too small, or the program will crash !
def EPS : f32 = 0.00001


type frame = {
  pos : vec3.vector,
  size : f32
}

type ray = { orig : vec3.vector, dir : vec3.vector }

type hit = #miss | #hit { t : f32 }

-- Get the point on the ray corresponding to the parameter t
def ray_eval (r : ray) (t : f32) : vec3.vector = 
  r.orig vec3.+ (vec3.scale t r.dir)

type camera = {
  pos : vec3.vector, -- the position in world space of the camera center
  -- the local axes of the camera :
  forward : vec3.vector,
  right : vec3.vector,
  up : vec3.vector,
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
    cam.forward vec3.+
    vec3.scale (dx * screen_world_size_x) cam.right vec3.+
    vec3.scale (dy * screen_world_size_y) cam.up
  in { orig = cam.pos, dir = vec3.normalise dir }

-- hit is true iff the ray intersects the grid
-- t_enter is the time that the ray enters the grid (undefined when hit = false)
-- t_leave is the time at which the ray leaves the grid (undefined when hit = false)
def rayframe_intersect (r : ray) (fram : frame) : { hit : bool, t_enter : f32, t_leave : f32 } =
  let t_0 = (fram.pos vec3.- r.orig) vec3./ r.dir
  let t_1 = (fram.pos vec3.+ (vec3_full fram.size) vec3.- r.orig) vec3./ r.dir
  -- The (unique) time that the ray enters the grid along each axis.
  let t_enter = {
    x = if r.dir.x >= 0.0 then t_0.x else t_1.x,
    y = if r.dir.y >= 0.0 then t_0.y else t_1.y,
    z = if r.dir.z >= 0.0 then t_0.z else t_1.z
  }
  -- The (unique) time that the ray leaves the grid along each axis.
  let t_leave = {
    x = if r.dir.x >= 0.0 then t_1.x else t_0.x,
    y = if r.dir.y >= 0.0 then t_1.y else t_0.y,
    z = if r.dir.z >= 0.0 then t_1.z else t_0.z
  }
  let hit = vec3_max t_enter <= vec3_min t_leave && 0.0 <= vec3_min t_leave
  in { hit = hit, t_enter = vec3_max t_enter, t_leave = vec3_min t_leave }

def raytrace [d] (fram : frame) (voxels : [d][d][d]bool) (r : ray) : hit =
  let { hit, t_enter, t_leave } = rayframe_intersect r fram in
  if !hit then #miss else
  let cell_size = fram.size / f32.i64 d
  let (_, hit) = 
    -- t_curr is at the proximal boundary of the current voxel : 
    -- testing which voxel the corresponding position is in is thus unstable.
    -- We add EPS to t_curr to get a position slightly inside the current voxel.
    loop (t_curr, _) = (f32.max 0.0 t_enter, #miss) while t_curr + EPS < t_leave do
      -- This is the voxel with float coordinates that t_curr is in.
      let norm_pos = vec3.scale (1.0 / cell_size) ((ray_eval r (t_curr + EPS)) vec3.- fram.pos)
      -- This is the voxel with integer coordinates that t_curr is in.
      let (x, y, z) = (
        norm_pos.x |> f32.floor |> i64.f32, 
        norm_pos.y |> f32.floor |> i64.f32, 
        norm_pos.z |> f32.floor |> i64.f32)
      in 
        if 0 <= x && x < d && 0 <= y && y < d && 0 <= z && z < d && voxels[x, y, z]
        -- We hit a voxel.
        then (t_leave, #hit { t = t_curr }) 
        -- Otherwise step forward
        -- These are the first times at which we cross the x/y/z axis after t_curr. 
        -- We have to use the same t as in norm_pos.
        else let (t_x, t_y, t_z) = (
          t_curr + EPS + (f32.i64 x + (if r.dir.x >= 0.0 then 1.0 else 0.0) - norm_pos.x) * cell_size / r.dir.x,
          t_curr + EPS + (f32.i64 y + (if r.dir.y >= 0.0 then 1.0 else 0.0) - norm_pos.y) * cell_size / r.dir.y,
          t_curr + EPS + (f32.i64 z + (if r.dir.z >= 0.0 then 1.0 else 0.0) - norm_pos.z) * cell_size / r.dir.z)
        in (vec3_min { x=t_x, y=t_y, z=t_z }, #miss)
  in hit

def shade (tap : tape) (r : ray) (h : hit) : argb.colour = 
  match h 
  case #miss -> argb.black
  case #hit h -> 
    let pos = ray_eval r h.t
    let grad = gradient_tape_evaluator.eval tap 
      { v = pos.x, dx = 1.0, dy = 0.0, dz = 0.0 }
      { v = pos.y, dx = 0.0, dy = 1.0, dz = 0.0 }
      { v = pos.z, dx = 0.0, dy = 0.0, dz = 1.0 }
      (gradient.constant 0.0)
    let normal = vec3.normalise { x = grad.dx, y = grad.dy, z = grad.dz }
    let color = (vec3_full 0.5) vec3.+ (vec3.scale 0.5 normal)
    in argb.from_rgba color.x color.y color.z 1.0

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
  let cell_size = fram.size / f32.i64 d
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


---- A simple cubic frame that will contain all the voxels.
--type frame = {
--  pos : vec3.vector,
--  size : f32
--}
--
--type ray = { orig : vec3.vector, dir : vec3.vector }
--
--type hit = #miss | #hit { t : f32 }
--
---- Get the point on the ray corresponding to the parameter t
--def ray_eval (r : ray) (t : f32) : vec3.vector = 
--  r.orig vec3.+ (vec3.scale t r.dir)
--
--type camera = {
--  pos : vec3.vector, -- the position in world space of the camera center
--  -- the local axes of the camera :
--  forward : vec3.vector,
--  right : vec3.vector,
--  up : vec3.vector,
--  fov_rad : f32, -- the horizontal field of view of the camera, in radians
--  pixel_width : i64, -- the pixel width of the screen
--  pixel_height : i64 -- the pixel height of the screen
--}
--
---- This returns a ray passing starting at the camera center 
---- and corresponding to the direction of the pixel (x, y).
--def camera_make_ray (cam : camera) pixel_x pixel_y : ray =
--  -- The world size of the screen at a unit distance from the camera, along the camera's local axes
--  let screen_world_size_x = 2.0 * f32.tan (cam.fov_rad / 2.0)
--  let screen_world_size_y = screen_world_size_x * (f32.i64 cam.pixel_height / f32.i64 cam.pixel_width) 
--  -- dx and dy are in the range [-1...1]
--  let dx = 2.0 * (f32.i64 pixel_x / f32.i64 cam.pixel_width) - 1.0
--  let dy = 2.0 * (f32.i64 pixel_y / f32.i64 cam.pixel_height) - 1.0
--  let dir = 
--    cam.forward vec3.+
--    vec3.scale (dx * screen_world_size_x) cam.right vec3.+
--    vec3.scale (dy * screen_world_size_y) cam.up
--  in { orig = cam.pos, dir = vec3.normalise dir }
--
---- hit is true iff the ray intersects the frame
---- t_enter is the time that the ray enters the frame (undefined when hit = false)
---- t_leave is the time at which the ray leaves the frame (undefined when hit = false)
--def rayframe_intersect (r : ray) (fram : frame) : { hit : bool, t_enter : f32, t_leave : f32 } =
--  let t_0 = (fram.pos vec3.- r.orig) vec3./ r.dir
--  let t_1 = (fram.pos vec3.+ (vec3_full fram.size) vec3.- r.orig) vec3./ r.dir
--  -- The (unique) time that the ray enters the frame along each axis.
--  let t_enter = {
--    x = if r.dir.x >= 0.0 then t_0.x else t_1.x,
--    y = if r.dir.y >= 0.0 then t_0.y else t_1.y,
--    z = if r.dir.z >= 0.0 then t_0.z else t_1.z
--  }
--  -- The (unique) time that the ray leaves the frame along each axis.
--  let t_leave = {
--    x = if r.dir.x >= 0.0 then t_1.x else t_0.x,
--    y = if r.dir.y >= 0.0 then t_1.y else t_0.y,
--    z = if r.dir.z >= 0.0 then t_1.z else t_0.z
--  }
--  let hit = vec3_max t_enter <= vec3_min t_leave && 0.0 <= vec3_min t_leave
--  in { hit = hit, t_enter = vec3_max t_enter, t_leave = vec3_min t_leave }
--
--def raytrace [d] (fram : frame) (vxls : [d][d][d]bool) (r : ray) : hit =
--  let { hit, t_enter, t_leave } = rayframe_intersect r fram in
--  if !hit then #miss else
--  let cell_size = fram.size / f32.i64 d
--  let (_, hit) = 
--    -- t_curr is at the proximal boundary of the current voxel : 
--    -- testing which voxel the corresponding position is in is thus unstable.
--    -- We add EPS to t_curr to get a position slightly inside the current voxel.
--    loop (t_curr, _) = (f32.max 0.0 t_enter, #miss) while t_curr + EPS < t_leave do
--      -- This is the voxel with float coordinates that t_curr is in.
--      let norm_pos = vec3.scale (1.0 / cell_size) ((ray_eval r (t_curr + EPS)) vec3.- fram.pos)
--      -- This is the voxel with integer coordinates that t_curr is in.
--      let (x, y, z) = (
--        norm_pos.x |> f32.floor |> i64.f32, 
--        norm_pos.y |> f32.floor |> i64.f32, 
--        norm_pos.z |> f32.floor |> i64.f32)
--      in 
--        if 0 <= x && x < d && 0 <= y && y < d && 0 <= z && z < d && vxls[x, y, z]
--        -- We hit a voxel.
--        then (t_leave, #hit { t = t_curr }) 
--        -- Otherwise step forward
--        -- These are the first times at which we cross the x/y/z axis after t_curr. 
--        -- We have to use the same t as in norm_pos.
--        else let (t_x, t_y, t_z) = (
--          t_curr + EPS + (f32.i64 x + (if r.dir.x >= 0.0 then 1.0 else 0.0) - norm_pos.x) * cell_size / r.dir.x,
--          t_curr + EPS + (f32.i64 y + (if r.dir.y >= 0.0 then 1.0 else 0.0) - norm_pos.y) * cell_size / r.dir.y,
--          t_curr + EPS + (f32.i64 z + (if r.dir.z >= 0.0 then 1.0 else 0.0) - norm_pos.z) * cell_size / r.dir.z)
--        let t_next = vec3_min { x=t_x, y=t_y, z=t_z }
--        in assert (t_curr < t_next) (t_next, #miss)
--  in hit
--
--def shade (_ : tape) (_ : ray) (h : hit) : argb.colour = 
--  match h 
--  case #miss -> argb.black
--  case #hit _ -> argb.red
--  --  let pos = ray_eval r h.t
--  --  let grad = gradient_tape_evaluator.eval tap 
--  --    { v = pos.x, dx = 1.0, dy = 0.0, dz = 0.0 }
--  --    { v = pos.y, dx = 0.0, dy = 1.0, dz = 0.0 }
--  --    { v = pos.z, dx = 0.0, dy = 0.0, dz = 1.0 }
--  --    (gradient.constant 0.0)
--  --  let normal = vec3.normalise { x = grad.dx, y = grad.dy, z = grad.dz }
--  --  let color = (vec3_full 0.5) vec3.+ (vec3.scale 0.5 normal)
--  --  in argb.from_rgba color.x color.y color.z 1.0
--
--
---- Assumes that the camera axis vectors are normalized, orthogonal and correctly oriented.
---- The camera field of view is the horizontal field of view in radians.
---- This returns a matrix of 32-bit ARGB colours.
--entry main 
--  -- The pixel sizes are passed in as 64bits integers because 
--  -- we use them as the size of the returned array.
--  -- We immediately convert them to i32 afterwards.
--  (pixel_width : i64) 
--  (pixel_height : i64) 
--  (cam_pos_x : f32) (cam_pos_y : f32) (cam_pos_z : f32) 
--  (cam_forward_x : f32) (cam_forward_y : f32) (cam_forward_z : f32) 
--  (cam_right_x : f32) (cam_right_y : f32) (cam_right_z : f32) 
--  (cam_up_x : f32) (cam_up_y : f32) (cam_up_z : f32)  
--  (cam_fov_rad : f32)
--  (tape_instrs : []u32)
--  (tape_constants : []f32)
--  (tape_slot_count : i64)
--    : [pixel_width][pixel_height]argb.colour =
--  let cam : camera = { 
--    pos = { x=cam_pos_x, y=cam_pos_y, z=cam_pos_z },
--    forward = { x=cam_forward_x, y=cam_forward_y, z=cam_forward_z }, 
--    right = { x=cam_right_x, y=cam_right_y, z=cam_right_z }, 
--    up = { x=cam_up_x, y=cam_up_y, z=cam_up_z }, 
--    fov_rad = cam_fov_rad,
--    pixel_width = pixel_width,
--    pixel_height = pixel_height
--  }
--  let fram : frame = { 
--    pos = { x = -10, y = -10, z = -10 }, 
--    size = 20.0
--  }
--  let tap : tape = { 
--    instrs = map decode_instruction tape_instrs, 
--    slot_count = tape_slot_count, 
--    constants = tape_constants 
--  }
--  let d = 8
--  let cell_size = fram.size / f32.i64 d
--  let vxls = 
--    tabulate_3d d d d (\x y z -> 
--      let pos = fram.pos vec3.+ vec3.scale cell_size (vec3_from_i64 x y z) 
--      let f = scalar_tape_evaluator.eval tap pos.x pos.y pos.z 0.0
--      in f <= 0.0)
--  in 
--    tabulate_2d pixel_width pixel_height (\x y -> 
--      let r = camera_make_ray cam x y
--      let h = raytrace fram vxls r
--      in shade tap r h)
--
--def test (z : f32) = 
--  let cam : camera = { 
--    pos = { x=0.0, y=0.0, z=z },
--    forward = { x=0.0, y=0.0, z= -1.0 }, 
--    right = { x=1.0, y=0.0, z=0.0 }, 
--    up = { x=0.0, y=1.0, z=0.0 }, 
--    fov_rad = 1.22,
--    pixel_width = 1200,
--    pixel_height = 900
--  }
--  let fram : frame = { 
--    pos = { x = -10, y = -10, z = -10 }, 
--    size = 20.0
--  }
--  let d = 8
--  let cell_size = fram.size / f32.i64 d
--  let vxls = 
--    tabulate_3d d d d (\x y z -> 
--      let pos = fram.pos vec3.+ vec3.scale cell_size (vec3_from_i64 x y z) 
--      let f = vec3.norm pos - 10.0
--      in f <= 0.0)
--  in 
--    tabulate_2d 120 90 (\x y -> 
--      let r = camera_make_ray cam x y
--      let h = raytrace fram vxls r
--      in match h case #miss -> argb.black case #hit _ -> argb.red)
--
--
---- 
---- compiled input { 
----   1200i64 900i64 
----   0f32 0f32 20f32 
----   0f32 0f32 -1f32 
----   1f32 0f32 0f32 
----   0f32 1f32 0f32 
----   1.22f32 }