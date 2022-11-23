import "lib/github.com/athas/matte/colour"
import "utils"

type ray = { orig : vec3.vector, dir : vec3.vector }

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

-- This describes a cube grid in 3D space
type grid = { 
  pos : vec3.vector, -- the corner (x_min, y_min, z_mn) of the grid in world space
  size : f32, -- the extent of the grid in world size : it is the same along each axis
  dim : i64 -- the number of cells along each axis
}

-- The world size of a single cell of the grid
def grid_cell_size (grd : grid) : f32 = grd.size / f32.i64 grd.dim

-- Converts integer voxel coordinates to a world position 
def grid_voxel2world (grd : grid) x y z : vec3.vector = 
  { 
    x = grd.pos.x + grd.size * (f32.i64 x / f32.i64 grd.dim), 
    y = grd.pos.y + grd.size * (f32.i64 y / f32.i64 grd.dim), 
    z = grd.pos.z + grd.size * (f32.i64 z / f32.i64 grd.dim)
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
def raygrid_intersect (r : ray) (grd : grid) : { hit : bool, t_enter : f32, t_leave : f32 } =
  let t_0 = (grd.pos vec3.- r.orig) vec3./ r.dir
  let t_1 = (grd.pos vec3.+ (vec3_full grd.size) vec3.- r.orig) vec3./ r.dir
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

def raytrace [n] (grd : grid) (voxels : [n][n][n]bool) (r : ray) : argb.colour =
  let { hit, t_enter, t_leave } = raygrid_intersect r grd in
  if !hit then 0 else
  let (_, color) = 
    -- t_curr is at the proximal boundary of the current voxel : 
    -- testing which voxel the corresponding position is in is thus unstable.
    -- We add EPS to t_curr to get a position slightly inside the current voxel.
    loop (t_curr, _) = (f32.max 0.0 t_enter, argb.black) while t_curr + EPS < t_leave do
      -- This is the voxel with float coordinates that t_curr is in.
      let norm_pos = vec3.scale (1.0 / grid_cell_size grd) ((ray_eval r (t_curr + EPS)) vec3.- grd.pos)
      -- This is the voxel with integer coordinates that t_curr is in.
      let (x, y, z) = (
        norm_pos.x |> f32.floor |> i64.f32, 
        norm_pos.y |> f32.floor |> i64.f32, 
        norm_pos.z |> f32.floor |> i64.f32)
      in 
        if 0 <= x && x < grd.dim && 0 <= y && y < grd.dim && 0 <= z && z < grd.dim && voxels[x, y, z]
        -- We hit a voxel.
        then (t_leave, argb.red) 
        -- Otherwise step forward
        -- These are the first times at which we cross the x/y/z axis after t_curr. 
        -- We have to use the same t as in norm_pos.
        else let (t_x, t_y, t_z) = (
          t_curr + EPS + (f32.i64 x + (if r.dir.x >= 0.0 then 1.0 else 0.0) - norm_pos.x) * (grid_cell_size grd) / r.dir.x,
          t_curr + EPS + (f32.i64 y + (if r.dir.y >= 0.0 then 1.0 else 0.0) - norm_pos.y) * (grid_cell_size grd) / r.dir.y,
          t_curr + EPS + (f32.i64 z + (if r.dir.z >= 0.0 then 1.0 else 0.0) - norm_pos.z) * (grid_cell_size grd) / r.dir.z)
        in (vec3_min { x=t_x, y=t_y, z=t_z }, argb.black)
  in color


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
  let grd : grid = { 
    pos = { x = -10, y = -10, z = -10 }, 
    size = 20.0, 
    dim = 256 
  }
  let voxels = 
    make_grid_3d grd.dim grd.dim grd.dim 
    |> map (map (map (\(x, y, z) -> let p = grid_voxel2world grd x y z in vec3.norm p <= 10.0)))
  in make_grid_2d pixel_width pixel_height
  |> map (map (\(x, y) -> camera_make_ray cam x y)) 
  |> map (map (raytrace grd voxels))


-- ==
-- compiled input { 
--   1200i64 900i64 
--   0f32 0f32 20f32 
--   0f32 0f32 -1f32 
--   1f32 0f32 0f32 
--   0f32 1f32 0f32 
--   1.22f32 }