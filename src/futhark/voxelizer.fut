import "tape"
import "utils"
import "vector"


-- A simple cubic frame that will contain all the voxels.
type frame = {
  pos : f32vec3.t,
  size : f32
}

type node_mask [dim] = [dim][dim][dim]bool
type child_list [dim] = [dim][dim][dim]i32

-- A non-terminal node, i.e. some cells are nodes.
type NT_node [dim] = {
  world_pos : f32vec3.t,
  leaf_mask : node_mask[dim],
  child_mask : node_mask[dim],
  child_list : child_list[dim] -- the index of a child should be -1 if it does not exists, so that scatter works nicely
}

-- A terminal node, i.e. all cells are leafs or empty.
type T_node [dim] = {
  leaf_mask : node_mask[dim]
}

-- It's a shame we have to hardcode the number of levels.
-- We can't have an array of levels because each level has a different size.
-- We could have a single array that holds the flattened levels, but this would
-- require either concatenating the levels (thus copying lots of data), or pre-allocating
-- a gigantic buffer (which seems quite clunky, but is what I would probably do in OpenCL or CUDA).
-- The best option would be that futhark support some kind of syntactic sugar to emulate 'lists of variables' 
-- that would be desugared to simple variables at compile time... maybe this is already possible 
-- using some obscur language feature ?
--type~ voxels [d0] [d1] [d2] = {
--  L0 : []NT_node[d0],
--  L1 : []NT_node[d1],
--  L2 : []T_node[d2]
--}
type~voxels [d0] = {
  L0 : []T_node[d0]
}


def voxelize_interval d (tap : tape) (cell_size : f32) (node_pos : f32vec3.t)
  : { child_mask : node_mask[d], leaf_mask : node_mask[d] } =
  let intervals = tabulate_3d d d d (\x y z -> 
      let cell_pos = f32vec3.(node_pos + scale cell_size (map f32.i64 { x, y, z }))
      in interval_tape_evaluator.eval tap 
        { low = cell_pos.x, high = cell_pos.x + cell_size }
        { low = cell_pos.y, high = cell_pos.y + cell_size }
        { low = cell_pos.z, high = cell_pos.z + cell_size }
        { low = 0.0,        high = 0.0 })
  -- The inside is where the density is negative or zero, 
  -- the outside is where the density is positive.
  let child_mask = map (map (map (\i -> i.low <= 0.0 && 0.0 < i.high))) intervals
  let leaf_mask = map (map (map (\i -> i.high <= 0.0))) intervals 
  in { child_mask, leaf_mask }

def voxelize_scalar d (tap : tape) (cell_size : f32) (node_pos : f32vec3.t)
  : { leaf_mask : node_mask[d] } =
  let densities = tabulate_3d d d d (\x y z -> 
      let cell_pos = f32vec3.(node_pos + scale cell_size (map f32.i64 { x, y, z }))
      in scalar_tape_evaluator.eval tap 
        (cell_pos.x + cell_size / 2.0)
        (cell_pos.y + cell_size / 2.0)
        (cell_pos.z + cell_size / 2.0)
        0.0)
  -- The inside is where the density is negative or zero, 
  -- the outside is where the density is positive.
  let leaf_mask = map (map (map (\d -> d <= 0.0))) densities
  in { leaf_mask }
  
-- We want to create an array that contains the world position of every node in the new level,
-- at the correct position for this node : for every node in the previous level and child cell, 
--   arr[node.child_list[cell]] = cell_position  
-- n is the number of nodes in the current level (i.e. the number of children of the previous level).
def build_world_pos [dp] n (cell_size : f32) (prev_lvl : []NT_node[dp]) 
  : *[n]f32vec3.t =
  -- We rely on the fact that scatter simply ignores out of bound indices.
  let (is, vs) = map (\node -> 
      tabulate_3d dp dp dp (\x y z -> 
        let cell_pos = f32vec3.(node.world_pos + scale cell_size (map f32.i64 { x, y, z }))
        in (i64.i32 node.child_list[x, y, z], cell_pos))) 
      prev_lvl
    |> flatten_4d |> unzip
  in scatter (replicate n f32vec3.zeros) is vs

def build_NT_level [dp] d n (tap : tape) (cell_size : f32) (prev_lvl : []NT_node[dp])
  : { child_count : i64, nodes : *[n]NT_node[d] } = 
  let world_pos = build_world_pos n cell_size prev_lvl
  -- Compute the child and leaf masks
  let masks = map (voxelize_interval d tap cell_size) world_pos
  let child_masks = map (\{ child_mask, leaf_mask=_ } -> child_mask) masks
  let leaf_masks = map (\{ child_mask=_, leaf_mask } -> leaf_mask) masks
  -- Compute the child lists and child count.
  -- We have to substract one because scan includes the last element of each prefix.
  let child_indices = 
    flatten_4d child_masks 
    |> map i32.bool 
    |> scan (+) 0
    |> map (\i -> i - 1)
  let child_count = i64.i32 (child_indices[length child_indices - 1] + 1)
  let child_lists = unflatten_4d n d d d child_indices
  -- Put it all together
  let nodes = map4 (\wp lm cm cl -> 
    { world_pos = wp, leaf_mask = lm, child_mask = cm, child_list = cl }) 
    world_pos leaf_masks child_masks child_lists
  in { child_count, nodes }

def build_T_level [dp] d n (tap : tape) (cell_size : f32) (prev_lvl : []NT_node[dp])
  : *[n]T_node[d] =
  let world_pos = build_world_pos n cell_size prev_lvl 
  in map (voxelize_scalar d tap cell_size) world_pos 

--def build_voxels d0 d1 d2 (tap : tape) (fram : frame) 
--  : voxels[d0][d1][d2] =
--  -- This is a phantom level, with one node that has one cell.
--  -- It is used to build the first level the same way as the following ones.
--  let phantom_lvl : [1]NT_node[1] = [{ 
--    world_pos = fram.pos,
--    leaf_mask =  [[[false]]],
--    child_mask = [[[true]]],
--    child_list = [[[0]]]
--  }]
--  let n0 = 1
--  let { child_count = n1, nodes = L0 } = build_NT_level d0 n0 tap (fram.size / f32.i64 d0)         phantom_lvl
--  let { child_count = n2, nodes = L1 } = build_NT_level d1 n1 tap (fram.size / f32.i64 (d0*d1))    L0
--  let L2                               = build_T_level  d2 n2 tap (fram.size / f32.i64 (d0*d1*d2)) L1
--  in { L0, L1, L2 }

def build_voxels d0 (tap : tape) (fram : frame) 
  : voxels[d0] =
  -- This is a phantom level, with one node that has one cell.
  -- It is used to build the first level the same way as the following ones.
  let phantom_lvl : [1]NT_node[1] = [{ 
    world_pos = fram.pos,
    leaf_mask =  [[[false]]],
    child_mask = [[[true]]],
    child_list = [[[0]]]
  }]
  let n0 = 1
  let L0 = build_T_level d0 n0 tap (fram.size / f32.i64 d0) phantom_lvl
  in { L0 }

