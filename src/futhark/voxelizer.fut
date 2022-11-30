import "tape"
import "utils"


type node_mask [dim] = [dim][dim][dim]bool
type child_list [dim] = [dim][dim][dim]i32

-- A non-terminal node, i.e. some cells are nodes.
type NT_node [dim] = {
  world_pos : vec3.vector,
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
type~ voxels [d0] [d1] [d2] = {
  L0 : []NT_node[d0],
  L1 : []NT_node[d1],
  L2 : []T_node[d2]
}


def voxelize_interval d (tap : tape) (cell_size : f32) (node_pos : vec3.vector)
  : { child_mask : node_mask[d], leaf_mask : node_mask[d] } = ???
  
def voxelize_scalar d (tap : tape) (cell_size : f32) (node_pos : vec3.vector)
  : { leaf_mask : node_mask[d] } = ???

-- We want to create an array that contains the world position of every node in the new level,
-- at the correct position for this node : for every node in the previous level and child cell, 
--   arr[node.child_list[cell]] = cell_position  
-- n is the number of nodes in the current level (i.e. the number of children of the previous level).
def build_world_pos [dp] n (cell_size : f32) (prev_lvl : []NT_node[dp]) 
  : *[n]vec3.vector =
  -- We rely on the fact that scatter simply ignores out of bound indices.
  let (is, vs) = map (\node -> 
      tabulate_3d dp dp dp (\x y z -> 
        let cell_pos = node.world_pos vec3.+ vec3.scale (1.0 / cell_size) (vec3_from_i64 x y z)
        in (i64.i32 node.child_list[x, y, z], cell_pos))) 
      prev_lvl
    |> flatten_4d |> unzip
  in scatter (replicate n vec3.zero) is vs

def build_NT_level [dp] d n (tap : tape) (cell_size : f32) (prev_lvl : []NT_node[dp])
  : { child_count : i64, nodes : *[n]NT_node[d] } = ???  

def build_T_level [dp] d n (tap : tape) (cell_size : f32) (prev_lvl : []NT_node[dp])
  : *[n]T_node[d] =
  let world_pos = build_world_pos n cell_size prev_lvl 
  in map (voxelize_scalar d tap cell_size) world_pos 

def build_voxels d0 d1 d2 (tap : tape) (grid_pos : vec3.vector) (grid_size : f32) 
  : voxels[d0][d1][d2] =
  -- This is a phantom level, with one node that has one cell.
  -- It is used to build the first level the same way as the following ones.
  let phantom_lvl : [1]NT_node[1] = [{ 
    world_pos = grid_pos,
    leaf_mask =  [[[false]]],
    child_mask = [[[true]]],
    child_list = [[[0]]]
  }]
  let n0 = 1
  let { child_count = n1, nodes = L0 } = build_NT_level d0 n0 tap (grid_size / f32.i64 d0)         phantom_lvl
  let { child_count = n2, nodes = L1 } = build_NT_level d1 n1 tap (grid_size / f32.i64 (d0*d1))    L0
  let L2                               = build_T_level  d2 n2 tap (grid_size / f32.i64 (d0*d1*d2)) L1
  in { L0, L1, L2 }

