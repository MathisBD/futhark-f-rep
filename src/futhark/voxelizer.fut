import "tape"
import "utils"


type node_mask [dim] = [dim][dim][dim]bool
type child_list [dim] = [dim][dim][dim]i32

-- A non-terminal level, i.e. some cells are nodes.
type NT_level [n][dim] = {
  world_pos : [n]vec3.vector, -- The world position of each node.
  leaf_masks : [n]node_mask[dim],
  child_masks : [n]node_mask[dim],
  child_lists : [n]child_list[dim]
}

-- A terminal level, i.e. all cells are leafs.
type T_level [n][dim] = {
    leaf_masks : [n]node_mask[dim]
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
  L0 : NT_level [][d0],
  L1 : NT_level [][d1],
  L2 : T_level [][d2]
}

def voxelize_interval (tap : tape) (dim : i64) (cell_size : f32) (node_pos : vec3.vector)
  : { child_mask : node_mask[dim], leaf_mask : node_mask[dim] } = ???
  
def voxelize_scalar (tap : tape) (dim : i64) (cell_size : f32) (node_pos : vec3.vector)
  : { leaf_mask : node_mask[dim] } = ???

def build_NT_level (tap : tape) (dim : i64) (prev_lvl : NT_level[][])
  : NT_level[][dim] = ???

def build_T_level (tap : tape) (dim : i64) (prev_cell_size : f32) (prev_lvl : NT_level[][])
  : T_level[][dim] =
  
-- for node in prev_lvl : 
--   for each cell:
--     if child_masks[node][cell]:
--       world_pos[child_lists[node][cell]] = f(world_pos[node], cell)


def build_voxels (tap : tape) d0 d1 d2 (grid_pos : vec3.vector) (grid_size : f32) : voxels[d0][d1][d2] =
  -- This is a phantom level, with one node that has one cell.
  -- It is used to build the first level the same way as the following ones.
  let phantom_lvl : NT_level[1][1] = { 
    world_pos = [grid_pos],
    leaf_masks =  [[[[false]]]],
    child_masks = [[[[true]]]],
    child_lists = [[[[0]]]]
  }
  let L0 = build_NT_level tap d0 phantom_lvl
  let L1 = build_NT_level tap d1 L0
  let L2 = build_T_level tap d2 L1 
  in { L0, L1, L2 }

