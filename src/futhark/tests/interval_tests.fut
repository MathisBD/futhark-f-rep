import "../tape"


def is_interval_valid (i : interval.t) = 
  i.low != f32.nan && i.high != f32.nan && i.low <= i.high

def inputs : []interval.t = [
  { low = 0.0, high = 0.0 },
  { low = 42.42, high = 42.42 },
  { low = 1.0, high = 2.0 },
  { low = -2.0, high = 1.0 },
  { low = -2.0, high = 2.0 },
  { low = 0.0, high = 1.0 },
  { low = -1.0, high = 0.0 },
  { low = -f32.inf, high = 1.0 },
  { low = 1.0, high = f32.inf },
  { low = -f32.inf, high = 0.0 },
  { low = 0.0, high = f32.inf },
  { low = -f32.inf, high = f32.inf },
  { low = -f32.inf, high = -f32.inf },
  { low = f32.inf, high = f32.inf }
]

-- ==
-- entry: sin_valid
-- input { } output { true }
entry sin_valid =
  map interval.sin inputs
  |> all is_interval_valid

-- ==
-- entry: cos_valid
-- input { } output { true }
entry cos_valid =
  map interval.cos inputs
  |> all is_interval_valid

-- ==
-- entry: exp_valid
-- input { } output { true }
entry exp_valid =
  map interval.exp inputs
  |> all is_interval_valid

-- ==
-- entry: sqrt_valid
-- input { } output { true }
entry sqrt_valid =
  map interval.sqrt inputs
  |> all is_interval_valid

-- ==
-- entry: neg_valid
-- input { } output { true }
entry neg_valid =
  map interval.neg inputs
  |> all is_interval_valid

-- ==
-- entry: add_valid
-- input { } output { true }
entry add_valid =
  map (\a -> map (\b -> interval.add a b) inputs) inputs
  |> flatten
  |> all is_interval_valid

-- ==
-- entry: sub_valid
-- input { } output { true }
entry sub_valid =
  map (\a -> map (\b -> interval.sub a b) inputs) inputs
  |> flatten
  |> all is_interval_valid

-- ==
-- entry: mul_valid
-- input { } output { true }
entry mul_valid =
  map (\a -> map (\b -> interval.mul a b) inputs) inputs
  |> flatten
  |> all is_interval_valid

-- ==
-- entry: div_valid
-- input { } output { true }
entry div_valid =
  map (\a -> map (\b -> interval.div a b) inputs) inputs
  |> flatten
  |> all is_interval_valid

-- ==
-- entry: min_valid
-- input { } output { true }
entry min_valid =
  map (\a -> map (\b -> interval.min a b) inputs) inputs
  |> flatten
  |> all is_interval_valid

-- ==
-- entry: max_valid
-- input { } output { true }
entry max_valid =
  map (\a -> map (\b -> interval.max a b) inputs) inputs
  |> flatten
  |> all is_interval_valid