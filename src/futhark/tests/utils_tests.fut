import "../utils"


-- We take care to test edge cases for this function.
-- ==
-- entry: contains_int_correct
-- input { 42f32    42f32    } output { true }
-- input { -5.3f32  18.5f32  } output { true }
-- input { -42.7f32 -42.1f32 } output { false }
-- input { 18f32    -5f32    } output { false }
-- input { 0f32     f32.inf  } output { true }
-- input { -f32.inf 0f32     } output { true }
-- input { f32.inf  f32.inf  } output { false }
-- input { -f32.inf -f32.inf } output { false }
-- input { -f32.inf f32.inf  } output { true }
-- input { f32.inf  -f32.inf } output { false }
-- input { f32.nan  0f32     } output { false }
-- input { 0f32     f32.nan  } output { false }
-- input { -f32.inf f32.nan  } output { false }
-- input { f32.nan  f32.inf  } output { false }
-- input { f32.nan  f32.nan  } output { false }
entry contains_int_correct (a : f32) (b : f32) =
  contains_int a b

