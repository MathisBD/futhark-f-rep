import "utils"


-- An abstract value type, on which we will perform tape evaluation.
module type value = {
  type t 
  val constant : f32 -> *t
  val sin : t -> *t
  val cos : t -> *t
  val exp : t -> *t
  val sqrt : t -> *t
  val neg : t -> *t
  val add : t -> t -> *t
  val sub : t -> t -> *t
  val mul : t -> t -> *t
  val div : t -> t -> *t
  val min : t -> t -> *t 
  val max : t -> t -> *t
  val copy : t -> *t
}

module scalar : (value with t = f32) = {
  type t = f32
  def constant (x : f32) = x
  def sin = f32.sin
  def cos = f32.cos
  def exp = f32.exp
  def sqrt = f32.sqrt
  def neg = f32.neg
  def add = (f32.+)
  def sub = (f32.-)
  def mul = (f32.*)
  def div = (f32./)
  def min = f32.min
  def max = f32.max
  def copy = id
}


module gradient : (value with t = { v : f32, dx : f32, dy : f32, dz : f32 }) = {
  -- This represents the value of the function and its three partial derivatives.
  type t = { v : f32, dx : f32, dy : f32, dz : f32 }
  
  def constant (x : f32) = 
    { v = x, dx = 0f32, dy = 0f32, dz = 0f32 }

  def sin (a : t) =
    { v  = f32.sin a.v, 
      dx = a.dx * f32.cos a.v, 
      dy = a.dy * f32.cos a.v, 
      dz = a.dz * f32.cos a.v }
  
  def cos (a : t) =
    { v  = f32.cos a.v, 
      dx = - a.dx * f32.sin a.v, 
      dy = - a.dy * f32.sin a.v, 
      dz = - a.dz * f32.sin a.v }
  
  def exp (a : t) =
    { v  = f32.exp a.v, 
      dx = a.dx * f32.exp a.v, 
      dy = a.dy * f32.exp a.v, 
      dz = a.dz * f32.exp a.v }
    
  def sqrt (a : t) =
    { v  = f32.sqrt a.v, 
      dx = a.dx / (2.0 * f32.sqrt a.v), 
      dy = a.dy / (2.0 * f32.sqrt a.v), 
      dz = a.dz / (2.0 * f32.sqrt a.v) }
    
  def neg (a : t) =
    { v  = - a.v, 
      dx = - a.dx, 
      dy = - a.dy, 
      dz = - a.dz }

  def add (a : t) (b : t) = 
    { v  = a.v + b.v, 
      dx = a.dx + b.dx, 
      dy = a.dy + b.dy, 
      dz = a.dz + b.dz }  
  
  def sub (a : t) (b : t) = 
    { v  = a.v - b.v, 
      dx = a.dx - b.dx, 
      dy = a.dy - b.dy, 
      dz = a.dz - b.dz }  
  
  def mul (a : t) (b : t) = 
    { v  = a.v * b.v, 
      dx = a.dx * b.v + a.v * b.dx, 
      dy = a.dy * b.v + a.v * b.dy, 
      dz = a.dz * b.v + a.v * b.dz }  
  
  def div (a : t) (b : t) = 
    { v  = a.v / b.v, 
      dx = (a.dx * b.v - a.v * b.dx) / (b.v**2), 
      dy = (a.dy * b.v - a.v * b.dy) / (b.v**2), 
      dz = (a.dz * b.v - a.v * b.dz) / (b.v**2) }  
  
  def min (a : t) (b : t) =
    if a.v < b.v then a else b
    
  def max (a : t) (b : t) =
    if a.v > b.v then a else b

  def copy = id
}

module interval : (value with t = { low : f32, high : f32 }) = {
  -- This represents an interval of floats, with endpoints included.
  -- Note that an endpoint can be inf/-inf, but it should NEVER be NAN.
  type t = { low : f32, high : f32 }

  -- If an endpoint of the interval a is NAN, replace it with inf/-inf.
  -- Call this after performing any operation on intervals if you suspect it could create NANs.
  def remove_nans (a : t) =
    { low  = if f32.isnan a.low then -f32.inf else a.low, 
      high = if f32.isnan a.high then f32.inf else a.high }

  def constant (x : f32) = 
    { low = x, high = x } 
    |> remove_nans

  def TWO_PI : f32 = 2*f32.pi

  -- The default behaviour for min/max is : f32.min 42.0 f32.nan is 42.0.
  -- Since f32.sin f32.inf is f32.nan we have to deal with this here.
  def sin (a : t) =
    { low  = if contains_int (a.low/TWO_PI - 3/4) (a.high/TWO_PI - 3/4) ||
                f32.isnan (f32.sin a.low) || f32.isnan (f32.sin a.high)
             then -1f32 
             else f32.min (f32.sin a.low) (f32.sin a.high),
      high = if contains_int (a.low/TWO_PI - 1/4) (a.high/TWO_PI - 1/4) ||
                f32.isnan (f32.sin a.low) || f32.isnan (f32.sin a.high)
             then 1f32 
             else f32.max (f32.sin a.low) (f32.sin a.high) }

  -- See comment above sin.
  def cos (a : t) = 
    { low  = if contains_int (a.low/TWO_PI - 1/2) (a.high/TWO_PI - 1/2) ||
                f32.isnan (f32.cos a.low) || f32.isnan (f32.cos a.high) 
             then -1f32 
             else f32.min (f32.cos a.low) (f32.cos a.high),
      high = if contains_int (a.low/TWO_PI) (a.high/TWO_PI) ||
                f32.isnan (f32.cos a.low) || f32.isnan (f32.cos a.high) 
             then 1f32 
             else f32.max (f32.cos a.low) (f32.cos a.high) }

  def exp (a : t) = 
    { low  = f32.exp a.low,
      high = f32.exp a.high }

  def sqrt (a : t) = 
    { low  = f32.sqrt a.low,
      high = f32.sqrt a.high }
    |> remove_nans

  def neg (a : t) = 
    { low  = - a.high,
      high = - a.low }
    
  def add (a : t) (b : t) =
    { low  = a.low + b.low,
      high = a.high + b.high }
    |> remove_nans

  def sub (a : t) (b : t) = 
    add a (neg b)
  
  -- We can create NANs if multiplying 0 with infinity.
  def mul (a : t) (b : t) =
    { low  = f32_min4 (a.low * b.low) (a.low * b.high) (a.high * b.low) (a.high * b.high), 
      high = f32_max4 (a.low * b.low) (a.low * b.high) (a.high * b.low) (a.high * b.high) }
  |> remove_nans 

  def inv (a : t) =
    if a.low <= 0.0 && a.high >= 0.0 
    then { low = -f32.inf, high = f32.inf }
    else { low = 1.0 / a.high, high = 1.0 / a.low }

  def div (a : t) (b : t) = 
    mul a (inv b)

  def min (a : t) (b : t) =
    { low  = f32.min a.low b.low,
      high = f32.min a.high b.high }

  def max (a : t) (b : t) =
    { low  = f32.max a.low b.low,
      high = f32.max a.high b.high }

  def copy = id
}


type tape_instr = { op : u8, out_slot : u8, in_slotA : u8, in_slotB : u8 }

type~ tape = { 
  instrs : []tape_instr, 
  constants : []f32,
  slot_count : i64  
}

def decode_instruction (i : u32) : tape_instr =
  let op       = u8.u32 ((i >> 24) & 0xFF)
  let out_slot = u8.u32 ((i >> 16) & 0xFF)
  let in_slotA = u8.u32 ((i >> 8)  & 0xFF)
  let in_slotB = u8.u32 ((i >> 0)  & 0xFF)
  in { op, out_slot, in_slotA, in_slotB }

-- The module V used for values can be scalars, intervals or gradients. 
module mk_tape_evaluator (V : value) = {
  def OP_CONST = 0u8
  def OP_SIN = 1u8
  def OP_COS = 2u8
  def OP_EXP = 3u8
  def OP_SQRT = 4u8
  def OP_NEG = 5u8
  def OP_ADD = 6u8
  def OP_SUB = 7u8
  def OP_MUL = 8u8
  def OP_DIV = 9u8
  def OP_MIN = 10u8
  def OP_MAX = 11u8
  def OP_COPY = 12u8

  -- Sequentially evaluate a tape given values for the axes.
  def eval (tap : tape) (x : V.t) (y : V.t) (z : V.t) (t : V.t) : V.t =
    let slots = replicate tap.slot_count (V.constant 0.0) 
      with [0] = x
      with [1] = y
      with [2] = z
      with [3] = t
    in 
    let slots = loop slots = slots for instr in tap.instrs do 
      let iA = i8.u8 instr.in_slotA 
      let iB = i8.u8 instr.in_slotB 
      let iO = i8.u8 instr.out_slot
      in slots with [iO] =
        if instr.op == OP_CONST then V.constant tap.constants[iA]
        else if instr.op == OP_SIN then V.sin slots[iA]
        else if instr.op == OP_COS then V.cos slots[iA]
        else if instr.op == OP_EXP then V.exp slots[iA]
        else if instr.op == OP_SQRT then V.sqrt slots[iA]
        else if instr.op == OP_NEG then V.neg slots[iA]
        else if instr.op == OP_ADD then V.add slots[iA] slots[iB]
        else if instr.op == OP_SUB then V.sub slots[iA] slots[iB]
        else if instr.op == OP_MUL then V.mul slots[iA] slots[iB]
        else if instr.op == OP_DIV then V.div slots[iA] slots[iB]
        else if instr.op == OP_MIN then V.min slots[iA] slots[iB]
        else if instr.op == OP_MAX then V.max slots[iA] slots[iB]
        else if instr.op == OP_COPY then V.copy slots[iA]
        else V.copy slots[iO]
    -- The output is always in slot 0
    in slots[0]
}

module scalar_tape_evaluator = mk_tape_evaluator scalar
module gradient_tape_evaluator = mk_tape_evaluator gradient 
module interval_tape_evaluator = mk_tape_evaluator interval
