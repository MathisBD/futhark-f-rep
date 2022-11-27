import "utils"

--
--module type value = {
--  type t 
--  val make : f32 -> f32 -> t
--  val sin : t -> t
--  val cos : t -> t
--  val exp : t -> t
--  val sqrt : t -> t
--  val neg : t -> t
--  val add : t -> t -> t
--  val sub : t -> t -> t
--  val mul : t -> t -> t
--  val div : t -> t -> t
--  val min : t -> t -> t 
--  val max : t -> t -> t
--}
--
--module scalar : value = {
--  type t = f32
--}
--
--module gradient : value = {
--  type t = { f : f32, dx : f32, dy : f32, dz : f32 }
--}
--
--module interval : value = {
--  type t = { low : f32, high : f32 }
--}
--
--

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

type tape_instr = { op : u8, out_slot : u8, in_slotA : u8, in_slotB : u8 }

type~ tape = { 
  instrs : []tape_instr, 
  constants : []f32,
  slot_count : i64  
}

def decode_instruction (instr : u32) : tape_instr =
  let op       = u8.u32 ((instr >> 24) & 0xFF)
  let out_slot = u8.u32 ((instr >> 16) & 0xFF)
  let in_slotA = u8.u32 ((instr >> 8)  & 0xFF)
  let in_slotB = u8.u32 ((instr >> 0)  & 0xFF)
  in { op, out_slot, in_slotA, in_slotB }


-- Sequentially evaluate a tape given values for the axes.
def tape_eval (tap : tape) x y z t : f32 =
  let slots = replicate tap.slot_count 0f32 
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
      if instr.op == OP_CONST then tap.constants[iA]
      else if instr.op == OP_SIN then f32.sin slots[iA]
      else if instr.op == OP_COS then f32.cos slots[iA]
      else if instr.op == OP_EXP then f32.exp slots[iA]
      else if instr.op == OP_SQRT then f32.sqrt slots[iA]
      else if instr.op == OP_NEG then -slots[iA]
      else if instr.op == OP_ADD then slots[iA] + slots[iB]
      else if instr.op == OP_SUB then slots[iA] - slots[iB]
      else if instr.op == OP_MUL then slots[iA] * slots[iB]
      else if instr.op == OP_DIV then slots[iA] / slots[iB]
      else if instr.op == OP_MIN then f32.min slots[iA] slots[iB]
      else if instr.op == OP_MAX then f32.max slots[iA] slots[iB]
      else if instr.op == OP_COPY then slots[iA]
      else slots[iO]
  -- The output is always in slot 0
  in slots[0]


-- Sequentially evaluate a tape's gradient given values for the axes.
def tape_gradient (tap : tape) x y z t : vec3.vector =
  -- Each slot stores the values : (gradient wrt. x, gradient wrt. y, gradient wrt. z, actual value).
  let slots = replicate tap.slot_count (0.0, 0.0, 0.0, 0.0) 
    with [0] = (1, 0, 0, x)
    with [1] = (0, 1, 0, y)
    with [2] = (0, 0, 1, z)
    with [3] = (0, 0, 0, t)
  in 
  let slots = loop slots = slots for instr in tap.instrs do 
    let iA = i8.u8 instr.in_slotA 
    let iB = i8.u8 instr.in_slotB 
    let iO = i8.u8 instr.out_slot
    in slots with [iO] =
      if instr.op == OP_CONST then 
        (0, 0, 0, tap.constants[iA])
      else if instr.op == OP_SIN then 
        (slots[iA].0 * f32.cos slots[iA].3, 
         slots[iA].1 * f32.cos slots[iA].3, 
         slots[iA].2 * f32.cos slots[iA].3, 
         f32.sin slots[iA].3)
      else if instr.op == OP_COS then 
        (- slots[iA].0 * f32.sin slots[iA].3, 
         - slots[iA].1 * f32.sin slots[iA].3, 
         - slots[iA].2 * f32.sin slots[iA].3, 
         f32.cos slots[iA].3)
      else if instr.op == OP_EXP then 
        (slots[iA].0 * f32.exp slots[iA].3, 
         slots[iA].1 * f32.exp slots[iA].3, 
         slots[iA].2 * f32.exp slots[iA].3, 
         f32.exp slots[iA].3)
      else if instr.op == OP_SQRT then 
        (slots[iA].0 / (2 * f32.sqrt slots[iA].3), 
         slots[iA].1 / (2 * f32.sqrt slots[iA].3), 
         slots[iA].2 / (2 * f32.sqrt slots[iA].3), 
         f32.sqrt slots[iA].3)
      else if instr.op == OP_NEG then 
        (-slots[iA].0, 
         -slots[iA].1, 
         -slots[iA].2, 
         -slots[iA].3)
      else if instr.op == OP_ADD  then 
        (slots[iA].0 + slots[iB].0, 
         slots[iA].1 + slots[iB].1, 
         slots[iA].2 + slots[iB].2, 
         slots[iA].3 + slots[iB].3)
      else if instr.op == OP_SUB then
        (slots[iA].0 - slots[iB].0, 
         slots[iA].1 - slots[iB].1, 
         slots[iA].2 - slots[iB].2, 
         slots[iA].3 - slots[iB].3)
      else if instr.op == OP_MUL then
        (slots[iA].0 * slots[iB].3 + slots[iA].3 * slots[iB].0, 
         slots[iA].1 * slots[iB].3 + slots[iA].3 * slots[iB].1, 
         slots[iA].2 * slots[iB].3 + slots[iA].3 * slots[iB].2, 
         slots[iA].3 * slots[iB].3)
      else if instr.op == OP_DIV then
        ((slots[iA].0 * slots[iB].3 - slots[iA].3 * slots[iB].0) / (slots[iB].3 ** 2), 
         (slots[iA].1 * slots[iB].3 - slots[iA].3 * slots[iB].1) / (slots[iB].3 ** 2), 
         (slots[iA].2 * slots[iB].3 - slots[iA].3 * slots[iB].2) / (slots[iB].3 ** 2), 
         slots[iA].3 / slots[iB].3)
      else if instr.op == OP_MIN then
        (if slots[iA].3 < slots[iB].3 then slots[iA] else slots[iB])
      else if instr.op == OP_MAX then
        (if slots[iA].3 > slots[iB].3 then slots[iA] else slots[iB])  
      else if instr.op == OP_COPY then slots[iA]
      else slots[iO]
  -- The output is always in slot 0
  in { x = slots[0].0, y = slots[0].1, z = slots[0].2 }


--module interval :  = {
--  type t = { low : f32, high : f32 }
--
--  def make low high : interval = { low, high }
--  
--  def sin (i : interval) = 
--    { low  = -1.0,
--      high = 1.0 }
--    
--  def cos (i : interval) = 
--    { low  = -1.0,
--      high = 1.0 }
--
--}
--  
---- Multiply two intervals
--def interval_mult (a : interval) (b : interval) : interval =
--  { low  = f32_min4 (a.low * b.low) (a.low * b.high) (a.high * b.low) (a.high * b.high),
--    high = f32_max4 (a.low * b.low) (a.low * b.high) (a.high * b.low) (a.high * b.high) }
--
---- Sequentially evaluate a tape given intervals for its inputs,
---- using basic interval arithmetic.
--def tape_interval (tap : tape) (x : interval.t) (y : interval.t) (z : interval.t) (t : interval.t) : interval.t =
--  let slots = replicate tap.slot_count 
--    with [0] = x
--    with [1] = y
--    with [2] = z
--    with [3] = t
--  in 
--  let slots = loop slots = slots for instr in tap.instrs do 
--    let iA = i8.u8 instr.in_slotA 
--    let iB = i8.u8 instr.in_slotB 
--    let iO = i8.u8 instr.out_slot
--    in slots with [iO] =
--      if instr.op == OP_CONST then 
--        { low  = tap.constants[iA], 
--          high = tap.constants[iA] }
--      else if instr.op == OP_SIN then 
--        { low  = -1.0, 
--          high = 1.0 }
--      else if instr.op == OP_COS then 
--        { low  = -1.0, 
--          high = 1.0 }
--      else if instr.op == OP_EXP then 
--        { low  = f32.exp slots[iA].low, 
--          high = f32.exp slots[iA].high }
--      else if instr.op == OP_SQRT then 
--        -- We clamp the input interval to [0%, +inf[
--        { low  = f32.sqrt (f32.max 0.0 slots[iA].low), 
--          high = f32.sqrt (f32.max 0.0 slots[iA].high) }
--      else if instr.op == OP_NEG then 
--        { low  = - slots[iA].high, 
--          high = - slots[iA].low }
--      else if instr.op == OP_ADD  then 
--        { low  = slots[iA].low + slots[iB].low, 
--          high = slots[iA].high + slots[iB].high }
--      else if instr.op == OP_SUB then
--        { low  = slots[iA].low - slots[iB].high,
--          high = slots[iA].high - slots[iB].low }
--      else if instr.op == OP_MUL then
--        interval_mult slots[iA] slots[iB]
--      else if instr.op == OP_DIV then
--        
--      else if instr.op == OP_MIN then
--        { low  = f32.min slots[iA].low slots[iB].low,
--          high = f32.min slots[iA].high slots[iB].high }
--      else if instr.op == OP_MAX then
--        { low  = f32.max slots[iA].low slots[iB].low,
--          high = f32.max slots[iA].high slots[iB].high }
--      else if instr.op == OP_COPY then slots[iA]
--      else slots[iO]
--  -- The output is in slot 0.
--  in slots[0]