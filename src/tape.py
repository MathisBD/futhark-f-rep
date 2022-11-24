import csg


# The set of tape operators is not exactly the same as the set of csg operators
# Nullary operator (no input)
OP_CONST = 0
# Unary operator
OP_SIN = 1
OP_COS = 2
OP_EXP = 3
OP_SQRT = 4
OP_NEG = 5
# Binary operators
OP_ADD = 6
OP_SUB = 7
OP_MUL = 8
OP_DIV = 9
OP_MIN = 10
OP_MAX = 11
OP_COPY = 12

def op_to_string(op):
    if   op == OP_CONST: return "CONST"
    elif op == OP_SIN: return "SIN"
    elif op == OP_COS: return "COS"
    elif op == OP_EXP: return "EXP"
    elif op == OP_SQRT: return "SQRT"
    elif op == OP_NEG: return "NEG"
    elif op == OP_ADD: return "ADD"
    elif op == OP_SUB: return "SUB"
    elif op == OP_MUL: return "MUL"
    elif op == OP_DIV: return "DIV"
    elif op == OP_MIN: return "MIN"
    elif op == OP_MAX: return "MAX"
    elif op == OP_COPY: return "COPY"
    else: assert(False)

# Each instruction is incoded in a 32-bits unsigned integer. 
def encode_instruction(op, out_slot, in_slotA, in_slotB):
    assert(type(op) == int and 0 <= op < 256)
    assert(type(out_slot) == int and 0 <= out_slot < 256)
    assert(type(in_slotA) == int and 0 <= in_slotA < 256)
    assert(type(in_slotB) == int and 0 <= in_slotB < 256)
    return (op << 24) | (out_slot << 16) | (in_slotA << 8)  | (in_slotB << 0)

def decode_instruction(instr):
    op       = (instr >> 24) & 0xFF
    out_slot = (instr >> 16) & 0xFF
    in_slotA = (instr >> 8)  & 0xFF
    in_slotB = (instr >> 0)  & 0xFF
    return op, out_slot, in_slotA, in_slotB
    
# This function only works if [op] corresponds to a tape instruction.
# For instance axis operators don't.
def tape_op_from_csg_op(op):
    assert(not csg.is_axis_op(op))
    if   op == csg.OP_CONST: return OP_CONST
    elif op == csg.OP_SIN: return OP_SIN
    elif op == csg.OP_COS: return OP_COS
    elif op == csg.OP_EXP: return OP_EXP
    elif op == csg.OP_SQRT: return OP_SQRT
    elif op == csg.OP_NEG: return OP_NEG
    elif op == csg.OP_ADD: return OP_ADD
    elif op == csg.OP_SUB: return OP_SUB
    elif op == csg.OP_MUL: return OP_MUL
    elif op == csg.OP_DIV: return OP_DIV
    elif op == csg.OP_MIN: return OP_MIN
    elif op == csg.OP_MAX: return OP_MAX
    else: assert(False)

class Tape:
    # Build a tape from a CSG expression
    def __init__(self, expr):
        # Make sure there is at most one copy of each axis node.
        expr = csg.merge_axes(expr)

        # Do a topological sort of the CSG expression :
        # each node appears after its inputs in the list
        self.nodes = []
        expr.topo_iter(lambda e: self.nodes.append(e))

        # Calculate the index in the sort of each node
        self.node_idx = dict()
        for i, node in enumerate(self.nodes):
            self.node_idx[node] = i

        # Build the constant pool
        self.constant_pool = []
        self.constant_idx = dict()
        for node in self.nodes:
            if node.op == csg.OP_CONST and node.constant not in self.constant_idx.keys():
                self.constant_idx[node.constant] = len(self.constant_pool)
                self.constant_pool.append(node.constant)

        # Build the instructions
        self.instructions = []
        self.slots = []
        self.build_instructions()

    def get_curr_slot(self, node):
        for i in range(len(self.slots)):
            if node == self.slots[i]:
                return i
        assert(False)

    def get_free_slot(self):
        self.slots.append(None)
        return len(self.slots) - 1

    def build_instructions(self):
        # Get the axis nodes
        x, y, z, t = None, None, None, None
        for node in self.nodes:
            if node.op == csg.OP_X: x = node
            if node.op == csg.OP_Y: y = node
            if node.op == csg.OP_Z: z = node
            if node.op == csg.OP_T: t = node

        # Initially, the axis nodes occupy the first slots
        self.slots += [x, y, z, t]

        # Process each instruction in topological order
        for node in self.nodes:
            if csg.is_axis_op(node.op): continue
            
            # Compute the input slots of the node
            in_slotA = 0
            in_slotB = 0
            if node.op == csg.OP_CONST:
                in_slotA = self.constant_idx[node.constant]
            elif csg.op_arity(node.op) == 1:
                in_slotA = self.get_curr_slot(node[0])
            elif csg.op_arity(node.op) == 2:
                in_slotA = self.get_curr_slot(node[0])
                in_slotB = self.get_curr_slot(node[1])
            else: 
                assert(False)
            
            # Get a slot for the output
            out_slot = self.get_free_slot()
            self.slots[out_slot] = node

            # Encode the instruction
            instr = encode_instruction(tape_op_from_csg_op(node.op), out_slot, in_slotA, in_slotB)
            self.instructions.append(instr)

        # Change the last instruction to output into slot 0
        op, _, in_slotA, in_slotB = decode_instruction(self.instructions[-1])
        self.instructions[-1] = encode_instruction(op, 0, in_slotA, in_slotB)

    def to_string(self, detailed = False):
        str = "[+] Tape: instr_count=%u slot_count=%u\n" % (len(self.instructions), len(self.slots))
        if detailed:
            for i, instr in enumerate(self.instructions):
                op, out_slot, in_slotA, in_slotB = decode_instruction(instr)
                str += "\t%2u %10s  out=%2u  inA=%2u  inB=%2u\n" % \
                    (i, op_to_string(op), out_slot, in_slotA, in_slotB)
        str += "[+] Constant pool: size=%u\n" % len(self.constant_pool)
        if detailed:
            for i, const in enumerate(self.constant_pool):
                str += "\t%2u %4.2f\n" % (i, const)
        return str