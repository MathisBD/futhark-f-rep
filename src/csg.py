import graphviz


# Nullary operators (no inputs)
OP_X = 0
OP_Y = 1
OP_Z = 2
OP_T = 3
OP_CONST = 4 # the node wraps a single float value
# Unary operators
OP_SIN = 5
OP_COS = 6
OP_EXP = 7
OP_SQRT = 8
OP_NEG = 9
# Binary operators
OP_ADD = 10
OP_SUB = 11
OP_MUL = 12
OP_DIV = 13
OP_MIN = 14
OP_MAX = 15

# The number of inputs an operator is supposed to have
def op_arity(op):
    if   op == OP_X: return 0
    elif op == OP_Y: return 0
    elif op == OP_Z: return 0
    elif op == OP_T: return 0
    elif op == OP_CONST: return 0
    elif op == OP_SIN: return 1
    elif op == OP_COS: return 1
    elif op == OP_EXP: return 1
    elif op == OP_SQRT: return 1
    elif op == OP_NEG: return 1
    elif op == OP_ADD: return 2
    elif op == OP_SUB: return 2
    elif op == OP_MUL: return 2
    elif op == OP_DIV: return 2
    elif op == OP_MIN: return 2
    elif op == OP_MAX: return 2
    else: assert(False)            

def is_axis_op(op):
    return op in [OP_X, OP_Y, OP_Z, OP_T]

def is_input_op(op):
    return op_arity(op) > 0

def op_to_string(op):
    if   op == OP_X: return "X"
    elif op == OP_Y: return "Y"
    elif op == OP_Z: return "Z"
    elif op == OP_T: return "T"
    elif op == OP_CONST: return "CONST"
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
    else: assert(False)            

class Node:
    # Create an axis node (X, Y, Z or T)
    @classmethod
    def axis(cls, op):
        assert(is_axis_op(op))
        node = cls()
        node.op = op
        return node 

    # Create a CONST node        
    @classmethod
    def constant(cls, const):
        node = cls()
        node.op = OP_CONST
        node.constant = float(const)
        return node

    # Create a node that has inputs
    @classmethod
    def input(cls, op, inputs):
        assert(is_input_op(op))
        assert(op_arity(op) == len(inputs))
        node = cls()
        node.op = op
        node.inputs = inputs
        return node

    # We overload standard math operators
    def __add__(self, other):
        return Node.input(OP_ADD, [self, other])
    def __sub__(self, other):
        return Node.input(OP_SUB, [self, other])
    def __mul__(self, other):
        return Node.input(OP_MUL, [self, other])
    def __truediv__(self, other):
        return Node.input(OP_DIV, [self, other])
    
    # This calls the function f on each node of the DAG rooted at self.
    def topo_iter(self, f):
        visited = set()
        
        def dfs(node):
            if node in visited:
                return
            visited.add(node)
            if is_input_op(node.op):
                for i in node.inputs:
                    dfs(i)
            f(node)

        dfs(self)

    # For every node n in the DAG rooted at self, 
    # replace n with f(n, [a_1...a_k]) where a_i is the result of f called on the i-th input of n.
    def topo_map(self, f):
        result = dict()

        def step(node):
            if is_input_op(node.op):
                args = [result[i] for i in node.inputs]
            else:
                args = []
            result[node] = f(node, args)

        self.topo_iter(step)
        return result[self]

    # Count the nodes in the DAG rooted at this node.
    def node_count(self):
        count = 0
        def step(_):
            nonlocal count 
            count += 1
        self.topo_iter(step)
        return count

    # Build a graphviz.Digraph that represents the csg DAG rooted at self.
    def to_dot_graph(self, label_edges = False):
        graph = graphviz.Digraph('CSG-DAG with %d nodes' % self.node_count())

        id = 0
        def step(node, input_ids):
            nonlocal id
            id += 1
            # Add the node
            if node.op == OP_CONST:
                label = "%.1f" % node.constant
            else:
                label = op_to_string(node.op)
            graph.node(str(id), label)
            # Add the edges to each input
            for i, inp in enumerate(input_ids):
                if label_edges and len(input_ids) > 1:
                    graph.edge(str(id), str(inp), str(i))
                else:
                    graph.edge(str(id), str(inp))
            return id
            
        self.topo_map(step)
        return graph


# Helper functions to build nodes
def X(): return Node.axis(OP_X)
def Y(): return Node.axis(OP_Y)
def Z(): return Node.axis(OP_Z)
def T(): return Node.axis(OP_T)
def const(c): return Node.constant(c)
def sin(node): return Node.input(OP_SIN, [node])
def cos(node): return Node.input(OP_COS, [node])
def exp(node): return Node.input(OP_EXP, [node])
def sqrt(node): return Node.input(OP_SQRT, [node])
def neg(node): return Node.input(OP_NEG, [node])
def add(node1, node2): return Node.input(OP_ADD, [node1, node2])
def sub(node1, node2): return Node.input(OP_SUB, [node1, node2])
def mul(node1, node2): return Node.input(OP_MUL, [node1, node2])
def div(node1, node2): return Node.input(OP_DIV, [node1, node2])
def min(node1, node2): return Node.input(OP_MIN, [node1, node2])
def max(node1, node2): return Node.input(OP_MAX, [node1, node2])
