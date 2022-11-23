import graphviz


class CSG:
    # Nullary operators (no inputs)
    X = 0
    Y = 1
    Z = 2
    T = 3
    CONST = 4
    # Unary operators
    SIN = 5
    # Binary operators
    ADD = 10
    MUL = 11

    def is_axis_op(op):
        return op in [CSG.X, CSG.Y, CSG.Z, CSG.T]

    def is_input_op(op):
        return op in [CSG.SIN, CSG.ADD, CSG.MUL]

    def op_to_string(op):
        if op == CSG.X: return "X"
        elif op == CSG.Y: return "Y"
        elif op == CSG.Z: return "Z"
        elif op == CSG.T: return "T"
        elif op == CSG.CONST: return "CONST"
        elif op == CSG.SIN: return "SIN"
        elif op == CSG.ADD: return "ADD"
        elif op == CSG.MUL: return "MUL"
        else: assert(False)            

    class Node:
        def __init__(self, constant):
            self.op = CSG.CONST
            self.constant = constant

        def __init__(self, op, inputs):
            self.op = op
            self.inputs = inputs

        def __add__(self, other):
            return CSG.Node(CSG.ADD, [self, other])

        def __mul__(self, other):
            return CSG.Node(CSG.MUL, [self, other])

        def __sin__(self):
            return CSG.Node(CSG.SIN, [self])

        # This calls the function f on each node of the (acyclic) subgraph rooted at self.
        def topo_iter(self, f):
            visited = set()
            
            def dfs(node):
                if node in visited:
                    return
                visited.insert(node)
                if CSG.is_input_op(node.op):
                    map(dfs, node.inputs)
                f(node)

            dfs(self)

        # For every node n in the subgraph rooted at self, 
        # replace n with f(n, [a_1...a_k]) where a_i is the result of f called on the i-th input of n.
        def topo_map(self, f):
            result = dict()

            def step(node):
                if CSG.is_input_op(node.op):
                    args = [result[i] for i in node.inputs]
                else:
                    args = []
                result[node] = f(node, args)

            self.topo_iter(step)
            return result[self]

        def to_dot_graph(self, label_edges = False):
            graph = graphviz.Digraph('csg-tree')

            id = 0
            def step(node, input_ids):
                # Add the node
                if node.op == CSG.CONST:
                    label = "%.1f" % node.constant
                else:
                    label = CSG.op_to_string(node.op)
                graph.node(str(id), label)
                # Add the edges to each input
                for i in input_ids:
                    if label_edges:
                        graph.edge(str(id), str(i), str(i))
                    else:
                        graph.edge(str(id), str(i))
                id += 1
                return id-1
                
            self.topo_map(step)
            return graph


        