import math
from PIL import Image, ImageDraw, ImageFont
from .base import *
from .simplify import simplify
from .fraction import fraction
from .linear import linear_solve
from .inverse import inverse

# =====================================================================
# 1. CORE DATA STRUCTURES
# =====================================================================


class Junction:

    def __init__(self, name: str):
        self.name = name

    def __repr__(self):
        return f"Junction('{self.name}')"


class Resistor:

    def __init__(self, name: str, resistance: float):
        self.name = name
        self.resistance = resistance  # in Ohms

    def __repr__(self):
        return f"Resistor('{self.name}', {self.resistance} ohms)"


class DirectedWire:

    def __init__(
        self,
        source: Junction | Resistor,
        target: Junction | Resistor,
        current_name: str = "",
    ):
        self.source = source
        self.target = target
        self.current_name = current_name  # e.g., 'I1', 'I2'

    def __repr__(self):
        label = f" ({self.current_name})" if self.current_name else ""
        return f"Wire({self.source.name} -> {self.target.name}{label})"


class CircuitGraph:

    def __init__(self):
        self.adjacency_list = {}
        self.degree = {}
        self.current_counter = 1

    def add_vertex(self, vertex: Junction | Resistor):
        if vertex not in self.adjacency_list:
            self.adjacency_list[vertex] = []
            self.degree[vertex] = 0

    def connect(
        self,
        source: Junction | Resistor,
        target: Junction | Resistor,
        incoming_current: str = None,
    ):
        self.add_vertex(source)
        self.add_vertex(target)

        src_degree = self.degree[source] + 1

        if src_degree > 2 or not incoming_current:
            current_name = f"I{self.current_counter}"
            self.current_counter += 1
        else:
            current_name = incoming_current

        wire = DirectedWire(source, target, current_name)
        self.adjacency_list[source].append(wire)

        self.degree[source] += 1
        self.degree[target] += 1

        return wire, current_name

    def display(self):
        print("=== CIRCUIT GRAPH ===")
        for vertex, outgoing_wires in self.adjacency_list.items():
            deg = self.degree[vertex]
            node_type = "Principal Junction" if deg > 2 else "Series Node"
            connections = [
                f"--> {w.target.name} [Current: {w.current_name}]"
                for w in outgoing_wires
            ]
            print(
                f"{vertex} (Degree: {deg}, {node_type}) connections:"
                f" {connections}"
            )


# =====================================================================
# 2. EQUIVALENT RESISTANCE SOLVER FUNCTION
# =====================================================================


def solve_equivalent_resistance(
    circuit_graph: CircuitGraph, j1: Junction, j2: Junction
) -> TreeNode:
    """Calculates the equivalent resistance between two junctions in a CircuitGraph."""

    # 1. Variable Mapping
    symbolic_vars = []

    # Collect Nodal Potentials
    junction_nodes = [
        node
        for node in circuit_graph.adjacency_list.keys()
        if isinstance(node, Junction)
    ]
    for j in junction_nodes:
        symbolic_vars.append(f"V_{j.name}")

    # Collect Branch Currents & I_total
    current_vars = set()
    for src, wires in circuit_graph.adjacency_list.items():
        for w in wires:
            if w.current_name:
                current_vars.add(w.current_name)

    sorted_currents = sorted(
        list(current_vars),
        key=lambda x: int(x[1:]) if x[1:].isdigit() else x,
    )
    symbolic_vars.extend(sorted_currents)
    symbolic_vars.append("I_total")

    # Target variable: R_eq
    req_symbol = f"R_eq_{j1.name}_{j2.name}"
    symbolic_vars.append(req_symbol)

    # Map symbols to v_0, v_1, ...
    var_map = {orig: f"v_{i}" for i, orig in enumerate(symbolic_vars)}

    # AST Helper functions
    def make_var_node(symbol: str) -> TreeNode:
        return TreeNode(var_map[symbol])

    def make_const_node(val: int) -> TreeNode:
        return TreeNode(f"d_{val}")

    def build_addition_chain(node_list):
        if not node_list:
            return make_const_node(0)
        res = node_list[0]
        for n in node_list[1:]:
            res = res + n
        return res

    system_trees = []

    # 2. Build KCL Equation at J1
    outgoing_currents = [
        make_var_node(w.current_name)
        for w in circuit_graph.adjacency_list.get(j1, [])
    ]
    kcl_tree = TreeNode(
        "f_eq", [make_var_node("I_total"), build_addition_chain(outgoing_currents)]
    )
    system_trees.append(kcl_tree)

    # 3. Build Branch Nodal Equations: (V_J1 - V_J2) = I_k * R_k
    v_diff = make_var_node(f"V_{j1.name}") - make_var_node(f"V_{j2.name}")
    for wire in circuit_graph.adjacency_list.get(j1, []):
        if isinstance(wire.target, Resistor):
            res_node = wire.target
            i_var = make_var_node(wire.current_name)
            r_val = make_const_node(int(res_node.resistance))
            system_trees.append(TreeNode("f_eq", [v_diff, i_var * r_val]))

    # 4. Ground Reference: V_J2 = 0
    ground_eq = TreeNode(
        "f_eq", [make_var_node(f"V_{j2.name}"), make_const_node(0)]
    )
    system_trees.append(ground_eq)

    # 5. Definition of R_eq: R_eq = (V_J1 - V_J2) / I_total
    req_tree = TreeNode(
        "f_eq", [make_var_node(req_symbol), v_diff / make_var_node("I_total")]
    )
    system_trees.append(req_tree)

    # 6. Substitute Boundary Condition I_total = 1
    i_total_var_node = tree_form(var_map["I_total"])
    one_node = tree_form("d_1")
    system_trees = [
        replace(tree, i_total_var_node, one_node) for tree in system_trees
    ]

    # 7. Solve Linear System
    eq = simplify(fraction(simplify(operation("f_and", system_trees))))
    solved_system = linear_solve(eq)

    # 8. Extract Equation for R_eq
    out = None
    target_v_var = var_map[req_symbol]

    if solved_system.name == "f_and":
        for item in solved_system.children:
            if vlist(item) and vlist(item)[0] == target_v_var:
                out = inverse(item.children[0], target_v_var)
                break
    else:
        if vlist(solved_system) and vlist(solved_system)[0] == target_v_var:
            out = inverse(solved_system.children[0], target_v_var)

    return out
