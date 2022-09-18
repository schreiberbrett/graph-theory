from tqdm import tqdm
from dataclasses import dataclass
from typing import List, Optional, Tuple, Set, TypeVar, Dict, Any

T = TypeVar('T')
def unriffle(o: List[T]) -> List[Tuple[List[T], List[T]]]:
	if len(o) == 0:
		return [([], [])]
		
	car_o, cdr_o = o[0], o[1:]
	
	return [
		(l, r)
		for cdr_l, cdr_r in unriffle(cdr_o)
		for l, r in [
			([car_o] + cdr_l, cdr_r),
			(cdr_l, [car_o] + cdr_r)
		]
	]
Biclique = List[str] # List[Literal['l', 'r', 'o']]
Hyperedge = List[str] # List[Literal['x', 'o']]
ProofStep = List[str] # List[Literal['a', 'b', 'u', 'v', 'w', 'o']]
Hypergraph = List[Hyperedge]
def odd_paths(graph: Dict[int, List[int]], start: int, end: int) -> List[List[int]]:
	paths: List[List[int]] = []
	fringe: List[Tuple[int, List[int]]] = [(start, [])]

	while len(fringe) > 0:
		vertex, path = fringe.pop()
		if (len(path) % 2) == 1 and vertex == end:
			paths.append(path)
			continue

		for neighbor in graph[vertex]:
			if neighbor not in path:
				fringe.append((neighbor, path + [neighbor]))

	return paths

def to_ints(e: Hyperedge) -> List[int]:
	result: List[int] = []
	for i in range(len(e)):
		if e[i] == 'x':
			result.append(i)
			
	return result
		

def odd_cycles(graph: Dict[int, List[int]]) -> Hypergraph:
	num_vertices = len(graph)

	result: Hypergraph = []
	seen: Set[str] = set()

	for vertex in graph:
		for	odd_cycle in odd_paths(graph, vertex, vertex):
			hyperedge: Hyperedge = ['o'] * num_vertices
			
			for v in odd_cycle:
				hyperedge[v] = 'x'
				
			hyperedge_str = ''.join(hyperedge)
			if hyperedge_str not in seen:
				seen.add(hyperedge_str)
				result.append(hyperedge)

	return result
def all_bicliques(graph: List[List[bool]]) -> List[Biclique]:
	num_vertices = len(graph)
	result: List[Biclique] = []
	ls_and_os_and_rs = unriffle([x for x in range(num_vertices)])
	
	for ls_and_os, rs in unriffle([x for x in range(num_vertices)]):
		for ls, os, in unriffle(ls_and_os):
			if (
				len(ls) != 0 and
				len(rs) != 0 and
				is_biclique(l, r, graph)
			):
				biclique = ['o'] * num_vertices
				
				for l in ls:
					biclique[l] = 'l'
				
				for r in rs:
					biclique[r] = 'r'
				
				result.append(biclique)
	
	return result
def any_overlap(a: Biclique, b: Biclique) -> bool:
	num_vertices = len(a[0])
    
	for v in range(num_vertices):
		if (
			(a[v] == 'l' and b[v] == 'l') or
			(a[v] == 'l' and b[v] == 'r') or
			(a[v] == 'r' and b[v] == 'l') or
			(a[v] == 'r' and b[v] == 'r')
		):
			return True

	return False
def any_edge_has_less_than_three(h: Hypergraph) -> bool:
    for hyperedge in h:
        edge_size = 0
        for vertex in hyperedge:
            if vertex == 'x':
                edge_size += 1

        if edge_size < 3:
            return True
            
    return False
def attempt_proof_step(
    a: Hyperedge,
    b: Hyperedge,
    biclique: Biclique
) -> Optional[Tuple[ProofStep, Hyperedge]]:

    num_vertices = len(a)
    proof_step: ProofStep = []
    new_h: Hyperedge = []
    
    for v in range(num_vertices):
        x = a[v] + b[v] + biclique[v]

        if x == 'xol':
            proof_step.append('a')
            new_h.append('o')

        elif x == 'oxr':
            proof_step.append('b')
            new_h.append('o')
                
        elif x == 'xoo':
            proof_step.append('u')
            new_h.append('x')
                
        elif x == 'oxo':
            proof_step.append('v')
            new_h.append('x')
                
        elif x == 'xxo':
            proof_step.append('w')
            new_h.append('x')
                
        elif x == 'ooo':
            proof_step.append('o')
            new_h.append('o')
                
        else:
            return None
            
    return (proof_step, new_h)
def remove2add1(h: Hypergraph, i: int, j: int, new_h: Hyperedge) -> Hypergraph:
    result: Hypergraph = []
    
    for index in range(len(h)):
        if index != i and index != j:
            result.append(h[index])
            
    result.append(new_h)
    return result
def venn_diagram(a: Hyperedge, b: Hyperedge) -> Tuple[List[int], List[int], List[int], List[int]]:
	left, right, both, neither = [], [], [], []
	
	num_vertices = len(a)
	
	for i in range(num_vertices):
		if a[i] == 'x' and b[i] == 'o':
			left.append(i)
			
		if a[i] == 'o' and b[i] == 'x':
			right.append(i)
	
		if a[i] == 'x' and b[i] == 'x':
			both.append(i)
			
		if a[i] == 'o' and b[i] == 'o':
			neither.append(i)
			
	return left, right, both, neither
def is_biclique(us: List[int], vs: List[int], graph: List[List[bool]]) -> bool:
	for u in us:
		for v in vs:
			if not graph[u][v]:
				return False

	return True
Proof = List[Tuple[Hyperedge, Hyperedge, Hyperedge]]

def proves(h: Hypergraph, graph: List[List[bool]]) -> Optional[Proof]:
	if len(h) == 0:
		return None

	num_vertices = len(h[0])

	if any_edge_has_less_than_three(h):
		return []
		
	

	# Any biclique between two hyperedges leads to a new proveable hypergraph
	for i in range(len(h)):
		for j in range(len(h)):
			if i != j:
				left, right, both, neither = venn_diagram(h[i], h[j])

				ls = unriffle(left)
				rs = unriffle(right)

				for l, _ in ls:
					for r, _ in rs:
						if (
							len(l) != 0 and
							len(r) != 0 and
							is_biclique(l, r, graph)
						):							
							
							# new_hyperedge = (h[i] \/ h[j]) - (l \/ r)
							new_hyperedge = union(h[i], h[j])

							for v in l:
								new_hyperedge[v] = 'o'
							
							for v in r:
								new_hyperedge[v] = 'o'

							# The new_graph is a copy of the graph except...
							new_graph = [
								[graph[i][j] for j in range(len(graph[i]))]
								for i in range(len(graph))
							]

							# ...it discards any edges incident to the biclique
							for i_ in range(num_vertices):
								for j_ in range(num_vertices):
									if (i_ in l) or (i_ in r) or (j_ in l) or (j_ in r):
										new_graph[i_][j_] = False
							
							
							new_h = []
							for index in range(len(h)):
								e = h[index]
								if not (is_subset(new_hyperedge, e) or index == i or index == j):
									new_h.append(e)
								
							new_h.append(new_hyperedge)
							
							result = proves(new_h, new_graph)
							
							if result is not None:
								return [(h[i], h[j], new_hyperedge)] + result

	return None
	
def print_proof(proof: Optional[Proof]) -> None:
	if proof is None:
		print('No proof')

	for l, r, o in proof:
		print('l:', to_ints(l))
		print('r:', to_ints(r))
		print('o:', to_ints(o))
		print('---')

def unique(x: List[List[bool]]) -> List[List[bool]]:
	found: Set[str] = set()
	result = []
	
	for bool_list in x:
		representation = ''.join(['1' if bit else '0' for bit in bool_list])
		if representation not in found:
			found.add(representation)
			result.append(bool_list)
			
	return result
def decode(g6: str) -> Tuple[Dict[int, List[int]], int]:
	def as_int(char):
		return ord(char) - 63

	num_vertices = as_int(g6[0])
	
	bits = [
		x == '1'
		for c in g6[1:]
		for x in format(as_int(c), '6b')
	]

	graph = {x: [] for x in range(num_vertices)}

	index = 0
	for i in range(num_vertices):
		for j in range(i):
			if bits[index]:
				graph[i].append(j)
				graph[j].append(i)
			
			index += 1
			
	return graph, num_vertices

# TODO
def encode(int_graph: List[List[int]], num_vertices: int) -> str:
	def to_char(i):
		return chr(i + 63)
		
	index = 0
	for i in range(num_vertices):
		for j in range(i):
			if j in int_graph[i]:
				bits[index] = True
			else:
				bits[index] = False
				
			index += 1
def sperner_family(hypergraph: Hypergraph) -> Hypergraph:
    result = []
    for i in range(len(hypergraph)):
        any_subset = False
        for j in range(len(hypergraph)):
            if i != j and is_subset(hypergraph[j], hypergraph[i]):
                any_subset = True
                break

        if not any_subset:
            result.append(hypergraph[i])
            
    return result
def is_subset(subset: Hyperedge, superset: Hyperedge) -> bool:
    num_vertices = len(subset)
    
    for i in range(num_vertices):
        if subset[i] == 'x' and superset[i] == 'o':
            return False
            
    return True
    
def disjoint(a: Hyperedge, b: Hyperedge) -> bool:
	num_vertices = len(a)
	
	for i in range(num_vertices):
		if a[i] == 'x' and b[i] == 'x':
			return False
			
	return True
	
def union(a: Hyperedge, b: Hyperedge) -> Hyperedge:
	num_vertices = len(a)
	
	result = []
	for i in range(num_vertices):
		if a[i] == 'x' or b[i] == 'x':
			result.append('x')
		else:
			result.append('o')
			
	return result
k4_hypergraph: Hypergraph = [
    ['x', 'x', 'x', 'o'],
    ['o', 'x', 'x', 'x']]
    
k4_bicliques: List[Biclique] = [
    ['l', 'o', 'o', 'r']]


abc_hypergraph: Hypergraph = [
    ['x', 'o', 'x', 'x', 'o', 'o', 'o', 'o', 'o'],
    ['o', 'x', 'o', 'o', 'x', 'x', 'o', 'o', 'o'],
    ['o', 'o', 'o', 'o', 'o', 'o', 'x', 'x', 'x']]
    
abc_bicliques: List[Biclique] = [
    ['l', 'r', 'o', 'o', 'o', 'o', 'o', 'o', 'o'],
    ['o', 'o', 'o', 'o', 'o', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'o', 'l', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'o', 'l', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'l', 'o', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'l', 'o', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'l', 'l', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'o', 'l', 'l', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'o', 'o', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'o', 'l', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'o', 'l', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'l', 'o', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'l', 'o', 'l', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'l', 'l', 'o', 'r', 'o', 'o'],
    ['o', 'o', 'l', 'l', 'l', 'l', 'r', 'o', 'o']]
def to_adjacency_matrix(int_graph: Dict[int, List[int]]) -> List[List[bool]]:
	num_vertices = len(int_graph)
	result = []

	for vertex in int_graph:
		adj_list = [False] * num_vertices
		for neighbor in int_graph[vertex]:
			adj_list[neighbor] = True
		
		result.append(adj_list)
		
	return result

def to_int_graph(adj_matrix: List[List[bool]]) -> Dict[int, List[int]]:
	num_vertices = len(adj_matrix[0])
	
	return {v : [x for x in range(num_vertices) if adj_matrix[v][x]] for v in range(num_vertices)}

def three_color(int_graph: Dict[int, List[int]]) -> Optional[List[str]]:
	return three_color_rec(int_graph, [])

def three_color_rec(int_graph: Dict[int, List[int]], so_far: List[int]) -> Optional[List[str]]:
	num_colored_vertices = len(so_far)
	
	if num_colored_vertices == len(int_graph):
		return so_far
	
	v = num_colored_vertices
	
	neighbor_has_1 = False
	neighbor_has_2 = False
	neighbor_has_3 = False

	for neighbor in int_graph[v]:
		if neighbor < v: # if neighbor has been colored
			if so_far[neighbor] == 1:
				neighbor_has_1 = True
				
			if so_far[neighbor] == 2:
				neighbor_has_2 = True
				
			if so_far[neighbor] == 3:
				neighbor_has_3 = True

	if not neighbor_has_1:
		attempt = three_color_rec(int_graph, so_far + [1])
		if attempt is not None:
			return attempt
			
	if not neighbor_has_2:
		attempt = three_color_rec(int_graph, so_far + [2])
		if attempt is not None:
			return attempt
			
	if not neighbor_has_3:
		attempt = three_color_rec(int_graph, so_far + [3])
		if attempt is not None:
			return attempt
			
	return None
		

def run_proves(known_4chrom_graph6s: List[str]) -> None:
	for graph6 in known_4chrom_graph6s:
		int_graph, num_vertices = decode(graph6)
		
		odd_cycle_hypergraph = sperner_family(odd_cycles(int_graph))

		graph = to_adjacency_matrix(int_graph)
		
		proves_result = proves(odd_cycle_hypergraph, graph)
		
		if proves_result is None:
			print('Found an exception')
			print(graph6)
			break


def run_both(graph6s: List[str]) -> None:
	for graph6 in tqdm(graph6s):
		int_graph, num_vertices = decode(graph6)
		
		three_color_result = three_color(int_graph)
		
		is_three_colorable = three_color_result is not None
		
		odd_cycle_hypergraph = sperner_family(odd_cycles(int_graph))

		graph = to_adjacency_matrix(int_graph)
		
		proves_result = proves(odd_cycle_hypergraph, graph)
		
		is_not_three_colorable = proves_result is not None
		
		if is_three_colorable and is_not_three_colorable:
			print('Both true')
			print(graph6)
			for step in proves_result:
				print(step)
			break
			
		elif not is_three_colorable and not is_not_three_colorable:
			print('Both false')
			print(graph6)
			break


moser_spindle: Dict[int, List[int]] = {
	0: [1, 2, 6],
	1: [0, 2, 3],
	2: [0, 1, 3],
	3: [1, 2, 4, 5],
	4: [3, 5, 6],
	5: [3, 4, 6],
	6: [0, 4, 5]
}

triple_moser_spindle: Dict[int, List[int]] = {
	0: [1, 2, 9],
	1: [0, 2, 3],
	2: [0, 1, 3],
	3: [1, 2, 4, 5],
	4: [3, 5, 6],
	5: [3, 4, 6],
	6: [4, 5, 7, 8],
	7: [6, 8, 9],
	8: [6, 7, 9],
	9: [0, 7, 8]
}

quad_moser_spindle: Dict[int, List[int]] = {
	0: [1, 2, 12],
	1: [0, 2, 3],
	2: [0, 1, 3],
	3: [1, 2, 4, 5],
	4: [3, 5, 6],
	5: [3, 4, 6],
	6: [4, 5, 7, 8],
	7: [6, 8, 9],
	8: [6, 7, 9],
	9: [7, 8, 10, 11],
	10: [9, 11, 12],
	11: [9, 10, 12],
	12: [0, 10, 11]
}

import sys
import time


odd_cycle_hypergraph = sperner_family(odd_cycles(triple_moser_spindle))

print('Odd cycles:')
for edge in odd_cycle_hypergraph:
	print(to_ints(edge))
print()

graph = to_adjacency_matrix(triple_moser_spindle)

proof = proves(odd_cycle_hypergraph, graph)

print_proof(proof)

for i in range(5):
	sys.stdout.write('\a')
	sys.stdout.flush()
	time.sleep(1)

# gs = []
# with open('src/tf_graphs_15v_4chrom-crit.g6') as file:
# 	for graph6 in file:
# 		gs.append(graph6)

# run_proves(['JHO\MageEG?'])
Expr = Union[Val[T], Op[T]]

@dataclass(frozen=True)
class Val(Generic[T]):
	val: T
	
@dataclass(frozen=True)
class Op(Generic[T]):
	l: Expr[T]
	r: Expr[T]
	

def solve(q: Set[T], op: Callable[[T, T], Set[T]], y: T) -> Optional[Expr[T]]:
	if y in q:
		return Val(y)
		
	for x in q:
		
	pass


