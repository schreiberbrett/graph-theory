from dataclasses import dataclass
from typing import List, Optional, Tuple, Set, TypeVar, Dict

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
def odd_paths(graph: List[List[int]], start: int, end: int) -> List[List[int]]:
	paths = []
	fringe = [(start, [])]

	while len(fringe) > 0:
		vertex, path = fringe.pop()
		if (len(path) % 2) == 1 and vertex == end:
			paths.append(path)
			continue

		for neighbor in graph[vertex]:
			if neighbor not in path:
				fringe.append((neighbor, path + [neighbor]))

	return paths
	
def odd_cycles(num_vertices: int, graph: List[List[int]]) -> Hypergraph:
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
	result = []
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
    result = []
    
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
def proves(h: Hypergraph, graph: List[List[bool]]) -> Optional[List[ProofStep]]:
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
							nonoverlapping = [x
								for x in h
								if disjoint(x, h[i]) and disjoint(x, h[j])
							]
							
							
							# new_hyperedge = (h[i] \/ h[j]) - (l \/ r)
							new_hyperedge = union(h[i], h[j])

							for v in l:
								new_hyperedge[v] = 'o'
							
							for v in r:
								new_hyperedge[v] = 'o'
								
							new_graph = [[False] * num_vertices] * num_vertices
							
							for i in range(num_vertices):
								for j in range(num_vertices):
									if i in l or i in r or j in l or j in r:
										new_graph[i][j] = False
									else:
										new_graph[i][j] = graph[i][j]

							if proves(remove2add1(h, i, j, new_hyperedge) + new_hyperedge, new_graph):
								return True
	return False
def unique(x: List[List[bool]]) -> List[List[bool]]:
	found = set()
	result = []
	
	for bool_list in x:
		representation = ''.join(['1' if bit else '0' for bit in bool_list])
		if representation not in found:
			found.add(representation)
			result.append(bool_list)
			
	return result
def decode(g6: str) -> Tuple[List[List[int]], int]:
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
	
def union(a: Hyperedge, b: Hyperedge) -> bool:
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
def to_adjacency_matrix(num_vertices: int, int_graph: Dict[int, List[int]]) -> List[List[bool]]:
	result = []

	for vertex in int_graph:
		adj_list = [False] * num_vertices
		for neighbor in int_graph[vertex]:
			adj_list[neighbor] = True
		
		result.append(adj_list)
		
	return result
			

def run(graph6: str) -> bool:
	int_graph, num_vertices = decode(graph6)
	
	h = sperner_family(odd_cycles(num_vertices, int_graph))

	return proves(h, to_adjacency_matrix(num_vertices, int_graph))


print(run('JHO\MageEG?')) # Grotzsch graph

