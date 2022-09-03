from dataclasses import dataclass
from typing import List, Optional, Tuple, Set, TypeVar

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

	
	
for x in unriffle(['a', 'b', 'c']):
	print(x)
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
def proves(h: Hypergraph, bicliques: List[Biclique]) -> Optional[List[ProofStep]]:
    num_vertices = len(h[0])

    if any_edge_has_less_than_three(h):
        return []
    
    # Any biclique between two hyperedges leads to a new proveable hypergraph
    for i in range(len(h)):
        for j in range(len(h)):
            for biclique in bicliques:
                if i != j:
                    attempt = attempt_proof_step(h[i], h[j], biclique)

                    if attempt is not None:
                        proof_step, new_hyperedge = attempt

                        proof_steps = proves(
                            remove2add1(h, i, j, new_hyperedge),
                            [x for x in bicliques if not any_overlap(biclique, x)]
                        )

                        if proof_steps is not None:
                            return proof_steps + [proof_step]
    
    return None
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
    

g = 'JHO\MageEG?' # Grotzsch graph
# g = 'ECfw'

int_graph, num_vertices = decode(g)

print(int_graph)

for x in sperner_family(odd_cycles(num_vertices, int_graph)):
	print(x)
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

