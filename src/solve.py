import itertools
from typing import Deque, FrozenSet, Hashable, Optional, TypeVar, List, Callable, Tuple, Set, Generator, Dict
from collections import deque
from tqdm import tqdm

A = TypeVar('A', bound=Hashable)
EdgeSet = FrozenSet[Tuple[int, int]]
Hyperedge = FrozenSet[int]
Hypergraph = FrozenSet[Hyperedge]
AdjacencyList = Dict[int, List[int]]

def main():
	gs: List[str] = []
	with open('src/tf_graphs_15v_4chrom-crit.g6') as file:
		for graph6 in file:
			gs.append(graph6)

	determine(gs)

def determine(four_critical_g6s: List[str]) -> Optional[str]:
	g6: str
	for g6 in tqdm(four_critical_g6s):
		adjacency_list = decode(g6)
		graph: EdgeSet = convert(adjacency_list)
		hypergraph: Hypergraph = odd_cycles(adjacency_list)
		
		not_four_colorable, paths = solve(graph, hypergraph)

		print(paths)

		if not not_four_colorable:
			print('Found an exception:', four_critical_g6s)

def decode(g6: str) -> AdjacencyList:
	def as_int(char: str):
		return ord(char) - 63

	num_vertices = as_int(g6[0])
	
	bits = [
		x == '1'
		for c in g6[1:]
		for x in format(as_int(c), '6b')
	]

	graph: AdjacencyList = {x: [] for x in range(num_vertices)}

	index = 0
	for i in range(num_vertices):
		for j in range(i):
			if bits[index]:
				graph[i].append(j)
				graph[j].append(i)
			
			index += 1
			
	return graph

def odd_cycles(graph: AdjacencyList) -> Hypergraph:
	def odd_paths(start: int, end: int) -> List[List[int]]:
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
	result: List[Hyperedge] = []
	seen: Set[Hyperedge] = set()

	for vertex in graph:
		for	odd_cycle in odd_paths(vertex, vertex):
			hyperedge = frozenset(odd_cycle)

			if hyperedge not in seen:
				seen.add(hyperedge)
				result.append(hyperedge)

	return frozenset(result)

def convert(adjacency_list: AdjacencyList) -> EdgeSet:
	edge_set: Set[Tuple[int, int]] = set()
	
	for u in adjacency_list:
		for v in adjacency_list[u]:
			u1, v1 = min(u, v), max(u, v)
			edge_set.add((u1, v1))

	return frozenset(edge_set)

def solve(graph: EdgeSet, hypergraph: Hypergraph) -> Tuple[bool, int]:
	n = 0
	for (_, h) in bfs(run, (graph, hypergraph)):
		n += 1
		if any(len(e) < 3 for e in h):
			return True, n
	
	return False, n
	
def bfs(neighbors_of: Callable[[A], List[A]], start: A) -> Generator[A, None, None]:
	seen: Set[A] = set([start])
	queue: Deque[A] = deque([start])
	
	while len(queue) > 0:
		vertex = queue.popleft()
		
		yield vertex
		
		for neighbor in neighbors_of(vertex):
			if neighbor not in seen:
				seen.add(neighbor)
				queue.append(neighbor)
				
def run(x: Tuple[EdgeSet, Hypergraph]) -> List[Tuple[EdgeSet, Hypergraph]]:
	graph, hypergraph = x
	
	def is_biclique(l: Set[int], r: Set[int]) -> bool:
		return all(has_edge(graph, x, y) for x in l for y in r)
		
	def xi(e1: Hyperedge, e2: Hyperedge) -> List[Tuple[Hyperedge, Hyperedge]]:
		b1, common, b2 = venn_diagram(e1, e2)
		
		result: List[Tuple[Hyperedge, Hyperedge]] = []
		
		for i1, o1 in unriffle(b1):
			if len(i1) > 0:
				for i2, o2 in unriffle(b2):
					if len(i2) > 0:
						if is_biclique(i1, i2):
							e3 = frozenset(itertools.chain(o1, o2, common))
							used = frozenset(itertools.chain(i1, i2))
							result.append((e3, used))
							
		return result
		
		
	result: List[Tuple[EdgeSet, Hypergraph]] = []
	for e1, e2 in itertools.combinations(hypergraph, 2):
		for e3, used in xi(e1, e2):
			new_graph = frozenset((u, v) for (u, v) in graph if u not in used and v not in used)
			
			new_hypergraph = set(hypergraph)
			new_hypergraph.remove(e1)
			new_hypergraph.remove(e2)
			new_hypergraph.add(e3)
			
			result.append((new_graph, frozenset(new_hypergraph)))
			
	return result
	
def has_edge(graph: EdgeSet, u: int, v: int) -> bool:
	u1, v1 = min(u, v), max(u, v)
	return (u1, v1) in graph
	
def unriffle(x: Set[A]) -> List[Tuple[Set[A], Set[A]]]:
	def go(x: Set[A]) -> List[Tuple[Set[A], Set[A]]]:
		if len(x) == 0:
			return [(set(), set())]
			
		first = x.pop()
		rest = x
		
		result: List[Tuple[Set[A], Set[A]]] = []
		
		for l_rec, r_rec in go(rest):
			l1 = l_rec.copy()
			l2 = l_rec.copy()
			r1 = r_rec.copy()
			r2 = r_rec.copy()
			
			l1.add(first)
			r2.add(first)
			
			result.append((l1, r1))
			result.append((l2, r2))
			
		return result
			
	return go(x.copy())
		
	
def venn_diagram(e1: FrozenSet[A], e2: FrozenSet[A]) -> Tuple[Set[A], Set[A], Set[A]]:
	l: Set[A] = set()
	common: Set[A] = set()
	r: Set[A] = set()
	
	for x in e1:
		if x in e2:
			common.add(x)
		else:
			l.add(x)
			
	for x in e2:
		if x in e1:
			common.add(x)
		else:
			r.add(x)
		
	return l, common, r

def make_hypergraph(l: List[List[int]]) -> Hypergraph:
	return frozenset(
		frozenset(x)
		for x in l
	)

'''
graph = frozenset([
	(0, 15),
	(0, 5),
	(0, 8),
	(4, 15),
	(4, 5),
	(4, 8),
	(15, 18),
	(8, 18),
	
	(10, 16),
	(3, 10),
	(7, 16),
	(3, 7),
	
	(9, 17),
	
	(12, 13),
	(1, 12),
	(6, 12),
	(12, 14)
])

hypergraph = make_hypergraph([
	[0, 4, 3, 16, 18],
	[6, 14, 8, 5, 15],
	[1, 17, 13],
	[7, 9, 10],
	[12, 11, 2]
])
'''
	

if __name__ == '__main__':
	main()