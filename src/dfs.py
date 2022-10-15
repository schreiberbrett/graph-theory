import itertools
import settools
from typing import Generator, List, Tuple, Set, Dict, FrozenSet
from tqdm import tqdm

Graph = List[Tuple[int, int]]

def has_edge(graph: Graph, u: int, v: int) -> bool:
    return (min(u, v), max(u, v)) in graph

Hyperedge = Set[int]
Hypergraph = List[Hyperedge]

def main2():
	gs: List[str] = []
	with open('src/mtf_graph_11v_4chrom.g6') as file:
		for graph6 in file:
			gs.append(graph6)

	for graph6 in tqdm(gs):
		adj_list = decode(graph6)
		graph, hypergraph = convert(adj_list)
		if not solve(graph, hypergraph):
			print('Found an exception:', graph6)
			break

def main():
	print(solve(counterexample_graph, counterexample_hypergraph))

def solve(graph: Graph, hypergraph: Hypergraph) -> bool:
    if any(len(e) < 3 for e in hypergraph):
        return True

    for e1, e2 in itertools.combinations(hypergraph, 2):
        for a1, b1, c, b2, a2 in find_bicliques(graph, e1, e2):
            e3 = b1 | c | b2

            new_graph = [(u, v) for (u, v) in graph if
                u not in a1 and u not in a2 and
                v not in a1 and v not in a2
            ]

            new_hypergraph = [e for e in hypergraph
                if not a1.issubset(e) and not a2.issubset(e)
				# if e != e1 and e != e2
            ]

            new_hypergraph.append(e3)

            if solve(new_graph, new_hypergraph):
                print(a1, b1, c, b2, a2)
                return True

    return False

def find_bicliques(graph: Graph, e1: Hyperedge, e2: Hyperedge) -> Generator[Tuple[Hyperedge, Hyperedge, Hyperedge, Hyperedge, Hyperedge], None, None]:
    a1b1, c, b2a2 = settools.venn_diagram(e1, e2)

    for a1, b1 in settools.unriffle(a1b1):
        if len(a1) == 0:
            continue

        for b2, a2 in settools.unriffle(b2a2):
            if len(a2) == 0:
                continue

            if all(has_edge(graph, x, y) for x in a1 for y in a2):
                yield a1, b1, c, b2, a2

AdjacencyList = Dict[int, List[int]]

def convert(adjacency_list: AdjacencyList) -> Tuple[Graph, Hypergraph]:
    return to_graph(adjacency_list), settools.sperner_family(odd_cycles(adjacency_list))

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
	seen: Set[FrozenSet[int]] = set()

	for vertex in graph:
		for	odd_cycle in odd_paths(vertex, vertex):
			hyperedge = set(odd_cycle)
			frozen_hyperedge = frozenset(odd_cycle)

			if frozen_hyperedge not in seen:
				seen.add(frozen_hyperedge)
				result.append(hyperedge)

	return result

def to_graph(adjacency_list: AdjacencyList) -> Graph:
	graph: List[Tuple[int, int]] = []
	
	for u in adjacency_list:
		for v in adjacency_list[u]:
			graph.append((min(u, v), max(u, v)))

	return graph

counterexample_graph: Graph = [
	(0, 4),
	(0, 8),
	(3, 4),
	(3, 8),
	(2, 5),
	(6, 9),
	(1, 11),
	(7, 11),
	(10, 11)
]

counterexample_hypergraph: Hypergraph = [
	{0, 1, 2},
	{3, 4, 5, 6, 7},
	{8, 9, 10},
	{11, 12, 13}
]


moser_spindle: AdjacencyList = {
	0: [1, 2, 6],
	1: [0, 2, 3],
	2: [0, 1, 3],
	3: [1, 2, 4, 5],
	4: [3, 5, 6],
	5: [3, 4, 6],
	6: [0, 4, 5]
}

double_moser_spindle: AdjacencyList = {
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

grotzsch_graph: AdjacencyList = {
	0: [1, 2, 3, 4, 5],
	1: [0, 8, 10],
	2: [0, 9, 10],
	3: [0, 6, 9],
	4: [0, 6, 7],
	5: [0, 7, 8],
	6: [3, 4, 8, 10],
	7: [4, 5, 9, 10],
	8: [1, 5, 6, 9],
	9: [2, 3, 7, 8],
	10: [1, 2, 6, 7]
}

def find_isochromatic_vertices(g: Graph, not_1: Hyperedge) -> Hyperedge:
	'''Given a graph `g`, and a set of vertices `not_1` guaranteed to not all be the same color in any 3-coloring of `g`, return the unique set of vertices that forms a biclique with `not_1`.'''
	all_the_same_color: Hyperedge = set()

	for u in vertices(g):
		if u not in not_1 and all(has_edge(g, u, v) for v in not_1):
			all_the_same_color.add(u)

	return all_the_same_color


def contract_vertices(g: Graph, vertices: Hyperedge) -> Graph:
	'''Perform a vertex contraction on all the given vertices. Relabels all the vertices'''
	if len(vertices) == 0:
		return g

	contract_to = min(v for v in vertices)
	contract_from = {v for v in vertices if v != contract_to}

	new_graph: Set[Tuple[int, int]] = set()

	for (u, v) in g:
		new_u = contract_to if u in contract_from else u
		new_v = contract_to if v in contract_from else v

		new_graph.add((min(new_u, new_v), max(new_u, new_v)))

	# return list(new_graph)
	return normalize(list(new_graph))

def contract_all_vertices(g: Graph, vertices: Hypergraph) -> Graph:
	'''Perform n vertex contractions corresponding to each hyperedge in the hypergraph'''

	for hyperedge in vertices:
		pass

def normalize(g: Graph) -> Graph:
	mapping = dict((y, x) for x, y in enumerate(vertices(g)))

	new_graph: List[Tuple[int, int]] = []
	for (u, v) in g:
		new_u = mapping[u]
		new_v = mapping[v]
		new_graph.append((min(new_u, new_v), max(new_u, new_v)))

	return new_graph

def vertices(g: Graph):
	seen: Set[int] = set()

	for (u, v) in g:
		if u not in seen:
			yield u
			seen.add(u)

		if v not in seen:
			yield v
			seen.add(v)


brett: Graph = [
	(0, 2),
	(1, 2),
	(2, 3),
	(2, 4),
	(3, 4),
	(3, 5),
	(4, 5),
	(5, 6),
	(5, 7),
	(5, 8),
	(5, 9),
	(8, 9),
	(8, 10),
	(9, 10)
]



















if __name__ == '__main__':
	main()
