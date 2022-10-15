from typing import Dict, FrozenSet, Generator, List, Tuple

from graph import AdjacencyList, AdjacencyMatrix, EdgeList, edge_list_to_adjacency_matrix
from settools import unriffle


Biclique = Tuple[FrozenSet[int], FrozenSet[int]]

def size(x: Biclique) -> int:
    (l, r) = x
    return len(l) + len(r)

def all_bicliques_original(graph: AdjacencyMatrix) -> List[Biclique]:
    vertices = set(range(len(graph)))

    bicliques: List[Biclique] = []
    for a1, a2o in unriffle(vertices):
        if len(a1) == 0:
            continue

        for a2, _o in unriffle(a2o):
            if len(a2) == 0:
                continue

            if all(graph[x][y] for x in a1 for y in a2):
                bicliques.append((frozenset(a1), frozenset(a2)))
    
    return bicliques

def all_bicliques(edge_list: EdgeList) -> List[Biclique]:
    adjacency_matrix = edge_list_to_adjacency_matrix(edge_list)
    num_vertices = len(adjacency_matrix)

    # (n, m) -> (n + 1, m)
    def f(n: int, m: int) -> List[Biclique]:
        return [
            (l.union([u]), r)
            for (l, r) in bicliques_of_size[(n, m)]
            for u in range(num_vertices)
            if u not in l and all(adjacency_matrix[u][v] for v in r)
        ]

    # (n, m) -> (n, m + 1)
    def g(n: int, m: int) -> List[Biclique]:
        return [
            (l, r.union([u]))
            for (l, r) in bicliques_of_size[(n, m)]
            for u in range(num_vertices)
            if u not in r and all(adjacency_matrix[u][v] for v in l)
        ]

    bicliques_of_size: Dict[Tuple[int, int], List[Biclique]] = {}

    bicliques_of_size[(1, 1)] = [(frozenset([u]), frozenset([v])) for u, v in edge_list]

    for n in range(1, num_vertices):
        bicliques_of_size[(n + 1, 1)] = f(n, 1)

    for n in range(2, num_vertices):
        for m in range(1, n):
            bicliques_of_size[(n, m + 1)] = g(n, m)

    return list(set(
        biclique
        for bicliques in bicliques_of_size.values()
        for biclique in bicliques
    ))

def shares_edge(a: Biclique, b: Biclique) -> bool:
    (x, y) = a
    (z, w) = b

    return (
        (len(x & z) > 0 and len(y & w) > 0) or
        (len(x & w) > 0 and len(y & z) > 0)
    )


def all_odd_cycles(graph: AdjacencyList) -> Generator[FrozenSet[int], None, None]:
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

	for vertex in graph:
		for	odd_cycle in odd_paths(vertex, vertex):
			yield frozenset(odd_cycle)
