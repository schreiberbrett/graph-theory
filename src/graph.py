from typing import Dict, List, Tuple

AdjacencyMatrix = List[List[bool]]
AdjacencyList = Dict[int, List[int]]
EdgeList = List[Tuple[int, int]]

def parse(graph6: str) -> AdjacencyList:
	def as_int(char: str):
		return ord(char) - 63

	num_vertices = as_int(graph6[0])
	
	bits = [
		x == '1'
		for c in graph6[1:]
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

def to_edgelist(adjacency_list: AdjacencyList) -> EdgeList:
	graph: EdgeList = []
	
	for u in adjacency_list:
		for v in adjacency_list[u]:
			graph.append((min(u, v), max(u, v)))

	return graph

def to_adjacency_list(edge_list: EdgeList) -> AdjacencyList:
	adjacency_list: AdjacencyList = {}

	for (u, v) in edge_list:
		if u not in adjacency_list:
			adjacency_list[u] = []

		adjacency_list[u].append(v)

		if v not in adjacency_list:
			adjacency_list[v] = []

		adjacency_list[v].append(u)

	return adjacency_list

def to_adjacency_matrix(adjacency_list: AdjacencyList) -> AdjacencyMatrix:
    adjacency_matrix: AdjacencyMatrix = [
        [False for _ in adjacency_list]
        for _ in adjacency_list
    ]

    for u in adjacency_list:
        for v in adjacency_list[u]:
            adjacency_matrix[u][v] = True

    return adjacency_matrix

def edge_list_to_adjacency_matrix(edge_list: EdgeList) -> AdjacencyMatrix:
	return to_adjacency_matrix(to_adjacency_list(edge_list))