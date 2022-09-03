#include <stdlib.h>
#include <stdbool.h>

int main() {
	return 0;
}
typedef char *Biclique; // 'l', 'r', 'o'
typedef char *Hyperedge; // 'x', 'o'
typedef char *ProofStep; // 'a', 'b', 'u', 'v', 'w', 'o'
typedef Hyperedge *Hypergraph;
static bool any_overlap(Biclique a, Biclique b, int num_vertices) {
	for (int v = 0; v < num_vertices; v++) {
		if (
			(a[v] == 'l' && b[v] == 'l') ||
			(a[v] == 'l' && b[v] == 'r') ||
			(a[v] == 'r' && b[v] == 'l') ||
			(a[v] == 'r' && b[v] == 'r')
		) {
			return true;
		}
	}
	
	return false;
}
static bool any_edge_has_less_than_three(Hypergraph h, int num_hyperedges, int num_vertices) {
	for (int i = 0; i < num_hyperedges; i++) {
		Hyperedge hyperedge = h[i];
		int edge_size = 0;
		for (int j = 0; j < num_vertices; j++) {
			char vertex = hyperedge[j];
			if (vertex == 'x') {
				edge_size += 1;
			}
		}

		if (edge_size < 3) {
			return true;
		}
	}
	
	return false;
}
static bool attempt_proof_step(Hyperedge a, Hyperedge b, Biclique q, int num_vertices, ProofStep proof_step, Hyperedge new_h) {
	for (int v = 0; v < num_vertices; v++) {
		if (a[v] == 'x' && b[v] == 'o' && q[v] == 'l') {
			proof_step[v] = 'a';
			new_h[v] = 'o';

		} else if (a[v] == 'o' && b[v] == 'x' && q[v] == 'r') {
			proof_step[v] = 'b';
			new_h[v] = 'o';

		} else if (a[v] == 'x' && b[v] == 'o' && q[v] == 'o') {
			proof_step[v] = 'u';
			new_h[v] = 'x';
			
		} else if (a[v] == 'o' && b[v] == 'x' && q[v] == 'o') {
			proof_step[v] = 'v';
			new_h[v] = 'x';
		
		} else if (a[v] == 'x' && b[v] == 'x' && q[v] == 'o') {
			proof_step[v] = 'w';
			new_h[v] = 'x';
		
		} else if (a[v] == 'o' && b[v] == 'o' && q[v] == 'o') {
			proof_step[v] = 'o';
			new_h[v] = 'o';
		
		} else {
			return false;
		}
	}

	return true;
}
static void remove2add1(Hypergraph h, int i, int j, int num_hyperedges, Hyperedge new_h, Hypergraph out) {
	int l = 0;
	for (int index = 0; index < num_hyperedges; index++) {
		if (index != i && index != j) {
			out[l] = h[index];
			l++;
		}
	}
	
	out[l] = new_h;
}
static bool proves(Hypergraph h, Biclique *bicliques, int num_hyperedges, int num_vertices, int num_bicliques, ProofStep *out) {
	if (any_edge_has_less_than_three(h, num_hyperedges, num_vertices)) {
		return true;
	}
	
	Hyperedge new_h = malloc(num_vertices * sizeof(char));
	ProofStep proof_step = malloc(num_vertices * sizeof(char));
	Hypergraph new_hypergraph = malloc(num_hyperedges * sizeof(Hyperedge));
	
	for (int i = 0; i < num_hyperedges; i++) {
		for (int j = 0; j < num_hyperedges; j++) {
			for (int k = 0; k < num_bicliques; k++) {
				if (i != j) {
					bool attempt = attempt_proof_step(h[i], h[j], bicliques[k], num_vertices, proof_step, new_h);
	
					if (attempt) {
						remove2add1(h, i, j, num_hyperedges, new_h, new_hypergraph);
						
						
					
					}
				}
			}
		}
	}
	
	free(new_h);
	free(proof_step);
	free(new_hypergraph);
	
	return false;
}

