from dataclasses import dataclass
from typing import List, Optional, Tuple
Biclique = List[str] # List[Literal['l', 'r', 'o']]
Hyperedge = List[str] # List[Literal['x', 'o']]
ProofStep = List[str] # List[Literal['a', 'b', 'u', 'v', 'w', 'o']]
Hypergraph = List[Hyperedge]
def any_overlap(a: Biclique, b: Biclique) -> bool:
    num_vertices = len(a[0])
    
    for v in range(num_vertices):
        x = a[v] + b[v]
        
        if x == 'll' or x == 'lr' or x == 'rl' or x == 'rr':
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
    
test = [
    ['x', 'x', 'o', 'x', 'o'],
    ['x', 'o', 'o', 'x', 'o'],
    ['x', 'o', 'o', 'x', 'x']]
    
print(sperner_family(test));
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
    
print(proves(abc_hypergraph, abc_bicliques))

