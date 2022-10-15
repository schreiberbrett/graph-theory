from functools import partial
from itertools import permutations
import multiprocessing
import random
from time import sleep
from typing import FrozenSet, Generator, Iterable, List, Optional, Set, Tuple

from tqdm import tqdm
from analysis import Biclique, all_bicliques, all_odd_cycles, shares_edge, size
from searchstrategies import dfs
from graph import EdgeList, parse, to_edgelist
from settools import SpernerFamily
import sys

def search_permutation(odd_cycles: SpernerFamily[int], bicliques: Iterable[Biclique]) -> bool:
    needs_3 = odd_cycles.copy()
    bicliques = list(bicliques)

    skip: List[bool] = [False] * len(bicliques)
    for i, biclique in enumerate(bicliques):
        if skip[i]:
            continue

        a1, a2 = biclique
        
        a1b1cs: List[Set[int]] = []
        a2b2cs: List[Set[int]] = []
        for e in needs_3:
            if e.issuperset(a1) and not e.issuperset(a2):
                a1b1cs.append(e)
            
            if not e.issuperset(a1) and e.issuperset(a2):
                a2b2cs.append(e)

        derived: List[Set[int]] = []
        a1a2 = a1 | a2
        for a1b1c in a1b1cs:
            for a2b2c in a2b2cs:
                b1b2c = (a1b1c | a2b2c) - a1a2
                derived.append(b1b2c)

        if any(len(x) < 3 for x in derived):
            return True

        for x in derived:
            needs_3.add(x)

        for j in range(i + 1, len(bicliques)):
            skip[j] = shares_edge(biclique, bicliques[j])

    return False

def decide3(graph6: str) -> Tuple[bool, str]:
    adjacency_list = parse(graph6)

    odd_cycles = SpernerFamily.of(all_odd_cycles(adjacency_list))

    edge_list = to_edgelist(adjacency_list)

    bicliques = all_bicliques(edge_list)
    

    for permutation in permutations(bicliques, len(edge_list)):
        if search_permutation(odd_cycles, permutation):
            return True, graph6

    return False, graph6

    '''
    nproc = multiprocessing.cpu_count()

    with multiprocessing.Pool(nproc) as pool:
        for x in pool.imap_unordered(partial(search_permutation, odd_cycles), permutations(bicliques, len(edge_list))):
            if x:
                pool.terminate()
                return True

    return False'''

def full_dfs(bicliques: List[Biclique], needs_3: SpernerFamily[int]) -> Optional[List[Biclique]]:
    if any(len(x) < 3 for x in needs_3):
        return []

    for a1, a2 in bicliques:
        a1b1cs: List[Set[int]] = []
        a2b2cs: List[Set[int]] = []
        for e in needs_3:
            if e.issuperset(a1) and not e.issuperset(a2):
                a1b1cs.append(e)
            
            if not e.issuperset(a1) and e.issuperset(a2):
                a2b2cs.append(e)

        derived = SpernerFamily[int]()

        a1a2 = a1 | a2

        for a1b1c in a1b1cs:
            for a2b2c in a2b2cs:
                b1b2c = (a1b1c | a2b2c) - a1a2
                derived.add(b1b2c)

        filtered_bicliques = [
            x
            for x in bicliques
            if not shares_edge(x, (a1, a2))
        ]

        result = full_dfs(filtered_bicliques, needs_3 | derived)
        if result is not None:
            return [(a1, a2)] + result
    
    return None

def run(x: Tuple[FrozenSet[Biclique], SpernerFamily[int]]) -> Generator[Tuple[FrozenSet[Biclique], SpernerFamily[int]], None, None]:
    bicliques, needs_3 = x
    for a1, a2 in bicliques:
        a1b1cs: List[Set[int]] = []
        a2b2cs: List[Set[int]] = []
        for e in needs_3:
            if e.issuperset(a1) and not e.issuperset(a2):
                a1b1cs.append(e)
            
            if not e.issuperset(a1) and e.issuperset(a2):
                a2b2cs.append(e)

        derived = SpernerFamily[int]()

        a1a2 = a1 | a2

        for a1b1c in a1b1cs:
            for a2b2c in a2b2cs:
                b1b2c = (a1b1c | a2b2c) - a1a2
                derived.add(b1b2c)

        filtered_bicliques = [
            x
            for x in bicliques
            if not shares_edge(x, (a1, a2))
        ]

        yield (frozenset(filtered_bicliques), needs_3 | derived)


def worker(x: Tuple[SpernerFamily[int], List[Biclique]]):
    needs_3, bicliques = x
    my_bicliques = bicliques.copy()
    random.shuffle(my_bicliques)
    return full_dfs(my_bicliques, needs_3)

def decide(graph6: str) -> Optional[List[Biclique]]:
    adjacency_list = parse(graph6)

    needs_3 = SpernerFamily.of(all_odd_cycles(adjacency_list))

    bicliques = all_bicliques(to_edgelist(adjacency_list))
    
    nproc = multiprocessing.cpu_count()

    result: Optional[List[Biclique]] = None
    with multiprocessing.Pool(nproc) as pool:
        for x in pool.imap_unordered(worker, ((needs_3, bicliques) for _ in range(nproc))):
            result = x
            pool.terminate()
            break

    return result

def decide2(graph6: str) -> bool:
    adjacency_list = parse(graph6)
    needs_3 = SpernerFamily.of(all_odd_cycles(adjacency_list))

    bicliques = all_bicliques(to_edgelist(adjacency_list))

    bicliques.sort(key=size, reverse=False)

    for (_, derived_needs_3) in dfs(run, (frozenset(bicliques), needs_3)):
        if any(len(x) < 3 for x in derived_needs_3):
            return True

    return False


def main():
    gs: List[str] = []
    with open('src/tf_graphs_15v_4chrom-crit.g6') as file:
        for graph6 in file:
            gs.append(graph6)

    nproc = multiprocessing.cpu_count()

    with multiprocessing.Pool(processes=nproc) as pool:
        for result, graph6 in tqdm(pool.imap_unordered(decide3, gs), total=len(gs)):
            if result == False:
                print('Found an exception:', graph6)
                pool.terminate()
                break

    for _ in range(6):
        sys.stdout.write('\a')
        sys.stdout.flush()
        sleep(1)

counterexample_graph: EdgeList = [
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

counterexample_needs_3 = SpernerFamily.of([
	{0, 1, 2},
	{3, 4, 5, 6, 7},
	{8, 9, 10},
	{11, 12, 13}
])

if __name__ == '__main__':
    main()