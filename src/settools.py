from typing import Generic, Iterable, TypeVar, Hashable, Set, List, Tuple

A = TypeVar('A', bound=Hashable)

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

def venn_diagram(e1: Set[A], e2: Set[A]) -> Tuple[Set[A], Set[A], Set[A]]:
    '''Returns the left-disjoint elements, the common elements, and the right-disjoint elements'''
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

def sperner_family(set_family: List[Set[A]]) -> List[Set[A]]:
    unique_set_family = [set(x) for x in {frozenset(x) for x in set_family}]

    result: List[Set[A]] = []
    for i in range(len(unique_set_family)):
        any_subset = False
        for j in range(len(unique_set_family)):
            if i != j and unique_set_family[j].issubset(unique_set_family[i]):
                any_subset = True
                break

        if not any_subset:
            result.append(unique_set_family[i])

    return result


class SpernerFamily(Generic[A], Hashable):
    family: List[Set[A]] = list()

    def __init__(self: 'SpernerFamily[A]'):
        self.family = list()

    def add(self: 'SpernerFamily[A]', elem: Set[A]) -> None:
        for x in self.family:
            if x.issubset(elem):
                return

        self.family = [x for x in self.family if not x.issuperset(elem)]

        self.family.append(elem)

    def union(self: 'SpernerFamily[A]', other: 'SpernerFamily[A]') -> 'SpernerFamily[A]':
        result = SpernerFamily[A]()

        for x in self.family:
            result.add(x)

        for x in other.family:
            result.add(x)

        return result

    def copy(self: 'SpernerFamily[A]') -> 'SpernerFamily[A]':
        result = SpernerFamily[A]()
        result.family = self.family.copy()
        return result

    def __or__(self: 'SpernerFamily[A]', other: 'SpernerFamily[A]') -> 'SpernerFamily[A]':
        return self.union(other)

    def __iter__(self):
        return self.family.__iter__()

    def __hash__(self):
        return frozenset(self.family).__hash__()

    @staticmethod
    def of(l: Iterable[Iterable[A]]) -> 'SpernerFamily[A]':
        result = SpernerFamily[A]()

        for x in l:
            result.add(set(x))

        return result
