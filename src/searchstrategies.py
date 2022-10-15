
from collections import deque
from typing import Callable, Deque, Generator, Hashable, List, Set, TypeVar

A = TypeVar('A', bound=Hashable)

def bfs(neighbors_of: Callable[[A], Generator[A, None, None]], start: A) -> Generator[A, None, None]:
	seen: Set[A] = set([start])
	queue: Deque[A] = deque([start])
	
	while len(queue) > 0:
		vertex = queue.popleft()
		
		yield vertex
		
		for neighbor in neighbors_of(vertex):
			if neighbor not in seen:
				seen.add(neighbor)
				queue.append(neighbor)


def dfs(neighbors_of: Callable[[A], Generator[A, None, None]], start: A) -> Generator[A, None, None]:
	seen: Set[A] = set([start])
	stack: List[A] = [start]
	
	while len(stack) > 0:
		vertex = stack.pop()
		
		yield vertex
		
		for neighbor in neighbors_of(vertex):
			if neighbor not in seen:
				seen.add(neighbor)
				stack.append(neighbor)