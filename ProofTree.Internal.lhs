
\begin{code}
{-# LANGUAGE DuplicateRecordFields #-}

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import TwoSet (TwoSet, wrap)
\end{code}

For notational convenience, we define a Graph to have a distinction between black and white vertices, and a distinction between solid and dotted edges. Since this is an undirected graph, ordered pairs are inappropriate, so a custom \TwoSet is used instead.
\begin{code}
data BW = Black | White deriving (Eq, Ord, Show)
data SD = Solid | Dotted deriving (Eq, Ord, Show)

data Graph a = Graph {
    vertices :: Map a BW,
    edges :: Map (TwoSet a) SD
    }

hasEdge :: Ord a => Graph a -> a -> a -> Bool
hasEdge g u v = (wrap u v) `Map.member` (edges g)
\end{code}

The base case of any proof tree is an Odd Cycle, which must use all 3 colors. In this case the input graph is the original graph.

There is also a recursive case that uses an Odd Cycle. Any Quasi Edge result has the potential of producing new odd cycles. In this case the input is the Graph who just acquired quasi edges, and therefore has potential for new odd cycles.

\begin{code}
data OddCycle a = OddCycle {
    input :: Graph a,
    oddCycle :: Graph a
    }
\end{code}



\begin{code}
data Xi a = Xi {
    inputs :: Set (Graph a),
    outputs :: Set (Graph a)
    }
\end{code}


\begin{code}
data Q a = Q {
    input :: Graph a,

    vertices :: Set a,
    quasiEdges :: Set (TwoSet a),

    outputs :: Graph a
    }
\end{code}

Our proof circuit is a collection of gates.

\begin{code}
data Gate a
    = GateOddCycle (OddCycle a)
    | GateXi (Xi a)
    | GateQ (Q a)
\end{code}


Next we reach the definition of xi the function.

Disjointness laws by definition:
\begin{itemize}
\item $b_1 \leftrightarrow w_1 \leftrightarrow g_1$
\item $b_2 \leftrightarrow w_2 \leftrightarrow g_2$
\item $g_1 \leftrightarrow g_2$
\item $g_1 \leftrightarrow w_2$
\item $g_2 \leftrightarrow w_1$
\end{itemize}

Disjointness laws that force the number of black vertices to strictly increase:
\begin{itemize}
\item $w_1 \leftrightarrow b_2$
\item $w_2 \leftrightarrow b_1$
\end{itemize}

So actually, the only two sets that are allowed to overlap are:
\begin{itemize}
\item $b_1$, $b_2$
\item $w_1$, $w_2$
\end{itemize}
and this is easier to remember.

Thankfully, the representation of graphs already makes a disjoint distinction between black and white vertices. And the only eligible $g_1, g_2$ are among the white vertices that the graphs do not have in common.

Algorithm: given any two graphs, take the venn diagram of their white vertices, and only consider the sets of vertices $g_1, g_2$ that they do not have in common. Call this pair of sets the \emph{disjoint members} of the two graphs. This narrow consideration ensures that the above disjointness laws are satisfied.

Below is an algorithm that finds the disjoint members of two sets in a single pass.

\begin{code}
disjointMembersAttempt :: (Ord a) => Set a -> Set a -> (Set a, Set a)
disjointMembersAttempt a b = go (Set.toAscList a) (Set.toAscList b) [] [] where
    go [] []         d1 d2 = (Set.fromAscList d1, Set.fromAscList d2)
    go (x:xs) []     d1 d2 = go xs [] ((if x `Set.member` b then id else (x:)) d1) d2
    go [] (y:ys)     d1 d2 = go [] ys d1 ((if y `Set.member` a then id else (y:)) d2)
    go (x:xs) (y:ys) d1 d2 = go xs ys
        ((if x `Set.member` b then id else (x:)) d1)
        ((if y `Set.member` a then id else (y:)) d2)
\end{code}

This implementation is verbose and we sacrifice an extra pass for gained readability.

\begin{code}
disjointMembers :: (Ord a) => Set a -> Set a -> (Set a, Set a)
disjointMembers a b = (
    Set.filter (\x -> Set.notMember x b) a,
    Set.filter (\x -> Set.notMember x a) b
    )
\end{code}

Note, then we need a function that can find the mutual edges between $g_1$ and $g_2$.

This must return a set of pairs rather than a set of sets, because the membership in $us$ versus the membership in $vs$ must be preserved.
\begin{code}
mutualEdges :: Graph a -> Set a -> Set a -> Set (a, a)
mutualEdges g us vs = Set.fromList [ (u, v) | 
    u <- Set.toList us,
    v <- Set.toList vs,
    hasEdge g u v
    ]
\end{code}

Find all complete bipartite subgraphs of a bipartite graph.

\begin{code}
completeBipartiteSubgraphs :: Set (a, a) -> [Set (a, a)]
completeBipartiteSubgraphs = undefined
\end{code}

\begin{code}
xi :: Graph a -> Graph a -> Graph a -> [Graph a]
xi referenceGraph b1 b2 = [ Graph {vertices=undefined, edges=undefined} |
    (b1, w1, g1) <- breakdowns $ b1, -- satisfies pairwise disjoint b1, w1, g1
    not $ null g1,
    (b2, w2, g2) <- breakdowns $ b2, -- satisfies pairwise disjoint b2, w2, g2
    not $ null g2,

    -- satisfies g1 <-> b2, g1 <-> w2, g1 <-> g2
    not $ any [(x `elem` b2) || (x `elem` w2) || (x `elem` g2) | x <- g1],

    -- satisfies g2 <-> b1, g2 <-> w1
    not $ any [(x `elem` b1) || (x `elem` w1)                  | x <- g2]
    ]


    
\end{code}
\begin{code}

\end{code}