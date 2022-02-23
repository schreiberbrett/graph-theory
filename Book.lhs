\documentclass{article}
%include polycode.fmt

\usepackage{hyperref}
\hypersetup{
    colorlinks,
    citecolor=black,
    filecolor=black,
    linkcolor=black,
    urlcolor=black
}

\title{Proof Complexity and 3COLOR}
\author{Brett Schreiber}
\date{ }
  
\begin{document}
  
\maketitle
  
\tableofcontents

\begin{code}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace (trace)
\end{code}

%format `Set.member` = "\in "
%format `Set.notMember` = "\notin "
%format Delta = "\Delta "
%format Xi = "\Xi "
%format hasEdge (g) (u) (v) = "(" u "\sim" v ") \in " g
%format Set a = "\{" a "\}"
%format `Set.isSubsetOf` = "\subseteq "

For notational convenience, we define a Graph to have a distinction between black and white vertices, and a distinction between solid and dotted edges. Since this is an undirected graph, ordered pairs are inappropriate, so a custom TwoSet is used instead.
\begin{code}
data BW = Black | White deriving (Eq, Ord, Show)
data SD = Solid | Dotted deriving (Eq, Ord, Show)

data Graph a = Graph {
    vertices :: Map a BW,
    edges :: Set (TwoSet a)
    } deriving (Eq, Ord, Show)

hasEdge :: Ord a => Graph a -> a -> a -> Bool
hasEdge g u v = wrap u v `Set.member` edges g

wVertices :: (Ord a) => Graph a -> Set a
wVertices g = Map.keysSet
    $ Map.filter (== White)
    $ vertices g

neighbors :: (Ord a) => Graph a -> Map a (Set a)
neighbors g = Map.fromSet (\v -> Set.filter (hasEdge g v) vs) vs
    where vs = Map.keysSet (vertices g)

\end{code}

The base case of any proof tree is $\Delta$: an Odd Cycle, which must use all 3 colors. In this case the input graph is the original graph.

There is also a recursive case that uses an Odd Cycle. Any Quasi Edge result has the potential of producing new odd cycles. In this case the input is the Graph who just acquired quasi edges, and therefore has potential for new odd cycles. \footnote{Please excuse me for mixing Greek and Latin letters for the same class of symbols. $\Delta$ looks the most like an odd cycle. Likewise, $\Xi$ looks the most like a complete bipartite subgraph. $Q$ does not look like any particular subgraph, but instead it stands for "Quasi-Edge"}


\section{Definitions}

\begin{description}

\item[color-constrained] A subset of a graph's vertices $S$ is said to be \emph{color-constrained} if something can be said about $S$ in any 3-coloring of the graph. For example, $S$ might need to use all 3 colors. Or $S$ cannot use 1 color.

\item[color constraint] A proof that $S$ is \emph{color-constrained}. The demonstration depends on the specific \emph{color constraint} in question. There are a finite number of color constraints in \ref{color_constraints}.


\item[greedy smothering] An algorithm on $S_1$ and $S_2$ that produces a minimal smothering, if one exists.

%format g = "G"
%format s1 = "S_1"
%format s2 = "S_2"

\begin{code}
greedySmothering :: Graph a -> Set a -> Set a -> Maybe (Set (a, a))
greedySmothering g s1 s2 = undefined
\end{code}

\item[inclusive smothering] A \emph{maximal smothering} that better conveys the meaning (but loses the useful antonymy with \emph{minimal smothering}).

\item[maximal smothering] A \emph{smothering} that uses all edges. There is only one for each given $S_1$ and $S_2$. If there is a mutual smothering, then it is the inclusive smothering.

\item[minimal smothering] A \emph{smothering} that does not have any redundant edges. If the minimal smothering is not the only smothering, then there are multiple minimal smotherings.

\item[mutual smothering] Two subsets of vertices $S_1$ and $S_2$ have a \emph{mutual smothering} if:
    \begin{enumerate}
    \item $S_1$ \emph{smothers} $S_2$
    \item $S_2$ \emph{smothers} $S_1$
    \end{enumerate} Any complete bipartite graph contains a trivial mutual smothering.

\item[quasi-edge] A graph contains a \emph{quasi-edge} between its vertices $u$, $v$ if the following holds:
    \begin{enumerate}
    \item The graph does not contain the edge $(u \sim v)$.
    \item Any 3-coloring of the graph must color $u$ and $v$ different colors; A 3-coloring where $u$ and $v$ are the same color is impossible.
    \end{enumerate}

\item[quasi-edge-eligible] A subset of the graph's vertices $S$ is said to be \emph{quasi-edge-eligible} if $S$ \emph{smothers} some \emph{3-set} in $G$.

\item[smothers] a subset of vertices $A$ \emph{smothers} a subset of vertices $B$ when every vertex in $B$ has a neighbor in $A$.

\item[smothering] A set of edges which demonstrates that $A$ \emph{smothers} $B$.

\end{description}

\section{$\Delta$ - the base case}

\begin{code}
data Delta a = Delta {
    input :: Graph a,
    oddCycle :: Graph a
    }

allOddCycles :: forall a. (Ord a) => Graph a -> [Graph a]
allOddCycles g = do
    (v, _) <- Map.toList $ vertices g
    sequence <- go v v [v] (Set.singleton v) 1 (neighbors g)
    
    return $ fromWalks [sequence ++ [head sequence]]


    where
        go :: (Ord a) => a -> a -> [a] -> Set a -> Int -> Map a (Set a) -> [[a]]
        go s t path seen len n = if
            (s `Set.member` (n Map.! t)) && len >= 3 && odd len
            then path:rest
            else rest where
            rest = [ x |
                v <- Set.toList (n Map.! t),
                v `Set.notMember` seen,
                x <- go s v (v:path) (Set.insert v seen) (len + 1) n
                ]
\end{code}

But we don't care about all odd cycles. We can restrict our attention to the "smallest" odd cycles. Yes, it's true that this set of 5 vertices needs to use all 3 colors, since it contains a pentagon as a subgraph. But look! It also contains a triangle as subgraph. So, it's these 3 vertices in particular that need to use all 3 colors. And now it seems redundant to be talking about the original set of 5 vertices, because any subset that needs 3 colors is sufficient proof that the superset needs 3 colors.

\begin{code}
filterSubcycles :: Ord a => [Graph a] -> [Graph a]
filterSubcycles cycles = Map.elems $ Map.fromList basis
    where
        cycleSets = map (\x -> (Map.keysSet $ vertices x, x)) cycles

        basis = filter
            (\(a, b) -> not $ any (\(c, _) -> c `Set.isProperSubsetOf` a) cycleSets)
            cycleSets
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

    qVertices :: Set a,
    quasiEdges :: Set (TwoSet a),

    outputs :: Graph a
    }
\end{code}

Our proof circuit is a collection of gates.

\begin{code}
data Gate a
    = GateDelta (Delta a)
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

%format s1
%format s2
%format t1
%format t2

\begin{spec}
disjointMembers :: (Ord a) => Set a -> Set a -> (Set a, Set a)
disjointMembers s1 s2 = go (Set.toAscList s1) (Set.toAscList s2) [] [] where
    go [] []         t1 t2 = (Set.fromAscList t1, Set.fromAscList t2)
    go (x:xs) []     t1 t2 = go xs [] ((if x `Set.member` s2 then id else (x:)) t1) t2
    go [] (y:ys)     t1 t2 = go [] ys t1 ((if y `Set.member` s1 then id else (y:)) t2)
    go (x:xs) (y:ys) t1 t2 = go xs ys
        ((if x `Set.member` s2 then id else (x:)) t1)
        ((if y `Set.member` s1 then id else (y:)) t2)
\end{spec}

This implementation is verbose and the gains of a single pass are not yet known. Below is a terse two-pass version.

\begin{code}
disjointMembers :: (Ord a) => Set a -> Set a -> (Set a, Set a)
disjointMembers s1 s2 = (
    Set.filter (`Set.notMember` s2) s1,
    Set.filter (`Set.notMember` s1) s2
    )
\end{code}

Note, then we need a function that can find the mutual edges between $g_1$ and $g_2$.

This must return a set of pairs rather than a set of sets, because the membership in $us$ versus the membership in $vs$ must be preserved.
\begin{code}
mutualEdges :: Ord a => Graph a -> Set a -> Set a -> Set (a, a)
mutualEdges g us vs = Set.fromList [ (u, v) | 
    u <- Set.toList us,
    v <- Set.toList vs,
    hasEdge g u v
    ]
\end{code}

Find all complete bipartite subgraphs of a bipartite graph.

\begin{code}
completeBipartiteSubgraphs :: Ord a => Set (a, a) -> [Set (a, a)]
completeBipartiteSubgraphs s = [ allPairs ls rs |
    s' <- subsets s,
    let (ls, rs) = fromPairs s',
    l <- Set.toList ls,
    r <- Set.toList rs,
    (l, r) `Set.member` s
    ]
\end{code}

%format g = "G"

%format h_1 = "H_1"
%format h_2 = "H_2"

%format i_1 = "I_1"
%format i_2 = "I_2"

%format xiAll a b = a "\otimes " b
%format `xiAll` = "\otimes "

\begin{code}
xiAll :: Ord a => Graph a -> Graph a -> Set (Set (a, a))
xiAll h_1 h_2 = Set.fromList $ subsets (allPairs i_1 i_2)
    where
        (i_1, i_2) = disjointMembers (wVertices h_1) (wVertices h_2)
\end{code}

But for efficiency's sake, we need a version of this operator with respect to same base graph |g|.

%format xiLimited g a b = a "\otimes_{" g "} " b

\begin{code}
xiLimited :: Ord a => Graph a -> Graph a -> Graph a -> Set (Set (a, a))
xiLimited g h_1 h_2 = Set.filter (\s -> f s `Set.isSubsetOf` edges g) (h_1 `xiAll` h_2) where
    f = Set.map (uncurry wrap)
\end{code}

\section{The odd cycles as a multigraph}
Given a list of odd cycles, we can find out which odd cycles $\xi_g$ with each other, and in how many different ways. This is a multigraph. We call this multigraph $\aleph$. Any subtrees of $\aleph$ whose $g$s do not overlap is a valid |Graph|.

%format aleph = "\aleph "

Further subtrees will be $\beth$, $\gimel$, etc.

We need a multigraph which will allow us to store information on its edges (and, in general, distinguish multiple edges that connect the same two vertices.

\begin{code}
data Multigraph a b = Multigraph {
    mVertices :: Set a,
    mEdges :: Map (TwoSet a) (Set b)
    } deriving (Eq, Ord, Show)
\end{code}

Note that we could have omitted the field |mVertices| from the above definition, as long as every pair was accounted for in |mEdges|, but rediscovering all the vertices would be costly.

In fact, a multigraph is exactly isomorphic to a binary nondeterministic operator on a finite domain. We want to visualize this multigraph $\aleph$.

Thus we should be able to define |memoize|.

\begin{code}
memoize :: Ord a => (a -> a -> Set b) -> Set a -> Multigraph a b
memoize f domain = Multigraph {
    mVertices = domain,
    mEdges = Map.fromList [ (wrap u v, f u v) |
        let xs = Set.toList domain, u <- xs, v <- xs
        ]}
\end{code}

Applying |memoize| to $\otimes_G$

%format xiLimited g = "\otimes_{" g "}"
%format aleph g = "\aleph_{" g "}"
\begin{code}
aleph :: Ord a => Graph a -> Multigraph (Graph a) (Set (a, a))
aleph g = memoize (xiLimited g) (Set.fromList (filterSubcycles (allOddCycles g)))
\end{code}


\section{Show me what each of your objects looks like, and I can show you a multigraph visualization of it}

%format v1
%format v2

\begin{code}
renderMultigraph :: (Show a) => (a -> String) -> Multigraph a b -> String
renderMultigraph pngFilename m =
    "graph G {\n" ++
    "    overlap=false;\n" ++
    "    node [shape=none, label=\"\"];\n" ++
    vertexDecls ++
    edgeDecls ++
    "}\n"
    where
        vertexDecls = unlines $ map vDecl $ Set.toList $ mVertices m
        vDecl v = pngFilename v ++ " [image=\"" ++ pngFilename v ++ ".png\"];"

        edgeDecls = unlines $ map eDecl $ Map.toList $ mEdges m
        eDecl (k, s) = unlines $ map (\_ -> pngFilename v1 ++ " -- " ++ pngFilename v2 ++ ";") $ Set.toList s
            where
                (v1, v2) = unwrap k

isValidMultigraph :: Multigraph a b -> Bool
isValidMultigraph m = undefined where x = mEdges m
\end{code}


If the vertices of a graph are indexable, and have an x, y coordinate, then the graph is renderable.

\begin{code}
renderGraph :: Graph (Int, (Int, Int)) -> String
renderGraph g =
    "graph G {\n" ++
    "    overlap=false;\n" ++
    "    node [shape=circle, style=filled, fixedsize=true];\n" ++
    vertexDecls ++
    edgeDecls ++
    "}\n"
    where
        vertexDecls = unlines $ map vDecl $ Map.toList $ vertices g
        vDecl ((v, (x, y)), c) = show v ++ "[" ++ 
            "fillcolor=" ++ (if c == White then "white" else "black") ++ ", " ++
            "fontcolor=" ++ (if c == Black then "white" else "black") ++ ", " ++
            "pos=\"" ++ show x ++ "," ++ show y ++ "!\"" ++
            "];"

        edgeDecls = unlines $ map eDecl $ Set.toList $ edges g

        eDecl t = show v1 ++ " -- " ++ show v2 ++ ";"
            where
                ((v1, _), (v2, _)) = unwrap t
\end{code}

\section{Example Graphs}

\subsection{A tedious manual way to generate graphs}

\begin{code}
fromEdgeList :: (Ord a) => [(a, a)] -> Graph a
fromEdgeList edges = Graph {
    vertices = Map.fromList $ [(x, White) | (u, v) <- edges, x <- [u, v]],
    edges = Set.fromList $ map (uncurry wrap) edges 
    }

fromWalks :: (Ord a) => [[a]] -> Graph a
fromWalks walks = fromEdgeList $ do
    walk <- walks
    zip walk (tail walk)


fromWalks2 :: (Ord a, Ord b) => [(a, b)] -> [[a]] -> Graph (a, b)
fromWalks2 lookupTable walks = fromWalks $ map (map (\y -> (y, m Map.! y))) walks
    where
      m = Map.fromList lookupTable
\end{code}

\subsection{An easier way}
TODO

\section{All The Color Constraints}



\label{color_constraints}
\section{Just The Important Color Constraints}

Formula:
\[
    F_{01*}(G) = \{S \mid S \subseteq V(G), \chi(S) \not\in 1, \chi(S) \in 2, (\chi(S) \in 3 \lor \chi(S) \not\in 3) \}
\]

\begin{code}
data N = O | I | KleeneStar

f :: Graph g -> (N, N, N) -> Set (Set a)
f g (KleeneStar, KleeneStar, KleeneStar) = allSubsets (vertices g)

\end{code}

\begin{description}
\item[$F_{111}$]
\end{description}


\section{A redefinition of the Hajos construction using isochromacies}

\subsection{Some sample graphs}

\subsubsection{Three pentagons (or three triangles -- it depends how you look at it)}
\begin{code}
triplePentagon :: Graph (Int, (Int, Int))
triplePentagon = fromWalks2 [
    (0, (4, 2)),
    (1, (2, 3)),
    (2, (6, 3)),
    (3, (4, 1)),
    (4, (3, 4)),
    (5, (5, 4)),
    (6, (8, 3)),
    (7, (7, 0)),
    (8, (1, 0)),
    (9, (0, 3))] [
    [0, 1, 4, 5, 2, 0,2, 6, 7, 3, 0, 3, 8, 9, 1, 0, 1, 4, 5],
    [5, 6, 7, 8, 9, 4]]
\end{code}

\subsection{Three triangles}
The canonical example to demonstrate the special case of associativity.

\begin{code}
threeTriangles :: Graph (Int, (Int, Int))
threeTriangles = fromWalks2 [
    (0, (0, 0)),
    (1, (8, 0)),
    (2, (1, 1)),
    (3, (7, 1)),
    (4, (2, 0)),
    (5, (6, 0)),
    (6, (3, 0)),
    (7, (5, 0)),
    (8, (4, 1))
    ] [[0, 2, 4, 6, 8, 7, 5, 3, 1, 5, 7, 6, 4, 0]]
\end{code}

\subsection{Two triangles}
The smallest $\Xi$.

\begin{code}
twoTriangles :: Graph (Int, (Int, Int))
twoTriangles = fromWalks2 [
    (0, (0, 2)),
    (1, (0, 0)),
    (2, (1, 1)),
    (3, (2, 1)),
    (4, (3, 2)),
    (5, (3, 0))
    ] [[2, 1, 0, 2, 3, 4, 5, 3]]
\end{code}

\section{Finding all spanning forests of a multigraph}

\subsection{Under the condition that committing to an edge removes other edges}

More generally, the tree might change.


\section{Set Theory Functions \& Isomorphisms}
\begin{code}
subsets :: Ord a => Set a -> [Set a]
subsets s = if Set.null s
    then [Set.empty]
    else 
      let (x, xs) = Set.deleteFindMin s in do
      s' <- subsets xs
      [s', Set.insert x s']

fromPairs :: Ord a => Set (a, a) -> (Set a, Set a)
fromPairs pairs = let (x, y) = go (Set.toAscList pairs) [] [] in (Set.fromList x, Set.fromList y) where
    go [] ls rs = (ls, rs)
    go ((l, r):rest) ls rs = go rest (l:ls) (r:rs)

allPairs :: Ord a => Set a -> Set a -> Set (a, a)
allPairs xs ys = Set.fromList $ [(x, y) | x <- Set.toList xs, y <- Set.toList ys]
\end{code}

Consider a set that can contain exactly two values.
\begin{code}
newtype TwoSet a = TwoSet (Set a) deriving (Eq, Ord)


instance Show a => Show (TwoSet a) where
    show (TwoSet x) = if Set.size x /= 2 then trace ("Found one: " ++ show x) (show (unwrap (TwoSet x))) else show x


wrap :: (Ord a) => a -> a -> TwoSet a
wrap x y = TwoSet $ Set.insert y $ Set.singleton x

unwrap :: Show a => TwoSet a -> (a, a)
unwrap (TwoSet x) = if Set.size x /= 2 then trace ("Problem: " ++ show x) undefined else (l, r) where [l, r] = Set.toList x

\end{code}

Here we consider a set with only two values.

\section{Visualizing proof circuits with Graphviz}

The @dot@ layout engine is great for laying out tiered circuits, which is exactly what these graph proof steps turn out to be. First, each graph is rendered with @neato@ and saved as an indexed image. Then, each |Graph| is associated with its image, and a directed graph (a circuit) is built connecting input graphs to gates, and gates to output graphs.

\subsection{Writing Graphviz files}

\begin{code}
writeAleph :: Graph (Int, (Int, Int)) -> IO ()
writeAleph g = do
    writeFile "original.dot" (renderGraph g)

    mapM_
        (\(oddCycle, f) -> writeFile f (renderGraph oddCycle))
        (Map.toList pngs)

    writeFile "final.dot" (renderMultigraph (pngs Map.!) m)
      where
        m = aleph g
        pngs = Map.fromList $ zip
            (Set.toList (mVertices m))
            ["bwgraph-" ++ show i ++ "-images.dot" | i <- [0..]]


\end{code}

\section{Putting it all together}

\begin{code}
main :: IO ()
main = writeAleph twoTriangles
\end{code}

\end{document}