import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.List (sort, intercalate)

import Debug.Trace (trace)

(\/) :: (Ord a) => Set a -> Set a -> Set a
(\/) = Set.union

neighbors :: (Ord a) => Graph a -> Map a (Set a)
neighbors g = Map.fromSet (\v ->
    Set.filter (hasEdge g v) (vertices g)) (vertices g)

oddCycleGates :: (Ord a) => Graph a -> Set (Gate (BWGraph a))
oddCycleGates g = Set.fromList $ map (Base . mkCycle) $ oddCycles g

oddCycles :: Ord a => Graph a -> [[a]]
oddCycles g = Map.elems
    $ Map.fromList
    $ map (\cycle -> (Set.fromList cycle, cycle))
    $ allOddCycles g

allOddCycles :: (Ord a) => Graph a -> [[a]]
allOddCycles g = do
    v <- Set.toList $ vertices g
    go g v v [v] (Set.singleton v) 1 (neighbors g)

nubOrd :: (Ord a) => [a] -> [a]
nubOrd = Set.toList . Set.fromList

go :: (Ord a) => Graph a -> a -> a -> [a] -> Set a -> Int -> Map a (Set a) -> [[a]]
go g s t path seen len n = if
    (s `Set.member` (n Map.! t)) && len >= 3 && odd len
    then path:rest
    else rest where
      rest = [ x |
        v <- Set.toList (n Map.! t),
        v `Set.notMember` seen,
        x <- go g s v (v:path) (Set.insert v seen) (len + 1) n
        ]

partitions :: (Ord a) => Set a -> [(Set a, Set a)]
partitions s = if null s then [(Set.empty, Set.empty)] else do
    let (first, rest) = Set.deleteFindMax s
    (l, r) <- partitions rest
    [(Set.insert first l, r), (l, Set.insert first r)]

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint x y = null $ x `Set.intersection` y

data Graph a = Graph {
    vertices :: Set a,
    edges :: Set (a, a)
    }

fromEdgeList :: (Ord a) => [(a, a)] -> Graph a
fromEdgeList edges = Graph {
    vertices = Set.fromList $ [x | (u, v) <- edges, x <- [u, v]],
    edges = Set.fromList $ [(min u v, max u v) | (u, v) <- edges]
    }


fromWalks :: (Ord a) => [[a]] -> Graph a
fromWalks walks = fromEdgeList $ do
    walk <- walks
    zip walk (tail walk)


data BW = Black | White deriving (Eq, Ord, Show)

data BWGraph a = BWGraph {
    bwVertices :: Map a BW,
    bwEdges :: Set (a, a)
    } deriving (Eq, Ord)

data Gate a = Base a | Rec (Set a) a deriving (Eq, Ord, Show)

out :: Gate a -> a
out (Base x)  = x
out (Rec _ x) = x

repr :: (Show a) => (a, BW) -> String
repr (x, White) = show x ++ "w"
repr (x, Black) = show x ++ "b"

instance (Show a, Ord a) => (Show (BWGraph a)) where
    show g = show (
        unwords $ map repr $ sort $ Map.toList (bwVertices g),
        show (bwEdges g)
        )

wVertices :: (Ord a) => BWGraph a -> Set a
wVertices bwGraph = Map.keysSet
    $ Map.filter (== White)
    $ bwVertices bwGraph

hasEdge :: (Ord a) => Graph a -> a -> a -> Bool
hasEdge g u v = let (u', v') = (min u v, max u v) in
    (u', v') `Set.member` edges g

mkCBS :: (Ord a) => Set a -> Set a -> Set (a, a)
mkCBS us vs = Set.fromList $ [(u', v') |
    u <- Set.toList us,
    v <- Set.toList vs,
    let (u', v') = (min u v, max u v)
    ]

isCBS :: (Ord a) => Graph a -> Set a -> Set a -> Bool
isCBS g b1 b2 = and [hasEdge g u v | u <- Set.toList b1, v <- Set.toList b2]

mkCycle :: (Ord a) => [a] -> BWGraph a
mkCycle vs = white
    $ fromEdgeList
    $ (head vs, last vs):zip vs (tail vs)

white :: (Ord a) => Graph a -> BWGraph a
white g = BWGraph {bwVertices=Map.fromSet (const White) (vertices g), bwEdges=edges g}

merge :: (Ord a) => (BWGraph a, Set a) -> (BWGraph a, Set a) -> BWGraph a
merge (b1, i1) (b2, i2) = BWGraph {
    bwVertices = Map.mapWithKey (\v bw -> if v `Set.member` (i1 \/ i2) then Black else bw) (Map.union (bwVertices b1) (bwVertices b2)),
    bwEdges = bwEdges b1 \/ bwEdges b2 \/ mkCBS i1 i2
    }

e :: (Ord a) => Int -> Graph a -> [BWGraph a]
e 0 graph = map mkCycle $ oddCycles graph
e n graph = nubOrd $ [ x |
    b1 <- en_1,
    b2 <- en_1,
    x <- xi graph b1 b2
    ] where en_1 = e (n - 1) graph

xi :: (Ord a) => Graph a -> BWGraph a -> BWGraph a -> [BWGraph a]
xi g b1 b2 = [merge (b1, i1) (b2, i2) |
    b1 /= b2,
    (o1, i1) <- partitions $ wVertices b1,
    not $ null i1,
    (o2, i2) <- partitions $ wVertices b2,
    not $ null i2,
    disjoint i1 i2,
    disjoint o1 i2,
    disjoint o2 i1,
    isCBS g i1 i2
    ]

calc :: (Ord a) => Graph a -> Set (Gate (BWGraph a)) -> Set (Gate (BWGraph a)) -> Set (Gate (BWGraph a))
calc graph known new = if Set.null result
    then known \/ new
    else trace (show (Set.size result)) $ calc graph (known \/ new) result
    where
      result = Set.fromList $ [ Rec (Set.fromList [b1, b2]) x |
        b1 <- map out $ Set.toList known,
        b2 <- map out $ Set.toList new,
        x <- xi graph b1 b2
        ]

calcAll :: (Ord a) => Graph a -> Set (Gate (BWGraph a))
calcAll graph = calc graph oc oc where
    oc = oddCycleGates graph

toGraphviz :: BWGraph Integer -> String
toGraphviz g = "graph G {\n" ++
    "    node [shape=circle, style=filled];\n" ++

    unlines (map (\(v, bw) -> "    " ++ show v ++ " [fillcolor=" ++ (if bw == White then "white" else "black") ++ " fontcolor=" ++ (if bw == Black then "white" else "black") ++ "];") (Map.toList (bwVertices g))) ++

    unlines (map (\(u, v) -> "    " ++ show u ++ " -- " ++ show v ++ ";") (Set.toList (bwEdges g))) ++


    "}"

{-
maxBy :: (Ord b) => (a -> b) -> [a] -> a
maxBy f [x] = x
maxBy f (x:xs) = let y = maxBy f xs in
    if f x >= f y then x else y
-}

writeAll :: Graph Integer -> IO ()
writeAll g = do
    mapM_
        (\(b, i) -> writeFile
            ("bwgraph-" ++ i ++ "-images.dot")
            (toGraphviz b))
        (Map.toList names)

    writeFile "final.dot" (toDigraph names gates)
      where
        names = Map.fromList
            $ (\x -> zip x (map show [0..]))
            $ map out
            $ Set.toList gates

        bretts = Map.fromList $ (\x -> zip x (map show [0..])) $ Set.toList gates

        gates = calcAll g



-- pick :: Graph Integer -> IO ()
-- pick g = putStrLn $ toGraphviz $ maxBy (\g -> Map.size $ Map.filter (== Black) $ bwVertices g) $ calcAll g

brett = fromEdgeList [
    (0, 1),
    (0, 2),
    (1, 2),
    (1, 3),
    (2, 4),
    (3, 4),
    (3, 5),
    (4, 5)
    ]

tripleBowtie = fromEdgeList [
    (0, 1),
    (0, 2),
    (1, 2),
    (2, 3),
    (3, 4),
    (3, 5),
    (4, 5),
    (5, 6),
    (6, 7),
    (6, 8),
    (7, 8)
    ]

tripleBowtieAndExtra = fromEdgeList [
    (0, 1),
    (0, 2),
    (1, 2),
    (2, 3),
    (3, 4),
    (3, 5),
    (4, 5),
    (5, 6),
    (6, 7),
    (6, 8),
    (7, 8),
    (1, 9),
    (4, 9),
    (7, 9),
    (10, 9),
    (11, 9)
    ]

boxedDiamond = fromWalks [
    [0, 1, 2, 3, 4, 5, 0],
    [0, 2, 3, 1]]

toDigraph :: (Ord a) => Map a String -> Set (Gate a) -> String
toDigraph graphs gates =
    "digraph G {\n" ++
    "    overlap=false;\n" ++
    "    node [shape=none, label=\"\"];\n" ++

    unlines (map (\(_, name) -> "    " ++ name ++ " [image=\"bwgraph-" ++ name ++ "-images.dot.png\"];" ) $ Map.toList graphs) ++

    "    node [shape=circle, label=\"Ξ\"];\n" ++

    unlines (zipWith (\g i -> digraphString g i (graphs Map.!)) (Set.toList gates) [0..]) ++

    "}"

digraphString :: Gate a -> Int -> (a -> String) -> String
digraphString (Base _) _  _ = ""
-- with intermediate xi gate
digraphString (Rec ins out) index name = "    {" ++ intercalate ", " (map name (Set.toList ins)) ++ "} -> xi" ++ show index ++ " -> " ++ name out ++ ";"
-- without
-- digraphString (Rec ins out) index name = "    {" ++ intercalate ", " (map name (Set.toList ins)) ++ "} -> " ++ name out ++ ";"



main :: IO ()
main = writeAll boxedDiamond
