import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.List (sort, intercalate, any, nubBy)

import Debug.Trace (trace)

main :: IO ()
main = writeAll2 quasiEdge

(\/) :: (Ord a) => Set a -> Set a -> Set a
(\/) = Set.union

(.<) :: (Ord a) => a -> Set a -> Bool
(.<) = Set.member

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
    (s .< (n Map.! t)) && len >= 3 && odd len
    then path:rest
    else rest where
      rest = [ x |
        v <- Set.toList (n Map.! t),
        v `Set.notMember` seen,
        x <- go g s v (v:path) (Set.insert v seen) (len + 1) n
        ]

partitions :: (Ord a) => Set a -> [(Set a, Set a)]
partitions s = if Set.null s then [(Set.empty, Set.empty)] else do
    let (first, rest) = Set.deleteFindMax s
    (l, r) <- partitions rest
    [(Set.insert first l, r), (l, Set.insert first r)]

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint x y = Set.null $ x `Set.intersection` y

mutuallyDisjoint :: (Ord a) => Set a -> Set a -> Set a -> Bool
mutuallyDisjoint x y z =
    disjoint x y &&
    disjoint y z &&
    disjoint z x

data Graph a = Graph {
    vertices :: Set a,
    edges :: Set (a, a)
    }

fromEdgeList :: (Ord a) => [(a, a)] -> Graph a
fromEdgeList edges = Graph {
    vertices = Set.fromList $ [x | (u, v) <- edges, x <- [u, v]],
    edges = Set.fromList $ [(min u v, max u v) | (u, v) <- edges]
    }

fromWalks2 :: (Ord a, Ord b) => [(a, b)] -> [[a]] -> Graph (a, b)
fromWalks2 lookupTable walks = fromWalks $ map (\x -> map (\y -> (y, m Map.! y)) x) walks
    where
      m = Map.fromList lookupTable
      

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

isRec :: Gate a -> Bool
isRec (Base _) = False
isRec (Rec _ _) = True

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
    
bVertices :: (Ord a) => BWGraph a -> Set a
bVertices bwGraph = Map.keysSet
    $ Map.filter (== Black)
    $ bwVertices bwGraph

hasEdge :: (Ord a) => Graph a -> a -> a -> Bool
hasEdge g u v = let (u', v') = (min u v, max u v) in
    (u', v') .< edges g

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

merge :: (Ord a) => (BWGraph a, Set a, Set a) -> (BWGraph a, Set a, Set a) -> BWGraph a
merge (b1, o1, i1) (b2, o2, i2) = BWGraph {
    bwVertices = Map.mapWithKey (\v bw -> if v .< (i1 \/ i2) then Black else if v .< (o1 \/ o2) then White else bw) (Map.union (bwVertices b1) (bwVertices b2)),
    bwEdges = bwEdges b1 \/ bwEdges b2 \/ mkCBS i1 i2
    }
    
-- 

e :: (Ord a) => Int -> Graph a -> [BWGraph a]
e 0 graph = map mkCycle $ oddCycles graph
e n graph = nubOrd $ [ x |
    b1 <- en_1,
    b2 <- en_1,
    x <- xi graph known3s b1 b2
    ] where
        en_1 = e (n - 1) graph
        known3s = undefined

xi :: (Ord a) => Graph a -> [Set a] -> BWGraph a -> BWGraph a -> [BWGraph a]
xi g known3s b1 b2 = map (uncurry merge)
    -- uncomment to only consider largest bipartites
    -- -- $ allMaxBy (\((_, i1), (_, i2)) -> Set.size i1 + Set.size i2)

    $ [((b1, o1, i1), (b2, o2, i2)) |
        -- mutuallyDisjoint (bVertices b1) (bVertices b2) (wVertices b1),
        -- mutuallyDisjoint (bVertices b1) (bVertices b2) (wVertices b2),
        (o1, i1) <- partitions $ wVertices b1,
        not $ Set.null i1,
        (o2, i2) <- partitions $ wVertices b2,
        not $ Set.null i2,
        
        -- This may be as powerful as the last two (commented (?)) conditions
        disjoint o1 (bVertices b2),
        disjoint o2 (bVertices b1),
        
        -- let i1i2 = i1 \/ i2
        -- Set.size i1i2 > (max (Set.size i1) (Set.size i2))
        
        mutuallyDisjoint i1 i2 o1,
        mutuallyDisjoint i1 i2 o2,
        isCBS g i1 i2,

        let o1o2 = o1 \/ o2 -- ,
        -- comment / uncomment one or both
        -- o1o2 `notElem` known3s,
        -- not $ any (`Set.isProperSubsetOf` o1o2) known3s
        ]



-- Split up a pair of triply-disjoint sets into 12 disjoint sets (some intersections MUST be empty)
-- This can later be adapted to assert the emptiness of certain intersections
splitUp
    :: (Set a, Set a, Set a)
    -> (Set a, Set a, Set a)
    -> (
        Set a, Set a, Set a, Set a,
        Set a, Set a, Set a, Set a,
        Set a, Set a, Set a, Set a,
        Set a, Set a, Set a
    )
splitUp (a, b, c) (d, e, f) = undefined


splitUpHelper :: [a] -> (Set a, Set a, Set a) -> (Set a, Set a, Set a) ->
    (   [a], [a], [a], [a],
        [a], [a], [a], [a],
        [a], [a], [a], [a],
        [a], [a], [a]
    ) -> (
        [a], [a], [a], [a],
        [a], [a], [a], [a],
        [a], [a], [a], [a],
        [a], [a], [a]
    )
splitUpHelper [] _ _ soFar = soFar
splitUpHelper (x:xs) (a, b, c) (d, e, f) soFar = undefined

    where
     (ad, ae, af, a_,
      bd, be, bf, b_,
      cd, ce, cf, c_,
      d_, e_, f_) = splitUpHelper xs (a, b, c) (d, e, f) soFar

data Out a = DEF a a a

leq :: (Ord a) => Gate (BWGraph a) -> Gate (BWGraph a) -> Bool
leq g1 g2 = wVertices (out g1) `Set.isProperSubsetOf` wVertices (out g2)


calc :: (Ord a) => Int -> Graph a -> Set (Gate (BWGraph a)) -> Set (Gate (BWGraph a)) -> Set (Gate (BWGraph a))
calc 0 graph known new = known \/ new
calc n graph known new = if Set.null result
    then known_and_new
    else trace (show (Set.size result)) $ calc (n - 1) graph known_and_new result
    where
      result = Set.fromList
          $ filter (\x -> not $ any (\y -> y `leq` x) iteration) -- keep only the smallest
          $ iteration
      
      iteration = [ Rec (Set.fromList [b1, b2]) x |
        b1 <- map out $ Set.toList known,
        b2 <- map out $ Set.toList new,
        x <- xi graph (Set.toList (Set.map (wVertices . out) known_and_new)) b1 b2
        ]

      known_and_new = known \/ new

isInput :: (Ord a) => a -> Gate a -> Bool
isInput x (Base _) = False
isInput x (Rec ins _) = x .< ins

isUnusedOddCycle :: (Ord a) => Gate a -> Set (Gate a) -> Bool
isUnusedOddCycle (Rec _ _) _ = False
isUnusedOddCycle (Base g)  s = not $ any (isInput g) $ Set.toList s

justMyFamily :: (Ord a) => Map (Set a) (Set a) -> a -> Map (Set a) (Set a)
justMyFamily m x = case Map.minViewWithKey (Map.filter (\outs -> x .< outs) m) of
    Just ((ins, outs), _) -> Map.insert ins outs $ Map.unions $ map (justMyFamily m) $ Set.toList ins
    Nothing -> Map.empty


calcAll :: (Ord a) => Graph a -> Set (Gate (BWGraph a))
calcAll graph = Set.filter (\x -> not $ isUnusedOddCycle x gates) gates where
    gates = calc 6 graph oc oc

    oc = Set.fromList
        $ nubBy leq
        $ Set.toList
        $ oddCycleGates graph

toGraphviz :: BWGraph Integer -> String
toGraphviz g = "graph G {\n" ++
    "    overlap=false;\n" ++
    "    node [shape=circle, style=filled, fixedsize=true];\n" ++

    unlines (map (\(v, bw) -> "    " ++ show v ++ " [fillcolor=" ++ (if bw == White then "white" else "black") ++ " fontcolor=" ++ (if bw == Black then "white" else "black") ++ "];") (Map.toList (bwVertices g))) ++

    unlines (map (\(u, v) -> "    " ++ show u ++ " -- " ++ show v ++ ";") (Set.toList (bwEdges g))) ++


    "}"
    
toGraphviz2 :: BWGraph (Integer, (Integer, Integer)) -> String
toGraphviz2 g = "graph G {\n" ++
    "    overlap=false;\n" ++
    "    node [shape=circle, style=filled, fixedsize=true];\n" ++

    unlines (map (\((v, (x, y)), bw) -> "    " ++ show v ++ " [fillcolor=" ++ (if bw == White then "white" else "black") ++ ", fontcolor=" ++ (if bw == Black then "white" else "black") ++ ", pos=\"" ++ show x ++ "," ++ show y ++ "!\"];") (Map.toList (bwVertices g))) ++

    unlines (map (\((u, _), (v, _)) -> "    " ++ show u ++ " -- " ++ show v ++ ";") (Set.toList (bwEdges g))) ++


    "}"

toDigraph :: (Ord a) => {-(a -> Bool) ->-} Map a String -> Set (Gate a) -> String
toDigraph {-criteria-} graphs gates =
    "digraph G {\n" ++
    "    overlap=false;\n" ++
    --"    splines=ortho;\n" ++
    "    node [shape=none, label=\"\"];\n" ++

    "    original [image=\"original.dot.png\"];\n" ++
    "    penta [shape=triangle, label=\"⬠\", style=filled, fillcolor=gray];\n" ++

    -- visual version: the BW graphs are fully rendered
    unlines (map (\(_, name) -> "    " ++ name ++ " [image=\"bwgraph-" ++ name ++ "-images.dot.png\"];" ) $ Map.toList graphs) ++
    
    "    original -> penta -> {" ++ (intercalate ", " oddCycleNames) ++ "}\n" ++
    

    -- TODO (REDO) abstract version: just the symbols, not graphs
    -- unlines (map (\(_, name) -> "    " ++ name ++ " [shape=circle, label=\"" ++ name ++ "\"];" ) $ Map.toList graphs) ++


    "    node [shape=square, label=\"Ξ\", style=filled, fillcolor=gray];\n" ++

    unlines (zipWith (\(ins, outs) index -> digraphString ins outs index (graphs Map.!)) pairs [0..]) ++

    "}" where
      oddCycleNames = map (\(Base x) -> graphs Map.! x) $ filter (not . isRec) $ Set.toList gates
      
      pairs = Map.toList (asMap gates)
      -- uncomment (and add `criteria` parameter) to clean up the tree -- just view one random BWGraph's family
      
      {-
      pairs = Map.toList
          $ justMyFamily (asMap gates)
          $ head
          $ filter criteria
          $ map out (Set.toList gates)
      -}



digraphString :: Set a -> Set a -> Int -> (a -> String) -> String
-- with intermediate xi gate
digraphString ins outs index name = "    " ++ group ins ++  " -> xi" ++ (show index) ++ " -> " ++ group outs ++ ";"
     where
       group vertices = "{" ++ intercalate ", " (map name $ Set.toList vertices) ++ "}"
       


toMultimap :: (Ord a, Ord b) => [(a, b)] -> Map a (Set b)
toMultimap pairs = Map.fromListWith (\/) $ map (\(x, y) -> (x, Set.singleton y)) pairs

asMap :: (Ord a) => Set (Gate a) -> Map (Set a) (Set a)
asMap gates = toMultimap
    $ map (\(Rec ins out) -> (ins, out))
    $ filter isRec
    $ Set.toList gates

{-
maxBy :: (Ord b) => (a -> b) -> [a] -> a
maxBy f [x] = x
maxBy f (x:xs) = let y = maxBy f xs in
    if f x >= f y then x else y
-}

allMaxBy :: (Ord b) => (a -> b) -> [a] -> [a]
allMaxBy f [] = []
allMaxBy f (x:xs) = case allMaxBy f xs of
    [] -> [x]
    (y:ys) -> case compare (f x) (f y) of
        LT -> y:ys
        EQ -> x:y:ys
        GT -> [x]

writeAll :: Graph Integer -> IO ()
writeAll g = do
    writeFile "original.dot" (toGraphviz (white g))

    mapM_
        (\(b, i) -> writeFile
            ("bwgraph-" ++ i ++ "-images.dot")
            (toGraphviz b))
        (Map.toList names)

    writeFile "final.dot" (toDigraph names gates) -- or add this criteria: (\x -> (Set.size (wVertices x)) == 2)
      where
        names = Map.fromList
            $ (\x -> zip x (map show [0..]))
            $ map out
            $ Set.toList gates

        bretts = Map.fromList $ (\x -> zip x (map show [0..])) $ Set.toList gates

        gates = calcAll g


-- use when you have the positions specified
writeAll2 :: Graph (Integer, (Integer, Integer)) -> IO ()
writeAll2 g = do
    writeFile "original.dot" (toGraphviz2 (white g))

    mapM_
        (\(b, i) -> writeFile
            ("bwgraph-" ++ i ++ "-images.dot")
            (toGraphviz2 b))
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

tripleBowtie = fromWalks2 [
    (0, (0, 0)),
    (1, (8, 0)),
    (2, (1, 1)),
    (3, (7, 1)),
    (4, (2, 0)),
    (5, (6, 0)),
    (6, (3, 0)),
    (7, (5, 0)),
    (8, (4, 1))
    ] $ [[0, 2, 4, 6, 8, 7, 5, 3, 1, 5, 7, 6, 4, 0]]

beginner = fromWalks [
    [0, 1, 2, 3, 4, 5], [0, 2], [3, 5]
    ]

tripleBowtieAndExtra = fromWalks2 [
    (0, (0, 3)),
    (1, (2, 3)),
    (2, (3, 3)),
    (3, (5, 3)),
    (4, (6, 3)),
    (5, (8, 3)),
    (6, (1, 2)),
    (7, (4, 2)),
    (8, (7, 2)),
    (9, (4, 1)),
    (10, (3, 0)),
    (11, (5, 0))] $ [
    [0, 6, 1, 2, 7, 3, 4, 8, 5, 8, 9, 11, 10, 9, 6, 1, 0], [2, 3, 4, 5], [7, 9]] 

boxedDiamond = fromWalks [
    [0, 1, 2, 3, 4, 5, 0],
    [0, 2, 3, 1]]

twoPentagons = fromWalks [
    [0, 1, 2, 3, 4, 5, 6, 7, 0],
    [1, 5], [2, 6],
    [1, 8, 6], [2, 9, 5]
    ]

quadTie = fromWalks [
    [0, 1, 2, 0, 4, 5, 6, 4],
    [5, 7, 8, 9, 7],
    [6, 10, 11, 12, 10]
    ]

doubleTree = fromWalks [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0],
    [1, 11], [10, 8], [2, 4], [5, 7], [1, 7], [2, 8]
    ]

diamondDiamondBackedge = fromWalks [
    [0, 1, 2, 3, 4, 5, 6, 0, 2, 1, 3, 5, 4, 6]]
    
    
rearrangementLemma = fromWalks [
    [0, 1, 2, 0, 3, 4, 5, 6, 7, 3, 8, 9, 10, 11, 3],
    [6, 10]]
    

quasiEdge = fromWalks2 [
    (0, (0, 3)),
    (1, (4, 3)),
    (2, (1, 5)),
    (3, (3, 1)),
    (4, (3, 5)),
    (5, (1, 1)),
    (6, (2, 4)),
    (7, (2, 2)),
    (8, (2, 8))] $ [
    [0, 2, 6, 4, 1, 3, 7, 5, 0], [0, 8, 1], [2, 4], [5, 3], [6, 7]]
    
quasiEdgeWithoutExtra = fromWalks [
    [0, 1, 2, 3, 4, 5, 6, 7, 0],
    [1, 3], [2, 6], [7, 5]]
    
k4 = fromWalks [
    [0, 1, 2, 3, 0], [1, 3], [2, 0]]
    
    
deniz = fromWalks [
    [0, 1, 2, 3, 4, 5, 0], [1, 3, 5], [4, 2]]
    
    
brett2 = fromWalks [
    [0, 1, 2, 3, 4, 5, 6, 7, 0], [0, 2, 4, 6, 0], [7, 8, 9, 7], [3, 10, 11, 3]]


diamond = fromWalks2 [
        (0, (0, 1)),
        (1, (1, 2)),
        (2, (1, 0)),
        (3, (2, 1))
    ] $ [[0, 1, 2, 3, 1], [0, 2]]
    
iceCream = fromWalks [[0, 1, 2, 3, 4 ,5, 0], [5, 1]]

threeWhite = fromWalks2 [
        (0, (0, 3)),
        (1, (2, 3)),
        (2, (0, 1)),
        (3, (1, 2)),
        (4, (2, 1)),
        (5, (1, 0))
    ] $ [
        [0, 1, 4, 5, 3, 2, 0], [2, 5], [3, 4]]

wheel5 = fromWalks2 [
        (0, (2, 3)),
        (1, (4, 2)),
        (2, (3, 0)),
        (3, (1, 0)),
        (4, (0, 2)),
        (5, (2, 1))
    ] $ [
        [0, 1, 2, 3, 4, 0], [5, 0], [5, 1], [5, 2], [5, 3], [5, 4]]
    

moserSpindle = fromWalks2 [
    (0, (0, 0)),
    (1, (1, 2)),
    (2, (2, 1)),
    (3, (3, 3)),
    (4, (4, 1)),
    (5, (5, 2)),
    (6, (6, 0))
    ] $ [[0, 1, 2, 3, 4, 5, 6, 0], [0, 2, 1, 3, 5, 4, 6]]
    
    
fish = fromWalks2 [
    (0, (0, 2)),
    (1, (1, 4)),
    (2, (2, 3)),
    (3, (3, 2)),
    (4, (4, 4)),
    (5, (4, 0)),
    (6, (2, 1)),
    (7, (1, 0))] $ [
        [0, 1, 2, 3, 4, 5, 3, 6, 7, 0],
        [2, 6]]
        
fishWithCycle = fromWalks2 [
    (0, (0, 2)),
    (1, (1, 4)),
    (2, (2, 3)),
    (3, (3, 2)),
    (4, (4, 4)),
    (5, (4, 0)),
    (6, (2, 1)),
    (7, (1, 0))] $ [
        [0, 1, 2, 3, 4, 5, 3, 6, 7, 0],
        [2, 6], [1, 4], [7, 5]]
        
        
bug = fromWalks2 [
    (0, (1, 2)),
    (1, (3, 2)),
    (2, (0, 1)),
    (3, (2, 1)),
    (4, (4, 1)),
    (5, (0, 0)),
    (6, (2, 0)),
    (7, (4, 0))] $ [
    [0, 1, 4, 7, 6, 5, 2, 0, 3, 1, 3, 6]]
    
    
tripleSpindle = fromWalks2 [
    (0, (1, 4)),
    (1, (5, 4)),
    (2, (0, 3)),
    (3, (6, 3)),
    (4, (2, 3)),
    (5, (4, 3)),
    (6, (1, 1)),
    (7, (5, 1)),
    (8, (3, 2)),
    (9, (3, 0))] $ [
    [0, 1, 3, 5, 7, 9, 8, 6, 2, 4, 0, 1, 5, 3, 7, 8, 9, 6, 4, 2, 0]]
