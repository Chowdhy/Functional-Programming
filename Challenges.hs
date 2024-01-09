{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (TileEdge(..),Tile(..),Puzzle,isPuzzleComplete,
                   Rotation(..),solveCircuit,
                   LExpr(..),Bind(..),prettyPrint,parseLetx,
                   LamExpr(..),letEnc,compareRedn)
                    where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Parsing
import Data.List (nub, sort, find)
import Data.Maybe (isJust,isNothing, fromJust)
import Data.Char (isSpace)

-- Challenge 1
-- Testing Circuits

data TileEdge = North | East | South | West  deriving (Eq,Ord,Show,Read)
data Tile = Source [ TileEdge ] | Sink [ TileEdge ] | Wire [ TileEdge ]  deriving (Eq,Show,Read)
type Puzzle = [ [ Tile ] ]

-- New type defined to simplify type declarations
type Coordinate = (Int, Int)

isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete p | not $ validPuzzle p = False
                   | otherwise = allWiresConnected p && validSources p && validSinks p

-- | Checks if a tile is valid according to the coursework tile specifications.
validTile :: Tile -> Bool
validTile (Source es) = not $ null (nub es)
validTile (Sink es) = not $ null (nub es)
validTile (Wire es) = length (nub es) /= 1

-- | Checks that all rows of a puzzle are of equal length.
equalLengthRows :: Puzzle -> Bool
equalLengthRows [] = True
equalLengthRows (r:[]) = True
equalLengthRows (r1:r2:rs) | length r1 == length r2 = equalLengthRows (r2:rs)
                           | otherwise = False

-- | Checks that a puzzle is valid by comparing row lengths and making sure that all individual tiles are valid.
validPuzzle :: Puzzle -> Bool
validPuzzle p = equalLengthRows p && all validTile [t | ts <- p, t <- ts]

-- | When given a Tile's coordinates and a list of its TileEdges, returns a list of all adjacent coordinates.
getAdjacent :: [TileEdge] -> Coordinate -> Puzzle -> [Coordinate]
getAdjacent [] _ _ = []
getAdjacent (North:es) c@(x, y) p = (x, y - 1) : getAdjacent es c p
getAdjacent (East:es) c@(x, y) p = (x + 1, y) : getAdjacent es c p
getAdjacent (South:es) c@(x, y) p = (x, y + 1) : getAdjacent es c p
getAdjacent (West:es) c@(x, y) p = (x - 1, y) : getAdjacent es c p

isSink :: Tile -> Bool
isSink (Sink _) = True
isSink _ = False

isSource :: Tile -> Bool
isSource (Source _) = True
isSource _ = False

isWire :: Tile -> Bool
isWire (Wire _) = True
isWire _ = False

-- | Returns True if the Tile at the starting Coordinate is connected to a Tile that satisfies the given predicate.
-- The function traverses the specified Puzzle until it finds a satisfying Tile, or until it loops.
connectedTo :: Puzzle -> (Tile -> Bool) -> Coordinate -> Bool
connectedTo = connectedTo' []
  where
    connectedTo' :: [Coordinate] -> Puzzle -> (Tile -> Bool) -> Coordinate -> Bool
    connectedTo' cs p pred c@(x, y) | isJust t && pred t' = True
                                    | c `elem` cs = False
                                    | otherwise = any (connectedTo' (c:cs) p pred) (getAdjacent es c p)
                                    where
                                      t = getTileAt p c
                                      Just t' = t
                                      es = getEdges t'

-- | Returns the new Coordinate after traversing from the starting Coordinate via the provided TileEdge.
coordinates :: TileEdge -> Coordinate -> Coordinate
coordinates North (x, y) = (x, y - 1)
coordinates East (x, y) = (x + 1, y)
coordinates South (x, y) = (x, y + 1)
coordinates West (x, y) = (x - 1, y)

-- | Checks that the Tile at the specified Coordinate, when it has a TileEdge, is connected to a reciprocating Tile for each TileEdge.
validEdges :: [TileEdge] -> Coordinate -> Puzzle -> Bool
validEdges [] _ _ = True
validEdges (e:es) c p | isJust t && complementingEdge e `elem` es' = validEdges es c p
                      | otherwise = False
                      where
                        t = getTileAt p $ coordinates e c
                        Just t' = t
                        es' = getEdges t'

getEdges :: Tile -> [TileEdge]
getEdges (Source es) = es
getEdges (Sink es) = es
getEdges (Wire es) = es

getTileAt :: Puzzle -> Coordinate -> Maybe Tile
getTileAt = getTileAt' (1, 1)
  where
    getTileAt' :: Coordinate -> Puzzle -> Coordinate -> Maybe Tile
    getTileAt' _ [] _ = Nothing
    getTileAt' (_, y) ([]:rs) (x', y') = getTileAt' (1, y + 1) rs (x', y')
    getTileAt' (x, y) ((t:ts):rs) (x', y') | x == x' && y == y' = Just t
                                           | y > y' || x' < 1 = Nothing
                                           | otherwise = getTileAt' (x + 1, y) (ts:rs) (x', y')

dimensions :: Puzzle -> (Int, Int)
dimensions [] = (0, 0)
dimensions p@(r:rs) = (length r, length p)

-- | Checks that every Tile in a Puzzle, when it has a TileEdge, is connected to a reciprocating Tile for each TileEdge.
allWiresConnected :: Puzzle -> Bool
allWiresConnected = allWiresConnected' (1, 1) []
  where
    allWiresConnected' :: Coordinate -> Puzzle -> Puzzle -> Bool
    allWiresConnected' _ _ [] = True
    allWiresConnected' c@(x, y) l p@(r:rs) = rowWiresConnected r c (l ++ p) && allWiresConnected' (x, y + 1) (l ++ [r]) rs

    -- | Checks that every Tile in a row is connected to reciprocating Tiles.
    rowWiresConnected :: [Tile] -> Coordinate -> Puzzle -> Bool
    rowWiresConnected [] _ _ = True
    rowWiresConnected (t:ts) c@(x, y) p = validEdges (getEdges t) c p && rowWiresConnected ts (x + 1, y) p

-- | Checks that every Sink in a Puzzle is connected to a Source.
validSinks :: Puzzle -> Bool
validSinks = validConnections (1, 1) [] isSink isSource

-- | Checks that every Source in a Puzzle is connected to a Sink.
validSources :: Puzzle -> Bool
validSources = validConnections (1, 1) [] isSource isSink

-- | When provided two Predicates, checks that every Tile satisfying the first predicate in a Puzzle is connected to a Tile satisfying the second predicate.
validConnections :: Coordinate -> Puzzle -> (Tile -> Bool) -> (Tile -> Bool) -> Puzzle -> Bool
validConnections _ _ _ _ [] = True
validConnections c@(x, y) l pred pred' p@(r:rs) = validRowConnections r c pred pred' (l ++ p) && validConnections (x, y + 1) (l ++ [r]) pred pred' rs
  where
    -- | When provided two Predicates, checks that Tile satisfying the first predicate in a row is connected to a Tile satisfying the second predicate.
    validRowConnections :: [Tile] -> Coordinate -> (Tile -> Bool) -> (Tile -> Bool) -> Puzzle -> Bool
    validRowConnections [] _ _ _ _ = True
    validRowConnections (t:ts) c@(x, y) pred pred' p | pred t = connectedTo p pred' c && validRowConnections ts (x + 1, y) pred pred' p
                                                     | otherwise = validRowConnections ts (x + 1, y) pred pred' p

-- Challenge 2
-- Solving Circuits
data Rotation = R0 | R90 | R180 | R270
  deriving (Eq,Show,Read)

type TileRotation = (Rotation, Tile)

solveCircuit :: Puzzle -> Maybe [[ Rotation ]]
solveCircuit p | not $ validPuzzle p = Nothing
               | otherwise = solveCircuit' (1, 1) (dimensions p) [] [] p
  where
    solveCircuit' :: Coordinate -> Coordinate -> [[TileRotation]] -> [TileRotation] -> Puzzle -> Maybe [[Rotation]]
    solveCircuit' _ _ trs _ [] | validSources sp && validSinks sp = Just (compileRotations trs)
                               | otherwise = Nothing
                               where
                                 sp = compilePuzzle trs
    solveCircuit' (x, y) b trs trs' ([]:rs) = solveCircuit' (1, y + 1) b (trs ++ [trs']) [] rs
    solveCircuit' (x, y) b trs trs' ((t:ts):rs) = solveBranches (validRotations b (x, y) t (trs ++ [trs'])) (x, y) b trs trs' (ts:rs)

    solveBranches :: [TileRotation] -> Coordinate -> Coordinate -> [[TileRotation]] -> [TileRotation] -> Puzzle -> Maybe [[Rotation]]
    solveBranches [] _ _ _ _ _ = Nothing
    solveBranches (tr:trs) c@(x, y) b trs' trs'' p | isJust solution = solution
                                                   | otherwise = solveBranches trs c b trs' trs'' p
                                                   where
                                                     solution = solveCircuit' (x + 1, y) b trs' (trs'' ++ [tr]) p

rotateTile :: Rotation -> Tile -> Tile
rotateTile R0 t = t
rotateTile R90 (Source es) = Source $ map rotateEdge90 es
rotateTile R90 (Sink es) = Sink $ map rotateEdge90 es
rotateTile R90 (Wire es) = Wire $ map rotateEdge90 es
rotateTile R180 t = (rotateTile R90 . rotateTile R90) t
rotateTile R270 t = (rotateTile R90 . rotateTile R180) t

-- | Provides the TileEdge resulting from a 90 degree clockwise rotation of the original TileEdge.
rotateEdge90 :: TileEdge -> TileEdge
rotateEdge90 North = East
rotateEdge90 East = South
rotateEdge90 South = West
rotateEdge90 West = North

-- | Provides all ways of uniquely rotating a Tile.
rotations :: Tile -> [TileRotation]
rotations t | length es == 4 || null es = [(R0, t)]
            | length es == 2 && (head es == complementingEdge (es!!1)) = [(r, rotateTile r t) | r <- [R0, R90]]
            | otherwise = [(r, rotateTile r t) | r <- [R0, R90, R180, R270]]
            where
              es = nub $ getEdges t

containsEdge :: TileEdge -> Tile -> Bool
containsEdge e t = e `elem` getEdges t

-- | Filters through rotations to leave those that do not contain any of the specified TileEdges.
rotationsWithout :: [TileEdge] -> [TileRotation] -> [TileRotation]
rotationsWithout es trs = [rot | rot@(r, t) <- trs, and [not $ containsEdge e t | e <- es]]

-- | Filters through rotations to leave those that contain all of the specified TileEdges.
rotationsWith :: [TileEdge] -> [TileRotation] -> [TileRotation]
rotationsWith es trs = [rot | rot@(r, t) <- trs, and [containsEdge e t | e <- es]]

-- | Returns TileRotations that satisfy specified edge constraints.
satisfyConstraints :: [(TileEdge, TileRotation)] -> [TileRotation] -> [TileRotation]
satisfyConstraints [] trs = trs
satisfyConstraints ((e, (_, t)):trs') trs | containsEdge (complementingEdge e) t = rotationsWith [e] (satisfyConstraints trs' trs)
                                          | otherwise = rotationsWithout [e] (satisfyConstraints trs' trs)

-- | Provides a list of valid rotations for a Tile with respect to whether it is situated on an edge.
validRotations :: Coordinate -> Coordinate -> Tile -> [[TileRotation]] -> [TileRotation]
validRotations b@(w, l) c@(x, y) t trs | c == (1, 1) = rotationsWithout [North, West] $ rotations t
                                       | c == (w, 1) = rotationsWithout [North, East] $ satisfyConstraints [(West, trs!!0!!(x-2))] $ rotations t
                                       | c == (1, l) = rotationsWithout [South, West] $ satisfyConstraints [(North, trs!!(y-2)!!0)] $ rotations t
                                       | c == (w, l) = rotationsWithout [South, East] $ satisfyConstraints [(North, trs!!(y-2)!!(x-1)), (West, trs!!(y-1)!!(x-2))] $ rotations t
                                       | y == 1 = rotationsWithout [North] $ satisfyConstraints [(West, trs!!0!!(x-2))] $ rotations t
                                       | y == l = rotationsWithout [South] $ satisfyConstraints [(North, trs!!(y-2)!!(x-1)), (West, trs!!(y-1)!!(x-2))] $ rotations t
                                       | x == 1 = rotationsWithout [West] $ satisfyConstraints [(North, trs!!(y-2)!!0)] $ rotations t
                                       | x == w = rotationsWithout [East] $ satisfyConstraints [(North, trs!!(y-2)!!(x-1)), (West, trs!!(y-1)!!(x-2))] $ rotations t
                                       | otherwise = satisfyConstraints [(North, trs!!(y-2)!!(x-1)), (West, trs!!(y-1)!!(x-2))] $ rotations t

-- | When given a nested list of TileRotations and a function that transforms TileRotations, returns a nested list of transformed TileRotations.
compile :: (TileRotation -> a) -> [[TileRotation]] -> [[a]]
compile f = map $ map f

compilePuzzle :: [[TileRotation]] -> Puzzle
compilePuzzle = compile snd

compileRotations :: [[TileRotation]] -> [[Rotation]]
compileRotations = compile fst

-- | Returns the opposite TileEdge to the original TileEdge (a 180 degree rotation).
complementingEdge :: TileEdge -> TileEdge
complementingEdge = rotateEdge90 . rotateEdge90

-- Challenge 3
-- Pretty Printing Let Expressions

data LExpr = Var Int | App LExpr LExpr | Let Bind  LExpr LExpr | Pair LExpr LExpr | Fst LExpr | Snd LExpr  | Abs Bind LExpr
    deriving (Eq,Show,Read)
data Bind = Discard | V Int
    deriving (Eq,Show,Read)

prettyPrint :: LExpr -> String
prettyPrint (Var x) = 'x' : show x
prettyPrint (App e1@(Abs _ _) e2@(App _ _)) = '(' : prettyPrint e1 ++ ") (" ++ prettyPrint e2 ++ ")"
prettyPrint (App e1@(Let _ _ _) e2@(App _ _)) = '(' : prettyPrint e1 ++ ") (" ++ prettyPrint e2 ++ ")"
prettyPrint (App e1 e2@(App _ _)) = prettyPrint e1 ++ " (" ++ prettyPrint e2 ++ ")"
prettyPrint (App e1@(Abs _ _) e2) = '(' : prettyPrint e1 ++ ") " ++ prettyPrint e2
prettyPrint (App e1@(Let _ _ _) e2) = '(' : prettyPrint e1 ++ ") " ++ prettyPrint e2
prettyPrint (App e1 e2) = prettyPrint e1 ++ " " ++ prettyPrint e2
prettyPrint (Let b e1@(Abs _ _) e2) = "let " ++ prettyBind b ++ " " ++ fst absPair ++ "= " ++ snd absPair ++ " in " ++ prettyPrint e2
  where
    absPair = prettyAbs e1
prettyPrint (Let b e1 e2) = "let " ++ prettyBind b ++ " = " ++ prettyPrint e1 ++ " in " ++ prettyPrint e2
prettyPrint (Pair e1 e2) = '(' : prettyPrint e1 ++ ", " ++ prettyPrint e2 ++ ")"
prettyPrint (Fst e) = "fst (" ++ prettyPrint e ++ ")"
prettyPrint (Snd e) = "snd (" ++ prettyPrint e ++ ")"
prettyPrint e@(Abs _ _) = '\\' : fst absPair ++ "-> " ++ snd absPair
  where
    absPair = prettyAbs e

-- | Breaks a (nested) Abs expression into a composition of Abs expressions.
prettyAbs :: LExpr -> (String, String)
prettyAbs = prettyAbs' []
  where
    prettyAbs' :: String -> LExpr -> (String, String)
    prettyAbs' cs (Abs b e@(Abs _ _)) = prettyAbs' (cs ++ prettyBind b ++ " ") e
    prettyAbs' cs (Abs b e) = (cs ++ prettyBind b ++ " ", prettyPrint e)
    prettyAbs' _ _ = error "Not of Abs format"

prettyBind :: Bind -> String
prettyBind Discard = "_"
prettyBind (V x) = prettyPrint (Var x)

-- Challenge 4 - Parsing Let Expressions

parseLetx :: String -> Maybe LExpr
parseLetx s | null e || (not . null) (snd $ head e) = Nothing
            | otherwise = Just $ fst $ head e
            where
              e = parse (token expr) s

expr :: Parser LExpr
expr = letExpr <|> absExpr <|> appExpr <|> tighterExpr

tighterExpr :: Parser LExpr
tighterExpr = fstExpr <|> sndExpr <|> pairExpr <|> var <|> bracketedExpr

letExpr :: Parser LExpr
letExpr = do symbol "let"
             b <- token bind
             bs <- many $ token bind
             symbol "="
             e1 <- token expr
             symbol "in"
             e2 <- token expr
             return (Let b (foldr Abs e1 bs) e2)

absExpr :: Parser LExpr
absExpr = do char '\\'
             bs <- some (token bind)
             symbol "->"
             e <- expr
             return (foldr Abs e bs)

appExpr :: Parser LExpr
appExpr = do e <- tighterExpr
             es <- some spacedExpr
             return (foldl App e es)
  where
    spacedExpr :: Parser LExpr
    spacedExpr = do some $ sat isSpace
                    tighterExpr <|> expr

fstExpr :: Parser LExpr
fstExpr = do symbol "fst"
             e <- token bracketedExpr
             return (Fst e)

sndExpr :: Parser LExpr
sndExpr = do symbol "snd"
             e <- token bracketedExpr
             return (Snd e)

pairExpr :: Parser LExpr
pairExpr = do char '('
              e1 <- token expr
              char ','
              e2 <- token expr
              char ')'
              return (Pair e1 e2)

bracketedExpr :: Parser LExpr
bracketedExpr = do char '('
                   e <- token expr
                   char ')'
                   return e

var :: Parser LExpr
var = do char 'x'
         Var <$> nat

bind :: Parser Bind
bind = bindV <|> do char '_'
                    return Discard

bindV :: Parser Bind
bindV = do char 'x'
           V <$> nat

-- Challenge 5
-- Let Encoding in Lambda 

data LamExpr = LamVar Int | LamApp LamExpr LamExpr | LamAbs Int LamExpr
                deriving (Eq, Show, Read)

type Mapping = (Bind, Int)

firstElem :: Eq a => a -> [(a, b)] -> Bool
firstElem _ [] = False
firstElem v ((x, y):xs) | x == v = True
                        | otherwise = firstElem v xs

-- | Finds the first available non-free bind naming.
countUp :: Int -> [Int] -> Int
countUp x fs | x `notElem` fs = x
             | otherwise = countUp (x + 1) fs

letEnc :: LExpr -> LamExpr
letEnc e = letEnc' (unaffecting:frees) [(Discard, unaffecting)] [] e
  where
    frees = findFrees [] e
    unaffecting = countUp 0 frees
    letEnc' :: [Int] -> [Mapping] -> [Bind] -> LExpr -> LamExpr
    letEnc' fs ms bs (Var x) | V x `elem` bs = LamVar $ snd m'
                             | otherwise = LamVar x
                             where
                               (m, m') = (find (\(x', _) -> V x == x') ms, fromJust m)
    letEnc' fs ms bs e1@(Abs b e) | isJust firstMap = LamAbs (snd firstMap') (letEnc' fs ms bs e)
                                  where
                                    (firstMap, firstMap') = (findMap b ms, fromJust firstMap)
    letEnc' fs ms bs e@(Let b e1 e2) | isJust firstMap = LamApp (LamAbs (snd firstMap') (letEnc' fs ms bs e2)) (letEnc' fs ms bs e1)
                                     where
                                       (firstMap, firstMap') = (findMap b ms, fromJust firstMap)
    letEnc' fs ms bs e | isJust b = letEnc' (mapTo:fs) ((b', mapTo):ms) (b':bs) e
                       where
                         (b, b') = (getBind e, fromJust b)
                         mapTo = countUp 0 fs
    letEnc' fs ms bs (Pair e1 e2) = LamAbs unaffecting (LamApp (LamApp (LamVar unaffecting) $ letEnc' fs ms bs e1) $ letEnc' fs ms bs e2)
    letEnc' fs ms bs (Fst e) = LamApp (letEnc' fs ms bs e) (LamAbs 0 (LamAbs 1 (LamVar 0)))
    letEnc' fs ms bs (Snd e) = LamApp (letEnc' fs ms bs e) (LamAbs 0 (LamAbs 1 (LamVar 1)))
    letEnc' fs ms bs (App e1 e2) = LamApp (letEnc' fs ms bs e1) (letEnc' fs ms bs e2)

    getBind :: LExpr -> Maybe Bind
    getBind (Abs b _) = Just b
    getBind (Let b _ _) = Just b
    getBind _ = Nothing

    findMap :: Bind -> [Mapping] -> Maybe Mapping
    findMap b = find (\(b', _) -> b' == b)

findFrees :: [Bind] -> LExpr -> [Int]
findFrees bs (Var x) | V x `elem` bs = []
                     | otherwise = [x]
findFrees bs (Abs b e) = findFrees (b:bs) e
findFrees bs (Let b e1 e2) = findFrees (b:bs) e2 ++ findFrees bs e1
findFrees bs (Pair e1 e2) = findFrees bs e1 ++ findFrees bs e2
findFrees bs (Fst e) = findFrees bs e
findFrees bs (Snd e) = findFrees bs e
findFrees bs (App e1 e2) = findFrees bs e1 ++ findFrees bs e2

-- Challenge 6
-- Compare Innermost Reduction for Let_x and its Lambda Encoding

------------
-- LAMBDA --
------------

free :: Int -> LamExpr -> Bool
free x (LamVar y) =  x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2)  = (free x e1) || (free x e2)

rename :: Int -> LamExpr -> Int
rename x e | free (x+1) e = rename (x+1) e
           | otherwise = x+1

subst :: LamExpr -> Int ->  LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e  |  x /= y && not (free x e)  = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e  |  x /= y &&     (free x e)  = let x' = (rename x e1) in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e  | x == y  = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e)

isLamValue :: LamExpr -> Bool
isLamValue (LamVar _) = True
isLamValue (LamAbs _ _) = True
isLamValue _ = False

-- CALL BY VALUE -- 
cbvlam1 :: LamExpr -> Maybe LamExpr
-- Contexts
cbvlam1 (LamApp e1 e2) | not (isLamValue e1) =
  do e' <- cbvlam1 e1
     return (LamApp e' e2)
cbvlam1 (LamApp e1 e2) | not (isLamValue e2) =
  do e' <- cbvlam1 e2
     return (LamApp e1 e')
-- Reductions 
cbvlam1 (LamApp (LamAbs x e1) e) | isLamValue e = Just (subst e1 x e)
-- Otherwise terminated or blocked
cbvlam1 _ = Nothing

-- CALL BY NAME --
cbnlam1 :: LamExpr -> Maybe LamExpr
-- Reductions 
cbnlam1 (LamApp (LamAbs x e1) e) = Just (subst e1 x e)
-- Contexts
cbnlam1 (LamApp e1 e2) =
  do e' <- cbnlam1 e1
     return (LamApp e' e2)
-- Otherwise terminated or blocked
cbnlam1 _ = Nothing

---------
-- LET --
--------- 

-- | Checks if a variable name is free in a LExpr.
letFree :: Int -> LExpr -> Bool
letFree x (Var y) = x == y
letFree x (Abs (V y) e) | x == y = False
                        | otherwise = letFree x e
letFree x (Abs Discard e) = letFree x e
letFree x (App e1 e2) = letFree x e1 || letFree x e2
letFree x (Fst e) = letFree x e
letFree x (Snd e) = letFree x e
letFree x (Pair e1 e2) = letFree x e1 && letFree x e2
letFree x (Let (V y) e1 e2) | x == y = False
                            | otherwise = letFree x e1
letFree x (Let Discard e1 e2) = letFree x e1

-- | Renames one variable throughout the given LExpr.
letRename :: Int -> LExpr -> Int
letRename x e | letFree (x + 1) e = letRename (x + 1) e
              | otherwise = x + 1

-- | Substitutes an LExpr in place of a specified variable in the given LExpr.
letSubst :: LExpr -> Int -> LExpr -> LExpr
letSubst (Var x) y e | x == y = e
                     | otherwise = Var x
letSubst (Abs (V x) e1) y e | x /= y && not (letFree x e) = Abs (V x) (letSubst e1 y e)
                            | x /= y && letFree x e = let x' = letRename x e1 in letSubst (Abs (V x') (letSubst e1 x (Var x'))) y e
                            | otherwise = Abs (V x) e1
letSubst (App e1 e2) y e = App (letSubst e1 y e) (letSubst e2 y e)
letSubst (Fst e1) y e = Fst (letSubst e1 y e)
letSubst (Snd e1) y e = Snd (letSubst e1 y e)
letSubst (Pair e1 e2) y e = Pair (letSubst e1 y e) (letSubst e2 y e)
letSubst (Let (V x) e1 e2) y e | x /= y && not (letFree x e) = Let (V x) (letSubst e1 y e) (letSubst e2 y e)
                               | x /= y && letFree x e = let x' = letRename x e2 in letSubst (Let (V x') (letSubst e1 x (Var x')) (letSubst e2 x (Var x'))) y e
                               | otherwise = Let (V x) e1 e2

isLetValue :: LExpr -> Bool
isLetValue (Var x) = True
isLetValue (Abs _ _) = True
isLetValue (Pair e1 e2) = isLetValue e1 && isLetValue e2
isLetValue _ = False

-- | Performs one step of Call by Value reduction.
cbvlet1 :: LExpr -> Maybe LExpr
-- Contexts
cbvlet1 (App e1 e2) | not $ isLetValue e1 =
  do e' <- cbvlet1 e1
     return (App e' e2)
                    | not $ isLetValue e2 =
  do e' <- cbvlet1 e2
     return (App e1 e')
cbvlet1 (Let b e1 e2) | not $ isLetValue e1 =
  do e' <- cbvlet1 e1
     return (Let b e' e2)
cbvlet1 (Pair e1 e2) | not $ isLetValue e1 =
  do e' <- cbvlet1 e1
     return (Pair e' e2)
                     | not $ isLetValue e2 =
  do e' <- cbvlet1 e2
     return (Pair e1 e')
cbvlet1 (Fst e) | not (isLetValue e) =
  do e' <- cbvlet1 e
     return (Fst e')
cbvlet1 (Snd e) | not (isLetValue e) =
  do e' <- cbvlet1 e
     return (Snd e')
cbvlet1 (App (Abs b e1) e) | not $ isLetValue e =
  do e' <- cbvlet1 e
     return (App (Abs b e1) e')
-- Reductions
cbvlet1 (App (Abs Discard e1) _) = Just e1
cbvlet1 (App (Abs (V x) e1) e) | isLetValue e = Just (letSubst e1 x e)
cbvlet1 (Let Discard _ e) = Just e
cbvlet1 (Let (V x) e1 e2) = Just (letSubst e2 x e1)
cbvlet1 (Fst (Pair e _)) = Just e
cbvlet1 (Snd (Pair _ e)) = Just e
cbvlet1 (Fst e) = Just e
cbvlet1 (Snd e) = Just e
-- Otherwise terminated or blocked
cbvlet1 x = Nothing

-- | Performs one step of Call by Name reduction.
cbnlet1 :: LExpr -> Maybe LExpr
-- Reductions
cbnlet1 (App (Abs Discard e1) _) = Just e1
cbnlet1 (App (Abs (V x) e1) e) | isLetValue e = Just (letSubst e1 x e)
cbnlet1 (Let Discard _ e) = Just e
cbnlet1 (Let (V x) e1 e2) = Just (letSubst e2 x e1)
cbnlet1 (Fst (Pair e _)) = Just e
cbnlet1 (Snd (Pair _ e)) = Just e
cbnlet1 (Fst e) = Just e
cbnlet1 (Snd e) = Just e
-- Contexts
cbnlet1 (App e1 e2) | not $ isLetValue e1 =
  do e' <- cbnlet1 e1
     return (App e' e2)
-- Otherwise terminated or blocked
cbnlet1 _ = Nothing

compareRedn :: LExpr -> Int -> (Int,Int,Int,Int)
compareRedn e u = (cbvletRedn u e, cbvlamRedn u e', cbnletRedn u e, cbnlamRedn u e')
  where
    e' = letEnc e

-- | Returns number of reductions (or upper bound) of a given type of expression.
callBy :: Eq a => (a -> Maybe a) -> Int -> Int -> a -> Int
callBy f x u e | x == u || isNothing (f e) = x
               | otherwise = callBy f (x + 1) u e'
               where
                 Just e' = f e

cbvletRedn :: Int -> LExpr -> Int
cbvletRedn = callBy cbvlet1 0

cbnletRedn :: Int -> LExpr -> Int
cbnletRedn = callBy cbnlet1 0

cbvlamRedn :: Int -> LamExpr -> Int
cbvlamRedn = callBy cbvlam1 0

cbnlamRedn :: Int -> LamExpr -> Int
cbnlamRedn = callBy cbnlam1 0