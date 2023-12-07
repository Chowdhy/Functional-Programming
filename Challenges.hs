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
import Data.List (nub)
import Data.Maybe (isJust)

-- Challenge 1
-- Testing Circuits

data TileEdge = North | East | South | West  deriving (Eq,Ord,Show,Read)
data Tile = Source [ TileEdge ] | Sink [ TileEdge ] | Wire [ TileEdge ]  deriving (Eq,Show,Read)
type Puzzle = [ [ Tile ] ]

-- New type defined to simplify type declarations
type Coordinate = (Int, Int)

isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete p | not $ validPuzzle p = error "Invalid puzzle"
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
getAdjacent (North:es) c@(x, y) p = (x, y-1) : getAdjacent es c p
getAdjacent (East:es) c@(x, y) p = (x+1, y) : getAdjacent es c p
getAdjacent (South:es) c@(x, y) p = (x, y+1) : getAdjacent es c p
getAdjacent (West:es) c@(x, y) p = (x-1, y) : getAdjacent es c p

isSink :: Tile -> Bool
isSink (Sink _) = True
isSink _ = False

isSource :: Tile -> Bool
isSource (Source _) = True
isSource _ = False

isWire :: Tile -> Bool
isWire (Wire _) = True
isWire _ = False

-- | Auxiliary function that calls connectedTo' with an empty accumulator.
connectedTo :: Puzzle -> (Tile -> Bool) -> Coordinate -> Bool
connectedTo = connectedTo' []

-- | Returns True if the Tile at the starting Coordinate is connected to a Tile that satisfies the given predicate.
-- The function traverses the specified Puzzle until it finds a satisfying Tile, or until it loops.
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
coordinates North (x,y) = (x,y-1)
coordinates East (x,y) = (x+1,y)
coordinates South (x,y) = (x, y+1)
coordinates West (x,y) = (x-1,y)

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
getTileAt = getTileAt' 1 1

getTileAt' :: Int -> Int -> Puzzle -> Coordinate -> Maybe Tile
getTileAt' _ _ [] _ = Nothing
getTileAt' _ y ([]:rs) (x', y') = getTileAt' 1 (y + 1) rs (x', y')
getTileAt' x y ((t:ts):rs) (x', y') | x == x' && y == y' = Just t
                                    | y > y' || x' < 1 = Nothing
                                    | otherwise = getTileAt' (x + 1) y (ts:rs) (x', y')

dimensions :: Puzzle -> (Int, Int)
dimensions [] = (0, 0)
dimensions p@(r:rs) = (length r, length p)

-- | Auxiliary function that calls allWiresConnected' from an initial state.
allWiresConnected :: Puzzle -> Bool
allWiresConnected = allWiresConnected' 1 1 []

-- | Checks that every Tile in a Puzzle, when it has a TileEdge, is connected to a reciprocating Tile for each TileEdge.
allWiresConnected' :: Int -> Int -> Puzzle -> Puzzle -> Bool
allWiresConnected' _ _ _ [] = True
allWiresConnected' x y l p@(r:rs) = rowWiresConnected r x y (l ++ p) && allWiresConnected' x (y+1) (l ++ [r]) rs

-- | Checks that every Tile in a row is connected to reciprocating Tiles.
rowWiresConnected :: [Tile] -> Int -> Int -> Puzzle -> Bool
rowWiresConnected [] _ _ _ = True
rowWiresConnected ((Source es):ts) x y p = validEdges es (x, y) p && rowWiresConnected ts (x+1) y p
rowWiresConnected ((Sink es):ts) x y p = validEdges es (x, y) p && rowWiresConnected ts (x+1) y p
rowWiresConnected ((Wire es):ts) x y p = validEdges es (x, y) p && rowWiresConnected ts (x+1) y p

-- | Checks that every Sink in a Puzzle is connected to a Source.
validSinks :: Puzzle -> Bool
validSinks = validConnections 1 1 [] isSink isSource

-- | Checks that every Source in a Puzzle is connected to a Sink.
validSources :: Puzzle -> Bool
validSources = validConnections 1 1 [] isSource isSink

-- | When provided two Predicates, checks that every Tile satisfying the first predicate in a Puzzle is connected to a Tile satisfying the second predicate.
validConnections :: Int -> Int -> Puzzle -> (Tile -> Bool) -> (Tile -> Bool) -> Puzzle -> Bool
validConnections _ _ _ _ _ [] = True
validConnections x y l pred pred' p@(r:rs) = validRowConnections r x y pred pred' (l ++ p) && validConnections x (y+1) (l ++ [r]) pred pred' rs

-- | When provided two Predicates, checks that Tile satisfying the first predicate in a row is connected to a Tile satisfying the second predicate.
validRowConnections :: [Tile] -> Int -> Int -> (Tile -> Bool) -> (Tile -> Bool) -> Puzzle -> Bool
validRowConnections [] _ _ _ _ _ = True
validRowConnections (t:ts) x y pred pred' p | pred t = connectedTo p pred' (x, y) && validRowConnections ts (x+1) y pred pred' p
                                            | otherwise = validRowConnections ts (x+1) y pred pred' p

{-
[ [ Wire [North,West] , Wire [North,South] , Source [North] ], [ Wire [North,West], Wire [East,West], Wire [North,East] ], [ Sink [West] , Wire [North,South] , Wire [North,West] ] ]
-}

-- Challenge 2
-- Solving Circuits
data Rotation = R0 | R90 | R180 | R270
  deriving (Eq,Show,Read)

type TileRotation = (Rotation, Tile)

solveCircuit :: Puzzle -> Maybe [[ Rotation ]]
solveCircuit p | any null [trs | rs <- puzzleRots, trs <- rs] = Nothing
               where
                 puzzleRots = puzzleRotations p

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
rotations t | es == 4 || es == 0 = [(R0, rotateTile R0 t)]
            | otherwise = [(R0, rotateTile R0 t), (R90, rotateTile R90 t), (R180, rotateTile R180 t), (R270, rotateTile R270 t)]
            where
              es = length $ nub $ getEdges t

containsEdge :: TileEdge -> Tile -> Bool
containsEdge e t = e `elem` getEdges t

-- | Filters through rotations to leave rotations that do not contain any of the specified TileEdges.
rotationsWithout :: [TileEdge] -> [TileRotation] -> [TileRotation]
rotationsWithout es trs = [rot | rot@(r, t) <- trs, and [not $ containsEdge e t | e <- es]]

-- | Provides a list of valid rotations for a Tile with respect to whether it is situated on an edge.
validRotations :: Coordinate -> Coordinate -> Tile -> [TileRotation]
validRotations b@(w, l) c@(x, y) t | y == 1 && x == 1 = rotationsWithout [North, West] $ rotations t
                                   | y == 1 && x == w = rotationsWithout [North, East] $ rotations t
                                   | x == 1 && y == l = rotationsWithout [South, West] $ rotations t
                                   | x == w && y == l = rotationsWithout [South, East] $ rotations t
                                   | y == 1 = rotationsWithout [North] $ rotations t
                                   | y == l = rotationsWithout [South] $ rotations t
                                   | x == 1 = rotationsWithout [West] $ rotations t
                                   | x == w = rotationsWithout [East] $ rotations t
                                   | otherwise = rotations t

-- | Auxiliary function that calls puzzleRotations' from an initial state.
puzzleRotations :: Puzzle -> [[[TileRotation]]]
puzzleRotations p = puzzleRotations' (dimensions p) 1 p

-- | Provides all valid rotations of Tiles in a Puzzle.
puzzleRotations' :: Coordinate -> Int -> Puzzle -> [[[TileRotation]]]
puzzleRotations' _ _ [] = []
puzzleRotations' b y (r:rs) = rowRotations b (1, y) r : puzzleRotations' b (y+1) rs

-- | Provides all valid rotations of Tiles in a row of a Puzzle.
rowRotations :: Coordinate -> Coordinate -> [Tile] -> [[TileRotation]]
rowRotations _ _ [] = []
rowRotations b c@(x, y) (t:ts) = validRotations b c t : rowRotations b (x+1, y) ts

-- | Returns the opposite TileEdge to the original TileEdge (an 180 degree rotation).
complementingEdge :: TileEdge -> TileEdge
complementingEdge = rotateEdge90 . rotateEdge90

containsComplement :: [TileEdge] -> [TileEdge] -> Bool
containsComplement [] _ = True
containsComplement (e:[]) es' = complementingEdge e `elem` es'
containsComplement (e:es) es' | complementingEdge e `elem` es' = True
                              | otherwise = containsComplement es es'

satisfyingRotations :: Tile -> (TileEdge, [TileRotation]) -> [TileRotation]
satisfyingRotations t (e, trs) | e `notElem` getEdges t = [tr | tr@(r, t') <- trs, not $ (containsEdge $ complementingEdge e) t']
                               | otherwise = [tr | tr@(r, t') <- trs, (containsEdge $ complementingEdge e) t']

canSatisfy :: Tile -> (TileEdge, [TileRotation]) -> Bool
canSatisfy t xs = not . null $ satisfyingRotations t xs

satisfies :: Tile -> Tile -> Tile -> Bool
satisfies n w c | South `elem` getEdges n && East `elem` getEdges w = containsEdge North c && containsEdge West c
                | South `elem` getEdges n = containsEdge North c && not (containsEdge East c)
                | East `elem` getEdges w = containsEdge West c && not (containsEdge North c)
                | otherwise = not $ containsEdge West c || containsEdge East c

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

prettyAbs :: LExpr -> (String, String)
prettyAbs = prettyAbs' []

-- | Breaks a (nested) Abs expression into a composition of Abs expressions.
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
              e = parse expr $ [c | c <- s, c /= ' ']

expr :: Parser LExpr
expr = letExpr <|> absExpr <|> appExpr

letExpr :: Parser LExpr
letExpr = do string "let"
             b <- bindD
             bs <- many bindD
             char '='
             e1 <- expr
             string "in"
             e2 <- expr
             return (Let b (absAssoc bs e1) e2)

absExpr :: Parser LExpr
absExpr = do char '\\'
             bs <- some bindD
             string "->"
             e <- expr
             return (absAssoc bs e)

-- | Returns a (nested Abs) expression based on the provided list of Binds and LExpr.
absAssoc :: [Bind] -> LExpr -> LExpr
absAssoc [] e = e
absAssoc (b:bs) e = (Abs b (absAssoc bs e))

appExpr :: Parser LExpr
appExpr = do e1 <- some fstExpr
             e2 <- many expr
             return (appAssoc (e1 ++ e2))
          <|> fstExpr

-- | Returns a (left associated nested App) expression based on the provided list of LExpr.
appAssoc :: [LExpr] -> LExpr
appAssoc (x:[]) = x
appAssoc (x1:x2:xs) = appAssoc ((App x1 x2):xs)

fstExpr :: Parser LExpr
fstExpr = do string "fst("
             e <- expr
             char ')'
             return (Fst e)
          <|> sndExpr

sndExpr :: Parser LExpr
sndExpr = do string "snd("
             e <- expr
             char ')'
             return (Snd e)
          <|> pairExpr

pairExpr :: Parser LExpr
pairExpr = do char '('
              e1 <- expr
              char ','
              e2 <- expr
              char ')'
              return (Pair e1 e2)
           <|> bracketedExpr

bracketedExpr :: Parser LExpr
bracketedExpr = var <|> do char '('
                           e <- expr
                           char ')'
                           return e

var :: Parser LExpr
var = do char 'x'
         ds <- some digit
         return (Var (read ds))

bindD :: Parser Bind
bindD = bindV <|> do char '_'
                     return Discard

bindV :: Parser Bind
bindV = do char 'x'
           ds <- some digit
           return (V (read ds))

-- Challenge 5
-- Let Encoding in Lambda 

data LamExpr = LamVar Int | LamApp LamExpr LamExpr | LamAbs Int LamExpr
                deriving (Eq, Show, Read)

letEnc :: LExpr -> LamExpr
letEnc (Var x) = LamVar (x + 1)
letEnc (Abs (V x) e) = LamAbs (x + 1) $ letEnc e
letEnc (Abs Discard e) = LamAbs 0 $ letEnc e
letEnc (Let (V x) e1 e2) = LamApp (LamAbs (x + 1) (letEnc e2)) (letEnc e1)
letEnc (Let Discard e1 e2) = LamApp (LamAbs 0 $ letEnc e2) $ letEnc e1
letEnc (Pair e1 e2) = LamAbs 0 (LamApp (LamApp (LamVar 0) $ letEnc e1) $ letEnc e2)
letEnc (Fst e@(Pair _ _)) = LamApp (letEnc e) (LamAbs 0 (LamAbs 1 (LamVar 0)))
letEnc (Snd e@(Pair _ _)) = LamApp (letEnc e) (LamAbs 0 (LamAbs 1 (LamVar 1)))

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



compareRedn :: LExpr -> Int -> (Int,Int,Int,Int)
compareRedn = undefined
