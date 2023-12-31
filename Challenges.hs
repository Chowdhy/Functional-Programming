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
coordinates North (x,y) = (x, y - 1)
coordinates East (x,y) = (x + 1, y)
coordinates South (x,y) = (x, y + 1)
coordinates West (x,y) = (x - 1, y)

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
  where
    getTileAt' :: Int -> Int -> Puzzle -> Coordinate -> Maybe Tile
    getTileAt' _ _ [] _ = Nothing
    getTileAt' _ y ([]:rs) (x', y') = getTileAt' 1 (y + 1) rs (x', y')
    getTileAt' x y ((t:ts):rs) (x', y') | x == x' && y == y' = Just t
                                        | y > y' || x' < 1 = Nothing
                                        | otherwise = getTileAt' (x + 1) y (ts:rs) (x', y')

dimensions :: Puzzle -> (Int, Int)
dimensions [] = (0, 0)
dimensions p@(r:rs) = (length r, length p)

-- | Checks that every Tile in a Puzzle, when it has a TileEdge, is connected to a reciprocating Tile for each TileEdge.
allWiresConnected :: Puzzle -> Bool
allWiresConnected = allWiresConnected' 1 1 []
  where
    allWiresConnected' :: Int -> Int -> Puzzle -> Puzzle -> Bool
    allWiresConnected' _ _ _ [] = True
    allWiresConnected' x y l p@(r:rs) = rowWiresConnected r x y (l ++ p) && allWiresConnected' x (y + 1) (l ++ [r]) rs

    -- | Checks that every Tile in a row is connected to reciprocating Tiles.
    rowWiresConnected :: [Tile] -> Int -> Int -> Puzzle -> Bool
    rowWiresConnected [] _ _ _ = True
    rowWiresConnected (t:ts) x y p = validEdges (getEdges t) (x, y) p && rowWiresConnected ts (x + 1) y p

-- | Checks that every Sink in a Puzzle is connected to a Source.
validSinks :: Puzzle -> Bool
validSinks = validConnections 1 1 [] isSink isSource

-- | Checks that every Source in a Puzzle is connected to a Sink.
validSources :: Puzzle -> Bool
validSources = validConnections 1 1 [] isSource isSink

-- | When provided two Predicates, checks that every Tile satisfying the first predicate in a Puzzle is connected to a Tile satisfying the second predicate.
validConnections :: Int -> Int -> Puzzle -> (Tile -> Bool) -> (Tile -> Bool) -> Puzzle -> Bool
validConnections _ _ _ _ _ [] = True
validConnections x y l pred pred' p@(r:rs) = validRowConnections r x y pred pred' (l ++ p) && validConnections x (y + 1) (l ++ [r]) pred pred' rs
  where
    -- | When provided two Predicates, checks that Tile satisfying the first predicate in a row is connected to a Tile satisfying the second predicate.
    validRowConnections :: [Tile] -> Int -> Int -> (Tile -> Bool) -> (Tile -> Bool) -> Puzzle -> Bool
    validRowConnections [] _ _ _ _ _ = True
    validRowConnections (t:ts) x y pred pred' p | pred t = connectedTo p pred' (x, y) && validRowConnections ts (x + 1) y pred pred' p
                                                | otherwise = validRowConnections ts (x + 1) y pred pred' p

-- Challenge 2
-- Solving Circuits
data Rotation = R0 | R90 | R180 | R270
  deriving (Eq,Show,Read)

type TileRotation = (Rotation, Tile)

solveCircuit :: Puzzle -> Maybe [[ Rotation ]]
solveCircuit p = solveCircuit' (1,1) (dimensions p) [] [] p
  where
    solveCircuit' :: Coordinate -> Coordinate -> [[TileRotation]] -> [TileRotation] -> Puzzle -> Maybe [[Rotation]]
    solveCircuit' _ _ trs _ [] | isPuzzleComplete $ compilePuzzle trs = Just (compileRotations trs)
                               | otherwise = Nothing
    solveCircuit' (x, y) b trs trs' ([]:rs) = solveCircuit' (1, y + 1) b (trs ++ [trs']) [] rs
    solveCircuit' (x, y) b trs trs' ((t:ts):rs) = solveBranches (validRotations b (x, y) t (trs++[trs'])) (x,y) b trs trs' (ts:rs)

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
rotations t | es == 4 || es == 0 = [(R0, rotateTile R0 t)]
            | otherwise = [(R0, rotateTile R0 t), (R90, rotateTile R90 t), (R180, rotateTile R180 t), (R270, rotateTile R270 t)]
            where
              es = length $ nub $ getEdges t

containsEdge :: TileEdge -> Tile -> Bool
containsEdge e t = e `elem` getEdges t

-- | Filters through rotations to leave those that do not contain any of the specified TileEdges.
rotationsWithout :: [TileEdge] -> [TileRotation] -> [TileRotation]
rotationsWithout es trs = [rot | rot@(r, t) <- trs, and [not $ containsEdge e t | e <- es]]

-- | Filters through rotations to leave those that contain all of the specified TileEdges.
rotationsWith :: [TileEdge] -> [TileRotation] -> [TileRotation]
rotationsWith es trs = [rot | rot@(r, t) <- trs, and [containsEdge e t | e <- es]]

satLeftConstraint :: TileRotation -> [TileRotation] -> [TileRotation]
satLeftConstraint (r, t') trs | containsEdge East t' = rotationsWith [West] trs
                              | otherwise = rotationsWithout [West] trs

satAboveConstraint ::  TileRotation -> [TileRotation] -> [TileRotation]
satAboveConstraint (r, t') trs | containsEdge South t' = rotationsWith [North] trs
                               | otherwise = rotationsWithout [North] trs

-- | Provides a list of valid rotations for a Tile with respect to whether it is situated on an edge.
validRotations :: Coordinate -> Coordinate -> Tile -> [[TileRotation]] -> [TileRotation]
validRotations b@(w, l) c@(x, y) t trs | c == (1, 1) = rotationsWithout [North, West] $ rotations t
                                       | c == (w, 1) = rotationsWithout [North, East] $ satLeftConstraint (trs!!0!!(w-2)) $ rotations t
                                       | c == (1, l) = rotationsWithout [South, West] $ satAboveConstraint (trs!!(l-2)!!0) $ rotations t
                                       | c == (w, l) = rotationsWithout [South, East] $ satAboveConstraint (trs!!(l-2)!!(w-1)) $ satLeftConstraint (trs!!(l-1)!!(w-2)) $ rotations t
                                       | y == 1 = rotationsWithout [North] $ satLeftConstraint (trs!!0!!(x-2)) $ rotations t
                                       | y == l = rotationsWithout [South] $ satAboveConstraint (trs!!(y-2)!!(x-1)) $ satLeftConstraint (trs!!(y-1)!!(x-2)) $ rotations t
                                       | x == 1 = rotationsWithout [West] $ satAboveConstraint (trs!!(y-2)!!0) $ rotations t
                                       | x == w = rotationsWithout [East] $ satAboveConstraint (trs!!(y-2)!!(x-1)) $ satLeftConstraint (trs!!(y-1)!!(x-2)) $ rotations t
                                       | otherwise = satAboveConstraint (trs!!(y-2)!!(x-1)) $ satLeftConstraint (trs!!(y-1)!!(x-2)) $ rotations t

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

containsComplement :: [TileEdge] -> [TileEdge] -> Bool
containsComplement [] _ = True
containsComplement (e:[]) es' = complementingEdge e `elem` es'
containsComplement (e:es) es' | complementingEdge e `elem` es' = True
                              | otherwise = containsComplement es es'

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
expr = letExpr <|> absExpr <|> appExpr

letExpr :: Parser LExpr
letExpr = do symbol "let"
             b <- token bindD
             bs <- many $ token bindD
             symbol "="
             e1 <- token expr
             symbol "in"
             e2 <- token expr
             return (Let b (absAssoc bs e1) e2)

absExpr :: Parser LExpr
absExpr = do char '\\'
             bs <- some bindD
             symbol "->"
             absAssoc bs <$> expr

-- | Returns a (nested Abs) expression based on the provided list of Binds and LExpr.
absAssoc :: [Bind] -> LExpr -> LExpr
absAssoc bs e = foldr Abs e bs

appExpr :: Parser LExpr
appExpr = do e1 <- fstExpr
             es1 <- many fstExpr'
             e2 <- many expr'
             return (appAssoc (e1 : es1 ++ e2))
          <|> token fstExpr
  where
    -- | Parses a spaced fstExpr.
    fstExpr' :: Parser LExpr
    fstExpr' = do some $ sat isSpace
                  fstExpr

    -- | Parses a spaced expr.
    expr' :: Parser LExpr
    expr' = do some $ sat isSpace
               expr

-- | Returns a (left associated nested App) expression based on the provided list of LExpr.
appAssoc :: [LExpr] -> LExpr
appAssoc (x:[]) = x
appAssoc (x1:x2:xs) = appAssoc (App x1 x2:xs)

fstExpr :: Parser LExpr
fstExpr = do symbol "fst"
             char '('
             e <- token expr
             char ')'
             return (Fst e)
          <|> sndExpr

sndExpr :: Parser LExpr
sndExpr = do symbol "snd"
             char '('
             e <- token expr
             char ')'
             return (Snd e)
          <|> pairExpr

pairExpr :: Parser LExpr
pairExpr = do char '('
              e1 <- token expr
              char ','
              e2 <- token expr
              char ')'
              return (Pair e1 e2)
           <|> bracketedExpr

bracketedExpr :: Parser LExpr
bracketedExpr = var <|> do string "("
                           e <- token expr
                           string ")"
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
letEnc (Fst e) = LamApp (letEnc e) (LamAbs 0 (LamAbs 1 (LamVar 0)))
letEnc (Snd e) = LamApp (letEnc e) (LamAbs 0 (LamAbs 1 (LamVar 1)))
letEnc (App e1 e2) = LamApp (letEnc e1) (letEnc e2)

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
                            | x /= y && letFree x e = let x' = (letRename x e1) in letSubst (Abs (V x') (letSubst e1 x (Var x'))) y e
                            | otherwise = Abs (V x) e1
letSubst (App e1 e2) y e = App (letSubst e1 y e) (letSubst e2 y e)
letSubst (Fst e1) y e = Fst (letSubst e1 y e)
letSubst (Snd e1) y e = Snd (letSubst e1 y e)
letSubst (Pair e1 e2) y e = Pair (letSubst e1 y e) (letSubst e2 y e)
letSubst (Let (V x) e1 e2) y e | x /= y && not (letFree x e) = Let (V x) e1 (letSubst e2 y e)
                               | x /= y && letFree x e = let x' = (letRename x e2) in letSubst (Let (V x') e1 (letSubst e2 x (Var x'))) y e
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
callBy f x u e | x == u || f e == Nothing = x
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