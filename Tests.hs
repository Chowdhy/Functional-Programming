import Test.HUnit
import Challenges

-- Challenge 1 Tests

c1test1 = TestCase (assertEqual "isPuzzleComplete []" True (isPuzzleComplete []))
c1test2 = TestCase (assertEqual "isPuzzleComplete [[Wire []]]" True (isPuzzleComplete [[Wire []]]))
c1test3 = TestCase (assertEqual "isPuzzleComplete [[Source [North]]]" False (isPuzzleComplete [[Source [North]]]))
c1test4 = TestCase (assertEqual "isPuzzleComplete [[Sink [North]]]" False (isPuzzleComplete [[Sink [North]]]))
c1test5 = TestCase (assertEqual "isPuzzleComplete [[Source [South]], [Sink [North]]]" True (isPuzzleComplete [[Source [South]], [Sink [North]]]))
c1test6 = TestCase (assertEqual "isPuzzleComplete [[Source [South, East], Wire [West, South]], [Wire [North, East], Wire [West, North]]]" False (isPuzzleComplete [[Source [South, East], Wire [West, South]], [Wire [North, East], Wire [West, North]]]))
c1test7 = TestCase (assertEqual "isPuzzleComplete [[Source [South, East], Wire [West, South]], [Wire [North, East], Wire [West, North]]]" False (isPuzzleComplete [[Sink [South, East], Wire [West, South]], [Wire [North, East], Wire [West, North]]]))
c1test8 = TestCase (assertEqual "isPuzzleComplete [[Source [South, East], Wire [West, South]], [Wire [North, East], Wire [West, North]]]" True (isPuzzleComplete [[Source [South, East], Wire [West, South]], [Wire [North, East], Sink [West, North]]]))
c1test9 = TestCase (assertEqual "isPuzzleComplete [[Source [South, East], Wire [West, South], Sink [South, East], Wire [West, South]], [Wire [North, East], Wire [West, North], Wire [North, East], Wire [West, North]]]" False (isPuzzleComplete [[Source [South, East], Wire [West, South], Sink [South, East], Wire [West, South]], [Wire [North, East], Wire [West, North], Wire [North, East], Wire [West, North]]]))

c1tests = TestLabel "Challenge 1 Tests" (
  TestList [
  TestLabel "Empty puzzle is complete" c1test1,
  TestLabel "Singleton empty wire is complete" c1test2,
  TestLabel "Singleton source is not complete" c1test3,
  TestLabel "Singleton sink is not complete" c1test4,
  TestLabel "Connected puzzle with only source and sink is complete" c1test5,
  TestLabel "Connected puzzle with source and no sink is not complete" c1test6,
  TestLabel "Connected puzzle with sink and no source is not complete" c1test7,
  TestLabel "Connected puzzle with wires, both source and sink, and path between them, is complete" c1test8,
  TestLabel "Connected puzzle with wires, both source and sink, but no path between them, is not complete" c1test9
  ])

-- Challenge 2 Tests
c2test1 = TestCase (assertEqual "solveCircuit ((Source [West, North] : replicate 18 (Wire [West, North]) ++ [Sink [West, North]]) : replicate 19 (replicate 20 (Wire [West, North])))" Nothing (solveCircuit ((Source [West, North] : replicate 18 (Wire [West, North]) ++ [Sink [West, North]]) : replicate 19 (replicate 20 (Wire [West, North])))))
c2test2 = TestCase (assertEqual "solveCircuit [[Source [East, South], Wire [West, South], Wire [East, South], Source [West, South]], [Wire [North, East], Wire [West, North, East], Wire [West, North, East], Wire [West, North]], [Wire [South, East], Wire [West, South, East], Wire [West, South, East], Wire [West, South]], [Sink [North, East], Wire [West, North], Wire [North, East], Sink [West, North]]] == Just (replicate 4 (replicate 4 R0))" False (solveCircuit [[Source [East, South], Wire [West, South], Wire [East, South], Source [West, South]], [Wire [North, East], Wire [West, North, East], Wire [West, North, East], Wire [West, North]], [Wire [South, East], Wire [West, South, East], Wire [West, South, East], Wire [West, South]], [Sink [North, East], Wire [West, North], Wire [North, East], Sink [West, North]]] == Just (replicate 4 (replicate 4 R0))))
c2test3 = TestCase (assertEqual "solveCircuit [[Source [East, South], Wire [West, South], Source [South]], [Wire [East, North], Sink [West, North], Wire [North, South]], [Wire [], Wire [], Sink [North]]] == Just (replicate 3 (replicate 3 R0))" True (solveCircuit [[Source [East, South], Wire [West, South], Source [South]], [Wire [East, North], Sink [West, North], Wire [North, South]], [Wire [], Wire [], Sink [North]]] == Just (replicate 3 (replicate 3 R0))))

c2tests = TestLabel "Challenge 2 Tests" (
  TestList [
  TestLabel "Unsolvable puzzle returns Nothing" c2test1,
  TestLabel "Puzzle that is already connected but not complete (and can be solved) returns a solution with non-zero rotations" c2test2,
  TestLabel "Puzzle that is already complete returns full-zero solution" c2test3
  ])

-- Challenge 3 Tests
rmSpace :: String -> String
rmSpace s = [c | c <- s, c /= ' ']
c3test1 = TestLabel "Spec example 1" $ TestCase (assertEqual "prettyPrint (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1)))" "(\\x1->x1)\\x1->x1" (rmSpace (prettyPrint (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))))))
c3test2 = TestLabel "Spec example 2" $ TestCase (assertEqual "prettyPrint (Let Discard (Var 0) (Abs (V 1) (App (Var 1) (Abs (V 1) (Var 1)))))" "let_=x0in\\x1->x1\\x1->x1" (rmSpace (prettyPrint (Let Discard (Var 0) (Abs (V 1) (App (Var 1) (Abs (V 1) (Var 1))))))))
c3test3 = TestLabel "Spec example 3" $ TestCase (assertEqual "prettyPrint (Abs (V 1) (Abs Discard (Abs (V 2) (App (Var 2) (Var 1)))))" "\\x1_x2->x2x1" (rmSpace (prettyPrint (Abs (V 1) (Abs Discard (Abs (V 2) (App (Var 2) (Var 1))))))))
c3test4 = TestLabel "Spec example 4" $ TestCase (assertEqual "prettyPrint (App (Var 2) (Abs (V 1) (Abs Discard (Var 1))))" "x2\\x1_->x1" (rmSpace (prettyPrint (App (Var 2) (Abs (V 1) (Abs Discard (Var 1)))))))
c3test5 = TestLabel "Let syntax sugar" $ TestCase (assertEqual "prettyPrint (Let (V 1) (Abs (V 2) (Abs Discard (Var 3))) (App (Var 4) (Var 5)))" "letx1x2_=x3inx4x5" (rmSpace (prettyPrint (Let (V 1) (Abs (V 2) (Abs Discard (Var 3))) (App (Var 4) (Var 5))))))
c3test6 = TestLabel "Prints Var correctly" $ TestCase (assertEqual "prettyPrint (Var 1)" "x1" (rmSpace $ prettyPrint (Var 1)))
c3test7 = TestLabel "Prints Abs correctly" $ TestCase (assertEqual "prettyPrint (Abs (V 1) (Var 2))" "\\x1->x2" (rmSpace $ prettyPrint (Abs (V 1) (Var 2))))
c3test8 = TestLabel "Prints Let correctly" $ TestCase (assertEqual "prettyPrint (Let (V 1) (Var 2) (Var 3))" "letx1=x2inx3" (rmSpace $ prettyPrint (Let (V 1) (Var 2) (Var 3))))
c3test9 = TestLabel "Prints Pair correctly" $ TestCase (assertEqual "prettyPrint (Pair (Var 1) (Var 2))" "(x1,x2)" (rmSpace $ prettyPrint (Pair (Var 1) (Var 2))))
c3test10 = TestLabel "Prints Fst correctly" $ TestCase (assertEqual "prettyPrint (Fst (Var 1))" "fst(x1)" (rmSpace $ prettyPrint (Fst (Var 1))))
c3test11 = TestLabel "Prints Snd correctly" $ TestCase (assertEqual "prettyPrint (Snd (Var 1))" "snd(x1)" (rmSpace $ prettyPrint (Snd (Var 1))))
c3test12 = TestLabel "Prints App correctly" $ TestCase (assertEqual "prettyPrint (App (Var 1) (Var 2))" "x1x2" (rmSpace $ prettyPrint (App (Var 1) (Var 2))))


c3tests = TestLabel "Challenge 3 Tests" (TestList [c3test1, c3test2, c3test3, c3test4, c3test5, c3test6, c3test7, c3test8, c3test9, c3test10, c3test11, c3test12])

-- Challenge 4 Tests

c4test1 = TestLabel "Brackets change App association" $ TestCase (assertEqual "parseLetx \"x1 (x2 x3)\"" (Just (App (Var 1) (App (Var 2) (Var 3)))) (parseLetx "x1 (x2 x3)"))
c4test2 = TestLabel "App associates left" $ TestCase (assertEqual "parseLetx \"x1 x2 x3\"" (Just (App (App (Var 1) (Var 2)) (Var 3))) (parseLetx "x1 x2 x3"))
c4test3 = TestLabel "Let syntax sugar" $ TestCase (assertEqual "parseLetx \"let x1 x3 = x2 in x1 x2\"" (Just (Let (V 1) (Abs (V 3) (Var 2)) (App (Var 1) (Var 2)))) (parseLetx "let x1 x3 = x2 in x1 x2"))
c4test4 = TestLabel "Let syntax sugar with Discard into an Abs" $ TestCase (assertEqual "parseLetx \"let x1 _ x3 = x3 in \\x3 -> x1 x3 x3\"" (Just (Let (V 1) (Abs Discard (Abs (V 3) (Var 3))) (Abs (V 3) (App (App (Var 1) (Var 3)) (Var 3))))) (parseLetx "let x1 _ x3 = x3 in \\x3 -> x1 x3 x3"))
c4test5 = TestLabel "App requires space" $ TestCase (assertEqual "parseLetx \"x1x2\"" Nothing (parseLetx "x1x2"))
c4test6 = TestLabel "Abs syntax sugar" $ TestCase (assertEqual "parseLetx \"\\x1 x2 x3 -> x4 x5\"" (Just (Abs (V 1) (Abs (V 2) (Abs (V 3) (App (Var 4) (Var 5)))))) (parseLetx "\\x1 x2 x3 -> x4 x5"))
c4test7 = TestLabel "All expressions" $ TestCase (assertEqual "parseLetx \"x1 (let x2 _ = x3 in x4) fst ((x5, snd (\\x6 -> x7 x8))) let x9 = x10 in \\_ -> x11 x12\"" (Just $ App (App (App (Var 1) (Let (V 2) (Abs Discard (Var 3)) (Var 4))) (Fst (Pair (Var 5) (Snd (Abs (V 6) (App (Var 7) (Var 8))))))) (Let (V 9) (Var 10) (Abs Discard (App (Var 11) (Var 12))))) (parseLetx "x1 (let x2 _ = x3 in x4) fst ((x5, snd (\\x6 -> x7 x8))) let x9 = x10 in \\_ -> x11 x12"))
c4test8 = TestLabel "Parses Var correctly" $ TestCase (assertEqual "parseLetx \"x1\"" (Just $ Var 1) (parseLetx "x1"))
c4test9 = TestLabel "Parses Abs correctly" $ TestCase (assertEqual "parseLetx \"\\x1 -> x2\"" (Just $ Abs (V 1) (Var 2)) (parseLetx "\\x1 -> x2"))
c4test10 = TestLabel "Parses Let correctly" $ TestCase (assertEqual "parseLetx \"let x1 = x2 in x3\"" (Just $ Let (V 1) (Var 2) (Var 3)) (parseLetx "let x1 = x2 in x3"))
c4test11 = TestLabel "Parses Pair correctly" $ TestCase (assertEqual "parseLetx \"(x1, x2)\"" (Just $ Pair (Var 1) (Var 2)) (parseLetx "(x1, x2)"))
c4test12 = TestLabel "Parses Fst correctly" $ TestCase (assertEqual "parseLetx \"fst (x1)\"" (Just $ Fst $ Var 1) (parseLetx "fst (x1)"))
c4test13 = TestLabel "Parses Snd correctly" $ TestCase (assertEqual "parseLetx \"snd (x1)\"" (Just $ Snd $ Var 1) (parseLetx "snd (x1)"))
c4test14 = TestLabel "Parses App correctly" $ TestCase (assertEqual "parseLetx \"x1 x2\"" (Just $ App (Var 1) (Var 2)) (parseLetx "x1 x2"))
c4test15 = TestLabel "Partially correct expression doesn't parse" $ TestCase (assertEqual "parseLetx \"x1 x2 y3\"" Nothing (parseLetx "x1 x2 y3"))
c4test16 = TestLabel "Completely incorrect expression doesn't parse" $ TestCase (assertEqual "parseLetx \"this is not correct\"" Nothing (parseLetx "this is not correct"))
c4test17 = TestLabel "Spacing doesn't unnecessarily matter" $ TestCase (assertBool "parseLetx \" let     x1= x2   in x3  \" == parseLetx \"let x1 = x2 in x3\"" (parseLetx " let     x1= x2   in x3  " == parseLetx "let x1 = x2 in x3"))
c4tests = TestLabel "Challenge 4 Tests" (TestList [c4test1, c4test2, c4test3, c4test4, c4test5, c4test6, c4test7, c4test8, c4test9, c4test10, c4test11, c4test12, c4test13, c4test14, c4test15, c4test16, c4test17 ])

-- Challenge 5 Tests

type Mapping = [(Int, Int)]

-- | Checks if two expressions are alpha equivalent.
alphaEquivalent :: LamExpr -> LamExpr -> Bool
alphaEquivalent = alphaEquivalent' []
  where
    alphaEquivalent' :: Mapping -> LamExpr -> LamExpr -> Bool
    alphaEquivalent' m (LamVar x) (LamVar y) | value == Nothing && key == Nothing = x == y
                                             | otherwise = value == Just y && key == Just x
                                             where
                                               value = findValue m x
                                               key = findKey m y
    alphaEquivalent' m (LamAbs x e1) (LamAbs y e2) = alphaEquivalent' ((x, y):m) e1 e2
    alphaEquivalent' m (LamApp x1 x2) (LamApp y1 y2) = alphaEquivalent' m x1 y1 && alphaEquivalent' m x2 y2
    alphaEquivalent' m x y = False

    findValue :: Mapping -> Int -> Maybe Int
    findValue [] _ = Nothing
    findValue ((x, y):ms) x' | x == x' = Just y
                             | otherwise = findValue ms x'

    findKey :: Mapping -> Int -> Maybe Int
    findKey = (findValue . map (\(x, y) -> (y, x))) . reverse

c5test1 = TestLabel "Spec example 1" $ TestCase (assertEqual "alphaEquivalent (LamApp (LamAbs 0 (LamAbs 2 (LamVar 2))) (LamAbs 2 (LamVar 2))) (letEnc (Let Discard (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1)))" True (alphaEquivalent (LamApp (LamAbs 0 (LamAbs 2 (LamVar 2))) (LamAbs 2 (LamVar 2))) (letEnc (Let Discard (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))))))
c5test2 = TestLabel "Spec example 2" $ TestCase (assertEqual "alphaEquivalent (LamApp (LamAbs 0 (LamApp (LamApp (LamVar 0) (LamAbs 2 (LamVar 2))) (LamAbs 0 (LamVar 2)))) (LamAbs 0 (LamAbs 1 (LamVar 0)))) (letEnc (Fst (Pair (Abs (V 1) (Var 1)) (Abs Discard (Var 2)))))) (letEnc (Fst (Pair (Abs (V 1) (Var 1)) (Abs Discard (Var 2)))))" True (alphaEquivalent (LamApp (LamAbs 0 (LamApp (LamApp (LamVar 0) (LamAbs 2 (LamVar 2))) (LamAbs 0 (LamVar 2)))) (LamAbs 0 (LamAbs 1 (LamVar 0)))) (letEnc (Fst (Pair (Abs (V 1) (Var 1)) (Abs Discard (Var 2)))))))
c5test3 = TestLabel "Var encodes properly" $ TestCase (assertEqual "alphaEquivalent (LamVar 1) (letEnc (Var 1))" True (alphaEquivalent (LamVar 1) (letEnc (Var 1))))
c5test4 = TestLabel "Abs encodes properly" $ TestCase (assertEqual "alphaEquivalent (LamAbs 0 (LamVar 1)) (letEnc (Abs (V 0) (Var 1)))" True (alphaEquivalent (LamAbs 0 (LamVar 1)) (letEnc (Abs (V 0) (Var 1)))))
c5test5 = TestLabel "Let encodes properly" $ TestCase (assertEqual "alphaEquivalent (LamApp (LamAbs 1 (LamVar 0)) (LamVar 1)) (letEnc (Let (V 1) (Var 0) (Var 1)))" True (alphaEquivalent (LamApp (LamAbs 3 (LamVar 3)) (LamVar 0)) (letEnc (Let (V 1) (Var 0) (Var 1)))))
c5test6 = TestLabel "Pair encodes properly" $ TestCase (assertEqual "alphaEquivalent (LamAbs 1 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 3))) (letEnc (Pair (Var 0) (Var 3)))" True (alphaEquivalent (LamAbs 1 (LamApp (LamApp (LamVar 1) (LamVar 0)) (LamVar 3))) (letEnc (Pair (Var 0) (Var 3)))))
c5test7 = TestLabel "Fst encodes properly" $ TestCase (assertEqual "alphaEquivalent (LamApp (LamVar 2) (LamAbs 0 (LamAbs 1 (LamVar 0)))) (letEnc (Fst (Var 2)))" True (alphaEquivalent (LamApp (LamVar 2) (LamAbs 0 (LamAbs 1 (LamVar 0)))) (letEnc (Fst (Var 2)))))
c5test8 = TestLabel "Snd encodes properly" $ TestCase (assertEqual "alphaEquivalent (LamApp (LamVar 2) (LamAbs 0 (LamAbs 1 (LamVar 1)))) (letEnc (Snd (Var 2)))" True (alphaEquivalent (LamApp (LamVar 2) (LamAbs 0 (LamAbs 1 (LamVar 1)))) (letEnc (Snd (Var 2)))))
c5test9 = TestLabel "Discard doesn't interfere" $ TestCase (assertEqual "alphaEquivalent (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamVar 6)))))) (letEnc (Abs Discard (Abs (V 5) (Abs (V 4) (Abs (V 3) (Abs (V 2) (Var 6)))))))" True (alphaEquivalent (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamAbs 4 (LamAbs 5 (LamVar 6)))))) (letEnc (Abs Discard (Abs (V 5) (Abs (V 4) (Abs (V 3) (Abs (V 2) (Var 3)))))))))
c5tests = TestLabel "Challenge 5 Tests" (TestList [c5test1, c5test2, c5test3, c5test4, c5test5, c5test6, c5test7])

-- Challenge 6 Tests

c6test1 = TestLabel "Spec example 1" $ TestCase (assertEqual "compareRedn (Let (V 3) (Pair (App (Abs (V 1) (App (Var 1) (Var 1))) (Abs (V 2) (Var 2))) (App (Abs (V 1) (App (Var 1) (Var 1))) (Abs (V 2) (Var 2)))) (Fst (Var 3))) 10" (6,8,4,6) (compareRedn (Let (V 3) (Pair (App (Abs (V 1) (App (Var 1) (Var 1))) (Abs (V 2) (Var 2))) (App (Abs (V 1) (App (Var 1) (Var 1))) (Abs (V 2) (Var 2)))) (Fst (Var 3))) 10))
c6test2 = TestLabel "Spec example 2" $ TestCase (assertEqual "compareRedn (Let Discard (App (Abs (V 1) (Var 1)) (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1)))) (Snd (Pair (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))) (Abs (V 1) (Var 1))))) 10" (5,7,2,4) (compareRedn (Let Discard (App (Abs (V 1) (Var 1)) (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1)))) (Snd (Pair (App (Abs (V 1) (Var 1)) (Abs (V 1) (Var 1))) (Abs (V 1) (Var 1))))) 10))
c6test3 = TestLabel "Spec example 3" $ TestCase (assertEqual "compareRedn (Let (V 2) (Let (V 1) (Abs (V 0) (App (Var 0) (Var 0))) (App (Var 1) (Var 1))) (Snd (Pair (Var 2) (Abs (V 1) (Var 1))))) 100" (100,100,2,4) (compareRedn (Let (V 2) (Let (V 1) (Abs (V 0) (App (Var 0) (Var 0))) (App (Var 1) (Var 1))) (Snd (Pair (Var 2) (Abs (V 1) (Var 1))))) 100))
c6test4 = TestLabel "Something that requires no reducing takes no steps" $ TestCase (assertEqual "compareRedn (Var 1) 1" (0, 0, 0, 0) (compareRedn (Var 1) 1))

c6tests = TestLabel "Challenge 6 Tests" (TestList [c6test1, c6test2, c6test3, c6test4])

--
tests = TestList [c1tests, c2tests, c3tests, c4tests, c5tests, c6tests]