import Test.HUnit
import Challenges

test1 = TestCase (assertEqual "isPuzzleComplete []" True (isPuzzleComplete []))
test2 = TestCase (assertEqual "isPuzzleComplete [[Wire []]]" True (isPuzzleComplete [[Wire []]]))
test3 = TestCase (assertEqual "isPuzzleComplete [[Source [North]]]" False (isPuzzleComplete [[Source [North]]]))
test4 = TestCase (assertEqual "isPuzzleComplete [[Sink [North]]]" False (isPuzzleComplete [[Sink [North]]]))

completenessTests = TestList [TestLabel "Empty puzzle is complete" test1, TestLabel "Singleton empty wire is complete" test2, TestLabel "Singleton source is not complete" test3, TestLabel "Singleton sink is not complete" test4]

tests = TestList[completenessTests]