-- -*- mode: haskell; -*-

module Poker_Test () where

import Poker (
  HoleCards(..),
  PokerRank(..),
  PokerHand(..),
  pokerEquity, newWin, newTie, newLoss,
  pokerRank, readHoleCards,

  bestPokerHand
  )

import Test.HUnit
import Cards
import Data.Maybe (fromJust)

testCards :: String -> [Card]
testCards str = fromJust $ readCards str

testHand :: String -> HoleCards
testHand str = fromJust $ readHoleCards str

testHand' :: PokerRank -> String -> PokerHand
testHand' rnk str = PokerHand rnk a b c d e
  where (a:b:c:d:e:_) = fromJust $ readCards str

assertTrue :: String -> Bool -> Assertion
assertTrue s b = assertBool s b

assertFalse :: String -> Bool -> Assertion
assertFalse s b = assertBool s (not b)

handTestCase :: String -> ([Card] -> Bool) -> (String -> Bool -> Assertion) -> String -> Test
handTestCase cards pred assertion message = TestCase $ assertion fullMessage (pred $ testCards cards)
  where fullMessage = cards ++ " is " ++ message


bestHandTestCase :: String -> PokerHand -> Test
bestHandTestCase cards expected = TestCase $ assertEqual fullMessage expected actual
  where fullMessage = cards ++ " best poker hand is " ++ (show expected)
        actual = bestPokerHand $ testCards cards

testPokerRank_1 = TestCase $ assertEqual "Board: 8d Qd 8c Jc 3d with Hand: Ac, Js should be TwoPair"
                  TwoPair
                  ( pokerRank (testCards "8d Qd 8c Jc 3d") (testHand "AcJs") )

rankTests = TestList [ testPokerRank_1 ]

bestPokerHandTests = TestList [
  bestHandTestCase "Kc 4s 2s As 9d 3s 5s" $ testHand' StraightFlush "5s 4s 3s 2s 1s",
  bestHandTestCase "Kc Qs Js As 9d Ts Ks" $ testHand' StraightFlush "As Ks Qs Js Ts",

  bestHandTestCase "6d 9s 9c Tc 9d 9h Qs" $ testHand' FourOfAKind "9s 9h 9d 9c Qs",
  bestHandTestCase "6d 2s 2c 4c 2d 2h 7s" $ testHand' FourOfAKind "2s 2h 2d 2c 7s",
  -- bestHandTestCase "6d 8s 9c Tc 8d 3c Qs" $ testHand' Pair "As Ac 3d 6s 9d",
  -- bestHandTestCase "6d 8s 9c Tc 8d 3c Qs" $ testHand' Pair "As Ac 3d 6s 9d",

  bestHandTestCase "6d Kd 9c Tc 8d 3d Qd" $ testHand' Flush "Kd Qd 8d 6d 3d",
  bestHandTestCase "6d Kd 9c Ad 8d 3d Qd" $ testHand' Flush "Ad Kd Qd 8d 6d",

  bestHandTestCase "7s Ks 4s 2s 3s Qd Kd" $ testHand' Flush "Ks 7s 4s 3s 2s",
  
  -- this doesn't work yet but since we only have 7-card tests that is okay
  -- bestHandTestCase "7s As 4s 2s 3s Qd Kd Ad 2d 3d" $ testHand' Flush "Ad Kd Qd 3d 2d",


  -- bestHandTestCase "6d Kd 9c Tc 8d 3d Qd" $ testHand' Flush "Kd Qd 8d 6d 3d",
  -- bestHandTestCase "6d Kd 9c Tc 8d 3d Qd" $ testHand' Flush "Kd Qd 8d 6d 3d",



  -- TODO: fix how low-straights get displayed/ordered
  bestHandTestCase "4c 5s As 2h 3d 4d 9c" $ testHand' Straight "5s 4d 3d 2h 1s",
  bestHandTestCase "5s 2h 3d 4c 6h 4d 9c" $ testHand' Straight "6h 5s 4d 3d 2h",
  bestHandTestCase "3d 4c 4d 9c 5s 6h 7d" $ testHand' Straight "7d 6h 5s 4d 3d",
  bestHandTestCase "Jd Qc Ks 9s Th 4d 3c" $ testHand' Straight "Ks Qc Jd Th 9s",
  bestHandTestCase "Th Jd Ah 4d Qc Ks 3c" $ testHand' Straight "Ah Ks Qc Jd Th",

  
  -- TODO: add code to handle Low Ace
  bestHandTestCase "6c 5h 4c 3s 2s Kd As" $ testHand' Straight "6s 5s 4s 3s 2s",



  bestHandTestCase "6d Qs 2c 4c 2d 2h 7s" $ testHand' ThreeOfAKind "2h 2d 2c Qs 7s",

  bestHandTestCase "6d 8s Qc Tc 8d 3c Qs" $ testHand' TwoPair "Qs Qc 8s 8d Tc",

  bestHandTestCase "6d 8s 9c Tc 8d 3c Qs" $ testHand' Pair "8s 8d Qs Tc 9c",

  bestHandTestCase "8c 4s 3d 7d As 2h Jh" $ testHand' HighCard "As Jh 8c 7d 4s",
  
  bestHandTestCase "4d 5d 6c 7s 8d 9d Td" $ testHand' Flush "Td 9d 8d 5d 4d"
  ]

equityTest_1 = TestCase $ assertEqual "AsKs vs 7d2c on 9c 3d 8s 4h Jd"
                  [newWin,newLoss]
                  ( pokerEquity (testCards "9c 3d 8s 4h Jd") [(testHand "AsKs"),(testHand "7d2c")] )

equityTests = TestList [ equityTest_1 ]

allTests = TestList [
  rankTests,
  equityTests,
  bestPokerHandTests ]

main = runTestTT allTests
