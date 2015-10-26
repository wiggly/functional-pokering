-- -*- mode: haskell; -*-

module Poker_Test () where

import Poker (
  HoldEmHand(..),
  PokerRank(..),
  isPair, isTwoPair, isThreeOfAKind, isStraight, isFlush, isFullHouse, isFourOfAKind, isStraightFlush,
  pokerEquity, newWin, newTie, newLoss,
  pokerRank, readHoldEmHand
  )

import Test.HUnit
import Cards
import Data.Maybe (fromJust)

testCards :: String -> [Card]
testCards str = fromJust $ readCards str

testHand :: String -> HoldEmHand
testHand str = fromJust $ readHoldEmHand str

assertTrue :: String -> Bool -> Assertion
assertTrue s b = assertBool s b

assertFalse :: String -> Bool -> Assertion
assertFalse s b = assertBool s (not b)

handTestCase :: String -> ([Card] -> Bool) -> (String -> Bool -> Assertion) -> String -> Test
handTestCase cards pred assertion message = TestCase $ assertion fullMessage (pred $ testCards cards)
  where fullMessage = cards ++ " is " ++ message

pairTests = TestList [
  handTestCase "Ks Kh 2c 3d 8s 4d 9c" isPair assertTrue "Pair",
  handTestCase "Ks Qh 2c 3d 8s 4d 9c" isPair assertFalse "NOT Pair"
  ]

twoPairTests = TestList [
  handTestCase "Ks Kh Qs Qh 3d 4d 9c" isTwoPair assertTrue "Two Pair",
  handTestCase "Ks Kh 2c 3d 8s 4d 9c" isTwoPair assertFalse "NOT Two Pair",
  handTestCase "Ks Qh 2c 3d 8s 4d 9c" isTwoPair assertFalse "NOT Two Pair"
  ]

threeOfAKindTests = TestList [
  handTestCase "Ks Kh Kd 2c 5s 4d 9c" isThreeOfAKind assertTrue "Three of a Kind",
  handTestCase "Ks Kh Qd 2c 5s 4d 9c" isThreeOfAKind assertFalse "NOT Three of a Kind"
  ]

straightTests = TestList [
  handTestCase "As 2h 3d 4c 5s 4d 9c" isStraight assertTrue "Straight",
  handTestCase "2h 3d 4c 5s 6h 4d 9c" isStraight assertTrue "Straight",
  handTestCase "3d 4c 5s 6h 7d 4d 9c" isStraight assertTrue "Straight",
  handTestCase "9s Th Jd Qc Ks 4d 3c" isStraight assertTrue "Straight",
  handTestCase "Th Jd Qc Ks Ah 4d 3c" isStraight assertTrue "Straight",
  handTestCase "As 2s 3d 4c 6s 4d 9c" isStraight assertFalse "NOT Straight"
  ]

flushTests = TestList [
  handTestCase "As 2s 3s 4s 6s 4d 9c" isFlush assertTrue "Flush",
  handTestCase "As 2s 3s 4s 5s 4d 9c" isFlush assertTrue "Flush",
  handTestCase "Js 8s 2s Ks 3s 4d 9c" isFlush assertTrue "Flush",
  handTestCase "As 2s 3s 4c 6s 4d 9c" isFlush assertFalse "NOT Flush",
  handTestCase "Ac 2h 3d 4c 6s 4d 9c" isFlush assertFalse "NOT Flush"
  ]

fullHouseTests = TestList [
  handTestCase "As Ah Ad Ks Kh 4d 9c" isFullHouse assertTrue "Full House",
  handTestCase "Ks As Kh Ah Ad 4d 9c" isFullHouse assertTrue "Full House",
  handTestCase "2s 2h 2d 3s 3h 4d 9c" isFullHouse assertTrue "Full House",
  handTestCase "2s 2h 2d As Kh 4d 9c" isFullHouse assertFalse "NOT Full House",
  handTestCase "2s 4h 2d 3s 3h 4d 9c" isFullHouse assertFalse "NOT Full House",
  handTestCase "2s 2h 2d 9s 3h 4d 8c" isFullHouse assertFalse "NOT Full House"
  ]

fourOfAKindTests = TestList [
  handTestCase "As Ah Ad Ac Ks 4d 9c" isFourOfAKind assertTrue "Four of a Kind",
  handTestCase "As Ah Ad Ac 2s 4d 9c" isFourOfAKind assertTrue "Four of a Kind",
  handTestCase "2s 2h 2d 2c Ks 4d 9c" isFourOfAKind assertTrue "Four of a Kind",
  handTestCase "2s Kh 2d 2c 2h 4d 9c" isFourOfAKind assertTrue "Four of a Kind",
  handTestCase "As 2h Ad Ac Ks 4d 9c" isFourOfAKind assertFalse "NOT Four of a Kind",
  handTestCase "As Ah Qd Ac Ks 4d 9c" isFourOfAKind assertFalse "NOT Four of a Kind",
  handTestCase "9s Ah Ad Ac Ks 4d 9c" isFourOfAKind assertFalse "NOT Four of a Kind"
  ]

straightFlushTests = TestList [
  handTestCase "As 2s 3s 4s 5s 4d 9c" isStraightFlush assertTrue "Straight Flush",
  handTestCase "2s 3s 4s 5s 6s 4d 9c" isStraightFlush assertTrue "Straight Flush",
  handTestCase "As 4s 5s 2s 3s 4d 9c" isStraightFlush assertTrue "Straight Flush",
  handTestCase "Ts Js Qs Ks As 4d 9c" isStraightFlush assertTrue "Straight Flush",
  handTestCase "Ts Js 9s Qs Ks 4d 9c" isStraightFlush assertTrue "Straight Flush",
  handTestCase "Ac 2s 3s 4s 5s 4d 9c" isStraightFlush assertFalse "NOT Straight Flush",
  handTestCase "As 6s 3s 4s 5s 4d 9c" isStraightFlush assertFalse "NOT Straight Flush",
  handTestCase "As 2s Qs 4s 5s 4d 9c" isStraightFlush assertFalse "NOT Straight Flush",
  handTestCase "As 2s 3s 4s 4c 4d 9c" isStraightFlush assertFalse "NOT Straight Flush"
  ]

testPokerRank_1 = TestCase $ assertEqual "Board: 8d Qd 8c Jc 3d with Hand: Ac, Js should be TwoPair"
                  TwoPair
                  ( pokerRank (testCards "8d Qd 8c Jc 3d") (testHand "AcJs") )

rankTests = TestList [ testPokerRank_1 ]


equityTest_1 = TestCase $ assertEqual "AsKs vs 7d2c on 9c 3d 8s 4h Jd"
                  [((testHand "AsKs"),newWin),((testHand "7d2c"),newLoss)]
                  ( pokerEquity (testCards "9c 3d 8s 4h Jd") [(testHand "AsKs"),(testHand "7d2c")] )



equityTests = TestList [ equityTest_1 ]



allTests = TestList [
  pairTests,
  twoPairTests,
  threeOfAKindTests,
  straightTests,
  flushTests,
  fullHouseTests,
  fourOfAKindTests,
  straightFlushTests,
  rankTests,
  equityTests ]

main = runTestTT allTests
