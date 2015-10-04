module Poker_Test () where

import Poker ( PokerRank(..), isPair, isTwoPair, isThreeOfAKind, isStraight, pokerRank )
import Test.HUnit
import Cards

testIsPair_1 = TestCase $ assertBool "A set of cards containing a pair should be true"
               ( isPair [
                    Card King Spades,
                    Card Queen Spades,
                    Card Ten Spades,
                    Card Nine Spades,
                    Card Eight Spades,
                    Card Seven Spades,
                    Card King Hearts
                    ] )

testIsPair_2 = TestCase $ assertBool "A set of cards containing no pair should not be true"
               ( not $ isPair [
                    Card King Spades,
                    Card Queen Spades,
                    Card Ten Spades,
                    Card Nine Spades,
                    Card Eight Spades,
                    Card Seven Spades,
                    Card Six Spades
                    ] )

pairTests = TestList [ testIsPair_1, testIsPair_2 ]

testIsTwoPair_1 = TestCase $ assertBool "A set of cards containing two pair should be true"
                  ( isTwoPair [
                       Card King Spades,
                       Card Queen Spades,
                       Card Ten Spades,
                       Card Nine Spades,
                       Card Eight Spades,
                       Card King Hearts,
                       Card Queen Hearts
                       ] )

testIsTwoPair_2 = TestCase $ assertBool "A set of cards containing one pair should not be true"
                  ( not $ isTwoPair [
                       Card King Spades,
                       Card Queen Spades,
                       Card Ten Spades,
                       Card Nine Spades,
                       Card Eight Spades,
                       Card Seven Spades,
                       Card King Hearts
                       ] )

testIsTwoPair_3 = TestCase $ assertBool "A set of cards containing no pair should not be true"
                  ( not $ isTwoPair [
                       Card King Spades,
                       Card Queen Spades,
                       Card Ten Spades,
                       Card Nine Spades,
                       Card Eight Spades,
                       Card Seven Spades,
                       Card Six Spades
                       ] )

twoPairTests = TestList [ testIsTwoPair_1, testIsTwoPair_2, testIsTwoPair_3 ]

testIsThreeOfAKind_1 = TestCase $ assertBool "A set of cards containin three of a kind should be true"
                       ( isThreeOfAKind [
                            Card King Spades,
                            Card Queen Spades,
                            Card Ten Spades,
                            Card Nine Spades,
                            Card Eight Spades,
                            Card King Hearts,
                            Card King Diamonds
                            ] )

testIsThreeOfAKind_2 = TestCase $ assertBool "A set of cards not containing three of a kind should be false"
                       ( not $ isTwoPair [
                            Card King Spades,
                            Card Queen Spades,
                            Card Ten Spades,
                            Card Nine Spades,
                            Card Eight Spades,
                            Card Seven Spades,
                            Card King Hearts
                            ] )

threeOfAKindTests = TestList [ testIsThreeOfAKind_1, testIsThreeOfAKind_2 ]

testIsStraight_1 = TestCase $ assertBool "A set of cards containing ranks A, 2, 3, 4, 5 should be true"
                   ( isStraight [
                        Card Five Spades,
                        Card Four Hearts,
                        Card Three Diamonds,
                        Card Two Clubs,
                        Card Ace Spades,
                        Card King Spades,
                        Card King Hearts
                        ] )

testIsStraight_2 = TestCase $ assertBool "A set of cards containing ranks 2, 3, 4, 5, 6 should be true"
                   ( isStraight [
                        Card Six Clubs,
                        Card Five Spades,
                        Card Four Hearts,
                        Card Three Diamonds,
                        Card Two Clubs,
                        Card King Spades,
                        Card King Hearts
                        ] )

testIsStraight_3 = TestCase $ assertBool "A set of cards containing ranks 3, 4, 5, 6, 7 should be true"
                   ( isStraight [
                        Card Seven Diamonds,
                        Card Six Clubs,
                        Card Five Spades,
                        Card Four Hearts,
                        Card Three Diamonds,
                        Card King Spades,
                        Card King Hearts
                        ] )

testIsStraight_4 = TestCase $ assertBool "A set of cards containing ranks 9, T, J, Q, K should be true"
                   ( isStraight [
                        Card King Hearts,
                        Card Queen Diamonds,
                        Card Jack Clubs,
                        Card Ten Spades,
                        Card Nine Hearts,
                        Card Two Spades,
                        Card Two Hearts
                        ] )
                   
testIsStraight_5 = TestCase $ assertBool "A set of cards containing ranks T, J, Q, K, A should be true"
                   ( isStraight [
                        Card Ace Spades,
                        Card King Hearts,
                        Card Queen Diamonds,
                        Card Jack Clubs,
                        Card Ten Spades,
                        Card Two Spades,
                        Card Two Hearts
                        ] )

testIsStraight_6 = TestCase $ assertBool "A set of cards containing ranks A, 4, 5, 6, 7, Q, K should be false"
               ( not $ isStraight [
                    Card Ace Spades,
                    Card King Hearts,
                    Card Queen Diamonds,
                    Card Six Clubs,
                    Card Four Spades,
                    Card Five Spades,
                    Card Ace Hearts
                    ] )

testIsStraight_7 = TestCase $ assertBool "A set of cards containing ranks 8d, Qd, 8c, Jc, 3d, Ac, Js should be false"
               ( not $ isStraight [
                    Card Eight Diamonds,
                    Card Queen Diamonds,
                    Card Eight Clubs,
                    Card Jack Clubs,
                    Card Three Diamonds,
                    Card Ace Clubs,
                    Card Jack Spades
                    ] )


straightTests = TestList [ testIsStraight_1, testIsStraight_2, testIsStraight_3, testIsStraight_4, testIsStraight_5 ]

flushTests = TestList []

testPokerRank_1 = TestCase $ assertEqual "Board: 8d, Qd, 8c, Jc, 3d with Hand: Ac, Js should be TwoPair"
                  TwoPair
                  ( pokerRank [
                       Card Eight Diamonds,
                       Card Queen Diamonds,
                       Card Eight Clubs,
                       Card Jack Clubs,
                       Card Three Diamonds
                       ]
                    (Card Ace Clubs, Card Jack Spades) )

rankTests = TestList [ testPokerRank_1 ]

allTests = TestList [ pairTests, twoPairTests, threeOfAKindTests, straightTests, flushTests, rankTests ]

main = runTestTT allTests
