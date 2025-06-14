import Category.{Flush, FourOfAKind, FullHouse, HighCard, OnePair, Straight, StraightFlush, ThreeOfAKind, TwoPairs}
import Rank.*
import Suit.*
import org.scalatest.AppendedClues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.targetName

class HandTest extends AnyFlatSpec with Matchers with AppendedClues {
  "hand" should "have a vaguely sane representation" in {
    Hand(As, _2c, _5d, Qh, Ts).debugString shouldBe
      """     s 4 f f s 3 2 1 h
        |sdch_f_k_h_l_t_k_p_p_c___AAAKKKQQQJJJTTT999888777666555444333222
        |1111000000000000000000000001000001000001000000000000001000000001""".stripMargin
  }

  "hand7" should "have a vaguely sane representation" in {
    Hand7(As, Kd, Tc, Th, _7s, _6c, _6h).debugString7 shouldBe
      """
        |hcds|hcds|hcds|hcds|hcds|hcds|hcds|1111|2222|3333|4444|5555|6666|7777
        |0001|0010|0100|1000|0001|0100|1000|1110|1101|1010|1010|0111|0110|0110""".stripMargin
  }

  "rank7" should "correctly rank a flush" in {
    val rank = Hand7(As, Js, Ts, _7s, _5h, _3s, _2s).rank7
    rank shouldBe expected(Category.Flush, Rank.Ace, Rank.Jack, Rank.Ten, Rank.Seven, Rank.Three)
  }

  it should "rank a flush higher than a straight" in {
    val rank = Hand7(As, Kd, Qc, Js, Ts, _3s, _2s).rank7
    rank shouldBe expected(Category.Flush, Rank.Ace, Rank.Jack, Rank.Ten, Rank.Three, Rank.Two)
  }

  it should "correctly rank a straight" in {
    val rank = Hand7(As, Kd, Qc, Js, Ts, _3d, _2h).rank7
    rank shouldBe Category.Straight.mask | Rank.Ace.value
  }

  it should "correctly rank a straight flush" in {
    val rank = Hand7(Ac, Kc, Qc, Jc, Tc, _3d, _2h).rank7
    rank shouldBe Category.StraightFlush.mask | Rank.Ace.value
  }

  it should "correctly rank an Ace to Five straight" in {
    val rank = Hand7(As, Td, Tc, _5d, _4c, _3s, _2s).rank7
    rank shouldBe Category.Straight.mask | Rank.Five.value
  }

  it should "rank a SF with a lower high card higher than a straight with a higher high card" in {
    val rank = Hand7(As, Kd, Qh, Jh, Th, _9h, _8h).rank7
    rank shouldBe Category.StraightFlush.mask | Rank.Queen.value
  }

  it should "correctly rank an Ace to Five straight flush" in {
    val rank = Hand7(Ad, Td, Tc, _5d, _4d, _3d, _2d).rank7
    rank shouldBe Category.StraightFlush.mask | Rank.Five.value
  }

  it should "correctly rank a Four of a Kind where the kicker is lower ranked than the quad" in {
    val rank = Hand7(_8s, _8d, _8c, _8h, _6s, _4d, _2h).rank7
    rank shouldBe Category.FourOfAKind.mask | Rank.Eight.value << 4 | Rank.Six.value
  }

  it should "correctly rank a Four of a Kind where the kicker is higher ranked than the quad" in {
    val rank = Hand7(Kd, Js, Jd, Jc, Jh, _4d, _2h).rank7
    rank shouldBe Category.FourOfAKind.mask | Rank.Jack.value << 4 | Rank.King.value
  }

  it should "correctly rank a Full House where the trip is higher ranked than the pair" in {
    val rank = Hand7(Kd, Kc, Kh, Jh, _6s, _6h, _5d).rank7
    rank shouldBe Category.FullHouse.mask | Rank.King.value << 4 | Rank.Six.value
  }

  it should "correctly rank a Full House where the trip is lower ranked than the pair" in {
    val rank = Hand7(Kd, Kc, Jh, _6s, _6c, _6h, _5d).rank7
    rank shouldBe Category.FullHouse.mask | Rank.Six.value << 4 | Rank.King.value
  }

  it should "correctly rank a Full House in a set of 7 cards with one trip and two pairs" in {
    val rank = Hand7(Kd, Kc, Jd, Jh, _6s, _6c, _6h).rank7
    rank shouldBe Category.FullHouse.mask | Rank.Six.value << 4 | Rank.King.value
  }

  it should "correctly rank a Three of a Kind where the trip is the first three cards" in {
    val rank = Hand7(Kd, Kc, Kh, Jh, _6s, _4s, _3d).rank7
    rank shouldBe Category.ThreeOfAKind.mask | Rank.King.value << 8 | Rank.Jack.value << 4 | Rank.Six.value
  }

  it should "correctly rank a Three of a Kind where the trip is cards two to four" in {
    val rank = Hand7(As, Kd, Kc, Kh, Jh, _6s, _4s).rank7
    rank shouldBe Category.ThreeOfAKind.mask | Rank.King.value << 8 | Rank.Ace.value << 4 | Rank.Jack.value
  }

  it should "correctly rank a Three of a Kind where the trip is cards three to five" in {
    val rank = Hand7(As, Kd, Td, Tc, Th, _6s, _4s).rank7
    rank shouldBe Category.ThreeOfAKind.mask | Rank.Ten.value << 8 | Rank.Ace.value << 4 | Rank.King.value
  }

  it should "correctly rank a Two Pair where the kicker is higher ranked than the high pair" in {
    val rank = Hand7(Ad, Jh, _9s, _6c, _6h, _2s, _2d).rank7
    rank shouldBe Category.TwoPairs.mask | Rank.Six.value << 8 | Rank.Two.value << 4 | Rank.Ace.value
  }

  it should "correctly rank a Two Pair where the kicker is lower ranked than the low pair" in {
    val rank = Hand7(Ad, Ac, _9s, _9c, _6s, _5h, _2d).rank7
    rank shouldBe Category.TwoPairs.mask | Rank.Ace.value << 8 | Rank.Nine.value << 4 | Rank.Six.value
  }

  it should "correctly rank a Two Pair where the kicker is in between the pairs" in {
    val rank = Hand7(Ad, Jh, _9s, _9c, _6h, _2s, _2d).rank7
    rank shouldBe Category.TwoPairs.mask | Rank.Nine.value << 8 | Rank.Two.value << 4 | Rank.Ace.value
  }

  it should "correctly rank a pair in cards one and two" in {
    val rank = Hand7(As, Ad, Ks, Jd, _8s, _6h, _5s).rank7
    rank shouldBe Category.OnePair.mask | Rank.Ace.value << 12 | Rank.King.value << 8 | Rank.Jack.value << 4 | Rank.Eight.value
  }

  it should "correctly rank a pair in cards two and three" in {
    val rank = Hand7(As, Ks, Kd, Jd, _8s, _6h, _5s).rank7
    rank shouldBe Category.OnePair.mask | Rank.King.value << 12 | Rank.Ace.value << 8 | Rank.Jack.value << 4 | Rank.Eight.value
  }

  it should "correctly rank a pair in cards three and four" in {
    val rank = Hand7(As, Ks, Jc, Jh, _8s, _6h, _5s).rank7
    rank shouldBe Category.OnePair.mask | Rank.Jack.value << 12 | Rank.Ace.value << 8 | Rank.King.value << 4 | Rank.Eight.value
  }

  it should "correctly rank a pair in cards four and five" in {
    val rank = Hand7(As, Ks, Jc, _8c, _8h, _6h, _5s).rank7
    rank shouldBe Category.OnePair.mask | Rank.Eight.value << 12 | Rank.Ace.value << 8 | Rank.King.value << 4 | Rank.Jack.value
  }

  it should "correctly rank a high card hand" in {
    val rank = Hand7(Ts, _9d, _8c, _7d, _5s, _3d, _2h).rank7
    rank shouldBe expected(Category.HighCard, Rank.Ten, Rank.Nine, Rank.Eight, Rank.Seven, Rank.Five)
  }

  it should "something" in {
    val rank1 = Hand7(Ad, _7d, _6d, _5d, _4d, _3s, _2s).rank7
    val rank2 = Hand7(Ad, Kh, Tc, _7d, _6d, _5d, _4d).rank7

    rank1 shouldBe expected(Category.Flush, Ace, Seven, Six, Five, Four)
    rank2 shouldBe expected(Category.Flush, Ace, Seven, Six, Five, Four)
  }

  "hand" should "compare hands properly" in {
    val bestTwoPair = Hand(
      card(Ace, Spades),
      card(Ace, Diamonds),
      card(King, Clubs),
      card(King, Hearts),
      card(Queen, Spades)
    )
    val worstTwoPair = Hand(
      card(Three, Spades),
      card(Three, Diamonds),
      card(Two, Clubs),
      card(Two, Hearts),
      card(Four, Spades)
    )
    val decentTwoPair = Hand(
      card(Ten, Spades),
      card(Ten, Diamonds),
      card(Six, Clubs),
      card(Six, Hearts),
      card(Queen, Spades)
    )
    val worstThreeOfAKind = Hand(
      card(Two, Spades),
      card(Two, Diamonds),
      card(Two, Clubs),
      card(Three, Hearts),
      card(Four, Spades)
    )

    val orderedHands = List(worstThreeOfAKind, bestTwoPair, decentTwoPair, worstTwoPair)

    orderedHands.sliding(2).foreach { hands =>
      hands.head.rank should be > hands(1).rank
      hands(1).rank should be < hands.head.rank
    }

    val highCard = Hand(card(Ace, Spades), card(King, Spades), card(Queen, Spades), card(Jack, Spades), card(Nine, Diamonds))
    val onePair = Hand(card(Ace, Spades), card(Ace, Diamonds), card(King, Spades), card(Queen, Spades), card(Jack, Spades))
    val straight = Hand(card(Ace, Spades), card(King, Diamonds), card(Queen, Spades), card(Jack, Spades), card(Ten, Spades))
    val flush = Hand(card(Ace, Spades), card(King, Spades), card(Queen, Spades), card(Jack, Spades), card(Nine, Spades))
    val fullHouse = Hand(card(Ace, Spades), card(Ace, Diamonds), card(Ace, Clubs), card(King, Spades), card(King, Diamonds))
    val fourOfAKind = Hand(card(Ace, Spades), card(Ace, Diamonds), card(Ace, Clubs), card(Ace, Hearts), card(King, Spades))
    val straightFlush = Hand(card(Ace, Spades), card(King, Spades), card(Queen, Spades), card(Jack, Spades), card(Ten, Spades))

    val allCategories = List(
      highCard,
      onePair,
      bestTwoPair,
      worstThreeOfAKind,
      straight,
      flush,
      fullHouse,
      fourOfAKind,
      straightFlush
    )

    allCategories.sliding(2).foreach { hands =>
      hands.head.rank should be < hands(1).rank withClue s"\n${hands.head.rank.debugString}\n${hands(1).rank.debugString}"
      hands(1).rank should be > hands.head.rank
    }
  }

  it should "allow all high card hands" in {
    // for each rank A -> 7 and each suit select C1 (7-high is the worst hand; 6-high must either be a straight or actually Ace-high)
    //  for each rank below C1 and each suit select C2
    //   for each rank below C2 and each suit select C3
    //    for each rank below C3 and each suit select C4
    //     for each rank below C2, such that it's not actually a straight, and each suit, such that it's not a flush, select C5
    var count = 0
    var prevHand = Hand(card(Ace, Spades), card(King, Spades), card(Queen, Spades), card(Jack, Spades), card(Nine, Diamonds))

    Rank.values.takeWhile(_ != Six).foreach { highCard =>
      Rank.values
        .drop(highCard.ordinal + 1) // keep the cards in rank order to keep this easier to reason about and minimise permutations
        .takeWhile(_ != Four) // following the above rule, this card has to be at least 5, otherwise there aren't enough lower rank cards to finish the hand
        .filter(highCard != Ace || _ != Five) // avoid accidentally making a 5-A straight flush
        .foreach { second =>
          Rank.values.drop(second.ordinal + 1).takeWhile(_ != Three).foreach { third =>
            Rank.values.drop(third.ordinal + 1).takeWhile(_ != Two).foreach { fourth =>
              Rank.values.drop(fourth.ordinal + 1).dropWhile(highCard.value - _.value < 5).foreach { fifth =>
                Suit.values.foreach { firstSuit =>
                  Suit.values.foreach { secondSuit =>
                    Suit.values.foreach { thirdSuit =>
                      Suit.values.foreach { fourthSuit =>
                        Suit.values
                          .filter(s => s != fourthSuit || fourthSuit != thirdSuit || thirdSuit != secondSuit || secondSuit != firstSuit) // exclude flushes
                          .foreach { fifthSuit =>
                            val hand = Hand(
                              card(highCard, firstSuit),
                              card(second, secondSuit),
                              card(third, thirdSuit),
                              card(fourth, fourthSuit),
                              card(fifth, fifthSuit),
                            )

                            hand.rank shouldBe HighCard.mask | (highCard.handMask + second.handMask + third.handMask + fourth.handMask + fifth.handMask)
                            count += 1

                            hand.rank should be <= prevHand.rank
                            prevHand = hand
                          }
                      }
                    }
                  }
                }
              }
            }
          }
        }
    }

    // NOTE there's 1302540 meaningfully distinct high cards
    count shouldBe 1302540
  }

  it should "allow all one pair hands" in {
    // for each rank and suit select C1
    //  for each different suit to C1 select C2 with the same rank
    //   for each rank and suit different to C1, select C3
    //    for each rank and suit different to C3, select C4
    //     for each rank and suit different to C4, select C5
    var count = 0
    var prevHand = Hand(card(Ace, Spades), card(Ace, Diamonds), card(King, Spades), card(Queen, Spades), card(Jack, Spades))

    Rank.values.foreach { pair =>
      Rank.values.takeWhile(_ != Three).filter(_ != pair).foreach { kickerOne =>
        Rank.values.drop(kickerOne.ordinal + 1).filter(_ != pair).dropRight(1).foreach { kickerTwo =>
          Rank.values.drop(kickerTwo.ordinal + 1).filter(_ != pair).foreach { kickerThree =>
            Suit.values.combinations(2).foreach { pairSuits =>
              Suit.values.foreach { kickerSuitOne =>
                Suit.values.foreach { kickerSuitTwo =>
                  Suit.values.foreach { kickerSuitThree =>
                    val hand = Hand(
                      card(pair, pairSuits(0)),
                      card(pair, pairSuits(1)),
                      card(kickerOne, kickerSuitOne),
                      card(kickerTwo, kickerSuitTwo),
                      card(kickerThree, kickerSuitThree),
                    )

                    val expected = OnePair.mask | (1L << (pair.value - 2 + 13)) | kickerMask(kickerOne, kickerTwo, kickerThree)
                    hand.rank shouldBe expected
                    count += 1

                    hand.rank should be <= prevHand.rank
                    prevHand = hand
                  }
                }
              }
            }
          }
        }
      }
    }

    // NOTE there's 1098240 meaningfully distinct pairs
    count shouldBe 1098240
  }

  it should "allow all two pair hands" in {
    // for each rank and suit select C1
    //  for each rank and suit different to C1, select C2
    //   for each suit different to C1, select C3 with the same rank
    //    for each suit different to C2, select C4 with the same rank
    //     for each suit and rank different to C1 and C2, select C5
    var count = 0
    var prevHand = Hand(card(Ace, Spades), card(Ace, Diamonds), card(King, Spades), card(King, Diamonds), card(Queen, Spades))

    Rank.values.foreach { pairOne =>
      Rank.values.drop(pairOne.ordinal + 1).foreach { pairTwo =>
        Rank.values.filter(r => r != pairOne && r != pairTwo).foreach { kicker =>
          Suit.values.combinations(2).foreach { pairOneSuits =>
            Suit.values.combinations(2).foreach { pairTwoSuits =>
              Suit.values.foreach { kickerSuit =>
                val hand = Hand(
                  card(pairOne, pairOneSuits(0)),
                  card(pairOne, pairOneSuits(1)),
                  card(pairTwo, pairTwoSuits(0)),
                  card(pairTwo, pairTwoSuits(1)),
                  card(kicker, kickerSuit)
                )

                hand.rank shouldBe TwoPairs.mask | 1L << (pairOne.value - 2 + 13) | 1L << (pairTwo.value - 2 + 13) | 1L << (kicker.value - 2)
                count += 1

                hand.rank should be <= prevHand.rank
                prevHand = hand
              }
            }
          }
        }
      }
    }

    // NOTE there's 123552 meaningfully distinct two pairs
    count shouldBe 123552
  }

  it should "allow all three of a kind hands" in {
    // for each rank and suit select C1
    //  for each suit different to C1, select C2 with the same rank
    //   for each suit different to C1 and C2, select C3 with the same rank
    //    for each suit and rank different to C1, select C4
    //     for each suit and rank different to C4, select C5
    var count = 0
    var prevHand = Hand(card(Ace, Spades), card(Ace, Diamonds), card(Ace, Clubs), card(King, Spades), card(Queen, Spades))

    Rank.values.foreach { trips =>
      Rank.values.filter(_ != trips).foreach { kickerOne =>
        Rank.values.drop(kickerOne.ordinal + 1).filter(_ != trips).foreach { kickerTwo =>
          Suit.values.combinations(3).foreach { tripsSuits =>
            Suit.values.foreach { fourthSuit =>
              Suit.values.foreach { fifthSuit =>
                val hand = Hand(
                  card(trips, tripsSuits(0)),
                  card(trips, tripsSuits(1)),
                  card(trips, tripsSuits(2)),
                  card(kickerOne, fourthSuit),
                  card(kickerTwo, fifthSuit)
                )

                hand.rank shouldBe ThreeOfAKind.mask | 1L << (trips.value - 2 + 26) | kickerMask(kickerOne, kickerTwo)
                count += 1

                hand.rank should be <= prevHand.rank
                prevHand = hand
              }
            }
          }
        }
      }
    }

    // NOTE there's 54912 meaningfully distinct straight flushes
    count shouldBe 54912
  }

  it should "allow all straight hands" in {
    // for each rank A-6 and each suit select C1
    //  for each suit, select C2 with one rank below C1
    //   for each suit, select C3 with one rank below C2
    //    for each suit, select C4 with one rank below C3
    //     for each suit different to C4, select C5 with one rank below C4
    // also 5-A straight
    // for each suit, select C1 with rank 5
    //  for each suit, select C2 with rank 4
    //   for each suit, select C3 with rank 3
    //    for each suit, select C4 with rank 2
    //     for each suit different to C4, select C5 with rank A
    var count = 0
    var prevHand = Hand(card(Ace, Spades), card(King, Diamonds), card(Queen, Spades), card(Jack, Spades), card(Ten, Spades))

    Rank.values.takeWhile(_ != Four).foreach { highCard =>
      Suit.values.foreach { firstSuit =>
        Suit.values.foreach { secondSuit =>
          Suit.values.foreach { thirdSuit =>
            Suit.values.foreach { fourthSuit =>
              Suit.values
                .filter(s => s != fourthSuit || fourthSuit != thirdSuit || thirdSuit != secondSuit || secondSuit != firstSuit) // exclude flushes
                .foreach { fifthSuit =>
                  val lowCard = if (highCard == Five) Ace else highCard - 4

                  val hand = Hand(
                    card(highCard, firstSuit),
                    card(highCard - 1, secondSuit),
                    card(highCard - 2, thirdSuit),
                    card(highCard - 3, fourthSuit),
                    card(lowCard, fifthSuit),
                  )

                  hand.rank shouldBe Straight.mask | (highCard.handMask + (highCard - 1).handMask + (highCard - 2).handMask + (highCard - 3).handMask + (if (highCard == Five) 0 else lowCard.handMask))
                  count += 1

                  hand.rank should be <= prevHand.rank
                  prevHand = hand
                }
            }
          }
        }
      }
    }

    // NOTE there's 10200 meaningfully distinct straights
    count shouldBe 10200
  }

  it should "allow all flush hands" in {
    // for each rank A-7 and each suit select C1
    //  for each rank below C1 and above 4 select C2 with the same suit
    //   for each rank below C2 and above 3 select C3 with the same suit
    //    for each rank below C3 and above 2 select C4 with the same suit
    //     for each rank below C2 and at least 5 ranks below C1 select C5 with the same suit
    var count = 0
    var prevHand = Hand(card(Ace, Spades), card(King, Spades), card(Queen, Spades), card(Jack, Spades), card(Nine, Spades))

    // we can't have a 6-high flush without it being a straight
    Rank.values.takeWhile(_ != Six).foreach { highCard =>
      Rank.values
        .drop(highCard.ordinal + 1) // keep the cards in rank order to keep this easier to reason about and minimise permutations
        .takeWhile(_ != Four) // following the above rule, this card has to be at least 5, otherwise there aren't enough lower rank cards to finish the hand
        .filter(highCard != Ace || _ != Five) // avoid accidentally making a 5-A straight flush
        .foreach { second =>
          Rank.values.drop(second.ordinal + 1).takeWhile(_ != Three).foreach { third =>
            Rank.values.drop(third.ordinal + 1).takeWhile(_ != Two).foreach { fourth =>
              Rank.values.drop(fourth.ordinal + 1).dropWhile(highCard.value - _.value < 5).foreach { fifth =>
                Suit.values.foreach { suit =>
                  val hand = Hand(
                    card(highCard, suit),
                    card(second, suit),
                    card(third, suit),
                    card(fourth, suit),
                    card(fifth, suit),
                  )

                  hand.rank shouldBe Flush.mask | (highCard.handMask + second.handMask + third.handMask + fourth.handMask + fifth.handMask)
                  count += 1

                  hand.rank should be <= prevHand.rank
                  prevHand = hand
                }
              }
            }
          }
        }
    }

    // NOTE there's 5108 meaningfully distinct flushes
    count shouldBe 5108
  }

  it should "allow all full house hands" in {
    // for each suit and rank select C1
    //  for each suit and rank different to C1 select C2
    //   for each suit different to C1, select C3 with the same rank
    //    for each suit different to C2, select C4 with the same rank
    //     for each suit different to C4 and C1, select C5 with the same rank
    var count = 0
    var prevHand = Hand(card(Ace, Spades), card(Ace, Diamonds), card(Ace, Clubs), card(King, Spades), card(King, Diamonds))

    Rank.values.foreach { trips =>
      Rank.values.filter(_ != trips).foreach { pair =>
        Suit.values.combinations(3).foreach { tripsSuits =>
          Suit.values.combinations(2).foreach { pairSuits =>
            val hand = Hand(
              card(trips, tripsSuits(0)),
              card(trips, tripsSuits(1)),
              card(trips, tripsSuits(2)),
              card(pair, pairSuits(0)),
              card(pair, pairSuits(1)),
            )

            hand.rank shouldBe FullHouse.mask | 1L << (trips.value - 2 + 26) | 1L << (pair.value - 2 + 13)
            count += 1

            hand.rank should be <= prevHand.rank
            prevHand = hand
          }
        }
      }
    }

    // NOTE there's 3744 meaningfully distinct full houses
    count shouldBe 3744
  }

  it should "allow all four of a kind hands" in {
    // for each suit and rank, select C1
    //  for each suit and rank different to C1, select C2
    //   select C3, C4, and C5 as the same rank as C1 but different suits
    var count = 0
    var prevHand = Hand(card(Ace, Spades), card(Ace, Diamonds), card(Ace, Clubs), card(Ace, Hearts), card(King, Spades))

    Rank.values.foreach { quads =>
      Rank.values.filter(_ != quads).foreach { kicker =>
        Suit.values.foreach { kickerSuit =>
          val hand = Hand(
            card(quads, Spades),
            card(quads, Diamonds),
            card(quads, Clubs),
            card(quads, Hearts),
            card(kicker, kickerSuit),
          )

          hand.rank shouldBe FourOfAKind.mask | 1L << (quads.value - 2 + 13) | 1L << (kicker.value - 2)
          count += 1

          hand.rank should be <= prevHand.rank
          prevHand = hand
        }
      }
    }

    // NOTE there's 624 meaningfully distinct four of a kinds
    count shouldBe 624
  }

  it should "allow all straight flush hands" in {
    // for each suit and rank from A-6 select C1
    //  select C2 as same suit as C1 and one rank lower
    //   select C3 as same suit as C2 and one rank lower
    //    select C4 as same suit as C3 and one rank lower
    //     select C5 as same suit as C4 and one rank lower
    var count = 0
    var prevHand = Hand(card(Ace, Spades), card(King, Spades), card(Queen, Spades), card(Jack, Spades), card(Ten, Spades))

    Rank.values.takeWhile(_ != Four).foreach { highCard =>
      Suit.values.foreach { suit =>
        val lowCard = if (highCard == Five) Ace else highCard - 4

        val hand = Hand(
          card(highCard, suit),
          card(highCard - 1, suit),
          card(highCard - 2, suit),
          card(highCard - 3, suit),
          card(lowCard, suit),
        )

        hand.rank shouldBe StraightFlush.mask | (highCard.handMask + (highCard - 1).handMask + (highCard - 2).handMask + (highCard - 3).handMask + (if (highCard == Five) 0 else lowCard.handMask))
        count += 1

        hand.rank should be <= prevHand.rank
        prevHand = hand
      }
    }

    // NOTE there's 40 meaningfully distinct straight flushes
    count shouldBe 40
  }

  extension (r: Rank) {
    @targetName("sub")
    // this is a bit unintuitive, but the short of it is the enum is declared in highest rank to lowest rank order, but ordinals go in the reverse order
    def -(ranks: Int): Rank = Rank.fromOrdinal(r.ordinal + ranks)
  }

  private def kickerMask(kickers: Rank*): Long = kickers.map(1L << _.value - 2).sum

  private def expected(category: Category, first: Rank, second: Rank, third: Rank, fourth: Rank, fifth: Rank): Long =
    category.mask | (first.value << 16) | (second.value << 12) | (third.value << 8) | (fourth.value << 4) | fifth.value
}
