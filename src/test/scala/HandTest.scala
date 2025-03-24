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

  it should "compare hands properly" in {
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
      hands.head.category should be > hands(1).category
      hands(1).category should be < hands.head.category
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
      hands.head.category should be < hands(1).category withClue s"\n${hands.head.category.debugString}\n${hands(1).category.debugString}"
      hands(1).category should be > hands.head.category
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

                            hand.category shouldBe HighCard.mask | (highCard.mask + second.mask + third.mask + fourth.mask + fifth.mask)
                            count += 1

                            hand.category should be <= prevHand.category
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
                    hand.category shouldBe expected
                    count += 1

                    hand.category should be <= prevHand.category
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

                hand.category shouldBe TwoPairs.mask | 1L << (pairOne.value - 2 + 13) | 1L << (pairTwo.value - 2 + 13) | 1L << (kicker.value - 2)
                count += 1

                hand.category should be <= prevHand.category
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

                hand.category shouldBe ThreeOfAKind.mask | 1L << (trips.value - 2 + 26) | kickerMask(kickerOne, kickerTwo)
                count += 1

                hand.category should be <= prevHand.category
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

                  hand.category shouldBe Straight.mask | (highCard.mask + (highCard - 1).mask + (highCard - 2).mask + (highCard - 3).mask + (if (highCard == Five) 0 else lowCard.mask))
                  count += 1

                  hand.category should be <= prevHand.category
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

                  hand.category shouldBe Flush.mask | (highCard.mask + second.mask + third.mask + fourth.mask + fifth.mask)
                  count += 1

                  hand.category should be <= prevHand.category
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

            hand.category shouldBe FullHouse.mask | 1L << (trips.value - 2 + 26) | 1L << (pair.value - 2 + 13)
            count += 1

            hand.category should be <= prevHand.category
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

          hand.category shouldBe FourOfAKind.mask | 1L << (quads.value - 2 + 13) | 1L << (kicker.value - 2)
          count += 1

          hand.category should be <= prevHand.category
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

        hand.category shouldBe StraightFlush.mask | (highCard.mask + (highCard - 1).mask + (highCard - 2).mask + (highCard - 3).mask + (if (highCard == Five) 0 else lowCard.mask))
        count += 1

        hand.category should be <= prevHand.category
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
}
