import Category.{Flush, FourOfAKind, FullHouse, HighCard, OnePair, Straight, StraightFlush, ThreeOfAKind, TwoPairs}
import Rank.*
import Suit.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.targetName

class HandTest extends AnyFlatSpec with Matchers {
  "hand" should "be ordered" in {
    Hand(As, _2c, _5d, Qh, Ts) shouldBe(As, Qh, Ts, _5d, _2c)
  }

  it should "not allow duplicates" in {
    assertThrows[IllegalArgumentException](Hand(As, As, _2c, _5d, Qh))
  }

  it should "allow all high card hands" in {
    // for each rank A -> 7 and each suit select C1 (7-high is the worst hand; 6-high must either be a straight or actually Ace-high)
    //  for each rank below C1 and each suit select C2
    //   for each rank below C2 and each suit select C3
    //    for each rank below C3 and each suit select C4
    //     for each rank below C2, such that it's not actually a straight, and each suit, such that it's not a flush, select C5
    var count = 0

    Suit.values.foreach { firstSuit =>
      Suit.values.foreach { secondSuit =>
        Suit.values.foreach { thirdSuit =>
          Suit.values.foreach { fourthSuit =>
            Suit.values
              .filter(s => s != fourthSuit || fourthSuit != thirdSuit || thirdSuit != secondSuit || secondSuit != firstSuit) // exclude flushes
              .foreach { fifthSuit =>
              Rank.values.takeWhile(_ != Six).foreach { highCard =>
                Rank.values
                  .drop(highCard.ordinal + 1) // keep the cards in rank order to keep this easier to reason about and minimise permutations
                  .takeWhile(_ != Four) // following the above rule, this card has to be at least 5, otherwise there aren't enough lower rank cards to finish the hand
                  .filter(highCard != Ace || _ != Five) // avoid accidentally making a 5-A straight flush
                  .foreach { second =>
                    Rank.values.drop(second.ordinal + 1).takeWhile(_ != Three).foreach { third =>
                      Rank.values.drop(third.ordinal + 1).takeWhile(_ != Two).foreach { fourth =>
                        Rank.values.drop(fourth.ordinal + 1).dropWhile(highCard.value - _.value < 5).foreach { fifth =>
                          val hand = Hand(
                            card(highCard, firstSuit),
                            card(second, secondSuit),
                            card(third, thirdSuit),
                            card(fourth, fourthSuit),
                            card(fifth, fifthSuit),
                          )

                          hand.category shouldBe HighCard
                          count += 1
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

    Suit.values.combinations(2).foreach { pairSuits =>
      Suit.values.foreach { kickerSuitOne =>
        Suit.values.foreach { kickerSuitTwo =>
          Suit.values.foreach { kickerSuitThree =>
            Rank.values.foreach { pair =>
              Rank.values.takeWhile(_ != Three).filter(_ != pair).foreach { kickerOne =>
                Rank.values.drop(kickerOne.ordinal + 1).filter(_ != pair).dropRight(1).foreach { kickerTwo =>
                  Rank.values.drop(kickerTwo.ordinal + 1).filter(_ != pair).foreach { kickerThree =>
                    val hand = Hand(
                      card(pair, pairSuits(0)),
                      card(pair, pairSuits(1)),
                      card(kickerOne, kickerSuitOne),
                      card(kickerTwo, kickerSuitTwo),
                      card(kickerThree, kickerSuitThree),
                    )

                    hand.category shouldBe OnePair
                    count += 1
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

    Suit.values.combinations(2).foreach { pairOneSuits =>
      Suit.values.combinations(2).foreach { pairTwoSuits =>
        Suit.values.foreach { kickerSuit =>
          Rank.values.foreach { pairOne =>
            Rank.values.drop(pairOne.ordinal + 1).foreach { pairTwo =>
              Rank.values.filter(r => r != pairOne && r != pairTwo).foreach { kicker =>
                val hand = Hand(
                  card(pairOne, pairOneSuits(0)),
                  card(pairOne, pairOneSuits(1)),
                  card(pairTwo, pairTwoSuits(0)),
                  card(pairTwo, pairTwoSuits(1)),
                  card(kicker, kickerSuit)
                )

                hand.category shouldBe TwoPairs
                count += 1
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

    Suit.values.combinations(3).foreach { tripsSuits =>
      Suit.values.foreach { fourthSuit =>
        Suit.values.foreach { fifthSuit =>
          Rank.values.foreach { trips =>
            Rank.values.filter(_ != trips).foreach { kickerOne =>
              Rank.values.drop(kickerOne.ordinal + 1).filter(_ != trips).foreach { kickerTwo =>
                val hand = Hand(
                  card(trips, tripsSuits(0)),
                  card(trips, tripsSuits(1)),
                  card(trips, tripsSuits(2)),
                  card(kickerOne, fourthSuit),
                  card(kickerTwo, fifthSuit)
                )

                hand.category shouldBe ThreeOfAKind
                count += 1
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

    Suit.values.foreach { firstSuit =>
      Suit.values.foreach { secondSuit =>
        Suit.values.foreach { thirdSuit =>
          Suit.values.foreach { fourthSuit =>
            Suit.values
              .filter(s => s != fourthSuit || fourthSuit != thirdSuit || thirdSuit != secondSuit || secondSuit != firstSuit) // exclude flushes
              .foreach { fifthSuit =>
                Rank.values.takeWhile(_ != Five).foreach { highCard =>
                  val hand = Hand(
                    card(highCard, firstSuit),
                    card(highCard - 1, secondSuit),
                    card(highCard - 2, thirdSuit),
                    card(highCard - 3, fourthSuit),
                    card(highCard - 4, fifthSuit),
                  )

                  hand.category shouldBe Straight
                  count += 1
                }

                val hand = Hand(
                  card(Five, firstSuit),
                  card(Four, secondSuit),
                  card(Three, thirdSuit),
                  card(Two, fourthSuit),
                  card(Ace, fifthSuit),
                )

                hand.category shouldBe Straight
                count += 1
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

    Suit.values.foreach { suit =>
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
                  val hand = Hand(
                    card(highCard, suit),
                    card(second, suit),
                    card(third, suit),
                    card(fourth, suit),
                    card(fifth, suit),
                  )

                  hand.category shouldBe Flush
                  count += 1
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

            hand.category shouldBe FullHouse
            count += 1
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

          hand.category shouldBe FourOfAKind
          count += 1
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

    Suit.values.foreach { suit =>
      Rank.values.takeWhile(_ != Five).foreach { highCard =>
        val hand = Hand(
          card(highCard, suit),
          card(highCard - 1, suit),
          card(highCard - 2, suit),
          card(highCard - 3, suit),
          card(highCard - 4, suit),
        )

        hand.category shouldBe StraightFlush
        count += 1
      }

      val hand = Hand(
        card(Five, suit),
        card(Four, suit),
        card(Three, suit),
        card(Two, suit),
        card(Ace, suit),
      )

      hand.category shouldBe StraightFlush
      count += 1
    }

    // NOTE there's 40 meaningfully distinct straight flushes
    count shouldBe 40
  }

  extension (r: Rank) {
    @targetName("sub")
    // this is a bit unintuitive, but the short of it is the enum is declared in highest rank to lowest rank order, but ordinals go in the reverse order
    def -(ranks: Int): Rank = Rank.fromOrdinal(r.ordinal + ranks)
  }
}
