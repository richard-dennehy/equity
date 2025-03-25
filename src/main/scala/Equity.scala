// given 0-5 community cards, and two pairs of hole cards, calculate the equity for each player
def equityProbably(community: Vector[Card], p1: (Card, Card), p2: (Card, Card)): Outcome = {
  val drawn = Vector(p1._1, p1._2, p2._1, p2._2) ++ community
  val outcome = Outcome(
    Equity(0.0, 0.0),
    Equity(0.0, 0.0),
    0
  )

  inline def updateOutcome(communityCards: Vector[Card]) = {
    val p1Best = bestHand(communityCards, p1)
    val p2Best = bestHand(communityCards, p2)

    if (p1Best > p2Best) {
      outcome.p1.win += 1
    } else if (p1Best == p2Best) {
      outcome.p1.draw += 1
      outcome.p2.draw += 1
    } else {
      outcome.p2.win += 1
    }

    outcome.possibilities += 1
  }

  if (community.size == 5) {
    // only one possible hand
    updateOutcome(community)
  } else {
    // draw 5th card
    var rank5 = Rank.Ace.value
    var suit5 = 0

    while (rank5 >= 2) {
      val card5 = card(Rank.fromValue(rank5), Suit.fromOrdinal(suit5))
      if (drawProbability(card5, drawn) != 0f) {
        if (community.size <= 3) {
          // draw 4th card
          var rank4 = rank5
          var suit4 = suit5 + 1

          // given the loop goes <R>s, <R>d, <R>c, <R>h, if outer suit is hearts, need to go to next rank, starting from Spades again
          if (suit4 >= 4) {
            rank4 -= 1
            suit4 = 0
          }

          while (rank4 >= 2) {
            val card4 = card(Rank.fromValue(rank4), Suit.fromOrdinal(suit4))

            if (drawProbability(card4, drawn :+ card5) != 0f) {
              if (community.size < 3) {
                // draw flop cards
                var rank3 = rank4
                var suit3 = suit4 + 1

                if (suit3 >= 4) {
                  rank3 -= 1
                  suit3 = 0
                }

                while (rank3 >= 2) {
                  val card3 = card(Rank.fromValue(rank3), Suit.fromOrdinal(suit3))
                  if (drawProbability(card3, drawn :+ card5 :+ card4) != 0f) {
                    var rank2 = rank3
                    var suit2 = suit3 + 1

                    if (suit2 >= 4) {
                      rank2 -= 1
                      suit2 = 0
                    }

                    while (rank2 >= 2) {
                      val card2 = card(Rank.fromValue(rank2), Suit.fromOrdinal(suit2))
                      if (drawProbability(card2, drawn :+ card5 :+ card4 :+ card3) != 0f) {
                        var rank1 = rank2
                        var suit1 = suit2 + 1

                        if (suit1 >= 4) {
                          rank1 -= 1
                          suit1 = 0
                        }

                        while (rank1 >= 2) {
                          val card1 = card(Rank.fromValue(rank1), Suit.fromOrdinal(suit1))
                          if (drawProbability(card1, drawn :+ card5 :+ card4 :+ card3 :+ card2) != 0f) {
                            updateOutcome(Vector(card1, card2, card3, card4, card5))
                          }

                          suit1 = suit1 + 1

                          if (suit1 >= 4) {
                            rank1 -= 1
                            suit1 = 0
                          }
                        }
                      }

                      suit2 = suit2 + 1
                      if (suit2 >= 4) {
                        rank2 -= 1
                        suit2 = 0
                      }
                    }
                  }

                  suit3 = suit3 + 1
                  if (suit3 >= 4) {
                    rank3 -= 1
                    suit3 = 0
                  }
                }
              } else {
                updateOutcome(community :+ card4 :+ card5)
              }
            }

            suit4 = suit4 + 1
            if (suit4 >= 4) {
              rank4 -= 1
              suit4 = 0
            }
          }
        } else {
          updateOutcome(community :+ card5)
        }
      }

      suit5 = suit5 + 1
      if (suit5 >= 4) {
        rank5 -= 1
        suit5 = 0
      }
    }
  }

  outcome.p1.win /= outcome.possibilities
  outcome.p1.draw /= outcome.possibilities

  outcome.p2.win /= outcome.possibilities
  outcome.p2.draw /= outcome.possibilities

  outcome
}

case class Outcome(
  var p1: Equity,
  var p2: Equity,
  var possibilities: Int
)

case class Equity(var win: Float, var draw: Float)

// Returns 0 <= p <= 1 as the probability of drawing a card
//
// If `drawn` contains a matching card, this will return 0.
//
// Not withstanding the above, the larger `drawn` is, the higher the chance of drawing any particular card (because this implies the deck is smaller).
inline def drawProbability(card: Card, drawn: Vector[Card]): Float = {
  val deckSize = 52f - drawn.size

  if (drawn.contains(card)) 0 else 1 / deckSize
}

// FIXME this function appears to be astoundingly slow
//  I haven't bothered measuring properly, but anecdotally, the runtime of the "empty community hand" test goes from ~200ms to ~2s when this function is introduced
//
// Given 5 community cards and a pair of hole cards, returns the best possible hand of 5 cards. This hand doesn't necessarily contain either hole card.
def bestHand(communityCards: Vector[Card], holeCards: (Card, Card)): RankedHand = {
  var best = Hand(communityCards(0), communityCards(1), communityCards(2), communityCards(3), communityCards(4)).rank

  inline def tryHand(hand: Hand) = {
    best = math.max(hand.rank, best)
  }

  // remove 1 and replace with A/B
  tryHand(Hand(communityCards(0), communityCards(1), communityCards(2), communityCards(3), holeCards._1))
  tryHand(Hand(communityCards(0), communityCards(1), communityCards(2), communityCards(3), holeCards._2))

  tryHand(Hand(communityCards(0), communityCards(1), communityCards(2), holeCards._1, communityCards(4)))
  tryHand(Hand(communityCards(0), communityCards(1), communityCards(2), holeCards._2, communityCards(4)))

  tryHand(Hand(communityCards(0), communityCards(1), holeCards._1, communityCards(3), communityCards(4)))
  tryHand(Hand(communityCards(0), communityCards(1), holeCards._2, communityCards(3), communityCards(4)))

  tryHand(Hand(communityCards(0), holeCards._1, communityCards(2), communityCards(3), communityCards(4)))
  tryHand(Hand(communityCards(0), holeCards._2, communityCards(2), communityCards(3), communityCards(4)))

  tryHand(Hand(holeCards._1, communityCards(1), communityCards(2), communityCards(3), communityCards(4)))
  tryHand(Hand(holeCards._2, communityCards(1), communityCards(2), communityCards(3), communityCards(4)))

  // remove 2 and replace with A and B
  // @formatter:off
  tryHand(Hand(communityCards(0), communityCards(1), communityCards(2),      holeCards._1,      holeCards._2))
  tryHand(Hand(communityCards(0), communityCards(1),      holeCards._1, communityCards(3),      holeCards._2))
  tryHand(Hand(communityCards(0), communityCards(1),      holeCards._1,      holeCards._2, communityCards(4)))
  tryHand(Hand(communityCards(0),      holeCards._1, communityCards(2), communityCards(3),      holeCards._2))
  tryHand(Hand(communityCards(0),      holeCards._1, communityCards(2),      holeCards._2, communityCards(4)))
  tryHand(Hand(communityCards(0),      holeCards._1,      holeCards._2, communityCards(3), communityCards(4)))
  tryHand(Hand(holeCards._1,      communityCards(1), communityCards(2), communityCards(3),      holeCards._2))
  tryHand(Hand(holeCards._1,      communityCards(1), communityCards(2),      holeCards._2, communityCards(4)))
  tryHand(Hand(holeCards._1,      communityCards(1),      holeCards._2, communityCards(3), communityCards(4)))
  tryHand(Hand(holeCards._1,      holeCards._2,      communityCards(2), communityCards(3), communityCards(4)))
  // @formatter:on

  best
}
