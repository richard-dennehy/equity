import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EquityTest extends AnyFlatSpec with Matchers {
  "equityProbably" should "vaguely know when the player is likely to lose" in {
    // FIXME the floats as reported by scalatest aren't equal to themselves - assume they're being truncated or something
    val odds = equityProbably(Vector(Ts, _5s, Js), (_9c, _6h), (Td, _4h))
    // TODO how am I supposed to know if these odds are correct? They don't quite match the random online calculator
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ).toString shouldBe "(0.05959596,0.045454547,0.8949495,0.045454547)"
  }

  it should "vaguely know when the player is likely to win" in {
    val odds = equityProbably(Vector(Ts, _5s, Jd), (Js, _6s), (Td, _4h))
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ).toString shouldBe "(0.88080806,0.0,0.11919192,0.0)"
  }

  it should "vaguely recognise ties" in {
    val odds = equityProbably(Vector(Tc, _5s, Js, _2c), (Ts, _4s), (Td, _4h))
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ).toString shouldBe "(0.20454545,0.79545456,0.0,0.79545456)"
  }

  it should "specifically recognise a guaranteed win" in {
    val odds = equityProbably(Vector(Qs, Js, Ts), (As, Ks), (_2d, _3c))
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ).toString shouldBe "(1.0,0.0,0.0,0.0)"
  }

  it should "do something reasonable when there are no community cards" in {
    val odds = equityProbably(Vector.empty, (_2s, _3s), (Tc, Kh))
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ).toString shouldBe "(0.37805086,0.0071365833,0.61481255,0.0071365833)"
  }

  "bestPossibleHands" should "return a guaranteed Royal Flush" in {
    fail()
  }

  it should "return a potential royal flush through to a guaranteed ???" in {
    fail()
  }

  it should "return a list of various hands through to a guaranteed High Card hand" in {
    fail()
  }

  "drawProbability" should "return 1/52 for any rank and suit when there are no drawn cards" in {
    Rank.values.foreach { rank =>
      Suit.values.foreach { suit =>
        drawProbability(Some(rank), Some(suit), Vector.empty) shouldBe 1f / 52f
      }
    }
  }

  it should "return 4/52 for any rank when there are no drawn cards" in {
    Rank.values.foreach { rank =>
      drawProbability(Some(rank), None, Vector.empty) shouldBe 1f / 13f
    }
  }

  it should "return 0 for a rank and suit that has already been drawn" in {
    Rank.values.foreach { rank =>
      Suit.values.foreach { suit =>
        drawProbability(Some(rank), Some(suit), Vector(card(rank, suit))) shouldBe 0
      }
    }
  }

  it should "return 3/51 for a rank when the drawn cards contains a card with that value" in {
    Rank.values.foreach { rank =>
      Suit.values.foreach { suit =>
        drawProbability(Some(rank), None, Vector(card(rank, suit))) shouldBe 3f / 51f
      }
    }
  }

  it should "return 1/48 for any rank and suit when there are 4 drawn cards, not containing that card" in {
    val drawn = Vector(Ts, Qh, _6c, _3d)

    Rank.values.foreach { rank =>
      Suit.values.filterNot(s => drawn.contains(card(rank, s))).foreach { suit =>
        drawProbability(Some(rank), Some(suit), drawn) shouldBe 1f / 48f
      }
    }
  }

  it should "return 13/52 for any suit when there are no drawn cards" in {
    Suit.values.foreach { suit =>
      drawProbability(None, Some(suit), Vector.empty) shouldBe 0.25
    }
  }

  it should "return 13/51 for any suit when a card of a different suit has been drawn" in {
    Suit.values.foreach { suit =>
      Rank.values.foreach { rank =>
        Suit.values.filterNot(_ == suit).foreach { drawnSuit =>
          drawProbability(None, Some(suit), Vector(card(rank, drawnSuit))) shouldBe 13f / 51f
        }
      }
    }
  }

  it should "return 12/51 for any suit when a card of that suit has been drawn" in {
    Suit.values.foreach { suit =>
      Rank.values.foreach { rank =>
        drawProbability(None, Some(suit), Vector(card(rank, suit))) shouldBe 12f / 51f
      }
    }
  }

  "bestGuaranteedHand" should "return None when there are no community cards" in {
    bestGuaranteedHand(Ts, _6c, Vector.empty) shouldBe None
  }

  it should "return the only possible hand when there are 3 community cards" in {
    bestGuaranteedHand(Ts, _6c, Vector(Td, _9h, _2s)) shouldBe Some(Hand(Ts, _6c, Td, _9h, _2s).rank)
  }

  it should "return a hand containing one hole card and 4 community cards if using the other hole card makes a worse hand" in {
    bestGuaranteedHand(Ts, _6c, Vector(Td, _9h, _7s, Jc)) shouldBe Some(Hand(Ts, Jc, Td, _9h, _7s).rank)
  }

  it should "return a hand containing both hole cards if they form the best hand" in {
    bestGuaranteedHand(Ts, _7s, Vector(Td, _9h, _6c, Jc)) shouldBe Some(Hand(Ts, Jc, Td, _9h, _7s).rank)
  }

  "possibleStraightFlushes" should "return an empty list when 5 community cards have a different suit to both hole cards" in {
    val cardA = _3h
    val cardB = _2d
    val community = Vector(_2s, _4s, _6c, _8s, Tc)
    val drawn = community :+ cardA :+ cardB

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "return an empty list when 3 of 3 community cards have a different suit to both hole cards" in {
    val cardA = _3h
    val cardB = _2d
    val community = Vector(_2s, _4s, _6c)
    val drawn = community :+ cardA :+ cardB

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "return an empty list when the community cards form a straight flush of a different suit to the hole cards" in {
    val cardA = _3h
    val cardB = _2d
    val community = Vector(_2s, _3s, _4s, _5s, _6s)
    val drawn = community :+ cardA :+ cardB

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "only return a straight flush containing at least one hole card when the community cards and hole cards have the same suit" in {
    val cardA = _9s
    val cardB = _7s
    val community = Vector(_2s, _3s, _4s, _5s, _6s)
    val drawn = community :+ cardA :+ cardB

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe Vector(PossibleHand(Hand(_3s, _4s, _5s, _6s, _7s).rank, 1.0))
  }

  it should "return a single straight flush with 4 community cards and a hole card" in {
    val cardA = _6s
    val cardB = _3h
    val community = Vector(_2s, _3s, _4s, _5s, _9d)
    val drawn = community :+ cardA :+ cardB

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe Vector(PossibleHand(Hand(_2s, _3s, _4s, _5s, _6s).rank, 1.0))
  }

  it should "return a single straight flush with 3 community cards and both hole cards" in {
    val cardA = _6s
    val cardB = _3s
    val community = Vector(_2s, _4s, _5s, _9d, Jc)
    val drawn = community :+ cardA :+ cardB

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe Vector(PossibleHand(Hand(_2s, _3s, _4s, _5s, _6s).rank, 1.0))
  }

  // FIXME tbh this isn't necessary - only need the best one
  it should "return multiple guaranteed straight flushes when possible" in {
    val cardA = _6s
    val cardB = _3s
    val community = Vector(_2s, _4s, _5s, _7s, _8s)
    val drawn = community :+ cardA :+ cardB

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(_4s, _5s, _6s, _7s, _8s).rank, 1.0),
      PossibleHand(Hand(_3s, _4s, _5s, _6s, _7s).rank, 1.0),
      PossibleHand(Hand(_2s, _3s, _4s, _5s, _6s).rank, 1.0),
    )
  }

  it should "return a guaranteed Ace To Five straight flush" in {
    val cardA = As
    val cardB = _3h
    val community = Vector(_2s, _3s, _4s, _5s, _9d)
    val drawn = community :+ cardA :+ cardB

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe Vector(PossibleHand(Hand(_2s, _3s, _4s, _5s, As).rank, 1.0))
  }

  it should "return multiple potential straight flushes when 1 of the 3 community cards have the same suit as both hole cards" in {
    val cardA = Ts
    val cardB = _8s
    val community = Vector(_5d, _8c, _9s)
    val drawn = community :+ cardA :+ cardB

    val odds = (1f / 47) * (1f / 46)

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(_8s, _9s, Ts, Js, Qs).rank, odds),
      PossibleHand(Hand(_7s, _8s, _9s, Ts, Js).rank, odds),
      PossibleHand(Hand(_6s, _7s, _8s, _9s, Ts).rank, odds),
    )
  }

  it should "return multiple potential straight flushes when 2 of the 3 community cards have the same suit as one hole card" in {
    val cardA = Ts
    val cardB = _8d
    val community = Vector(_5d, _8s, _9s)
    val drawn = community :+ cardA :+ cardB

    val odds = (1f / 47) * (1f / 46)

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(_8s, _9s, Ts, Js, Qs).rank, odds),
      PossibleHand(Hand(_7s, _8s, _9s, Ts, Js).rank, odds),
      PossibleHand(Hand(_6s, _7s, _8s, _9s, Ts).rank, odds),
    )
  }

  it should "return multiple potential straight flushes when there are no community cards and both hole cards have the same suit" in {
    val cardA = As
    val cardB = _5s
    val community = Vector.empty
    val drawn = community :+ cardA :+ cardB

    val odds = (1f / 50) * (1f / 49) * (1f / 48) * (1f / 47)

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(Ts, Js, Qs, Ks, As).rank, odds),
      PossibleHand(Hand(_5s, _6s, _7s, _8s, _9s).rank, odds),
      PossibleHand(Hand(_4s, _5s, _6s, _7s, _8s).rank, odds),
      PossibleHand(Hand(_3s, _4s, _5s, _6s, _7s).rank, odds),
      PossibleHand(Hand(_2s, _3s, _4s, _5s, _6s).rank, odds),
      PossibleHand(Hand(_2s, _3s, _4s, _5s, As).rank, odds / (1f / 47)),
    )
  }

  it should "return multiple potential straight flushes in both suits when there are no community cards and the hole cards have different suits" in {
    val cardA = Ad
    val cardB = _4s
    val community = Vector.empty
    val drawn = community :+ cardA :+ cardB

    val odds = (1f / 50) * (1f / 49) * (1f / 48) * (1f / 47)

    possibleStraightFlushes(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(Td, Jd, Qd, Kd, Ad).rank, odds),
      PossibleHand(Hand(_4s, _5s, _6s, _7s, _8s).rank, odds),
      PossibleHand(Hand(_3s, _4s, _5s, _6s, _7s).rank, odds),
      PossibleHand(Hand(_2s, _3s, _4s, _5s, _6s).rank, odds),
      PossibleHand(Hand(_2d, _3d, _4d, _5d, Ad).rank, odds),
      PossibleHand(Hand(_2s, _3s, _4s, _5s, As).rank, odds),
    )
  }

  "possibleStraights" should "return an empty list when the 5 community cards and hole cards cannot form a straight" in {
    val cardA = Ad
    val cardB = _4s
    val community = Vector(_2c, _6c, _8h, Th, Js)
    val drawn = community :+ cardA :+ cardB

    possibleStraights(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "return an empty list when the 3 community cards and hole cards cannot form a straight with 2 more cards" in {
    val cardA = Ad
    val cardB = _4s
    val community = Vector(_2c, _8h, Js)
    val drawn = community :+ cardA :+ cardB

    possibleStraights(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "return an empty list when the community cards and hole cards form a straight flush" in {
    val cardA = Ad
    val cardB = Jd
    val community = Vector(_8d, _9d, Td, Qd, Kd)
    val drawn = community :+ cardA :+ cardB

    possibleStraights(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "return a single straight with 4 community cards and a hole card" in {
    val cardA = Ad
    val cardB = _4s
    val community = Vector(_2c, Th, Jd, Qc, Ks)
    val drawn = community :+ cardA :+ cardB

    possibleStraights(cardA, cardB, community, drawn) shouldBe Vector(PossibleHand(Hand(Th, Jd, Qc, Ks, Ad).rank, 1.0))
  }

  it should "return a single straight with 3 community cards and a hole card" in {
    val cardA = Ad
    val cardB = Th
    val community = Vector(_2c, _4s, Jd, Qc, Ks)
    val drawn = community :+ cardA :+ cardB

    possibleStraights(cardA, cardB, community, drawn) shouldBe Vector(PossibleHand(Hand(Th, Jd, Qc, Ks, Ad).rank, 1.0))
  }

  it should "return a guaranteed Ace to Five straight" in {
    val cardA = Ad
    val cardB = _4s
    val community = Vector(_2c, _3h, _4d, _5c, Ks)
    val drawn = community :+ cardA :+ cardB

    possibleStraights(cardA, cardB, community, drawn) shouldBe Vector(PossibleHand(Hand(_2c, _3h, _4d, _5c, Ad).rank, 1.0))
  }

  it should "return an empty list when the community cards and hole cards form an Ace to Five straight flush" in {
    val cardA = Ac
    val cardB = _4s
    val community = Vector(_2c, _3c, _4c, _5c, Ks)
    val drawn = community :+ cardA :+ cardB

    possibleStraights(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "return multiple potential straights using 1 of 3 community cards and both hole cards" in {
    val cardA = _6d
    val cardB = _4s
    val community = Vector(_2c, _3h, _7d)
    val drawn = community :+ cardA :+ cardB

    possibleStraights(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(_4s, _5h, _6d, _7d, _8d).rank, (4f / 47) * (4f / 46)),
      PossibleHand(Hand(_3h, _4s, _5h, _6d, _7d).rank, 4f / 47),
      PossibleHand(Hand(_2c, _3h, _4s, _5h, _6d).rank, 4f / 47),
      PossibleHand(Hand(_2c, _3h, _4s, _5h, Ad).rank, (4f / 47) * (4f / 46)),
    )
  }

  it should "return multiple potential straights using 2 of 3 community cards and one hole card" in {
    val cardA = Jd
    val cardB = _3s
    val community = Vector(_2c, _4h, _8d)
    val drawn = community :+ cardA :+ cardB

    possibleStraights(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(_2c, _3s, _4h, _5s, _6s).rank, (4f / 47) * (4f / 46)),
      PossibleHand(Hand(_2c, _3s, _4s, _5s, Ad).rank, (4f / 47) * (4f / 46)),
    )
  }

  it should "return multiple potential straights when there are no community cards and both hole cards can appear in the same straight" in {
    val cardA = _7s
    val cardB = _3s
    val community = Vector.empty
    val drawn = community :+ cardA :+ cardB

    val draw4Odds =
      // draw 4 specific ranks of any suit
      (4f / 50) * (4f / 49) * (4f / 48) * (4f / 47) -
        // but not a flush
        (1f / 50) * (1f / 49) * (1f / 48) * (1f / 47)

    val draw3Odds =
      (4f / 50) * (4f / 49) * (4f / 48) -
        (1f / 50) * (1f / 49) * (1f / 48)

    possibleStraights(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(_7s, _8s, _9h, Ts, Js).rank, draw4Odds),
      PossibleHand(Hand(_6s, _7s, _8s, _9h, Ts).rank, draw4Odds),
      PossibleHand(Hand(_5s, _6s, _7s, _8s, _9h).rank, draw4Odds),
      PossibleHand(Hand(_4h, _5s, _6s, _7s, _8s).rank, draw4Odds),
      PossibleHand(Hand(_3s, _4h, _5s, _6s, _7s).rank, draw3Odds),
      PossibleHand(Hand(_2c, _3s, _4h, _5s, _6s).rank, draw4Odds),
      PossibleHand(Hand(_2c, _3s, _4s, _5s, Ad).rank, draw4Odds),
    )
  }

  it should "return multiple potential straights when there are no community cards and the hole cards cannot appear in the same straight" in {
    val cardA = _9s
    val cardB = _3s
    val community = Vector.empty
    val drawn = community :+ cardA :+ cardB

    val draw4Odds =
      // draw 4 specific ranks of any suit
      (4f / 50) * (4f / 49) * (4f / 48) * (4f / 47) -
        // but not a flush
        (1f / 50) * (1f / 49) * (1f / 48) * (1f / 47)

    possibleStraights(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(_9s, Tc, Js, Qs, Ks).rank, draw4Odds),
      PossibleHand(Hand(_8s, _9s, Tc, Js, Qs).rank, draw4Odds),
      PossibleHand(Hand(_7s, _8s, _9s, Tc, Js).rank, draw4Odds),
      PossibleHand(Hand(_6s, _7s, _8s, _9s, Tc).rank, draw4Odds),
      PossibleHand(Hand(_5c, _6s, _7s, _8s, _9s).rank, draw4Odds),
      PossibleHand(Hand(_3s, _4h, _5c, _6s, _7s).rank, draw4Odds),
      PossibleHand(Hand(_2c, _3s, _4h, _5c, _6s).rank, draw4Odds),
      PossibleHand(Hand(_2c, _3s, _4s, _5c, Ad).rank, draw4Odds),
    )
  }

  "possibleFourOfAKinds" should "return an empty list when there are 5 community cards and no 4 of a kinds are possible" in {
    val cardA = Ac
    val cardB = _4s
    val community = Vector(_2c, _3c, _4c, _5c, Ks)
    val drawn = community :+ cardA :+ cardB

    possibleFourOfAKinds(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "return an empty list when the community cards contain a four of a kind" in {
    val cardA = Ac
    val cardB = _4s
    val community = Vector(_2s, _2d, _2c, _2h, Ks)
    val drawn = community :+ cardA :+ cardB

    possibleFourOfAKinds(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "return an empty list when there are 3 community cards but no pairs with hole cards" in {
    val cardA = Ac
    val cardB = _6s
    val community = Vector(_2c, _3c, _4c)
    val drawn = community :+ cardA :+ cardB

    possibleFourOfAKinds(cardA, cardB, community, drawn) shouldBe empty
  }

  it should "return a guaranteed four of a kind" in {
    val cardA = As
    val cardB = Ad
    val community = Vector(_2c, _3c, _4c, Ac, Ah)
    val drawn = community :+ cardA :+ cardB

    possibleFourOfAKinds(cardA, cardB, community, drawn) shouldBe Vector(PossibleHand(Hand(As, Ad, Ac, Ah, Ks).rank, 1f))
  }

  it should "return a potential four of a kind when 2 of 3 community cards and a hole card have the same rank" in {
    val cardA = Ac
    val cardB = _6s
    val community = Vector(_2c, Ah, As)
    val drawn = community :+ cardA :+ cardB

    possibleFourOfAKinds(cardA, cardB, community, drawn) shouldBe Vector(PossibleHand(Hand(As, Ad, Ac, Ah, Ks).rank, 1f / 47))
  }

  it should "return 2 potential four of a kinds when there are 3 community cards and two pairs with the hole cards" in {
    val cardA = Ac
    val cardB = _6s
    val community = Vector(_6c, Ah, As)
    val drawn = community :+ cardA :+ cardB

    possibleFourOfAKinds(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(As, Ad, Ac, Ah, Ks).rank, 1f / 47),
      PossibleHand(Hand(_6s, _6d, _6c, _6h, As).rank, (1f / 47) * (1f / 46)),
    )
  }

  it should "return a potential four of a kind when 1 of 3 community cards has the same rank as both hole cards" in {
    val cardA = As
    val cardB = Ad
    val community = Vector(_6c, Th, Ac)
    val drawn = community :+ cardA :+ cardB

    possibleFourOfAKinds(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(As, Ad, Ac, Ah, Ks).rank, 1f / 47),
    )
  }

  it should "return a potential four of a kind when there are no community cards and the hole cards are a pair" in {
    val cardA = As
    val cardB = Ad
    val community = Vector.empty
    val drawn = community :+ cardA :+ cardB

    possibleFourOfAKinds(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(As, Ad, Ac, Ah, Ks).rank, (1f / 50) * (1f / 49)),
    )
  }

  it should "return 2 potential four of a kinds when there are no community cards and the hole cards have different ranks" in {
    val cardA = As
    val cardB = Kd
    val community = Vector.empty
    val drawn = community :+ cardA :+ cardB

    possibleFourOfAKinds(cardA, cardB, community, drawn) shouldBe Vector(
      PossibleHand(Hand(As, Ad, Ac, Ah, Ks).rank, (1f / 50) * (1f / 49) * (1f / 48)),
      PossibleHand(Hand(Ks, Kd, Kc, Kh, As).rank, (1f / 50) * (1f / 49) * (1f / 48)),
    )
  }

  "equityProbably" should "iterate over all possible outcomes when no community cards have been dealt" in {
    equityProbably(Vector.empty, (_2s, _4d), (_6c, _8h)).possibilities shouldBe 1712304 // == 48c5
  }

  it should "iterate exactly once when all 5 community cards have been dealt" in {
    equityProbably(Vector(Ts, Js, Qs, Ks, As), (_2s, _4d), (_6c, _8h)).possibilities shouldBe 1
  }

  it should "iterate over all possible outcomes when the flop has been dealt" in {
    equityProbably(Vector(Ts, Js, Qs), (_2s, _4d), (_6c, _8h)).possibilities shouldBe 990 // == 45c2
  }

  it should "iterate over all possible outcomes when the river has been dealt" in {
    equityProbably(Vector(Ts, Js, Qs, Ks), (_2s, _4d), (_6c, _8h)).possibilities shouldBe 44
  }
}
