import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EquityTest extends AnyFlatSpec with Matchers {
  // numbers taken from a random online calculator - let's hope it works

  "equity" should "vaguely know when the player is likely to lose" in {
    val odds = equity((_9c, _6h), (Td, _4h), Vector(Ts, _5s, Js), Vector.empty)
    odds.win shouldBe 0.064
    odds.draw shouldBe 0
  }

  it should "vaguely know when the player is likely to win" in {
    val odds = equity((Js, _6s), (Td, _4h), Vector(Ts, _5s, Js), Vector.empty)
    odds.win shouldBe 0.881
    odds.draw shouldBe 0
  }

  it should "vaguely recognise ties" in {
    val odds = equity((Ts, _4s), (Td, _4h), Vector(Ts, _5s, Js), Vector.empty)
    odds.win shouldBe 0.045
    odds.draw shouldBe 0.955
  }

  it should "specifically recognise a guaranteed win" in {
    val odds = equity((As, Ks), (_2d, _3c), Vector(Qs, Js, Ts), Vector.empty)
    odds.win shouldBe 1
    odds.draw shouldBe 0
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

  it should "return a guaranteed Ace To Five straight" in {
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
}
