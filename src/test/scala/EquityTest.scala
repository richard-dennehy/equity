import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EquityTest extends AnyFlatSpec with Matchers {
  "equityProbably" should "vaguely know when the player is likely to lose" in {
    val odds = equityProbably(Vector(Ts, _5s, Js), (_9c, _6h), (Td, _4h))
    // TODO how am I supposed to know if these odds are correct? They don't quite match the random online calculator
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ) shouldBe ((
      0.05959596f,
      0.045454547f,
      0.8949495f,
      0.045454547f
    ))
  }

  it should "vaguely know when the player is likely to win" in {
    val odds = equityProbably(Vector(Ts, _5s, Jd), (Js, _6s), (Td, _4h))
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ) shouldBe(
      0.88080806f,
      0.0f,
      0.11919192f,
      0.0f
    )
  }

  it should "vaguely recognise ties" in {
    val odds = equityProbably(Vector(Tc, _5s, Js, _2c), (Ts, _4s), (Td, _4h))
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ) shouldBe(
      0.20454545f,
      0.79545456f,
      0.0f,
      0.79545456f
    )
  }

  it should "specifically recognise a guaranteed win" in {
    val odds = equityProbably(Vector(Qs, Js, Ts), (As, Ks), (_2d, _3c))
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ) shouldBe(1.0f, 0.0f, 0.0f, 0.0f)
  }

  it should "do something reasonable when there are no community cards" in {
    val odds = equityProbably(Vector.empty, (_2s, _3s), (Tc, Kh))
    (
      odds.p1.win,
      odds.p1.draw,
      odds.p2.win,
      odds.p2.draw
    ) shouldBe(
      0.37805086f,
      0.0071365833f,
      0.61481255f,
      0.0071365833f
    )
  }

  "drawProbability" should "return 1/52 for any rank and suit when there are no drawn cards" in {
    Rank.values.foreach { rank =>
      Suit.values.foreach { suit =>
        drawProbability(card(rank, suit), Vector.empty) shouldBe 1f / 52f
      }
    }
  }

  it should "return 0 for a rank and suit that has already been drawn" in {
    Rank.values.foreach { rank =>
      Suit.values.foreach { suit =>
        drawProbability(card(rank, suit), Vector(card(rank, suit))) shouldBe 0
      }
    }
  }

  it should "return 1/48 for any rank and suit when there are 4 drawn cards, not containing that card" in {
    val drawn = Vector(Ts, Qh, _6c, _3d)

    Rank.values.foreach { rank =>
      Suit.values.filterNot(s => drawn.contains(card(rank, s))).foreach { suit =>
        drawProbability(card(rank, suit), drawn) shouldBe 1f / 48f
      }
    }
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

  "bestHand" should "return a straight flush when possible" in {
    bestHand(Vector(_6s, _7s, _8s, _9s, Ts), (_5s, Js)) shouldBe Hand(_7s, _8s, _9s, Ts, Js).rank
  }

  it should "return a four of a kind when possible" in {
    bestHand(Vector(_6s, _6d, _6c, _6h, Ks), (Ah, _4s)) shouldBe Hand(_6s, _6d, _6c, _6h, Ah).rank
  }

  it should "return a full house when possible" in {
    bestHand(Vector(_8s, _8d, _7h, _7d, _6s), (_7c, _9s)) shouldBe Hand(_8s, _8d, _7h, _7d, _7c).rank
  }

  it should "return a flush when possible" in {
    bestHand(Vector(_2d, _4d, _6d, Ts, Js), (_8d, Td)) shouldBe Hand(_2d, _4d, _6d, _8d, Td).rank
  }

  it should "return a straight when possible" in {
    bestHand(Vector(_2c, _3h, _5d, _7h, _7c), (As, _4d)) shouldBe Hand(_2c, _3h, _4d, _5d, As).rank
  }

  it should "return a three of a kind when possible" in {
    bestHand(Vector(_2c, _3h, _5d, _7h, _7c), (_7s, _4d)) shouldBe Hand(_7h, _7c, _7s, _5d, _4d).rank
  }

  it should "return two pairs when possible" in {
    bestHand(Vector(_2c, _3h, _5d, _7h, _7c), (_5s, _4d)) shouldBe Hand(_7h, _7c, _5s, _5d, _4d).rank
  }

  it should "return one pair when possible" in {
    bestHand(Vector(_2c, _3h, _5d, _7h, _7c), (Js, _4d)) shouldBe Hand(_7h, _7c, Js, _5d, _4d).rank
  }

  it should "return a high card when no other hand is possible" in {
    bestHand(Vector(_2c, _3h, _5d, _7h, Tc), (Js, _4d)) shouldBe Hand(Js, Tc, _7h, _5d, _4d).rank
  }

  it should "return the same hand regardless of the order of the cards" in {
    val expected = Hand(_2c, _3h, _4d, _5d, As).rank

    Vector(_2c, _3h, _4d, _5d, _7h, _7c, As).permutations.foreach { cards =>
      bestHand(cards.take(5), (cards(5), cards(6))) shouldBe expected
    }
  }
}
