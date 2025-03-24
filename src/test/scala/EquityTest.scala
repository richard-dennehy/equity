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
}
