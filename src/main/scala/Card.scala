import Category.{Flush, FourOfAKind, FullHouse, HighCard, OnePair, Straight, StraightFlush, ThreeOfAKind, TwoPairs}
import Rank.Ace

import scala.math.Ordered.orderingToOrdered

opaque type Card = (Rank, Suit)

def card(rank: Rank, suit: Suit): Card = (rank, suit)

extension (c: Card) {
  def rank: Rank = c._1
  def suit: Suit = c._2
}

enum Suit {
  case Spades, Diamonds, Clubs, Hearts
}

extension (s: Suit) {
  def mask: Long = Long.MinValue >>> s.ordinal
}

// face value
enum Rank(val value: Int) {
  case Ace extends Rank(14)
  case King extends Rank(13)
  case Queen extends Rank(12)
  case Jack extends Rank(11)
  case Ten extends Rank(10)
  case Nine extends Rank(9)
  case Eight extends Rank(8)
  case Seven extends Rank(7)
  case Six extends Rank(6)
  case Five extends Rank(5)
  case Four extends Rank(4)
  case Three extends Rank(3)
  case Two extends Rank(2)
}

object Rank {
  given Ordering[Rank] = (x: Rank, y: Rank) => x.value.compare(y.value)

  // inverse of `Rank.value` - given a value between 2 and 14 inclusive, returns the corresponding Rank
  inline def fromValue(value: Int): Rank = Rank.fromOrdinal(Rank.Two.ordinal - (value - 2))
}

extension (r: Rank) {
  def mask: Long = 1L << ((r.value - 2) * 3)
}

// bitset bullshit:
//  - represent a set of cards as a Long, to make it easier to check properties
//  - represent a deck (or a draw pile) as a Long
//  - need to be able to convert a Card into a mask
// top 4 bits: suits contained in the set (to check flushes)
// bottom 4 bits: 2s, 2d, 2c, 2h
// next 4 bits: 3s, 3d, 3c, 3h
//  ...etc
// A Hand of cards; sorted by rank
opaque type Hand = Long
// NOTE: 5-tuple seems like the simplest way to implement a fixed-size array

object Hand {
  def apply(first: Card, second: Card, third: Card, fourth: Card, fifth: Card): Hand = {
    val suitMask = first.suit.mask | second.suit.mask | third.suit.mask | fourth.suit.mask | fifth.suit.mask
    val rankMask = first.rank.mask + second.rank.mask + third.rank.mask + fourth.rank.mask + fifth.rank.mask

    suitMask | rankMask
  }

  given Ordering[Hand] = (h: Hand, other: Hand) => {
    (h.category, other.category) match {
      case (StraightFlush(aH), StraightFlush(bH)) =>
        aH.compare(bH)
      case (FourOfAKind(aQ, aK), FourOfAKind(bQ, bK)) =>
        aQ.compare(bQ) << 1 + aK.compare(bK)
      case (FullHouse(aT, aP), FullHouse(bT, bP)) =>
        aT.compare(bT) << 1 + aP.compare(bP)
      case (Flush(aK0, aK1, aK2, aK3, aK4), Flush(bK0, bK1, bK2, bK3, bK4)) =>
        aK0.compare(bK0) << 4 +
          aK1.compare(bK1) << 3 +
          aK2.compare(bK2) << 2 +
          aK3.compare(bK3) << 1 +
          aK4.compare(bK4)
      case (Straight(aH), Straight(bH)) =>
        aH.compare(bH)
      case (ThreeOfAKind(aT, aK0, aK1), ThreeOfAKind(bT, bK0, bK1)) =>
        aT.compare(bT) << 2 +
          aK0.compare(bK0) << 1 +
          aK1.compare(bK1)
      case (TwoPairs(aP0, aP1, aK), TwoPairs(bP0, bP1, bK)) =>
        aP0.compare(bP0) << 2 +
          aP1.compare(bP1) << 1 +
          aK.compare(bK)
      case (OnePair(aP, aK0, aK1, aK2), OnePair(bP, bK0, bK1, bK2)) =>
        aP.compare(bP) << 3 +
          aK0.compare(bK0) << 2 +
          aK1.compare(bK1) << 1 +
          aK2.compare(bK2)
      case (HighCard(aK0, aK1, aK2, aK3, aK4), HighCard(bK0, bK1, bK2, bK3, bK4)) =>
        aK0.compare(bK0) << 4 +
          aK1.compare(bK1) << 3 +
          aK2.compare(bK2) << 2 +
          aK3.compare(bK3) << 1 +
          aK4.compare(bK4)
      case (aCat, bCat) =>
        aCat.compare(bCat)
    }
  }
}

extension (h: Hand) {
  def debugString: String = maskDebugString(h)

  def category: Category = {
    val suitsMask = Suit.Spades.mask | Suit.Diamonds.mask | Suit.Clubs.mask | Suit.Hearts.mask

    // TODO some micro-optimisation around this - could see if this actually uses an intrinsic
    val isFlush = java.lang.Long.bitCount(h & suitsMask) == 1
    val ranks = h & ~suitsMask

    // given a bitset AAAKKKQQQJJJ...333222, a straight is either AAA=001;KKK=001;QQQ=001...etc or AAA=001;555=001;444=001...etc
    // TODO micro-optimisation ensure this is constant folded
    val aceLowStraightMask = Rank.Ace.mask | Rank.Five.mask | Rank.Four.mask | Rank.Three.mask | Rank.Two.mask
    val straightMask = Rank.Six.mask | Rank.Five.mask | Rank.Four.mask | Rank.Three.mask | Rank.Two.mask

    if (ranks == aceLowStraightMask && isFlush) {
      return StraightFlush(Rank.Five)
    } else if (ranks == aceLowStraightMask) {
      return Straight(Rank.Five)
    }

    var shift = 0
    while (shift <= 24) {
      if (ranks == straightMask << shift) {
        if (isFlush) {
          return StraightFlush(Rank.fromValue(6 + shift / 3))
        } else {
          return Straight(Rank.fromValue(6 + shift / 3))
        }
      }

      shift += 3
    }

    // flush implies no pairs etc
    if (isFlush) {
      // FIXME this hopefully can be removed with a different representation of Category (i.e. another mask rather than an ADT)
      val ranks = Rank.values.filter(r => (r.mask & h) == r.mask)

      return Flush(ranks(0), ranks(1), ranks(2), ranks(3), ranks(4))
    }
    // for bitset ...33332222, four of a kind is 222=100 or 333=100 etc
    val fourOfAKindMask = 4L

    shift = 0
    while (shift <= 36) {
      if (((ranks >> shift) & fourOfAKindMask) == fourOfAKindMask) {
        val quads = Rank.fromValue(2 + shift / 3)
        // mask off the quad bits and convert the remaining bit from a mask to a rank
        val kicker = Rank.fromValue(2 + java.lang.Long.numberOfTrailingZeros(ranks ^ (fourOfAKindMask << shift)) / 3)

        return FourOfAKind(quads, kicker)
      }

      shift += 3
    }

    // three of a kind and pair are more complicated, as there could be a full house or two pairs or neither of those
    var trips = 0
    var pairLow = 0
    var pairHigh = 0

    val tripsMask = 3L
    val pairMask = 2L

    // TODO this entire thing can probably be reduced to a single loop
    shift = 0
    while (shift <= 36) {
      if (((ranks >> shift) & tripsMask) == tripsMask) {
        trips = 2 + shift / 3
        if (pairLow != 0) {
          return FullHouse(Rank.fromValue(trips), Rank.fromValue(pairLow))
        }
      } else if (((ranks >> shift) & pairMask) == pairMask) {
        if (trips != 0) {
          pairLow = 2 + shift / 3
          return FullHouse(Rank.fromValue(trips), Rank.fromValue(pairLow))
        } else if (pairLow == 0) {
          pairLow = 2 + shift / 3
        } else {
          pairHigh = 2 + shift / 3

          // mask off the pairs bits and convert the remaining bit from a mask to a rank
          val pairsMask = (pairMask << shift) ^ (pairMask << ((pairLow - 2) * 3))
          val kicker = Rank.fromValue(2 + java.lang.Long.numberOfTrailingZeros(ranks ^ pairsMask) / 3)

          return TwoPairs(Rank.fromValue(pairHigh), Rank.fromValue(pairLow), kicker)
        }
      }

      shift += 3
    }

    if (trips != 0) {
      val ranks = Rank.values.filter(r => (r.mask & (h ^ (tripsMask << ((trips - 2) * 3)))) == r.mask)

      ThreeOfAKind(Rank.fromValue(trips), ranks(0), ranks(1))
    } else if (pairLow != 0) {
      val ranks = Rank.values.filter(r => (r.mask & h) == r.mask)

      OnePair(Rank.fromValue(pairLow), ranks(0), ranks(1), ranks(2))
    } else {
      val ranks = Rank.values.filter(r => (r.mask & h) == r.mask)

      HighCard(ranks(0), ranks(1), ranks(2), ranks(3), ranks(4))
    }
  }
}

private def maskDebugString(mask: Long): String =
  s"""sdch_____________________AAAKKKQQQJJJTTT999888777666555444333222
     |${mask.toBinaryString.reverse.padTo(64, "0").reverse.mkString}""".stripMargin

private def debugMask(ctx: String, mask: Long) = println(s"$ctx\n${maskDebugString(mask)}")

enum Category {
  case StraightFlush(highCard: Rank)
  case FourOfAKind(quad: Rank, kicker: Rank)
  case FullHouse(trips: Rank, pair: Rank)
  case Flush(k0: Rank, k1: Rank, k2: Rank, k3: Rank, k4: Rank)
  case Straight(highCard: Rank)
  case ThreeOfAKind(trips: Rank, k0: Rank, k1: Rank)
  case TwoPairs(highPair: Rank, lowPair: Rank, kicker: Rank)
  case OnePair(pair: Rank, k0: Rank, k1: Rank, k2: Rank)
  case HighCard(k0: Rank, k1: Rank, k2: Rank, k3: Rank, k4: Rank)
}

object Category {
  given Ordering[Category] = (x: Category, y: Category) => y.ordinal - x.ordinal
}

import Rank.{Ace, *}
import Suit.*

val As: Card = (Ace, Spades)
val Ks: Card = (King, Spades)
val Qs: Card = (Queen, Spades)
val Js: Card = (Jack, Spades)
val Ts: Card = (Ten, Spades)
val _9s: Card = (Nine, Spades)
val _8s: Card = (Eight, Spades)
val _7s: Card = (Seven, Spades)
val _6s: Card = (Six, Spades)
val _5s: Card = (Five, Spades)
val _4s: Card = (Four, Spades)
val _3s: Card = (Three, Spades)
val _2s: Card = (Two, Spades)

val Ad: Card = (Ace, Diamonds)
val Kd: Card = (King, Diamonds)
val Qd: Card = (Queen, Diamonds)
val Jd: Card = (Jack, Diamonds)
val Td: Card = (Ten, Diamonds)
val _9d: Card = (Nine, Diamonds)
val _8d: Card = (Eight, Diamonds)
val _7d: Card = (Seven, Diamonds)
val _6d: Card = (Six, Diamonds)
val _5d: Card = (Five, Diamonds)
val _4d: Card = (Four, Diamonds)
val _3d: Card = (Three, Diamonds)
val _2d: Card = (Two, Diamonds)

val Ac: Card = (Ace, Clubs)
val Kc: Card = (King, Clubs)
val Qc: Card = (Queen, Clubs)
val Jc: Card = (Jack, Clubs)
val Tc: Card = (Ten, Clubs)
val _9c: Card = (Nine, Clubs)
val _8c: Card = (Eight, Clubs)
val _7c: Card = (Seven, Clubs)
val _6c: Card = (Six, Clubs)
val _5c: Card = (Five, Clubs)
val _4c: Card = (Four, Clubs)
val _3c: Card = (Three, Clubs)
val _2c: Card = (Two, Clubs)

val Ah: Card = (Ace, Hearts)
val Kh: Card = (King, Hearts)
val Qh: Card = (Queen, Hearts)
val Jh: Card = (Jack, Hearts)
val Th: Card = (Ten, Hearts)
val _9h: Card = (Nine, Hearts)
val _8h: Card = (Eight, Hearts)
val _7h: Card = (Seven, Hearts)
val _6h: Card = (Six, Hearts)
val _5h: Card = (Five, Hearts)
val _4h: Card = (Four, Hearts)
val _3h: Card = (Three, Hearts)
val _2h: Card = (Two, Hearts)
