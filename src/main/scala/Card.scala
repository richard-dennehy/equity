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
}

extension (h: Hand) {
  def debugString: String = maskDebugString(h)

  def category: Long = {
    inline def shiftToRank(shift: Int): Int = (shift / 3) + 2

    inline def rankToShift(rank: Int): Int = (rank - 2) * 3

    val suitsMask = Suit.Spades.mask | Suit.Diamonds.mask | Suit.Clubs.mask | Suit.Hearts.mask

    // TODO some micro-optimisation around this - could see if this actually uses an intrinsic
    val isFlush = java.lang.Long.bitCount(h & suitsMask) == 1
    val ranks = h & ~suitsMask

    // given a bitset AAAKKKQQQJJJ...333222, a straight is either AAA=001;KKK=001;QQQ=001...etc or AAA=001;555=001;444=001...etc
    // TODO micro-optimisation ensure this is constant folded
    val aceLowStraightMask = Rank.Ace.mask | Rank.Five.mask | Rank.Four.mask | Rank.Three.mask | Rank.Two.mask
    val straightMask = Rank.Six.mask | Rank.Five.mask | Rank.Four.mask | Rank.Three.mask | Rank.Two.mask

    if (ranks == aceLowStraightMask && isFlush) {
      // FIXME maybe change the LSB to be an ace low bit to make this less awkward
      //  for the time being, mask off the Ace so it sorts correctly
      return StraightFlush.mask | (ranks ^ Ace.mask)
    } else if (ranks == aceLowStraightMask) {
      return Straight.mask | (ranks ^ Ace.mask)
    }

    var shift = 0
    while (shift <= 24) {
      if (ranks == straightMask << shift) {
        if (isFlush) {
          return StraightFlush.mask | ranks
        } else {
          return Straight.mask | ranks
        }
      }

      shift += 3
    }

    // flush implies no pairs etc
    if (isFlush) {
      return Flush.mask | ranks
    }
    // for bitset ...33332222, four of a kind is 222=100 or 333=100 etc
    val fourOfAKindMask = 4L

    shift = 0
    while (shift <= 36) {
      if (((ranks >> shift) & fourOfAKindMask) == fourOfAKindMask) {
        val quads = shiftToRank(shift) - 2
        // mask off the quad bits and convert the remaining bit from a mask to a rank
        val kicker = java.lang.Long.numberOfTrailingZeros(ranks ^ (fourOfAKindMask << shift)) / 3
        
        return FourOfAKind.mask | 1L << (quads + 13) | 1L << kicker
      }

      shift += 3
    }

    // three of a kind and pair are more complicated, as there could be a full house or two pairs or neither of those
    var trips = 0
    var pairLow = 0
    var pairHigh = 0
    var kickers = 0

    val tripsMask = 3L
    val pairMask = 2L
    // for a group of 3 bits, check if _only_ the bottom bit is set by ANDing it with 7 and checking the result is 1
    //  000 & 111 == 000
    //  001 & 111 == 001
    //  010 & 111 == 010
    //  011 & 111 == 011
    //  100 & 111 == 100
    //  101 & 111 == 101
    //  110 & 111 == 110
    //  111 & 111 == 111
    val kickerMask = 7L

    // TODO this entire thing can probably be reduced to a single loop
    shift = 0
    while (shift <= 36) {
      if (((ranks >> shift) & tripsMask) == tripsMask) {
        trips = shiftToRank(shift)
        if (pairLow != 0) {
          return FullHouse.mask | 1L << (trips - 2 + 26) | 1L << (pairLow - 2 + 13)
        }
      } else if (((ranks >> shift) & pairMask) == pairMask) {
        if (trips != 0) {
          pairLow = shiftToRank(shift)
          return FullHouse.mask | 1L << (trips - 2 + 26) | 1L << (pairLow - 2 + 13)
        } else if (pairLow == 0) {
          pairLow = shiftToRank(shift)
        } else {
          pairHigh = shiftToRank(shift)
          val pairsMask = (pairMask << shift) ^ (pairMask << ((pairLow - 2) * 3))
          val kicker = java.lang.Long.numberOfTrailingZeros(ranks ^ pairsMask) / 3

          return TwoPairs.mask | 1L << (pairLow - 2 + 13) | 1L << (pairHigh - 2 + 13) | 1L << kicker
        }
      } else if (((ranks >> shift) & kickerMask) == 1L) {
        kickers |= (1 << (shiftToRank(shift) - 2))
      }

      shift += 3
    }


    if (trips != 0) {
      ThreeOfAKind.mask | (1L << (trips - 2 + 26)) | kickers
    } else if (pairLow != 0) {
      OnePair.mask | (1L << (pairLow - 2 + 13)) | kickers
    } else {
      HighCard.mask | ranks
    }
  }
}

private def maskDebugString(mask: Long): String = {
  // For any hand A and hand B, if A > B, the numeric value of A must be > numeric value of B; the same applies for A == B and A < B.
  // This necessitates a different representation for some categories, e.g. for a pair, the pair rank must weigh more than any kicker,
  //  which requires rearranging the bits; for a straight, the ranks merely need to be ordered, which they already are
  if (Vector(FourOfAKind, FullHouse, ThreeOfAKind, TwoPairs, OnePair).exists(c => (c.mask & mask) == c.mask)) {
    s"""     s 4 f f s 3 2 1 h  |   trips    | pairs/quad |   kickers  |
       |sdch_f_k_h_l_t_k_p_p_c___AKQJT98765432AKQJT98765432AKQJT98765432
       |${mask.toBinaryString.reverse.padTo(64, "0").reverse.mkString}""".stripMargin
  } else {
    s"""     s 4 f f s 3 2 1 h
       |sdch_f_k_h_l_t_k_p_p_c___AAAKKKQQQJJJTTT999888777666555444333222
       |${mask.toBinaryString.reverse.padTo(64, "0").reverse.mkString}""".stripMargin
  }
}

private def debugMask(ctx: String, mask: Long) = println(s"$ctx\n${maskDebugString(mask)}")

enum Category {
  case StraightFlush
  case FourOfAKind
  case FullHouse
  case Flush
  case Straight
  case ThreeOfAKind
  case TwoPairs
  case OnePair
  case HighCard
}

object Category {
  given Ordering[Category] = (x: Category, y: Category) => y.ordinal - x.ordinal
}

extension (c: Category) {
  def mask: Long = 1L << (58 - c.ordinal * 2)
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
