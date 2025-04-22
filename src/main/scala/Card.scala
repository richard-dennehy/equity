import Category.{Flush, FourOfAKind, FullHouse, HighCard, OnePair, Straight, StraightFlush, ThreeOfAKind, TwoPairs}

import java.lang.Long.bitCount

opaque type Card = (Rank, Suit)

def card(rank: Rank, suit: Suit): Card = (rank, suit)
def card(rank: Int, suit: Suit): Card = (Rank.fromValue(rank), suit)

extension (c: Card) {
  def rank: Rank = c._1
  def suit: Suit = c._2
}

enum Suit {
  case Spades, Diamonds, Clubs, Hearts
}

extension (s: Suit) {
  // in the Hand mask, a suit is tagged in one of the top 4 bits
  inline def handMask: Long = Long.MinValue >>> s.ordinal
}

// A named card Rank ordered from highest value to lowest value
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
  def fromValue(value: Int): Rank = Rank.fromOrdinal(14 - value)
}

extension (r: Rank) {
  // In the Hand mask, each rank is given 3 bits each, starting from the Least Significant Bit. The mask returned here
  //  only uses the least significant of the three (i.e. this is 0b001 shifted by some multiple of three)
  inline def handMask: Long = 1L << ((r.value - 2) * 3)
  inline def -(offset: Int): Rank = Rank.fromOrdinal(r.ordinal + offset)
  inline def +(offset: Int): Rank = Rank.fromOrdinal(r.ordinal - offset)
}

// TODO mad idea - hand of 7 cards; produce highest ranking possible from any 5 of 7
//  - there are 133784560 possibilities here, which can fit in 28 bits - but this requires a clever encoding of the problem
//    would it be possible to store as a low card and 6 deltas between each card and the next, assuming the hand is sorted?

// Represent a Hand of 5 cards as a bitset such that identifying patterns is relatively simple. See `maskDebugString`
//  and `category` for more details, but briefly:
//  - the top 4 bits flag which suits are present; if only one bit is set, this hand is a flush, otherwise, the suits are irrelevant
//  - the bottom 39 bits represent Rank counts; the bottom 3 bits are the number of 2s, the next 3 are the number of 3s, etc
opaque type Hand = Long

// Use 64 bits to represent 7 cards - it's technically possible to fit this within 32 bits, but trying to find a reasonable encoding that
//  fits in 32 bits is very difficult (it appears to require variable width encoding)
//
// The bottom 4 bits are the lowest ranked card; this only needs 12 possible states as this card can't be an Ace
//
// The next 24 bits are rank deltas (6 * 4) - that is, AAAAKKK is represented as 0,0,0,1,0,0,12; AJ97532 is 3,2,2,2,2,1,2, etc
// This makes patterns such as straights and pairs easily identified
//
// The next 28 bits are the suits for each card (7 * 4) - while the suits could easily fit in 2 bits, there's no particular benefit
//  to making the representation more compact as it's still more than 32 bits
opaque type Hand7 = Long

object Hand7 {
  def apply(first: Card, second: Card, third: Card, fourth: Card, fifth: Card, sixth: Card, seventh: Card): Hand7 = {
    // IMPORTANT the cards _must_ be sorted - if it's not practical to ensure they're sorted when this method is called, then the first thing
    //  this method will need to do is sort the cards
    val suitMask =
      (1L << (first.suit.ordinal + 24)) +
        (1L << (second.suit.ordinal + 20)) +
        (1L << (third.suit.ordinal + 16)) +
        (1L << (fourth.suit.ordinal + 12)) +
        (1L << (fifth.suit.ordinal + 8)) +
        (1L << (sixth.suit.ordinal + 4)) +
        (1L << seventh.suit.ordinal)

    val cardsMask =
      (first.rank.value.toLong << 24) +
        (second.rank.value.toLong << 20) +
        (third.rank.value.toLong << 16) +
        (fourth.rank.value.toLong << 12) +
        (fifth.rank.value.toLong << 8) +
        (sixth.rank.value.toLong << 4) +
        seventh.rank.value

    (suitMask << 28) + cardsMask
  }
}

extension (h: Hand7) {
  def debugString7: String =
    s"""
       |hcds|hcds|hcds|hcds|hcds|hcds|hcds|1111|2222|3333|4444|5555|6666|7777
       |${h.toBinaryString.reverse.padTo(56, "0").reverse.mkString.grouped(4).mkString("|")}""".stripMargin

  // Return the ranking of the best possible hand of 5 that can be made from the 7 cards
  def rank7: RankedHand = {

    // A flush is possible if for any of the 4 suits, the bit count of the result of bitwise & with the suit mask is >= 5.
    // Multiple flushes are available if this bit count is > 5, but these must all be of the same suit
    var flushMask = 0x1111111L << 28
    var canFlush = false
    var flushSuit = 0
    var cardsWithFlushingSuit = 0L

    while (flushSuit < 4 && !canFlush) {
      cardsWithFlushingSuit = h & flushMask
      canFlush = bitCount(cardsWithFlushingSuit) >= 5
      flushMask <<= 4
      flushSuit += 1
    }

    cardsWithFlushingSuit = h & ((cardsWithFlushingSuit >> 28) * 15)

    val ranks = h & ((1L << 28) - 1)

    // If a flush is possible, the only other category worth looking for is a straight, as there aren't enough cards remaining
    //  to form a four of a kind or a full house, and trips/pairs are strictly worse

    // by shifting the ranks up by 4 (i.e. promoting each rank one level) and masking off the 1st card (which would otherwise subtract from the non-existent 0th card)
    //  then subtracting the above from the ranks, we get the deltas between each rank (except the 7th card)
    // we can then use this to check for straights and groups
    val cardDeltas = ranks - ((ranks << 4) & ((1 << 28) - 1))

    var straightMask = 0x1111000L
    var straightHighCard = -1L

    // optimisation (?) - if there aren't at least 4 bits set in the deltas, there can't be a straight, so don't bother looking for one
    if (bitCount(cardDeltas) >= 4) {
      // 1-5 straight e.g. AKQJTXY
      //
      // the bit patterns are somewhat complicated by the need to check that we have the right number of bits in the right places
      //  - if we just checked there were 4 bits set, something like KT777 would be considered a straight as there's a 3-delta
      //  followed by another 3-delta, which would add up to 4 bits; if we just checked the 4 specific bits were set, it would allow
      //  AJ852 as a straight because the bottom bit would be set on each delta - we therefore need to check that there are exactly 4 bits
      //  set in the entire section, _and_ that those bits are in the right place
      if (bitCount(cardDeltas & 0xFFFF000) == 4 && bitCount(cardDeltas & straightMask) == 4) {
        // TODO might have to check for flushes in all these branches, otherwise it's very difficult to identify the correct SF
        //  as we could have e.g. AdKsQhJhTh9h8h, where the best straight is Ace high, but the best Straight Flush is Queen high
        straightHighCard = (ranks & 0xF000000L) >> 24

        // mask together the 1-deltas with the flushing cards; if we get back the same 1-deltas, this straight is also a flush
        if ((cardDeltas & straightMask & cardsWithFlushingSuit) == (cardDeltas & straightMask)) {
          // we won't find anything better than this
          return StraightFlush.mask | straightHighCard
        }
      }

      if (straightHighCard == -1) {
        // 1-6 straight with a pair; AAKQJTX, AKKQJTX, etc
        straightMask = 0x1111100L
        if (bitCount(cardDeltas & 0xFFFFF00) == 4 && bitCount(cardDeltas & straightMask) == 4) {
          straightHighCard = (ranks & 0xF000000L) >> 24

          if ((cardDeltas & straightMask & cardsWithFlushingSuit) == (cardDeltas & straightMask)) {
            return StraightFlush.mask | straightHighCard
          }
        }
      }

      if (straightHighCard == -1) {
        // 1-7 straight with two pairs or trips; AAAKQJT, AAKKQJT, etc
        straightMask = 0x1111110L
        if (bitCount(cardDeltas & 0xFFFFFF0) == 4 && bitCount(cardDeltas & straightMask) == 4) {
          straightHighCard = (ranks & 0xF000000L) >> 24

          if ((cardDeltas & straightMask & cardsWithFlushingSuit) == (cardDeltas & straightMask)) {
            return StraightFlush.mask | straightHighCard
          }
        }
      }

      if (straightHighCard == -1) {
        // 2-6 straight e.g. XQJT98Y
        straightMask = 0x0111000L
        if (bitCount(cardDeltas & 0x0FFFF00) == 4 && bitCount(cardDeltas & straightMask) == 4) {
          straightHighCard = (ranks & 0x0F00000L) >> 20

          if ((cardDeltas & straightMask & cardsWithFlushingSuit) == (cardDeltas & straightMask)) {
            return StraightFlush.mask | straightHighCard
          }
        }
      }

      if (straightHighCard == -1) {
        // 2-7 straight with a pair e.g. XQQJT98, XQJJT98, etc
        straightMask = 0x0111100L
        if (bitCount(cardDeltas & 0x0FFFFF0) == 4 && bitCount(cardDeltas & straightMask) == 4) {
          straightHighCard = (ranks & 0x0F00000L) >> 20

          if ((cardDeltas & straightMask & cardsWithFlushingSuit) == (cardDeltas & straightMask)) {
            return StraightFlush.mask | straightHighCard
          }
        }
      }

      if (straightHighCard == -1) {
        // 3-7 straight e.g. XYJT987
        straightMask = 0x0011110L
        if (bitCount(cardDeltas & 0x00FFFFF) == 4 && bitCount(cardDeltas & straightMask) == 4) {
          straightHighCard = (ranks & 0x00F0000L) >> 16

          if ((cardDeltas & straightMask & cardsWithFlushingSuit) == (cardDeltas & straightMask)) {
            return StraightFlush.mask | straightHighCard
          }
        }
      }
    }

    // at this point, we can't have a straight flush, unless it's Ace-to-5
    // we have a straight if straightHighCard is set
    // if straightHighCard is set or cardsWithFlushingSuit is set, we _cannot_ have a FourOfAKind or FullHouse

    // TODO a flush takes priority over any straight so we can return if there's a flush, but we need to check for A-5 first
    //  in case there's a SF hiding there
    //  we _only_ care if there's an SF hiding there, so mask off the non-flushing cards before checking
    //  Else (iff there's no flush), if there's a straight, return it because there aren't enough cards remaining to make anything
    //  better
    //  Else2, if there's no flush and no straight, check for A-5 using all cards
    //  Or maybe check for groups at this point

    // if we have any straight, it will be better than an Ace-to-Five straight, unless the A-5 is also a flush
    if (straightHighCard != -1 && cardsWithFlushingSuit == 0) {
      return Straight.mask | straightHighCard
    }

    // check for A-5 straight - just look for the relevant cards in the hand
    // because the cards are sorted, if there are one or more Aces, there must be one in the highest position, and similarly for a 2 in the low position
    if ((ranks >> 24) == Rank.Ace.value && (ranks & 0xFFF) == Rank.Two.value) {
      // given the cards must be at least AXYZUV2, the best case scenario is AXY5432, and the worst is something like A543222
      // we therefore need to loop over the cards in either direction (the choice doesn't seem to make a difference)
      // looking for the 3, 4, and 5
      var position = 6
      var shift = 3
      var needle = Rank.Three.value

      while (position > 1 && needle < 6) {
        val rank = (ranks >> shift) & 0xFFF
        if (rank == needle) needle += 1

        shift += 3
        position -= 1
      }

      if (needle == Rank.Six.value) {
        return Straight.mask | Rank.Five.value
      }
    }

    //    println(cardsWithFlushingSuit.debugString7)
    println(cardDeltas.debugString7)

    ???
  }
}

object Hand {
  def apply(first: Card, second: Card, third: Card, fourth: Card, fifth: Card): Hand = {
    // OR the suits together as we only care which suits are present, but not how many of each
    val suitMask = first.suit.handMask | second.suit.handMask | third.suit.handMask | fourth.suit.handMask | fifth.suit.handMask
    // Add the ranks together so we can recognise duplicates (for pairs, trips, and quads)
    val rankMask = first.rank.handMask + second.rank.handMask + third.rank.handMask + fourth.rank.handMask + fifth.rank.handMask

    suitMask | rankMask
  }
}

type RankedHand = Long

extension (hand: Hand) {
  def debugString: String = maskDebugString(hand)

  // Return a numeric value such that for any two Hands A and B:
  //  - if A is a better hand than B, A.rank > B.rank
  //  - if A is an equivalent hand to B, A.rank == B.rank
  //  - if A is a worse hand than B, A.rank < B.rank
  //
  // Other than respecting the ordering above, this value is meaningless - use `maskDebugString` to decode it if necessary
  def rank: RankedHand = {
    inline def toOffset(shift: Int): Int = shift / 3

    inline def toPairMask(shift: Int): Long = 1L << (shift / 3 + 13)

    inline def toTripMask(shift: Int): Long = 1L << (shift / 3 + 26)

    val suitsMask = Suit.Spades.handMask | Suit.Diamonds.handMask | Suit.Clubs.handMask | Suit.Hearts.handMask

    // TODO some micro-optimisation around this - could see if this actually uses an intrinsic
    val isFlush = bitCount(hand & suitsMask) == 1
    val straightFlag = if (isFlush) StraightFlush.mask else Straight.mask

    // suit flags are no longer required at this point
    var ranks = hand & ~suitsMask

    // given a bitset AAA|...888|777|666|555|444|333|222, a straight is either:
    //              - 001|...000|000|000|001|001|001|001
    //              - 000|...000|000|001|001|001|001|001
    //              - 000|...000|001|001|001|001|001|000
    //              - 000|...001|001|001|001|001|000|000
    //              - etc
    // TODO micro-optimisation ensure this is constant folded
    val aceLowStraightMask = Rank.Ace.handMask | Rank.Five.handMask | Rank.Four.handMask | Rank.Three.handMask | Rank.Two.handMask
    val straightMask = Rank.Six.handMask | Rank.Five.handMask | Rank.Four.handMask | Rank.Three.handMask | Rank.Two.handMask

    if (ranks == aceLowStraightMask) {
      // mask off the Ace so it sorts correctly - this is a bit of a hack, but adding an "ace low" bit flag to the bottom of the mask (and shifting everything else up)
      //  is also a bit of a hack which is far more effort
      return straightFlag | (ranks ^ Ace.handMask)
    }

    // given a bitset        ...555|444|333|222:
    // a four of a kind is:
    //                     - ...000|000|000|100
    //                     - ...000|000|100|000
    //                     - ...000|100|000|000
    //                     - ...100|000|000|000
    //                     - etc
    //
    // a three of a kind is:
    //                     - ...000|000|000|011
    //                     - ...000|000|011|000
    //                     - ...000|011|000|000
    //                     - ...011|000|000|000
    //                     - etc
    //
    // a pair is:
    //                     - ...000|000|000|010
    //                     - ...000|000|010|000
    //                     - ...000|010|000|000
    //                     - ...010|000|000|000
    //                     - etc
    val fourOfAKindMask = 4L
    val tripsMask = 3L
    val pairMask = 2L

    // three of a kind and pair are more complicated, as there could be a full house or two pairs or neither of those
    var trips = -1L
    var pairLow = -1L
    var kickers = 0

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

    // loop over the relevant bits once
    var shift = 0
    while (shift <= 36) {
      // a straight can't exist past `straightMask << 24` so it's not necessary to check for it
      if (shift <= 24 && ranks == straightMask << shift) {
        return straightFlag | ranks
      } else if (shift > 24 && isFlush) {
        // ↑ TODO flush is a bit awkward because we need to check for a straight first
        return Flush.mask | ranks
      } else if (((ranks >> shift) & fourOfAKindMask) == fourOfAKindMask) {
        // ↑ shift ranks down instead of the mask up as it's equivalent but slightly shorter

        // mask off the quad bit and convert the remaining bit from a mask to an offset
        val kicker = java.lang.Long.numberOfTrailingZeros(ranks ^ (fourOfAKindMask << shift)) / 3

        return FourOfAKind.mask | toPairMask(shift) | 1L << kicker
      } else if (((ranks >> shift) & tripsMask) == tripsMask) {
        trips = toTripMask(shift)
        if (pairLow != -1) {
          return FullHouse.mask | trips | pairLow
        }
      } else if (((ranks >> shift) & pairMask) == pairMask) {
        if (trips != -1) {
          pairLow = toPairMask(shift)
          return FullHouse.mask | trips | pairLow
        } else if (pairLow == -1) {
          pairLow = toPairMask(shift)
          // drop the pair bit from the rank mask otherwise, when there's two pairs, we don't need to convert `pairLow`
          //  back to the shift used to create it
          ranks ^= (pairMask << shift)
        } else {
          val pairHigh = toPairMask(shift)
          val kicker = java.lang.Long.numberOfTrailingZeros(ranks ^ (pairMask << shift)) / 3

          return TwoPairs.mask | pairLow | pairHigh | 1L << kicker
        }
      } else if (((ranks >> shift) & kickerMask) == 1L) {
        // the kickers potentially spread across 39 bits (but only ever present in 13 of these) need to be compressed
        //  into the bottom 13 bits when this value is used in `ThreeOfAKind` or `OnePair`
        kickers |= (1 << toOffset(shift))
      }

      shift += 3
    }

    if (trips != -1) {
      ThreeOfAKind.mask | trips | kickers
    } else if (pairLow != -1) {
      OnePair.mask | pairLow | kickers
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
