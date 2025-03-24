import Category.{Flush, FourOfAKind, FullHouse, HighCard, OnePair, Straight, StraightFlush, ThreeOfAKind, TwoPairs}

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
}

// A Hand of cards; sorted by rank
opaque type Hand = (Card, Card, Card, Card, Card)
// NOTE: 5-tuple seems like the simplest way to implement a fixed-size array

object Hand {
  def apply(first: Card, second: Card, third: Card, fourth: Card, fifth: Card): Hand = {
    val cards = Array(first, second, third, fourth, fifth).sortInPlaceWith(_._1.value > _._1.value)

    if (cards.distinct.size != cards.size) throw IllegalArgumentException("Duplicate cards")

    (cards(0), cards(1), cards(2), cards(3), cards(4))
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
  def category: Category = {
    // this could obviously be simplified significantly
    val (first, second, third, fourth, fifth) = h

    val isFlush =
      first.suit == second.suit &&
        second.suit == third.suit &&
        third.suit == fourth.suit &&
        fourth.suit == fifth.suit

    val isStraight =
      (first.rank.value - second.rank.value == 1 || (first.rank == Ace && fifth.rank == Rank.Two)) &&
        second.rank.value - third.rank.value == 1 &&
        third.rank.value - fourth.rank.value == 1 &&
        fourth.rank.value - fifth.rank.value == 1

    // probably shove early returns in starting here

    val isFourOfAKind =
      second.rank == third.rank && third.rank == fourth.rank &&
        (first.rank == second.rank || fourth.rank == fifth.rank)

    val isThreeOfAKind =
      (first.rank == second.rank && second.rank == third.rank) ||
        (second.rank == third.rank && third.rank == fourth.rank) ||
        (third.rank == fourth.rank && fourth.rank == fifth.rank)

    // this counts trips as two pairs, which isn't exactly _ideal_
    val pairCount = Array(first, second, third, fourth, fifth).sliding(2).count(pair => pair(0).rank == pair(1).rank)

    if (isFlush && isStraight) {
      if (first.rank == Ace && fifth.rank == Rank.Two) {
        Category.StraightFlush(second.rank)
      } else {
        Category.StraightFlush(first.rank)
      }
    } else if (isFlush) {
      Category.Flush(first.rank, second.rank, third.rank, fourth.rank, fifth.rank)
    } else if (isStraight) {
      if (first.rank == Ace && fifth.rank == Rank.Two) {
        Category.Straight(second.rank)
      } else {
        Category.Straight(first.rank)
      }
    } else if (isFourOfAKind) {
      if (first.rank == second.rank) {
        Category.FourOfAKind(first.rank, fifth.rank)
      } else {
        Category.FourOfAKind(second.rank, first.rank)
      }
    } else if (isThreeOfAKind && pairCount == 2) {
      if (first.rank == second.rank) {
        Category.ThreeOfAKind(first.rank, fourth.rank, fifth.rank)
      } else if (second.rank == third.rank) {
        Category.ThreeOfAKind(second.rank, first.rank, fifth.rank)
      } else {
        Category.ThreeOfAKind(third.rank, first.rank, second.rank)
      }
    } else if (isThreeOfAKind && pairCount == 3) {
      if (first.rank == second.rank && second.rank == third.rank) {
        Category.FullHouse(first.rank, fourth.rank)
      } else {
        Category.FullHouse(third.rank, first.rank)
      }
    } else if (pairCount == 2) {
      if (first.rank == second.rank && third.rank == fourth.rank) {
        Category.TwoPairs(first.rank, third.rank, fifth.rank)
      } else if (first.rank == second.rank) {
        Category.TwoPairs(first.rank, fourth.rank, third.rank)
      } else {
        Category.TwoPairs(second.rank, fourth.rank, first.rank)
      }
    } else if (pairCount == 1) {
      if (first.rank == second.rank) {
        Category.OnePair(first.rank, third.rank, fourth.rank, fifth.rank)
      } else if (second.rank == third.rank) {
        Category.OnePair(second.rank, first.rank, fourth.rank, fifth.rank)
      } else if (third.rank == fourth.rank) {
        Category.OnePair(third.rank, first.rank, second.rank, fifth.rank)
      } else {
        Category.OnePair(fourth.rank, first.rank, second.rank, third.rank)
      }
    } else {
      Category.HighCard(first.rank, second.rank, third.rank, fourth.rank, fifth.rank)
    }
  }
}

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
