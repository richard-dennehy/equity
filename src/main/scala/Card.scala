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

// A Hand of cards; sorted by rank
opaque type Hand = (Card, Card, Card, Card, Card)
// NOTE: 5-tuple seems like the simplest way to implement a fixed-size array

object Hand {
  def apply(first: Card, second: Card, third: Card, fourth: Card, fifth: Card): Hand = {
    val cards = Array(first, second, third, fourth, fifth).sortInPlaceWith(_._1.value > _._1.value)

    if (cards.distinct.size != cards.size) throw IllegalArgumentException("Duplicate cards")

    (cards(0), cards(1), cards(2), cards(3), cards(4))
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
      second.rank.value == third.rank.value &&
        third.rank.value == fourth.rank.value &&
        (first.rank.value == second.rank.value || fourth.rank.value == fifth.rank.value)

    val isThreeOfAKind =
      (first.rank.value == second.rank.value && second.rank.value == third.rank.value) ||
        (second.rank.value == third.rank.value && third.rank.value == fourth.rank.value) ||
          (third.rank.value == fourth.rank.value && fourth.rank.value == fifth.rank.value)

    // this counts trips as two pairs, which isn't exactly _ideal_
    val pairCount = Array(first, second, third, fourth, fifth).sliding(2).count(pair => pair(0).rank == pair(1).rank)

    if (isFlush && isStraight) {
      Category.StraightFlush
    } else if (isFlush) {
      Category.Flush
    } else if (isStraight) {
      Category.Straight
    } else if (isFourOfAKind) {
      Category.FourOfAKind
    } else if (isThreeOfAKind && pairCount == 2) {
      Category.ThreeOfAKind
    } else if (isThreeOfAKind && pairCount == 3) {
      Category.FullHouse
    } else if (pairCount == 2) {
      Category.TwoPairs
    } else if (pairCount == 1) {
      Category.OnePair
    } else {
      Category.HighCard
    }
  }
}

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
