import Rank.{Ace, Five, Four, Three, Two}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// scenarios:
//  - 2 hole cards; no community cards; N dead cards; 50 - N deck cards; generate all potential hands
//  - 2 hole cards; 3 community cards; N dead cards; 47 - N deck cards; generate all potential hands
//  - 2 hole cards; 5 community cards; N dead cards; generate only best hand
//
// given 5 cards, what's the best hand unique to this player? (this is equivalent to just "what's the rank of this hand")
// given 7 cards, what's the best hand unique to this player? (that is, take 1 or 2 hole cards, fill out with the community cards, and rank it)
// given 5 cards, what are all the hands unique to this player?
//  - take 1 hole card, 0-2 deck cards, and 4-2 community cards
//  - any combination of hole cards and community cards is guaranteed to be playable
//    - only take the best of these
// given 2 cards, what are all the hands that contain 1 or 2 of these, and the probability of playing them?
//
// the hands the community cards can make are fixed for everyone - calculate these once?
//  - need to exclude hole cards from these as we know they can't be drawn from the deck
//  - the odds of a tie are based on how many community decks are available (plus the odds of players playing equivalent cards from their hole)
//
// theory: the odds of a tie based on the hole cards are knowable from the hole cards alone
//  - either player A and player B have the same ranking cards and will always tie (excluding flushes) or they have different ranks and can only tie by playing no hole cards
//
// for each player, come up with the list of hands that _only that player can play_
//  - is it possible or necessary to discard hand B if hand A is guaranteed to be playable and B is worse than A?
//    - probably yes because that's exactly what the "5 community card" scenario does
//  - is it necessary to keep track of both the hand itself and the probability it can be played?
//  - does the _probability_ that hand A can be played affect the _equity_ of hand B if B is worse than A?
//
// theory: the odds of a tie are at least the odds of the community cards "winning"
//
// idea: for each player (including the community cards), come up with the best hand they're _guaranteed_ to be able to play
//  then, for each player (and community), generate all the hands they could play, with a probability of the number of ways they could play it,
//  from best to worst and stop immediately when either the player is guaranteed to be able to play the hand, or they're guaranteed to lose.
// This produces a list of potentially winning hands for each player
//
// maybe:
//  - equity for any player is the sum of the probability of being able to play each hand multiplied by the odds that no other player can beat that hand
//  - this also (maybe?) needs to calculate the odds of a tie - is this just the sum of the probability of each hand multiplied by the probability of an equivalent hand being played elsewhere?
//  - what's the most efficient way to do this? there's potentially a whole lot of multiplications here
//
// ties:
//  - the community cards produce the best hand
//  - player A's best hand is the same rank as player B's best hand
//  - there's some subtle edge cases around calculating this properly
//
// properties:
//  - the odds of each player tying or winning needs to sum to one
//  - do all the winning hand probabilities sum to one - what does it mean if it doesn't?
//    - it can be zero if both players can play a hand but will tie
//
// mad theorising:
//  - given community cards 33334 and hole cards 5,6 vs 6,7, the 6,7 wins - but this has nothing to do with the four of a kind and everything to do with the existence of a kicker
//  - this makes the idea of checking 4 of a kinds that only any given player can directly play troublesome
//  - given hole cards 5,6 vs 6,7, in a High Card scenario, 6,7 always wins - it's therefore not necessary to check any particular High Card hands
//  - if a player plays a 4 of a kind, the kicker is irrelevant because nobody else can tie on the quads

def equity(heroHole: (Card, Card), villainHole: (Card, Card), community: Vector[Card], exclude: Vector[Card]): Equity =
  ???

// Returns a list of distinct hands, with the probability they can be played, that:
//  - are superior to `handToBeat`
//  - contain cardA _or_ contain cardB
//
// This doesn't include possible hands that don't include either `cardA` or `cardB` as there's no benefit to playing these - every player
//  at the table can play these hands, so the best possible result is a tie.
//
// The list is ranked in order from best hand to worst (i.e. ideally the first entry is a Royal Flush, and the last is a 7 High Card hand).
//
// Once the probability of generating a hand is 1, this method returns immediately (as there's no reason to consider worse hands).
//
// If a hand can be played multiple ways with different suits, only one of these hands will be returned, but with a higher probability
//
// If the `community` doesn't contain exactly 5 cards, cards will be drawn from the implied deck, excluding any cards listed in `drawn`.
//
// `drawn` is expected to contain _every_ card not contained in the deck, including `community`, `cardA`, and `cardB`.
//
// If this player cannot beat `handToBeat`, the returned list is empty.
def bestPossibleHands(cardA: Card, cardB: Card, community: Vector[Card], drawn: Vector[Card]): Vector[PossibleHand] = {
  val something = bestGuaranteedHand(cardA, cardB, community)

  // a straight may involve card A or card B, but not necessarily both
  // a straight involving both cards has higher odds
  // a straight involving community cards has higher odds
  // options:
  //  - shove hole cards and community cards together into hands of less than 5 and try to draw cards to finish straight if possible
  //  - try and be a bit cleverer - produce sets of cards needed to create a straight, then check for draw odds
  val straights = ???
  // look for flushes and almost flushes
  // look for straights and almost straights
  // count duplicates and look for matches in the deck

  //  case StraightFlush
  //  case FourOfAKind
  //  case FullHouse
  //  case Flush
  //  case Straight
  //  case ThreeOfAKind
  //  case TwoPairs
  //  case OnePair
  //  case HighCard
  ???
}

def bestGuaranteedHand(cardA: Card, cardB: Card, community: Vector[Card]): Option[RankedHand] = {
  if (community.size < 3) {
    None
  } else if (community.size == 3) {
    // there's no point playing this hand if it can't win
    Some(Hand(cardA, cardB, community(0), community(1), community(2)).rank)
  } else if (community.size == 4) {
    (community.combinations(3).map { c3 =>
      Hand(cardA, cardB, c3(0), c3(1), c3(2)).rank
    }.toVector
      :+ Hand(cardA, community(0), community(1), community(2), community(3)).rank
      :+ Hand(cardB, community(0), community(1), community(2), community(3)).rank
      ).maxOption
  } else { // 5
    (community.combinations(3).map { c3 =>
      Hand(cardA, cardB, c3(0), c3(1), c3(2)).rank
    } ++ community.combinations(4).flatMap { c4 =>
      Vector(Hand(cardA, c4(0), c4(1), c4(2), c4(3)).rank, Hand(cardB, c4(0), c4(1), c4(2), c4(3)).rank)
    }).maxOption
  }
}

def possibleStraights(cardA: Card, cardB: Card, community: Vector[Card], drawn: Vector[Card]): Vector[PossibleHand] = {
  assert(cardA.rank.value >= cardB.rank.value)

  // similarly to `possibleStraightFlushes`, but ignore suit, but don't ignore suit if it makes a flush
  // if A.rank - B.rank < 5
  //  for R going from A+5 (max Ace) down to B (min 6)
  //   odds = 0
  //   for all suit combinations such that at least one suit is different
  //    odds += draw(R, suit0) * draw(R - 1, suit1) * draw(R - 2, suit2) * draw(R - 3, suit3) * draw(R - 4, suit4)
  //   if odds != 0, possibleHands += (odds, R..R-4)
  //  Ace->5
  // odd implication that a set of suits doesn't need to be drawable when constructing possible hand since the suits are ignored anyway
  // mad idea: deliberately try to get a flush, then subtract that from odds - checking against all suits will add the flush back in and cancel out

  val possibleHands = ArrayBuffer.empty[PossibleHand]
  val availableCards = community :+ cardA :+ cardB

  inline def straights(max: Rank, min: Rank): Unit = {
    var highCard = max
    while (highCard.value >= min.value) {
      var odds = 0f
      var helper = DrawHelper(availableCards, drawn.toBuffer, 5 - community.size)

      // exclude the chance of getting a straight flush
      odds -= helper.draw(card(highCard, cardA.suit)) *
        helper.draw(card(highCard - 1, cardA.suit)) *
        helper.draw(card(highCard - 2, cardA.suit)) *
        helper.draw(card(highCard - 3, cardA.suit)) *
        helper.draw(card(highCard - 4, cardA.suit))

      helper = DrawHelper(availableCards, drawn.toBuffer, 5 - community.size)

      if (cardA.suit != cardB.suit) {
        odds -= helper.draw(card(highCard, cardB.suit)) *
          helper.draw(card(highCard - 1, cardB.suit)) *
          helper.draw(card(highCard - 2, cardB.suit)) *
          helper.draw(card(highCard - 3, cardB.suit)) *
          helper.draw(card(highCard - 4, cardB.suit))

        helper = DrawHelper(availableCards, drawn.toBuffer, 5 - community.size)
      }

      odds +=
        helper.drawRank(highCard) *
          helper.drawRank(highCard - 1) *
          helper.drawRank(highCard - 2) *
          helper.drawRank(highCard - 3) *
          helper.drawRank(highCard - 4)

      if (odds > 0f) {
        // slight strangeness - because of how ranking works, the suits are completely irrelevant here, so long as they don't flush
        possibleHands += PossibleHand(
          Hand(card(highCard, Suit.Spades), card(highCard - 1, Suit.Spades), card(highCard - 2, Suit.Spades), card(highCard - 3, Suit.Spades), card(highCard - 4, Suit.Diamonds)).rank,
          odds
        )
      }

      highCard -= 1
    }
  }

  inline def aceToFiveStraight(): Unit = {
    var odds = 0f
    var helper = DrawHelper(availableCards, drawn.toBuffer, 5 - community.size)

    // exclude the chance of getting a straight flush
    odds -= helper.draw(card(Five, cardA.suit)) *
      helper.draw(card(Four, cardA.suit)) *
      helper.draw(card(Three, cardA.suit)) *
      helper.draw(card(Two, cardA.suit)) *
      helper.draw(card(Ace, cardA.suit))

    helper = DrawHelper(availableCards, drawn.toBuffer, 5 - community.size)

    if (cardA.suit != cardB.suit) {
      odds -= helper.draw(card(Five, cardB.suit)) *
        helper.draw(card(Four, cardB.suit)) *
        helper.draw(card(Three, cardB.suit)) *
        helper.draw(card(Two, cardB.suit)) *
        helper.draw(card(Ace, cardB.suit))

      helper = DrawHelper(availableCards, drawn.toBuffer, 5 - community.size)
    }

    odds += helper.drawRank(Five) *
      helper.drawRank(Four) *
      helper.drawRank(Three) *
      helper.drawRank(Two) *
      helper.drawRank(Ace)

    if (odds > 0f) {
      possibleHands += PossibleHand(Hand(card(Rank.Ace, Suit.Hearts), card(Five, Suit.Spades), card(Four, Suit.Spades), card(Three, Suit.Spades), card(Two, Suit.Spades)).rank, odds)
    }
  }

  // both cards can appear in the same straight, so merge the iterators together
  if (cardA.rank.value - cardB.rank.value < 5) {
    // try and build a straight from R down to R-4, therefore R needs to stay bounded between Ace and 6
    val max = if (cardA.rank.value > Rank.Ten.value) Rank.Ace else cardA.rank + 4
    val min = if (cardB.rank.value < Rank.Six.value) Rank.Six else cardB.rank

    straights(max, min)

    if (cardB.rank.value <= 5) {
      aceToFiveStraight()
    }
  } else {
    val aMax = if (cardA.rank.value > Rank.Ten.value) Rank.Ace else cardA.rank + 4
    val aMin = if (cardA.rank.value < Rank.Six.value) Rank.Six else cardA.rank

    val bMax = if (cardB.rank.value > Rank.Ten.value) Rank.Ace else cardB.rank + 4
    val bMin = if (cardB.rank.value < Rank.Six.value) Rank.Six else cardB.rank

    straights(aMax, aMin)
    straights(bMax, bMin)

    if (cardA.rank == Rank.Ace || cardB.rank.value <= 5) {
      aceToFiveStraight()
    }
  }

  possibleHands.toVector
}

def possibleStraightFlushes(cardA: Card, cardB: Card, community: Vector[Card], drawn: Vector[Card]): Vector[PossibleHand] = {
  assert(cardA.rank.value >= cardB.rank.value)

  // if A.suit == B.suit and A.rank - B.rank < 5
  //  for R going from A+5 (max Ace) down to B (min 6)
  //   odds *= draw(R, A.suit) * draw(R - 1, A.suit) * draw(R - 2, A.suit) * draw(R - 3, A.suit) * draw(R - 4, A.suit)
  //   if odds != 0, possibleHands += (odds, R..R-4)
  //  potential Ace->5 if A is Ace or B <= 5
  // else
  //  for C in A and B
  //   for R going from C+5 (max Ace) down to C (min 6)
  //    odds *= draw(R, C.suit) * draw(R - 1, C.suit) * draw(R - 2, C.suit) * draw(R - 3, C.suit) * draw(R - 4, C.suit)
  //    if odds != 0, possibleHands += (odds, R..R-4)
  //  potential (2) Ace->5 if A is Ace or A <= 5 or B <= 5
  val possibleHands = ArrayBuffer.empty[PossibleHand]
  val availableCards = community :+ cardA :+ cardB

  inline def straightFlushes(suit: Suit, max: Int, min: Int): Unit = {
    var highCard = max
    while (highCard >= min) {
      val helper = DrawHelper(availableCards, drawn.toBuffer, 5 - community.size)

      val odds =
        helper.draw(card(highCard, suit)) *
          helper.draw(card(highCard - 1, suit)) *
          helper.draw(card(highCard - 2, suit)) *
          helper.draw(card(highCard - 3, suit)) *
          helper.draw(card(highCard - 4, suit))

      if (odds != 0f) {
        possibleHands += PossibleHand(
          Hand(card(highCard, suit), card(highCard - 1, suit), card(highCard - 2, suit), card(highCard - 3, suit), card(highCard - 4, suit)).rank,
          odds
        )
      }

      highCard -= 1
    }
  }

  inline def aceToFiveStraightFlush(suit: Suit): Unit = {
    val helper = DrawHelper(availableCards, drawn.toBuffer, 5 - community.size)

    val odds =
      helper.draw(card(Rank.Five, suit)) *
        helper.draw(card(Rank.Four, suit)) *
        helper.draw(card(Rank.Three, suit)) *
        helper.draw(card(Rank.Two, suit)) *
        helper.draw(card(Rank.Ace, suit))

    if (odds != 0f) {
      possibleHands += PossibleHand(
        Hand(card(Rank.Five, suit), card(Rank.Four, suit), card(Rank.Three, suit), card(Rank.Two, suit), card(Rank.Ace, suit)).rank,
        odds
      )
    }
  }

  // both cards can appear in the same straight, so merge the iterators together
  if (cardA.suit == cardB.suit && cardA.rank.value - cardB.rank.value < 5) {
    // try and build a straight from R down to R-4, therefore R needs to stay bounded between Ace and 6
    val max = math.min(Rank.Ace.value, cardA.rank.value + 4)
    val min = math.max(Rank.Six.value, cardB.rank.value)

    straightFlushes(cardA.suit, max, min)

    if (cardB.rank.value <= 5) {
      aceToFiveStraightFlush(cardA.suit)
    }
  } else {
    val aMax = math.min(Rank.Ace.value, cardA.rank.value + 4)
    val aMin = math.max(Rank.Six.value, cardA.rank.value)

    val bMax = math.min(Rank.Ace.value, cardB.rank.value + 4)
    val bMin = math.max(Rank.Six.value, cardB.rank.value)

    straightFlushes(cardA.suit, aMax, aMin)
    straightFlushes(cardB.suit, bMax, bMin)

    if (cardA.rank == Rank.Ace || cardB.rank.value <= 5) {
      if (cardA.suit == cardB.suit) {
        aceToFiveStraightFlush(cardA.suit)
      } else {
        aceToFiveStraightFlush(cardA.suit)
        aceToFiveStraightFlush(cardB.suit)
      }
    }
  }

  possibleHands.toVector
}

def possibleFourOfAKinds(cardA: Card, cardB: Card, community: Vector[Card], drawn: Vector[Card]): Vector[PossibleHand] = {
  val possibleHands = ArrayBuffer.empty[PossibleHand]

  var drawHelper = DrawHelper(community :+ cardA :+ cardB, drawn.toBuffer, 5 - community.size)
  val fourOfAKindAOdds =
    drawHelper.draw(card(cardA.rank, Suit.Spades)) *
      drawHelper.draw(card(cardA.rank, Suit.Diamonds)) *
      drawHelper.draw(card(cardA.rank, Suit.Clubs)) *
      drawHelper.draw(card(cardA.rank, Suit.Hearts))

  if (fourOfAKindAOdds != 0f) {
    // peculiarity: there's no reason to care about the kicker because there's no way to get conflicting four of a kinds like this,
    //  so just shove any other card into the hand
    val kicker = if (cardA.rank == Rank.Ace) Rank.King else Rank.Ace

    possibleHands += PossibleHand(
      Hand(
        card(cardA.rank, Suit.Spades),
        card(cardA.rank, Suit.Diamonds),
        card(cardA.rank, Suit.Clubs),
        card(cardA.rank, Suit.Hearts),
        card(kicker, Suit.Spades)
      ).rank,
      fourOfAKindAOdds
    )
  }

  if (cardA.rank != cardB.rank) {
    drawHelper = DrawHelper(community :+ cardA :+ cardB, drawn.toBuffer, 5 - community.size)

    val fourOfAKindBOdds =
      drawHelper.draw(card(cardB.rank, Suit.Spades)) *
        drawHelper.draw(card(cardB.rank, Suit.Diamonds)) *
        drawHelper.draw(card(cardB.rank, Suit.Clubs)) *
        drawHelper.draw(card(cardB.rank, Suit.Hearts))

    if (fourOfAKindBOdds != 0f) {
      // peculiarity: there's no reason to care about the kicker because there's no way to get conflicting four of a kinds like this,
      //  so just shove any other card into the hand
      val kicker = if (cardB.rank == Rank.Ace) Rank.King else Rank.Ace

      possibleHands += PossibleHand(
        Hand(
          card(cardB.rank, Suit.Spades),
          card(cardB.rank, Suit.Diamonds),
          card(cardB.rank, Suit.Clubs),
          card(cardB.rank, Suit.Hearts),
          card(kicker, Suit.Spades)
        ).rank,
        fourOfAKindBOdds
      )
    }
  }

  possibleHands.toVector
}

class DrawHelper(available: Vector[Card], drawn: mutable.Buffer[Card], var remainingDraws: Int) {
  // TODO issue with this interface is sometimes it's necessary to ignore the already drawn cards e.g. pairs
  //  potential fix: change the interface to essentially say "given these cards are definitely available and these cards definitely aren't, what's the probability of ending up with this hand of 5?"
  def draw(card: Card): Float = {
    if (available.contains(card)) return 1f
    if (remainingDraws == 0) return 0f

    val probability = drawProbability(Some(card.rank), Some(card.suit), drawn.toVector)
    if (probability == 0f) return 0f

    drawn += card
    remainingDraws -= 1

    probability
  }

  def drawRank(rank: Rank): Float = {
    if (available.exists(_.rank == rank)) return 1f
    if (remainingDraws == 0) return 0f

    // make a copy as this potentially gets mutated
    val drawn_ = drawn.toVector

    var totalProbability = 0f
    var probability = drawProbability(Some(rank), Some(Suit.Spades), drawn_)
    if (probability != 0.0) {
      drawn += card(rank, Suit.Spades)
    }
    totalProbability += probability

    probability = drawProbability(Some(rank), Some(Suit.Diamonds), drawn_)
    if (probability != 0f && totalProbability == 0f) {
      drawn += card(rank, Suit.Diamonds)
    }
    totalProbability += probability

    probability = drawProbability(Some(rank), Some(Suit.Clubs), drawn_)
    if (probability != 0f && totalProbability == 0f) {
      drawn += card(rank, Suit.Clubs)
    }
    totalProbability += probability

    probability = drawProbability(Some(rank), Some(Suit.Hearts), drawn_)
    if (probability != 0f && totalProbability == 0f) {
      drawn += card(rank, Suit.Hearts)
    }
    totalProbability += probability

    if (totalProbability == 0f) return 0f

    remainingDraws -= 1

    totalProbability
  }
}

// Returns 0 <= p <= 1 as the probability of drawing a card, filtered by `rank` and/or `suit`, i.e.
//  ignoring all other factors, `drawProbability(Two, None, ...) == drawProbability(Two, Spades, ...) * 4`
//
// If both `rank` and `suit` are None, this will throw an error - while the odds are 1 in this case, this scenario is more
//  likely a logical error.
//
// If `drawn` contains a card of a matching `rank` and `suit`, this will return 0.
//
// If `suit` is unspecified, the odds of drawing a card with the specified `rank` is reduced by the number of cards with the
//  same `rank` contained in `drawn`.
//
// Not withstanding the above, the larger `drawn` is, the higher the chance of drawing any particular card (because this implies the deck is smaller).
def drawProbability(rank: Option[Rank], suit: Option[Suit], drawn: Vector[Card]): Float = {
  val deckSize = 52f - drawn.size

  (rank, suit) match {
    case (Some(rank), Some(suit)) =>
      if (drawn.contains(card(rank, suit))) 0 else 1 / deckSize

    case (Some(rank), None) =>
      val drawnOfRank = drawn.count(_.rank == rank)
      (4 - drawnOfRank) / deckSize

    case (None, Some(suit)) =>
      val drawnOfSuit = drawn.count(_.suit == suit)
      (13 - drawnOfSuit) / deckSize

    case (None, None) =>
      throw IllegalArgumentException("Must filter by rank or suit")
  }
}

case class PossibleHand(hand: RankedHand, probability: Float)

case class Equity(win: Float, draw: Float)

// check for straight flush - 10 distinct; 40 ways; at most 2 different suits for any player
// check for four of a kind - 156 distinct; 624 ways; at most 2 different ways for any player
// check for full house - 156 distinct; 3744 ways; <N pairs> * <N trips> per player
// check for flush - 1277 distinct; 5108 ways; bounded by number of suits the player has - need to exclude straights
// straight - 10 distinct; 10200 ways; needs to be within 5 ranks of A or B though - exclude straight flush
// three of a kind - 858 distinct; 54912 ways; pair in hole gives 2 ways to trip, no pair means 4 ways, probably - exclude FH
// two pair - 858 distinct; 123552 ways; starting with pair means it's just odds of drawing a pair from community; otherwise, something something odds of pair from A, pair from B, pair from community, minus FH
// one pair - 2,860; 1098240 ways; starting with pair guarantees this; otherwise, odds of drawing one of three or one of other three, minus two pair/FH
// high card - odds of drawing any cards that don't otherwise form a hand

// list each distinct community hand with odds - upper bound ~7400
// for each potential community hand, calculate best hand for each player and find outcome
// NOTE this doesn't quite work because the community cards don't know which suits matter and which don't