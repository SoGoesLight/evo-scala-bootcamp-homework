package com.sogoeslight.homework.task5.algebraicdatatypes

object AlgebraicDataTypes {
  // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
  // task you completed to join the bootcamp. Use your best judgement about particular data types to include
  // in the solution, you can model concepts like:
  //
  // 1. Suit
  // 2. Rank
  // 3. Card
  // 4. Hand (Texas or Omaha)
  // 5. Board
  // 6. Poker Combination (High Card, Pair, etc.)
  // 7. Test Case (Board & Hands to rank)
  // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
  //
  // Make sure the defined model protects against invalid data. Use value classes and smart constructors as
  // appropriate. Place the solution under `adt` package in your homework repository.

  // 0. Error Message
  case class ErrorMessage(message: String)

  // 1. Suit
  sealed trait Suit
  object Suit {
    final case object Diamond extends Suit
    final case object Hearts extends Suit
    final case object Spades extends Suit
    final case object Clubs extends Suit

    def create(char: Char): Either[ErrorMessage, Suit] = {
      char.toLower match {
        case 'd'   => Right(Diamond)
        case 'h'   => Right(Hearts)
        case 's'   => Right(Spades)
        case 'c'   => Right(Clubs)
        case other => Left(ErrorMessage(s"$other is incorrect suit"))
      }
    }
  }

  // 2. Rank
  case class Rank private (value: Int)
  object Rank {
    case object A extends Rank(14)
    case object K extends Rank(13)
    case object Q extends Rank(12)
    case object J extends Rank(11)
    case object T extends Rank(10)
    case object Nine extends Rank(9)
    case object Eight extends Rank(8)
    case object Seven extends Rank(7)
    case object Six extends Rank(6)
    case object Five extends Rank(5)
    case object Four extends Rank(4)
    case object Three extends Rank(3)
    case object Two extends Rank(2)

    def create(charValue: Char): Either[ErrorMessage, Rank] = {
      charValue match {
        case 'A'   => Right(A)
        case 'K'   => Right(K)
        case 'Q'   => Right(Q)
        case 'J'   => Right(J)
        case 'T'   => Right(T)
        case '9'   => Right(Nine)
        case '8'   => Right(Eight)
        case '7'   => Right(Seven)
        case '6'   => Right(Six)
        case '5'   => Right(Five)
        case '4'   => Right(Four)
        case '3'   => Right(Three)
        case '2'   => Right(Two)
        case other => Left(ErrorMessage(s"$other is incorrect rank"))
      }
    }
  }

  // 3. Card
  sealed case class Card private (rank: Rank, suit: Suit)
  object Card {
    def create(rank: Rank, suit: Suit): Card = Card(rank, suit)
  }

  // 4. Hand (Texas or Omaha)
  sealed case class Hand private (cards: Set[Card])
  object Hand {
    def create(cards: Set[Card]): Option[Hand] = {
      if (cards.size == 2 || cards.size == 4) Some(Hand(cards)) else None
    }
  }

  // 5. Board
  sealed case class Board private (cards: Set[Card])
  object Board {
    def create(cards: Set[Card]): Option[Board] = {
      if (cards.size == 5) Some(Board(cards)) else None
    }
  }

  // 6. Poker Combination
  case class Combination private (value: Int)
  object Combination {
    final case object RoyalFlush extends Combination(10)
    final case object StraightFlush extends Combination(9)
    final case object FourOfAKind extends Combination(8)
    final case object FullHouse extends Combination(7)
    final case object Flush extends Combination(6)
    final case object Straight extends Combination(5)
    final case object ThreeOfAKind extends Combination(4)
    final case object TwoPair extends Combination(3)
    final case object Pair extends Combination(2)
    final case object HighCard extends Combination(1)
  }

  // 7. Test Case
  sealed case class TestCase private (board: Board, hands: List[Hand])

  // 8. Test Result
  sealed case class TestResult private (hands: List[Hand])
}
