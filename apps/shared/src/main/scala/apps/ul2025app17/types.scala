package apps
package ul2025app17

import cs214.webapp.UserId

//============ Color ============
enum Color (val emoji : String): // Specifies the possibel colors
    case Yellow extends Color("🟨" )
    case Red extends Color( "🟥" )
    case Blue extends Color( "🟦") 
    case Green extends Color("🟩")
    case TBD extends  Color("❓") // For wild cards, before a color have been effectively chosen

object Color : 
    def all : Vector[Color] = Vector(Yellow, Red, Blue, Green) 

//============ UnoCard ============
sealed trait UnoCard : // Specifies the different types of Uno cards
    def color : Color
    def value : String
case class NumberCard (c : Color, number : Int) extends UnoCard :
    val color = c
    val value = number.toString()
case class SkipCard(c: Color) extends UnoCard :
    val color = c
    val value = "🚫"
case class ReverseCard(c: Color) extends UnoCard :
    val color = c
    val value = "🔄"
case class DrawTwoCard(c: Color) extends UnoCard : 
    val color = c
    val value = "+2"
case class WildCard(c: Color) extends UnoCard : 
    val color = c
    val value = "🌈"
case class WildDrawFourCard(c: Color) extends UnoCard :
    val color = c
    val value = "🌈" + "+4"

//============ Hand ============

type Hand = List[UnoCard] // The hand of cards of a player

//============ Stack ============

trait MStackTrait[A]: // Represents a drawing Pile from which we can remove cards efficiently
  def push(a: A): Unit
  def pop(): A
  def isEmpty: Boolean
  def size: Int
  def contains(a: A): Boolean

case class MStack[A](var l: List[A] = Nil) extends MStackTrait[A]:
  def push(a: A): Unit =
    l = a :: l 
  def pop(): A =
    var a = l.head
    l = l.tail
    a
  def isEmpty: Boolean =
    l == Nil
  def size: Int =
    l.length
  def contains(a: A): Boolean =
    l.contains(a)

//============ Event ============

enum Event :
  case CardClicked(cardIndex : Int)  // Player selected or deselected a card from their hand. (index for index in Hand
                                    //if UnoCard than player could have two cards of the same type)
  case ColorChosen(color: Color) // Player chooses a color after playing a Wild card.
  case Play // Player attempts to play the selected cards.
  case Draw // Player chooses to draw one card. 
  case Pass // Player chooses to pass after having drawn a card / end their turn. 
  case UnoButtonPressed // Player presses UNO button. 
  case AcceptPenalty //Player accepts the +X penalty instead of stacking. 

//============ State ============

case class State (
    players : Seq[UserId], // Players at the beginning of the game
    turnOrder : Vector[UserId], // Indicates the remaining players order of rotation
    hands : Map[UserId, Hand], // Keeps track of each player's hand
    selected : Map[UserId, List[Int]], // Which cards the players select
    drawPile : MStack[UnoCard], // The pile from which players can draw cards
    topCard : UnoCard, // The card according to which we have to match color/symbol etc according to the rules
    penalty : Int, // The currently accumulated draw penalty from +X cards
    phase : Phase, // Indicates if we are done playing or not
    rankings: Map[Int, Seq[UserId]] // Keeps track of the players ranking
)

enum Phase : 
    case Playing(turn: TurnState)
    case Done

case class TurnState( 
    currentPlayer: UserId, // Whose player's turn is it ? 
    pressedUno: Boolean, // Did the player press uno during this turn ? 
    mustChoseColor: Boolean, // Does the player need to choose a color (because he played a wild card)
    haveDrawnCard: Boolean // Did the player draw a card from the drawPile ?
)
//============ View ============

case class View(
    state : StateView, // Indicates if the game has finished yet or not 
    myCards: Vector[CardView], // The players' card classified in different categories
    finished : Map[UserId, Boolean], // Indicates if each player has already emptied their hand
    otherCardCounts: Map[UserId, Int]) // Associates each player with the number of remaining cards they have

enum PhaseView : 
    case Waiting // Not our turn
    case ChoosingColor // We are choosing a color
    case DrawPenalty(penalty: Int) // We have to continue stacking or accept the penalty amount of cards
    case SelectingCards // We are choosing cards to play

enum StateView : 
    case Playing(phase : PhaseView, currentPlayer : UserId, stack : UnoCard) // the game is still going on
    case Finished (rankings : Map[Int, Seq[UserId]]) // done playing

enum CardView:
    case FaceDown // other players' cards
    case Selected(card: UnoCard) // card currently selected by the player
    case UnselectedValid(card: UnoCard) // card is valid if played next
    case UnselectedNotValid(card: UnoCard) // card is nor selected nor valid
 