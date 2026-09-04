package apps
package ul2025app17

import cs214.webapp.* 
import cs214.webapp.DecodingException

import scala.util.{Failure, Success, Try}
import ujson.*

import apps.ul2025app17.StateView.*

/**
 * Wire formats for encoding and decoding the domain types of the Uno web application.
 *
 * This object implements all JSON serialization/deserialization logic needed by the
 * web server and the frontend. It defines how Colors, Cards, Events, Views, and StateViews
 * are represented as JSON.
 */
object Wire extends AppWire[Event,View] : 

  // ------------------------------------------------------------
  // ColorWire
  // ------------------------------------------------------------

  /**
   * WireFormat implementation for the `Color` enum.
   *
   * Encodes colors as JSON strings ("Yellow", "Red", ...).
   * Decodes them back into the corresponding `Color` value.
   */
  object ColorWire extends WireFormat[Color]:
    def encode(c: Color): ujson.Value = Str(c.toString)
    def decode(js: ujson.Value): Try[Color] = Try:
      js.str match
        case "Yellow" => Color.Yellow
        case "Red"    => Color.Red
        case "Blue"   => Color.Blue
        case "Green"  => Color.Green
        case "TBD"    => Color.TBD
        case _    => throw DecodingException(f"Unknown color: $js")
  
  /** WireFormat for user identifiers (encoded as strings). */
  val UserIdWire = StringWire

  // ------------------------------------------------------------
  // UnoCardWire
  // ------------------------------------------------------------
  /**
   * WireFormat implementation for encoding and decoding Uno cards.
   *
   * Each card is represented as a JSON object with:
   *   - a `type` field naming the card type
   *   - additional fields depending on the subtype (`color`, `number`, ...)
   */
  object UnoCardWire extends WireFormat[UnoCard]:
    def encode(card: UnoCard): ujson.Value = card match
      case NumberCard(c, n) => 
        Obj("type" -> "NumberCard", "color" -> ColorWire.encode(c), "number" -> Num(n))
      case SkipCard(c) =>
        Obj("type" -> "SkipCard", "color" -> ColorWire.encode(c))
      case ReverseCard(c) =>
        Obj("type" -> "ReverseCard", "color" -> ColorWire.encode(c))
      case DrawTwoCard(c) =>
        Obj("type" -> "DrawTwoCard", "color" -> ColorWire.encode(c))
      case WildCard(c) =>
        Obj("type" -> "WildCard", "color" -> ColorWire.encode(c))
      case WildDrawFourCard(c) =>
        Obj("type" -> "WildDrawFourCard", "color" -> ColorWire.encode(c))

    def decode(js: ujson.Value): Try[UnoCard] = Try:
      val t = js("type").str
      t match
        case "NumberCard" =>
          val c = ColorWire.decode(js("color")).get
          val n = js("number").num.toInt
          NumberCard(c, n)
        case "SkipCard" =>
          val c = ColorWire.decode(js("color")).get
          SkipCard(c)
        case "ReverseCard" =>
          val c = ColorWire.decode(js("color")).get
          ReverseCard(c)
        case "DrawTwoCard" =>
          val c = ColorWire.decode(js("color")).get
          DrawTwoCard(c)
        case "WildCard" =>
          val c = ColorWire.decode(js("color")).get
          WildCard(c)
        case "WildDrawFourCard" =>
          val c = ColorWire.decode(js("color")).get
          WildDrawFourCard(c)
        case _ =>
          throw DecodingException(f"Unknown card : $js")


   // ------------------------------------------------------------
  // Event WireFormat
  // ------------------------------------------------------------
  /**
   * WireFormat for events sent from the UI to the backend.
   *
   * Each event is encoded as:
   *   - a "type" string
   *   - optional additional fields depending on the event
   */
  override object eventFormat extends WireFormat[Event] :
    override def encode(e: Event): Value = e match    
      case Event.CardClicked(cardIndex) =>
        Obj("type" -> "CardClicked", "cardIndex" -> IntWire.encode(cardIndex))
      case Event.ColorChosen(color) =>
        Obj("type" -> "ColorChosen", "color" -> ColorWire.encode(color))
      case Event.Play =>
        Obj("type" -> "Play")
      case Event.Draw =>
        Obj("type" -> "Draw")
      case Event.Pass =>
        Obj("type" -> "Pass")
      case Event.UnoButtonPressed =>
        Obj("type" -> "UnoButtonPressed")
      case Event.AcceptPenalty =>
        Obj("type" -> "AcceptPenalty")
    override def decode(json: Value): Try[Event] =  Try:
      json("type").str match
        case "CardClicked" =>
          val cardIndex = IntWire.decode(json("cardIndex")).get
          Event.CardClicked(cardIndex)
        case "ColorChosen" =>
          val c = ColorWire.decode(json("color")).get
          Event.ColorChosen(c)
        case "Play" => Event.Play
        case "Draw" => Event.Draw
        case "Pass" => Event.Pass
        case "UnoButtonPressed" => Event.UnoButtonPressed
        case "AcceptPenalty" => Event.AcceptPenalty
        case other => throw DecodingException(f"Unknown event : $other")


  // ------------------------------------------------------------
  // View WireFormat
  // ------------------------------------------------------------

  /**
   * WireFormat for encoding the complete View sent to the UI.
   *
   * A View contains:
   *   - the current state view
   *   - the cards of the current user
   *   - the "finished" status per player
   *   - the number of cards of all other players
   */
  override object viewFormat extends  WireFormat[View] : 

    // ------------------- PhaseView wire format -------------------
    /**
     * WireFormat for PhaseView, representing the current interaction phase.
     */
    object PhaseViewWire extends WireFormat[PhaseView]:
      def encode(p: PhaseView): Value = p match
        case PhaseView.Waiting =>
          Obj("type" -> "Waiting")

        case PhaseView.ChoosingColor =>
          Obj("type" -> "ChoosingColor")
        case PhaseView.DrawPenalty(penalty) =>
          Obj("type" -> "DrawPenalty",
              "penalty" -> IntWire.encode(penalty)
          )
        case PhaseView.SelectingCards =>
          Obj(
            "type" -> "SelectingCards")
      def decode(js: Value): Try[PhaseView] = Try:
        js("type").str match
          case "Waiting"        => PhaseView.Waiting
          case "ChoosingColor"  => PhaseView.ChoosingColor
          case "DrawPenalty"    => PhaseView.DrawPenalty(IntWire.decode(js("penalty")).get)
          case "SelectingCards" => PhaseView.SelectingCards
          case _ =>
            throw DecodingException(s"Unknown phase view : $js")
    
    // ------------------- StateView wire format -------------------
    /**
     * WireFormat for the StateView, representing the global game state.
     */
    object StateViewWire extends WireFormat[StateView]:
      val RankingsWire = MapWire(IntWire, SeqWire(UserIdWire))
      def encode(s: StateView): Value = s match
        case StateView.Playing(phase, currentPlayer, stack) =>
          Obj(
            "type" -> "Playing",
            "phase" -> PhaseViewWire.encode(phase),
            "currentPlayer" -> UserIdWire.encode(currentPlayer),
            "stack" -> UnoCardWire.encode(stack)
          )

        case StateView.Finished(rankings) =>
          Obj(
            "type" -> "Finished",
            "rankings" -> RankingsWire.encode(rankings)
          )

      def decode(js: Value): Try[StateView] = Try:
        js("type").str match
          case "Playing" =>
            val phase  = PhaseViewWire.decode(js("phase")).get
            val player = UserIdWire.decode(js("currentPlayer")).get
            val stack  = UnoCardWire.decode(js("stack")).get
            StateView.Playing(phase, player, stack)

          case "Finished" =>
            val rankings: Map[Int, Seq[UserId]] = 
              RankingsWire.decode(js("rankings")).get
            StateView.Finished(rankings)

          case _ =>
            throw DecodingException(s"Unknown state view : $js")
    
    // ------------------- CardView wire format -------------------
    /**
     * WireFormat for CardView, which describes how each card should appear to the player
     * (face-down, selected, valid, invalid).
     */
    object CardViewFormat extends WireFormat[CardView]:
      import CardView.* 
      override def encode(v : CardView) : Value = v match
        case FaceDown => 
          Obj("type" -> "FaceDown")
        case Selected(card : UnoCard) => 
          Obj("type" -> "Selected", "card" -> UnoCardWire.encode(card))
        case UnselectedValid(card : UnoCard) => 
          Obj("type" -> "UnselectedValid", "card" -> UnoCardWire.encode(card))
        case UnselectedNotValid(card : UnoCard) => 
          Obj("type" -> "UnselectedNotValid", "card" -> UnoCardWire.encode(card))

      override def decode(js : Value): Try[CardView] = Try:
        js("type").str match
          case "FaceDown" => FaceDown 
          case "Selected" => Selected(UnoCardWire.decode(js("card")).get)  
          case "UnselectedValid" => UnselectedValid(UnoCardWire.decode(js("card")).get)
          case "UnselectedNotValid" => UnselectedNotValid(UnoCardWire.decode(js("card")).get)

    // ------------------- Full View encode/decode -------------------

    /** Mapping from UserId → Boolean indicating whether each player finished. */
    val FinishedWire = MapWire(UserIdWire, BooleanWire)
    override def encode(v: View): Value =
      Obj(
        "state" -> StateViewWire.encode(v.state),
        "myCards" -> VectorWire(CardViewFormat).encode(v.myCards),
        "finished" -> FinishedWire.encode(v.finished),
        "otherCardCounts" -> MapWire(UserIdWire, IntWire).encode(v.otherCardCounts)
      )

    override def decode(js: Value): Try[View] = Try:
      val state = StateViewWire.decode(js("state")).get
      val myCards = VectorWire(CardViewFormat).decode(js("myCards")).get
      val finished: Map[UserId, Boolean] = FinishedWire.decode(js("finished")).get
      val otherCardCounts = MapWire(UserIdWire, IntWire).decode(js("otherCardCounts")).get
      View(state, myCards, finished, otherCardCounts)
