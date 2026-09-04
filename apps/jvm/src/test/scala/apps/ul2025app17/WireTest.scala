package apps.ul2025app17

import munit.*
import cs214.webapp.*
import cs214.webapp.DecodingException
import scala.util.{Success, Failure}
import ujson.*
import apps.ul2025app17.Color.*
import apps.ul2025app17.Event.*
import apps.ul2025app17.PhaseView.*
import apps.ul2025app17.StateView.*
import apps.ul2025app17.CardView.*

class WireTest extends FunSuite:

  // Helper function for round-trip testing
  def roundTrip[A](wire: WireFormat[A], value: A): A =
    val encoded = wire.encode(value)
    wire.decode(encoded).get

  // ============ Color Tests ============
  test("ColorWire: encode all colors") {
    assertEquals(Wire.ColorWire.encode(Color.Yellow), Str("Yellow"))
    assertEquals(Wire.ColorWire.encode(Color.Red), Str("Red"))
    assertEquals(Wire.ColorWire.encode(Color.Blue), Str("Blue"))
    assertEquals(Wire.ColorWire.encode(Color.Green), Str("Green"))
    assertEquals(Wire.ColorWire.encode(Color.TBD), Str("TBD"))
  }

  test("ColorWire: decode all colors") {
    assertEquals(Wire.ColorWire.decode(Str("Yellow")).get, Color.Yellow)
    assertEquals(Wire.ColorWire.decode(Str("Red")).get, Color.Red)
    assertEquals(Wire.ColorWire.decode(Str("Blue")).get, Color.Blue)
    assertEquals(Wire.ColorWire.decode(Str("Green")).get, Color.Green)
    assertEquals(Wire.ColorWire.decode(Str("TBD")).get, Color.TBD)
  }

  test("ColorWire: round-trip all colors") {
    for color <- Color.all :+ Color.TBD do
      assertEquals(roundTrip(Wire.ColorWire, color), color)
  }

  test("ColorWire: decode invalid color throws exception") {
    intercept[DecodingException] {
      Wire.ColorWire.decode(Str("Purple")).get
    }
  }

  // ============ UnoCard Tests ============
  test("UnoCardWire: encode NumberCard") {
    val card = NumberCard(Color.Red, 7)
    val encoded = Wire.UnoCardWire.encode(card)
    assertEquals(encoded("type").str, "NumberCard")
    assertEquals(encoded("color").str, "Red")
    assertEquals(encoded("number").num, 7.0)
  }

  test("UnoCardWire: encode SkipCard") {
    val card = SkipCard(Color.Blue)
    val encoded = Wire.UnoCardWire.encode(card)
    assertEquals(encoded("type").str, "SkipCard")
    assertEquals(encoded("color").str, "Blue")
  }

  test("UnoCardWire: encode ReverseCard") {
    val card = ReverseCard(Color.Green)
    val encoded = Wire.UnoCardWire.encode(card)
    assertEquals(encoded("type").str, "ReverseCard")
    assertEquals(encoded("color").str, "Green")
  }

  test("UnoCardWire: encode DrawTwoCard") {
    val card = DrawTwoCard(Color.Yellow)
    val encoded = Wire.UnoCardWire.encode(card)
    assertEquals(encoded("type").str, "DrawTwoCard")
    assertEquals(encoded("color").str, "Yellow")
  }

  test("UnoCardWire: encode WildCard") {
    val card = WildCard(Color.TBD)
    val encoded = Wire.UnoCardWire.encode(card)
    assertEquals(encoded("type").str, "WildCard")
    assertEquals(encoded("color").str, "TBD")
  }

  test("UnoCardWire: encode WildDrawFourCard") {
    val card = WildDrawFourCard(Color.TBD)
    val encoded = Wire.UnoCardWire.encode(card)
    assertEquals(encoded("type").str, "WildDrawFourCard")
    assertEquals(encoded("color").str, "TBD")
  }

  test("UnoCardWire: decode NumberCard") {
    val json = Obj("type" -> "NumberCard", "color" -> "Red", "number" -> 5)
    val decoded = Wire.UnoCardWire.decode(json).get
    assertEquals(decoded, NumberCard(Color.Red, 5))
  }

  test("UnoCardWire: decode SkipCard") {
    val json = Obj("type" -> "SkipCard", "color" -> "Blue")
    val decoded = Wire.UnoCardWire.decode(json).get
    assertEquals(decoded, SkipCard(Color.Blue))
  }

  test("UnoCardWire: decode ReverseCard") {
    val json = Obj("type" -> "ReverseCard", "color" -> "Green")
    val decoded = Wire.UnoCardWire.decode(json).get
    assertEquals(decoded, ReverseCard(Color.Green))
  }

  test("UnoCardWire: decode DrawTwoCard") {
    val json = Obj("type" -> "DrawTwoCard", "color" -> "Yellow")
    val decoded = Wire.UnoCardWire.decode(json).get
    assertEquals(decoded, DrawTwoCard(Color.Yellow))
  }

  test("UnoCardWire: decode WildCard") {
    val json = Obj("type" -> "WildCard", "color" -> "TBD")
    val decoded = Wire.UnoCardWire.decode(json).get
    assertEquals(decoded, WildCard(Color.TBD))
  }

  test("UnoCardWire: decode WildDrawFourCard") {
    val json = Obj("type" -> "WildDrawFourCard", "color" -> "TBD")
    val decoded = Wire.UnoCardWire.decode(json).get
    assertEquals(decoded, WildDrawFourCard(Color.TBD))
  }

  test("UnoCardWire: round-trip all card types") {
    val cards = List(
      NumberCard(Color.Red, 0),
      NumberCard(Color.Blue, 9),
      SkipCard(Color.Green),
      ReverseCard(Color.Yellow),
      DrawTwoCard(Color.Red),
      WildCard(Color.TBD),
      WildCard(Color.Red), // Wild with chosen color
      WildDrawFourCard(Color.TBD),
      WildDrawFourCard(Color.Blue) // WildDrawFour with chosen color
    )
    
    for card <- cards do
      assertEquals(roundTrip(Wire.UnoCardWire, card), card)
  }

  test("UnoCardWire: decode invalid card type throws exception") {
    val json = Obj("type" -> "InvalidCard", "color" -> "Red")
    intercept[DecodingException] {
      Wire.UnoCardWire.decode(json).get
    }
  }

  // ============ Event Tests ============
  test("EventWire: encode CardClicked") {
    val event = Event.CardClicked(3)
    val encoded = Wire.eventFormat.encode(event)
    assertEquals(encoded("type").str, "CardClicked")
    assertEquals(encoded("cardIndex").num, 3.0)
  }

  test("EventWire: encode ColorChosen") {
    val event = Event.ColorChosen(Color.Red)
    val encoded = Wire.eventFormat.encode(event)
    assertEquals(encoded("type").str, "ColorChosen")
    assertEquals(encoded("color").str, "Red")
  }

  test("EventWire: encode simple events") {
    assertEquals(Wire.eventFormat.encode(Event.Play)("type").str, "Play")
    assertEquals(Wire.eventFormat.encode(Event.Draw)("type").str, "Draw")
    assertEquals(Wire.eventFormat.encode(Event.Pass)("type").str, "Pass")
    assertEquals(Wire.eventFormat.encode(Event.UnoButtonPressed)("type").str, "UnoButtonPressed")
    assertEquals(Wire.eventFormat.encode(Event.AcceptPenalty)("type").str, "AcceptPenalty")
  }

  test("EventWire: decode CardClicked") {
    val json = Obj("type" -> "CardClicked", "cardIndex" -> 5)
    val decoded = Wire.eventFormat.decode(json).get
    assertEquals(decoded, Event.CardClicked(5))
  }

  test("EventWire: decode ColorChosen") {
    val json = Obj("type" -> "ColorChosen", "color" -> "Blue")
    val decoded = Wire.eventFormat.decode(json).get
    assertEquals(decoded, Event.ColorChosen(Color.Blue))
  }

  test("EventWire: decode simple events") {
    assertEquals(Wire.eventFormat.decode(Obj("type" -> "Play")).get, Event.Play)
    assertEquals(Wire.eventFormat.decode(Obj("type" -> "Draw")).get, Event.Draw)
    assertEquals(Wire.eventFormat.decode(Obj("type" -> "Pass")).get, Event.Pass)
    assertEquals(Wire.eventFormat.decode(Obj("type" -> "UnoButtonPressed")).get, Event.UnoButtonPressed)
    assertEquals(Wire.eventFormat.decode(Obj("type" -> "AcceptPenalty")).get, Event.AcceptPenalty)
  }

  test("EventWire: round-trip all events") {
    val events = List(
      Event.CardClicked(0),
      Event.CardClicked(7),
      Event.ColorChosen(Color.Red),
      Event.ColorChosen(Color.Blue),
      Event.Play,
      Event.Draw,
      Event.Pass,
      Event.UnoButtonPressed,
      Event.AcceptPenalty
    )
    
    for event <- events do
      assertEquals(roundTrip(Wire.eventFormat, event), event)
  }

  test("EventWire: decode invalid event type throws exception") {
    val json = Obj("type" -> "InvalidEvent")
    intercept[DecodingException] {
      Wire.eventFormat.decode(json).get
    }
  }

  // ============ CardView Tests ============
  test("CardViewFormat: encode FaceDown") {
    val view = CardView.FaceDown
    val encoded = Wire.viewFormat.CardViewFormat.encode(view)
    assertEquals(encoded("type").str, "FaceDown")
  }

  test("CardViewFormat: encode Selected") {
    val card = NumberCard(Color.Red, 5)
    val view = CardView.Selected(card)
    val encoded = Wire.viewFormat.CardViewFormat.encode(view)
    assertEquals(encoded("type").str, "Selected")
    assertEquals(encoded("card")("type").str, "NumberCard")
  }

  test("CardViewFormat: encode UnselectedValid") {
    val card = SkipCard(Color.Blue)
    val view = CardView.UnselectedValid(card)
    val encoded = Wire.viewFormat.CardViewFormat.encode(view)
    assertEquals(encoded("type").str, "UnselectedValid")
    assertEquals(encoded("card")("type").str, "SkipCard")
  }

  test("CardViewFormat: encode UnselectedNotValid") {
    val card = ReverseCard(Color.Green)
    val view = CardView.UnselectedNotValid(card)
    val encoded = Wire.viewFormat.CardViewFormat.encode(view)
    assertEquals(encoded("type").str, "UnselectedNotValid")
    assertEquals(encoded("card")("type").str, "ReverseCard")
  }

  test("CardViewFormat: decode FaceDown") {
    val json = Obj(
      "type" -> "FaceDown",
      "card" -> Obj("type" -> "NumberCard", "color" -> "Red", "number" -> 5)
    )
    val decoded = Wire.viewFormat.CardViewFormat.decode(json).get
    assertEquals(decoded, CardView.FaceDown)
  }

  test("CardViewFormat: decode Selected") {
    val json = Obj(
      "type" -> "Selected",
      "card" -> Obj("type" -> "NumberCard", "color" -> "Red", "number" -> 5)
    )
    val decoded = Wire.viewFormat.CardViewFormat.decode(json).get
    assertEquals(decoded, CardView.Selected(NumberCard(Color.Red, 5)))
  }

  test("CardViewFormat: round-trip all card view types") {
    val views = List(
      CardView.FaceDown,
      CardView.Selected(NumberCard(Color.Red, 3)),
      CardView.UnselectedValid(SkipCard(Color.Blue)),
      CardView.UnselectedNotValid(DrawTwoCard(Color.Green))
    )
    
    for view <- views do
      assertEquals(roundTrip(Wire.viewFormat.CardViewFormat, view), view)
  }

  // ============ PhaseView Tests ============
  test("PhaseViewWire: encode Waiting") {
    val phase = PhaseView.Waiting
    val encoded = Wire.viewFormat.PhaseViewWire.encode(phase)
    assertEquals(encoded("type").str, "Waiting")
  }

  test("PhaseViewWire: encode ChoosingColor") {
    val phase = PhaseView.ChoosingColor
    val encoded = Wire.viewFormat.PhaseViewWire.encode(phase)
    assertEquals(encoded("type").str, "ChoosingColor")
  }

  test("PhaseViewWire: encode DrawPenalty") {
    val phase = PhaseView.DrawPenalty(2)
    val encoded = Wire.viewFormat.PhaseViewWire.encode(phase)
    assertEquals(encoded("type").str, "DrawPenalty")
    assertEquals(encoded("penalty").num.toInt, 2)
  }

  test("PhaseViewWire: encode SelectingCards") {
    val phase = PhaseView.SelectingCards
    val encoded = Wire.viewFormat.PhaseViewWire.encode(phase)
    assertEquals(encoded("type").str, "SelectingCards")
  }

  test("PhaseViewWire: decode all phase views") {
    assertEquals(Wire.viewFormat.PhaseViewWire.decode(Obj("type" -> "Waiting")).get, PhaseView.Waiting)
    assertEquals(Wire.viewFormat.PhaseViewWire.decode(Obj("type" -> "ChoosingColor")).get, PhaseView.ChoosingColor)
    assertEquals(Wire.viewFormat.PhaseViewWire.decode(Obj("type" -> "DrawPenalty", "penalty" -> 3)).get, PhaseView.DrawPenalty(3))
    assertEquals(Wire.viewFormat.PhaseViewWire.decode(Obj("type" -> "SelectingCards")).get, PhaseView.SelectingCards)
  }

  test("PhaseViewWire: round-trip all phase views") {
    val phases = List(
      PhaseView.Waiting,
      PhaseView.ChoosingColor,
      PhaseView.DrawPenalty(8),
      PhaseView.SelectingCards
    )
    
    for phase <- phases do
      assertEquals(roundTrip(Wire.viewFormat.PhaseViewWire, phase), phase)
  }

  // ============ StateView Tests ============
  test("StateViewWire: encode Playing") {
    val phase = PhaseView.Waiting
    val player = "p1"
    val stack = NumberCard(Color.Blue, 5)
    val stateView = StateView.Playing(phase, player, stack)
    val encoded = Wire.viewFormat.StateViewWire.encode(stateView)
    assertEquals(encoded("type").str, "Playing")
    assertEquals(encoded("currentPlayer").str, "p1")
  }

  test("StateViewWire: encode Finished") {
    val rankings = Map(1 -> Seq("p1"), 2 -> Seq("p2", "p3"))
    val stateView = StateView.Finished(rankings)
    val encoded = Wire.viewFormat.StateViewWire.encode(stateView)
    //println(encoded("rankings").arr)
    assertEquals(encoded("type").str, "Finished")
    assert(encoded("rankings").arr.nonEmpty)
  }

  test("StateViewWire: decode Playing") {
    val json = Obj(
      "type" -> "Playing",
      "phase" -> Obj("type" -> "Waiting"),
      "currentPlayer" -> "p2",
      "stack" -> Obj("type" -> "NumberCard", "color" -> "Red", "number" -> 7)
    )
    val decoded = Wire.viewFormat.StateViewWire.decode(json).get
    assert(decoded.isInstanceOf[StateView.Playing])
    val playing = decoded.asInstanceOf[StateView.Playing]
    assertEquals(playing.currentPlayer, "p2")
    assertEquals(playing.stack, NumberCard(Color.Red, 7))
  }

  test("StateViewWire: decode Finished") {
    val json = Obj(
      "type" -> "Finished",
      "rankings" -> Arr(
            Arr(Num(1), Arr(Str("p1"))),
            Arr(Num(2), Arr(Str("p2")))
        ))
    
    val decoded = Wire.viewFormat.StateViewWire.decode(json).get
    assert(decoded.isInstanceOf[StateView.Finished])
    val finished = decoded.asInstanceOf[StateView.Finished]
    assertEquals(finished.rankings(1), Seq("p1"))
    assertEquals(finished.rankings(2), Seq("p2"))
  }

  test("StateViewWire: round-trip Playing") {
    val stateView = StateView.Playing(
      PhaseView.ChoosingColor,
      "p3",
      WildCard(Color.Red)
    )
    assertEquals(roundTrip(Wire.viewFormat.StateViewWire, stateView), stateView)
  }

  test("StateViewWire: round-trip Finished") {
    val rankings = Map(
      1 -> Seq("p1"),
      2 -> Seq("p2", "p3"),
      3 -> Seq("p4")
    )
    val stateView = StateView.Finished(rankings)
    val result = roundTrip(Wire.viewFormat.StateViewWire, stateView)
    assert(result.isInstanceOf[StateView.Finished])
    val finished = result.asInstanceOf[StateView.Finished]
    assertEquals(finished.rankings, rankings)
  }

  // ============ View Tests ============
  test("ViewFormat: encode complete view") {
    val stateView = StateView.Playing(
      PhaseView.Waiting,
      "p1",
      NumberCard(Color.Blue, 3)
    )
    val myCards = Vector(
      CardView.Selected(NumberCard(Color.Red, 5)),
      CardView.UnselectedValid(SkipCard(Color.Blue))
    )
    val finished = Map("p1" -> false, "p2" -> false)
    val other = Map("p2" -> 2)
    val view = View(stateView, myCards, finished, other)
    val encoded = Wire.viewFormat.encode(view)
    assertEquals(encoded("state")("type").str, "Playing")
    assert(encoded("myCards").arr.nonEmpty)
    assert(encoded("finished").arr.nonEmpty)
    assert(encoded("otherCardCounts").arr.nonEmpty)
  }

  test("ViewFormat: decode complete view") {
    val json = Obj(
      "state" -> Obj(
        "type" -> "Playing",
        "phase" -> Obj("type" -> "Waiting"),
        "currentPlayer" -> "p2",
        "stack" -> Obj("type" -> "SkipCard", "color" -> "Green")
      ),
      "myCards" -> Arr(
        Obj("type" -> "Selected", "card" -> Obj("type" -> "NumberCard", "color" -> "Red", "number" -> 5))
      ),
      "otherCardCounts" -> Arr(Arr(Str("p1"), Num(3)),Arr(Str("p2"), Num(2))),
      "finished" -> Arr(Arr(Str("p1"),Bool(false)),Arr(Str("p2"),Bool(true)))
    )
    val decoded = Wire.viewFormat.decode(json).get
    assert(decoded.state.isInstanceOf[StateView.Playing])
    assertEquals(decoded.myCards.size, 1)
    assertEquals(decoded.finished("p1"), false)
    assertEquals(decoded.finished("p2"), true)
    assertEquals(decoded.otherCardCounts("p1"), 3)
    assertEquals(decoded.otherCardCounts("p2"), 2)
  }

  test("ViewFormat: round-trip complete view") {
    val stateView = StateView.Playing(
      PhaseView.SelectingCards,
      "p1",
      DrawTwoCard(Color.Green)
    )
    val myCards = Vector(
      CardView.Selected(NumberCard(Color.Red, 5)),
      CardView.UnselectedValid(SkipCard(Color.Blue))
    )
    val finished = Map("p1" -> false, "p2" -> false, "p3" -> true)
    val other = Map("p2" -> 4)
    val view = View(stateView, myCards, finished, other)
    val result = roundTrip(Wire.viewFormat, view)
    assertEquals(result.state, stateView)
    assertEquals(result.myCards, myCards)
    assertEquals(result.otherCardCounts, other)
    assertEquals(result.finished, finished)
  }

  test("ViewFormat: round-trip finished game view") {
    val stateView = StateView.Finished(Map(
      1 -> Seq("p1"),
      2 -> Seq("p2"),
      3 -> Seq("p3", "p4")
    ))
    val myCards = Vector.empty[CardView]
    val other = Map("p2" -> 5)
    val finished = Map("p1" -> true, "p2" -> true, "p3" -> true, "p4" -> true)
    val view = View(stateView, myCards, finished, other)
    val result = roundTrip(Wire.viewFormat, view)
    assert(result.state.isInstanceOf[StateView.Finished])
    assertEquals(result.myCards, myCards)
    assertEquals(result.finished, finished)
  }

  // ============ Edge Cases ============
  test("Edge Case => UnoCardWire: handle all number values") {
    for n <- 0 to 9 do
      val card = NumberCard(Color.Red, n)
      assertEquals(roundTrip(Wire.UnoCardWire, card), card)
  }

  test("Edge Case => EventWire: handle large card indices") {
    val event = Event.CardClicked(100)
    assertEquals(roundTrip(Wire.eventFormat, event), event)
  }

  test("Edge Case => ViewFormat: handle empty rankings") {
    val stateView = StateView.Finished(Map.empty[Int, Seq[String]])
    val myCards = Vector.empty[CardView]
    val view = View(stateView, myCards, Map.empty[String, Boolean], Map.empty)
    val result = roundTrip(Wire.viewFormat, view)
    assert(result.state.isInstanceOf[StateView.Finished])
    assertEquals(result.myCards, myCards)
  }

  test("Edge Case => ViewFormat: handle empty SelectingCards") {
    val phase = PhaseView.SelectingCards
    val stateView = StateView.Playing(phase, "p1", NumberCard(Color.Blue, 0))
    val myCards = Vector.empty[CardView]
    val view = View(stateView, myCards, Map("p1" -> false), Map.empty)
    val result = roundTrip(Wire.viewFormat, view)
    assertEquals(result.state.asInstanceOf[StateView.Playing].phase, phase)
    assertEquals(result.myCards, myCards)
}
