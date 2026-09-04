package apps
package ul2025app17

import cs214.webapp.*
import cs214.webapp.client.*
import cs214.webapp.client.graphics.WebClientAppInstance
import org.scalajs.dom
import scalatags.JsDom.all.*
import scala.scalajs.js.annotation.{JSExportTopLevel}

/** The main entry point for the ul2025app17 web client. This object extends
  * WSClientApp, defining the application's unique ID and providing the
  * initialization method for the client instance.
  *
  * It is marked with @JSExportTopLevel to make the Scala.js code callable from
  * the surrounding JavaScript environment.
  */
@JSExportTopLevel("ul2025app17")
object UI extends WSClientApp:
  /** @inheritdoc
    */
  def appId: String = "ul2025app17"

  /** @inheritdoc
    */
  def uiId: String = "html"

  /** Initializes a new instance of the client application.
    *
    * @param userId
    *   The ID of the current user viewing the application.
    * @param sendMessage
    *   A function used to send events (ujson.Value) back to the server.
    * @param target
    *   The DOM element where the application UI should be rendered.
    * @return
    *   A new instance of the UI client logic.
    */
  def init(
      userId: UserId,
      sendMessage: ujson.Value => Unit,
      target: Target
  ): ClientAppInstance =
    UIInstance(userId, sendMessage, target)

/** Represents a single running instance of the UI for the application. This
  * class handles all rendering logic and user interaction, converting
  * server-sent 'View' states into interactive HTML, and translating user
  * actions into server-bound 'Event's.
  *
  * @param userId
  *   The ID of the current user.
  * @param sendMessage
  *   Function to send events to the server.
  * @param target
  *   The target DOM element.
  */
class UIInstance(
    userId: UserId,
    sendMessage: ujson.Value => Unit,
    target: Target
) extends WebClientAppInstance[Event, View](userId, sendMessage, target):

  /** Defines the wire format for communication (Events/Views).
    */
  override val wire = ul2025app17.Wire

  /** The core rendering function. Converts the server-provided immutable 'View'
    * state into the corresponding HTML structure.
    *
    * @param userId
    *   The current user's ID.
    * @param view
    *   The latest game state received from the server.
    * @return
    *   A scalatags Fragment (Frag) representing the UI.
    */
  override def render(userId: UserId, view: View): Frag =

    val allPlayers = view.finished.keys.toSeq
    val playerCount = allPlayers.size

    // --- Validation Checks ---
    if (playerCount < 2 || playerCount > 4) {
      val message =
        if (playerCount < 2)
          "The game requires at least 2 players to start."
        else
          "No more than 4 players are supported in this game."
      return renderErrorScreen(message)
    }

    view.state match

      case StateView.Finished(rankings) =>
        renderRankingsView(rankings)

      case StateView.Playing(phase, currentPlayer, stack) =>

        // --- State Extraction ---
        val currentPlayerId = view.state match
          case StateView.Playing(_, currentPlayer, _) => Some(currentPlayer)
          case _                                      => None

        val topDiscardCard: Option[UnoCard] = view.state match
          case StateView.Playing(_, _, card) => Some(card)
          case _                             => None

        val phaseView = view.state match
          case StateView.Playing(p, _, _) => Some(p)
          case _                          => None

        val drawPenaltyAmount: Int = phaseView match
          case Some(PhaseView.DrawPenalty(penalty)) => penalty
          case _                                    => 0

        val isMyTurn = currentPlayerId.contains(userId)
        val isChoosingColor = phaseView.contains(PhaseView.ChoosingColor)
        val isDrawPenalty = phaseView match {
          case Some(PhaseView.DrawPenalty(_)) => true
          case _                              => false
        }

        // --- Position and Card Data ---
        val relPositions = getRelativePositions(userId, allPlayers)

        val opponentHands = getOpponentCardViews(userId, view)

        // --- Main Board Rendering ---
        frag(
          div(
            cls := "game-board",

            // Player North
            div(
              cls := "player-area player-north",
              div(
                cls := "player-hand-horizontal",
                renderOpponentHandHorizontal(
                  opponentHands.getOrElse("north", Nil)
                )
              ),
              relPositions.get("north").flatten match {
                case Some(uid) =>
                  renderPlayerLabel("label-north", uid, currentPlayerId)
                case None =>
                  h3(
                    cls := "empty-seat-header",
                    "BE THE FIRST TO EMPTY YOUR HAND!"
                  )
              }
            ),

            // Player West
            div(
              cls := "player-area player-west",
              div(
                cls := "hand-label-wrapper wrapper-left-to-center",
                div(
                  cls := "player-hand-vertical",
                  renderOpponentHandVertical(
                    opponentHands.getOrElse("west", Nil)
                  )
                ),
                relPositions.get("west").flatten match {
                  case Some(uid) =>
                    renderPlayerLabel("label-west", uid, currentPlayerId)
                  case None => frag()
                }
              )
            ),

            // Center deck area (Draw Pile, Discard Pile, Popups)
            div(
              cls := "game-center",
              div(
                cls := "stack-area",
                // Draw Pile
                img(
                  src := backImagePath,
                  cls := "deck-card clickable-draw-pile",
                  onclick := { () => sendEvent(Event.Draw) }
                ),
                // Discard Pile (Top Card)
                topDiscardCard match {
                  case Some(card) =>
                    img(src := getCardImagePath(card), cls := "deck-card")
                  case None =>
                    img(src := backImagePath, cls := "deck-card")
                },
                // Popup Overlays
                if (isChoosingColor) {
                  renderColorChooser()
                } else if (isDrawPenalty) {
                  renderDrawPenaltyChooser(
                    topDiscardCard.get,
                    drawPenaltyAmount
                  )
                } else frag()
              )
            ),

            // Player East
            div(
              cls := "player-area player-east",
              div(
                cls := "hand-label-wrapper wrapper-center-to-right",
                relPositions.get("east").flatten match {
                  case Some(uid) =>
                    renderPlayerLabel("label-east", uid, currentPlayerId)
                  case None => frag()
                },
                div(
                  cls := "player-hand-vertical hand-rotated-180",
                  renderOpponentHandVertical(
                    opponentHands.getOrElse("east", Nil)
                  )
                )
              )
            ),

            // Player South (Self)
            div(
              cls := "player-area player-south",
              div(
                cls := "player-self-container",
                // Self-label
                renderPlayerLabel("label-south", userId, currentPlayerId),

                // Self-Hand
                renderMyHand(view.myCards, isMyTurn),

                div(
                  cls := "control-buttons-row",
                  button(
                    cls := "action-btn btn-uno",
                    onclick := { () => sendEvent(Event.UnoButtonPressed) }
                  )("UNO"),
                  button(
                    cls := "action-btn btn-play",
                    onclick := { () => sendEvent(Event.Play) }
                  )("▶"),
                  button(
                    cls := "action-btn btn-pass",
                    onclick := { () => sendEvent(Event.Pass) }
                  )("⏭")
                )
              )
            )
          )
        )

  // =========================================================================
  // I. Card Asset & Styling Helpers
  // =========================================================================

  /** Returns the file path for a specific UnoCard image based on its type and
    * color.
    *
    * @param card
    *   The UnoCard data object.
    * @return
    *   The static image file path (e.g., /static/ul2025app17/red-7.png).
    */
  private def getCardImagePath(card: UnoCard): String = card match
    case NumberCard(color, number) =>
      s"/static/ul2025app17/${color.toString.toLowerCase}-$number.png"
    case SkipCard(color)    => s"/static/ul2025app17/${color.toString.toLowerCase}-stop.png"
    case ReverseCard(color) =>
      s"/static/ul2025app17/${color.toString.toLowerCase}-reverse.png"
    case DrawTwoCard(color) =>
      s"/static/ul2025app17/${color.toString.toLowerCase}-plusTwo.png"
    case WildCard(_)         => "/static/ul2025app17/wild.png"
    case WildDrawFourCard(_) => "/static/ul2025app17/wildPlusFour.png"

  /** Returns the file path for the back of a card. */
  private val backImagePath = "/static/ul2025app17/back.png"

  /** Determines the image source URL for a given CardView state.
    *
    * @param cardView
    *   The CardView object indicating state (FaceDown, Selected, etc.).
    * @return
    *   The URL of the card image.
    */
  private def getCardViewImageSource(cardView: CardView): String =
    cardView match
      case CardView.FaceDown                 => backImagePath
      case CardView.Selected(card)           => getCardImagePath(card)
      case CardView.UnselectedValid(card)    => getCardImagePath(card)
      case CardView.UnselectedNotValid(card) => getCardImagePath(card)

  /** Generates the CSS class string for a given CardView state. Classes control
    * selection highlight, validity indication, and orientation.
    *
    * @param cardView
    *   The CardView object.
    * @param isVertical
    *   True if the card is displayed vertically (side hands).
    * @return
    *   A space-separated string of CSS classes.
    */
  private def getCardViewCssClass(
      cardView: CardView,
      isVertical: Boolean = false
  ): String =
    val baseClass =
      if (isVertical) "card-img card-img-vertical" else "card-img"
    cardView match
      case CardView.Selected(_)           => baseClass + " card-selected"
      case CardView.UnselectedValid(_)    => baseClass + " card-valid"
      case CardView.UnselectedNotValid(_) => baseClass + " card-not-valid"
      case CardView.FaceDown          => baseClass

  // =========================================================================
  // II. View Logic & Game State Helpers
  // =========================================================================

  /** Maps all players' IDs to their relative positions (North, West, East) from
    * the perspective of the current user (always South). The ordering is based
    * on the assumed turn order in the `allPlayers` sequence.
    *
    * @param myId
    *   The current user's ID.
    * @param allPlayers
    *   The sequence of all player IDs in turn order.
    * @return
    *   A map from seat label (String) to the player's UserId (Option).
    */
  private def getRelativePositions(
      myId: UserId,
      allPlayers: Seq[UserId]
  ): Map[String, Option[UserId]] =
    val myIndex = allPlayers.indexOf(myId)
    val count = allPlayers.size

    // Helper to get player at relative index (1 = next player, etc.)
    def getPlayer(offset: Int): UserId = allPlayers((myIndex + offset) % count)

    count match
      case 2 =>
        Map(
          "north" -> Some(getPlayer(1)),
          "west" -> None,
          "east" -> None
        )
      case 3 =>
        Map(
          "west" -> Some(getPlayer(1)), // Next player on left
          "east" -> Some(getPlayer(2)), // Last player on right
          "north" -> None
        )
      case 4 =>
        Map(
          "west" -> Some(getPlayer(1)),
          "north" -> Some(getPlayer(2)),
          "east" -> Some(getPlayer(3))
        )
      case _ => Map.empty

  /** Generates a map of seat labels (North, West, East) to the list of visible
    * cards (always face-down) for the players in those seats.
    *
    * @param myId
    *   The current user's ID.
    * @param view
    *   The current View state containing opponent card counts.
    * @return
    *   Map of seat label (String) to a list of face-down CardView objects.
    */
  private def getOpponentCardViews(
      myId: UserId,
      view: View
  ): Map[String, List[CardView]] =
    val allPlayers = view.finished.keys.toSeq
    val relPositions =
      getRelativePositions(myId, allPlayers)

    val faceDownCard = CardView.FaceDown

    relPositions.map { case (seat, maybeUid) =>
      val cards: List[CardView] = maybeUid match
        case None      => Nil
        case Some(uid) =>
          if uid == myId then view.myCards.toList
          else
            val count = view.otherCardCounts.getOrElse(uid, 0)
            List.fill(count)(faceDownCard)
      seat -> cards
    }.toMap

  // =========================================================================
  // III. UI Component Rendering
  // =========================================================================

  /** Renders a single card in the current player's hand (South). Handles click
    * events and styling based on CardView status.
    *
    * @param cardView
    *   The state of the card (Selected, Valid, NotValid).
    * @param index
    *   The zero-based index of the card in the player's hand.
    * @param isMyTurn
    *   True if it is the current player's turn, enabling clicks.
    * @return
    *   A scalatags Fragment representing the card image.
    */
  private def renderPlayerCard(
      cardView: CardView,
      index: Int,
      isMyTurn: Boolean
  ): Frag =
    val isClickable = isMyTurn && (cardView match
      case CardView.Selected(_)        => true
      case CardView.UnselectedValid(_) => true
      case _                           => false)

    val modifiers: Seq[Modifier] = if (isClickable) {
      Seq(
        onclick := { () =>
          sendEvent(Event.CardClicked(index))
        }
      )
    } else {
      Seq(
        attr("data-unclickable") := ""
      )
    }

    img(
      src := getCardViewImageSource(cardView),
      cls := getCardViewCssClass(cardView, isVertical = false),
      data("card-index") := index.toString,
      modifiers
    )

  /** Renders the current player's entire hand (South). Supports a two-row
    * wrapping layout if the number of cards exceeds the configured
    * `maxCardsPerRow` (9).
    *
    * @param cards
    *   The vector of CardView states for the player's hand.
    * @param isMyTurn
    *   True if rendering the hand on the current player's turn.
    * @return
    *   A Fragment containing the hand wrapper and card rows.
    */
  private def renderMyHand(cards: Vector[CardView], isMyTurn: Boolean): Frag =
    val maxCardsPerRow = 9
    val (firstRowCards, secondRowCards) = cards.splitAt(maxCardsPerRow)

    // Helper function to render a single row of cards
    val renderRow: (Vector[CardView], Int) => Frag = (rowCards, offset) =>
      div(
        cls := "player-hand-row",
        rowCards.zipWithIndex.map { case (cardView, index) =>
          renderPlayerCard(cardView, index + offset, isMyTurn)
        }
      )

    // Main wrapper to stack the rows vertically
    div(
      cls := "player-hand-wrapper player-hand-south-wrapper",
      renderRow(firstRowCards, 0),
      if (secondRowCards.nonEmpty) renderRow(secondRowCards, maxCardsPerRow)
      else frag()
    )

  /** Renders a player's hand for horizontal seats (North). Opponent hands only
    * contain face-down cards.
    *
    * @param cards
    *   The list of face-down CardView objects for the opponent.
    * @return
    *   A Fragment containing the horizontally displayed hand.
    */
  private def renderOpponentHandHorizontal(
      cards: List[CardView]
  ): Frag =
    div(
      cls := "player-hand-horizontal player-hand-north-wrapping hand-rotated-180",
      for cardView <- cards
      yield img(
        src := getCardViewImageSource(cardView),
        cls := getCardViewCssClass(cardView, isVertical = false)
      )
    )

  /** Renders a player's hand for vertical seats (West/East).
    *
    * @param cards
    *   The list of face-down CardView objects for the opponent.
    * @return
    *   A Fragment containing the vertically displayed hand.
    */
  private def renderOpponentHandVertical(cards: List[CardView]): Frag =
    val maxCardsPerColumn = 7
    val (firstColumnCards, secondColumnCards) =
      cards.toVector.splitAt(maxCardsPerColumn)

    // Helper to render a single column of vertical cards
    val renderColumn: Vector[CardView] => Frag = columnCards =>
      div(
        cls := "player-hand-column",
        for cardView <- columnCards
        yield img(
          src := getCardViewImageSource(cardView),
          cls := getCardViewCssClass(cardView, isVertical = true)
        )
      )

    div(
      cls := "player-hand-multi-column",
      if (secondColumnCards.nonEmpty) renderColumn(secondColumnCards)
      else frag(),
      renderColumn(firstColumnCards)
    )

  /** Renders the player label (user ID), highlighting the current player.
    *
    * @param seatLabelClass
    *   CSS class specific to the seat position (e.g., 'label-north').
    * @param userId
    *   The ID of the player being labeled.
    * @param currentPlayerId
    *   The ID of the player whose turn it is.
    * @return
    *   A Fragment containing the styled player label.
    */
  private def renderPlayerLabel(
      seatLabelClass: String,
      userId: UserId,
      currentPlayerId: Option[UserId]
  ): Frag =
    val isCurrent = currentPlayerId.contains(userId)
    val classes =
      s"player-label $seatLabelClass ${
          if (isCurrent) "current-player-label" else ""
        }"
    p(userId, cls := classes)

  /** Renders the modal overlay for choosing a new color. Each button sends an
    * `Event.ColorChosen` event to the server.
    *
    * @return
    *   A Fragment containing the color chooser modal.
    */
  private def renderColorChooser(): Frag =
    val colors = Color.all.filter(_ != Color.TBD)
    div(
      cls := "color-chooser-container",
      h3(cls := "color-chooser-title", "Choose a Color"),
      div(
        cls := "color-buttons-row",
        colors.map { color =>
          button(
            cls := s"color-button color-${color.toString.toLowerCase}",
            onclick := { () => sendEvent(Event.ColorChosen(color)) },
            span(color.toString)
          )
        }
      )
    )

  /** Renders the modal overlay prompting the player to accept the accumulated
    * draw penalty. The 'Accept' button sends an `Event.AcceptPenalty` event.
    *
    * @param topCard
    *   The card on the discard pile (used to determine the message).
    * @param penalty
    *   The total number of cards the player must draw (e.g., 2, 4, 6, ...).
    * @return
    *   A Fragment containing the draw penalty modal.
    */
  private def renderDrawPenaltyChooser(topCard: UnoCard, penalty: Int): Frag =
    div(
      cls := "draw-penalty-chooser-container",
      p(
        cls := "draw-penalty-chooser-message",
        topCard match
          case DrawTwoCard(c) =>
            s"You must draw + $penalty cards — unless you stack another +2/+4."
          case WildDrawFourCard(c) =>
            s"You must draw + $penalty cards — unless you stack another +4."
          case _ => ""
      ),
      div(
        cls := "penalty-buttons-row",
        button(
          cls := "penalty-button btn-accept",
          onclick := { () => sendEvent(Event.AcceptPenalty) }
        )("Accept")
      )
    )

  /** Renders the final screen displaying player rankings.
    *
    * @param rankings
    *   A Map where keys are the rank (1, 2, 3, ...) and values are a sequence
    *   of UserIds in that rank.
    * @return
    *   A Fragment containing the rankings table.
    */
  private def renderRankingsView(rankings: Map[Int, Seq[UserId]]): Frag =
    div(
      cls := "rankings-container",
      // Uno Banner
      div(cls := "uno-banner", "Game Over!"),
      // Rankings Box
      div(
        cls := "ranking-box",
        // Title
        div(cls := "ranking-title", "Rankings"),
        // A row in the grid for each ranking position
        for (rank, players) <- rankings.toSeq.sortBy(_._1)
        yield div(
          cls := "rank-row",
          div(cls := "rank", rankSuffix(rank)),
          div(cls := "ranking-players", players.mkString("|"))
        )
      )
    )

  /** Helper function that returns the ordinal suffix of the given rank.
    * @param rank
    *   The integer rank.
    * @return
    *   The rank string with its suffix (e.g., 1 -> "1st", 4 -> "4th").
    */
  private def rankSuffix(rank: Int): String =
    if (rank == 1) "1st"
    else if (rank == 2) "2nd"
    else if (rank == 3) "3rd"
    else s"${rank}th"

  /** Renders a persistent, full-screen error overlay for invalid game states
    * (e.g., incorrect player count during game initialization).
    *
    * @param message
    *   The specific error message to display to the user.
    * @return
    *   A Fragment containing the error screen.
    */
  private def renderErrorScreen(message: String): Frag =
    div(
      cls := "error-screen-container",
      h1(cls := "error-title", "Game Setup Error"),
      p(cls := "error-message", message)
    )

  /** Provides custom CSS styling for the application. This string is
    * concatenated with the base CSS from the WebClientAppInstance.
    *
    * The CSS is structured into 11 labeled sections for clarity, handling
    * layout, card styling, interaction effects, player labels, and modal
    * overlays.
    */
  override def css: String = super.css +
    """
      |/* ==================================================================== */
      |/* 1. Layout Grid & Player Areas */
      |/* ==================================================================== */
      | 
      | .game-board {
      |   display: grid;
      |   grid-template-areas:
      |      "north north north"
      |      "west center east"
      |      "south south south";
      |   grid-template-columns: grid-template-columns: minmax(200px, 1fr) 300px minmax(200px, 1fr);
      |   grid-template-rows: 1fr 300px 1fr;
      |
      |   justify-items: center;
      |   align-items: center;
      |   gap: 1rem;
      | }
      |
      | .player-area {display: flex; align-items: center; justify-content: center;}
      | 
      | .player-north { grid-area: north; flex-direction: column; }
      | .player-south { grid-area: south; flex-direction: column; }
      | .player-west  { grid-area: west; justify-content: flex-end;}
      | .player-east  { grid-area: east; justify-content: flex-start;}
      |
      | .hand-label-wrapper {display: flex; align-items: center; gap: 1.5rem;}
      | 
      | .player-west,
      | .player-east {
      |   min-width: 180px;
      |   min-height: 260px;
      |   display: flex;
      |   justify-content: center;
      |   align-items: center;
      | } 
      |
      |
      |/* ==================================================================== */
      |/* 2. Player Labels */
      |/* ==================================================================== */
      | 
      | .player-label { 
      |   font-weight: bold;
      |   font-size: 1.2rem; 
      |   padding: 5px 10px;
      |   border: 2px solid transparent; 
      |  }
      |
      | .label-north { margin-top: 1rem; }
      | .label-south { margin-bottom: 1rem; }
      | .label-west { transform: rotate(90deg); margin-left: -0.5rem;}
      | .label-east { transform: rotate(-90deg); margin-right: -0.5rem;}
      |
      | .current-player-label {
      |    color: #141414ff;
      |    border: 2px solid #ffd700;
      |    background-color: #ffd900bb;
      |    box-shadow: 0 0 10px #ff2c07ff;
      | }
      |
      |
      |/* ==================================================================== */
      |/* 3. Card Base Styling & Dimensions */
      |/* ==================================================================== */
      |
      | .card-img { 
      |    width: 60px; 
      |    height: auto; 
      |    border-radius: 0.25rem; 
      |    box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.3);
      |    transition: transform 0.2s, box-shadow 0.2s, opacity 0.2s; 
      | }
      | 
      | .card-img-vertical {transform: rotate(90deg); margin-top: -15px; margin-bottom: -15px;}
      |
      | .hand-rotated-180 {transform: rotate(180deg);}
      |
      | .card-valid {cursor: pointer; border: 2px solid transparent;}
      | .card-valid:hover {ransform: translateY(-10px); box-shadow: 0 8px 15px rgba(0, 255, 0, 0.7);}
      |
      | .card-selected {
      |    cursor: pointer;
      |    transform: translateY(-20px);
      |    border: 3px solid #ffd700;
      |    box-shadow: 0 10px 20px rgba(255, 215, 0, 0.9);
      | }
      | .card-selected:hover {transform: translateY(-25px);}
      |
      | .card-not-valid {opacity: 0.6; cursor: not-allowed; filter: grayscale(50%);}
      |
      |
      |/* ==================================================================== */
      |/* 4. Hand Layouts */
      |/* ==================================================================== */
      | 
      | .player-hand-horizontal {
      |   display: flex;
      |   gap: 0.5rem;
      |   justify-content: center;
      |   margin: 0;
      | }
      |
      | .player-hand-north-wrapping { flex-wrap: wrap; }
      |
      | .player-hand-south-wrapper {
      |   display: flex;
      |   flex-direction: column;
      |   align-items: center;
      |   gap: 0.5rem;
      |   margin-top: 10px; 
      |   margin-bottom: 20px;
      | }
      |
      | .player-hand-row {
      |   display: flex;
      |   gap: 0.5rem;
      |   justify-content: center;
      |   margin: 0;
      | }
      |
      | .player-hand-multi-column {
      |   display: flex;
      |   flex-direction: row;
      |   gap: 0.5rem;
      |    justify-content: center;
      |   align-items: center;
      |   padding: 10px 0;
      | }
      |
      | .player-hand-column {
      |   display: flex;
      |   flex-direction: column;
      |   justify-content: center;
      |   gap: 0.5rem;
      | }
      |
      | .player-hand-vertical {
      |   display: flex;
      |   flex-direction: column;
      |   justify-content: center;
      |   gap: 0.5rem;
      | }
      |
      |
      |/* ==================================================================== */
      |/* 5. Center Deck */
      |/* ==================================================================== */
      | .game-center { 
      |    grid-area: center; 
      |    display: flex; 
      |    justify-content: center; 
      |    align-items: center; 
      | }
      |
      | .stack-area { 
      |    background-color: #f00202ff;
      |    display: flex; 
      |    justify-content: center; 
      |    align-items: center; 
      |    gap: 1rem; 
      |    width: 200px; 
      |    height: 150px; 
      |    border-radius: 0.5rem;
      |    border: 3px solid #333;
      |    position: relative;
      | }
      | 
      | .deck-card { width: 77px; height: auto; border-radius: 0.25rem; }
      | .clickable-draw-pile {cursor: pointer; transition: transform 0.2s, box-shadow 0.2s;}
      |  .clickable-draw-pile:hover {
      |    transform: scale(1.05); 
      |    box-shadow: 0 0 10px rgba(0, 0, 255, 0.7);
      |  }
      |
      |
      |/* ==================================================================== */
      |/* 6. Player Self (South) Controls */
      |/* ==================================================================== */
      | .player-self-container {
      |   display: flex;
      |   flex-direction: column;
      |   align-items: center;
      |   gap: 5px;
      | }
      |
      | .action-btn {
      |   position: static;
      |   border: 2px solid #333;
      |   border-radius: 50%;
      |   width: 50px;
      |   height: 50px;
      |   font-weight: bold;
      |   font-size: 1rem;
      |   cursor: pointer;
      |   display: flex;
      |   justify-content: center;
      |   align-items: center;
      |   color: white;
      |   box-shadow: 0 4px 6px rgba(0, 0, 0, 0.4);
      |   transition: all 0.2s ease;
      | }
      | 
      | .action-btn:hover { transform: scale(1.05); }
      |
      | .btn-uno {
      |   background-color: #ff0000; /* Red */
      |   top: -50px; 
      |   left: 2.6rem;
      |   font-size: 1.2rem;
      |   color: yellow;
      | }
      | .btn-play {
      |   background-color: #008000; /* Green */
      |   top: -50px;
      |   right: 5rem;
      |   color: white;
      | }
      | .btn-pass {
      |   background-color: #0000ff; /* Blue */
      |   top: -50px;
      |   right: 1.3rem;
      |   color: white;
      | }
      |
      |
      |/* ==================================================================== */
      |/* 7. Color Chooser Overlay */
      |/* ==================================================================== */
      |
      |.color-chooser-container {
      |    position: fixed;
      |    top: 50%;
      |    left: 50%;
      |    transform: translate(-50%, -50%);
      |    z-index: 1000; /* Ensure it's on top of everything */
      |    background: rgba(0, 0, 0, 0.9);
      |    padding: 30px;
      |    border-radius: 15px;
      |    box-shadow: 0 0 20px rgba(255, 255, 255, 0.5);
      |    text-align: center;
      |}
      |
      | .color-chooser-title {
      |   color: white;
      |   margin-bottom: 20px;
      |   font-size: 1.5em;
      | }
      |
      | .color-buttons-row {
      |   display: flex;
      |   gap: 15px;
      |   justify-content: center;
      | }
      |
      | .color-button {
      |   width: 80px;
      |   height: 80px;
      |   border: 3px solid white;
      |   border-radius: 50%;
      |   cursor: pointer;
      |   font-size: 1em;
      |   font-weight: bold;
      |   color: white;
      |   transition: transform 0.1s ease-in-out, border-width 0.1s ease-in-out;
      | }
      |
      | .color-button:hover {
      |   transform: scale(1.1);
      |   border-width: 8px; /* Make the white border thicker on hover */
      | }
      |
      | /* Specific Color Styles */
      | .color-red {
      |   background-color: #d82c2a; /* Standard UNO Red */
      | }
      | .color-blue {
      |   background-color: #2b74b8; /* Standard UNO Blue */
      | }
      | .color-green {
      |   background-color: #49a84d; /* Standard UNO Green */
      | }
      | .color-yellow {
      |   background-color: #ffc900; /* Standard UNO Yellow */
      | }
      |
      |
      |/* ==================================================================== */
      |/* 8. Draw Penalty Overlay */
      |/* ==================================================================== */
      |
      | .draw-penalty-chooser-container {
      |    position: absolute;
      |    top: 0%;
      |    left: 50%;
      |    transform: translate(-50%, -10px);
      |    
      |    z-index: 1000;
      |    width: 250px;
      |    
      |    background: rgba(0, 0, 0, 0.9);
      |    padding: 15px;
      |    border-radius: 15px;
      |    box-shadow: 0 0 20px rgba(255, 0, 0, 0.5); 
      |    text-align: center;
      | }
      |
      | .draw-penalty-chooser-message {
      |    color: #ccc;
      |    margin-bottom: 20px;
      |    font-size: 1.1em;
      | }
      |
      | .penalty-buttons-row {
      |    display: flex;
      |    gap: 20px;
      |    justify-content: center;
      | }
      |
      | .penalty-button {
      |    padding: 8px 15px;
      |    border: none;
      |    border-radius: 8px;
      |    font-size: 1em;
      |    font-weight: bold;
      |    cursor: pointer;
      |    transition: background-color 0.2s, transform 0.2s;
      | }
      |
      | .btn-accept {
      |    background-color: #d82c2a; /* Red for accepting draw */
      |    color: white;
      | }
      |
      | .btn-accept:hover {
      |    background-color: #a82020;
      |    transform: scale(1.05);
      | }
      |
      |/* ==================================================================== */
      |/* 9. Error Screen */
      |/* ==================================================================== */
      |
      | .error-screen-container {
      |   position: fixed;
      |   top: 0;
      |   left: 0;
      |   width: 100%;
      |   height: 100%;
      |   background: rgba(0, 0, 0, 0.9);
      |   color: white;
      |   display: flex;
      |   flex-direction: column;
      |   justify-content: center;
      |   align-items: center;
      |   z-index: 1000;
      |   text-align: center;
      | }
      |
      | .error-title {
      |   color: #ff4444; /* Red color */
      |   font-size: 2.5em;
      |   margin-bottom: 0.5em;
      | }
      |
      | .error-message {
      |   font-size: 1.5em;
      |   font-weight: bold;
      | }
      |
      | .control-buttons-row {
      |   display: flex;
      |   flex-direction: row;
      |   gap: 2rem; 
      |   justify-content: center;
      | }
      |
      |
      |/* ==================================================================== */
      |/* 10. Control Buttons Row */
      |/* ==================================================================== */
      |
      | .rankings-container {
      |   display: flex;
      |   flex-direction: column;
      |   align-items: center;
      |   gap: 2rem;
      |   margin-top: 3rem;
      |   font-family: sans-serif;
      | }
      |
      | .uno-banner {
      |    background: #E62127;         
      |    color: white;
      |    font-size: 3rem;
      |    font-weight: bold;
      |    padding: 1rem 4rem;
      |    border-radius: 0.5rem;
      |    text-align: center;
      | }
      |
      | .ranking-box {
      |    width: 60%;
      |    border: 3px solid #E62127;   
      |    border-radius: 0.5rem;
      |    display: flex;
      |    flex-direction: column;
      |    overflow: hidden;
      | }
      |
      |  .ranking-title {
      |    background: #f5d000ab;
      |    color: #E62127;
      |    font-size: 1.8rem;
      |    font-weight: bold;
      |    text-align: center;
      |    padding: 1rem;
      |    border-bottom: 2px solid #E62127;
      |  }
      |
      |  .rank-row {
      |    display: grid;
      |    grid-template-columns: 1fr 3fr;
      |    align-items: center;
      |    padding: 1rem 2rem;
      |    border-bottom: 1px solid #028192ff;
      |    font-size: 1.3rem;
      |  }
      |
      |  .ranking-box .rank-row:last-child {
      |    border-bottom: none;
      |  }
      |
      |  .rank {
      |    font-weight: bold;
      |    color: black;
      |    text-align: left;
      |  }
      |
      |  .ranking-players {
      |    color: #028192ff;
      |    font-weight: 600;
      |    text-align: center;
      |  }
      |
      |/* ==================================================================== */
      |/* 11. Custom Header for Empty Seats */
      |/* ==================================================================== */
      |
      |.empty-seat-header {
      |  color: #ffffffff;
      |  background: rgba(0, 0, 0, 1);
      |  padding: 10px 20px;
      |  border-radius: 8px;
      |  font-size: 1.4rem;
      |  font-weight: bold;
      |  text-align: center;
      |  margin-top: 1rem;
      |  border: 2px solid #ff0000ff;
      |  box-shadow: 0 0 10px rgba(255, 0, 0, 0.85);
      |}
      """.stripMargin
