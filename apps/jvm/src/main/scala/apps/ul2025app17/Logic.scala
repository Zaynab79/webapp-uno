package apps
package ul2025app17

import cs214.webapp.*
import cs214.webapp.server.{StateMachine}
import ujson.Value
import apps.ul2025app17.Event.*
import scala.util.Try
import apps.ul2025app17.{Color, DrawTwoCard, NumberCard, ReverseCard, SkipCard, State, UnoCard, View, WildCard, WildDrawFourCard, MStack, Phase}
import apps.ul2025app17.Logic.drawPile
import apps.ul2025app17.Phase.Playing

  // =========================================================================
  // I. LOGIC UTILITY OBJECT
  // =========================================================================
  /** 
  * Provides methods for deck generation, card validation, and helper functions related to UNO rules.
  */
object Logic:

  /** 
      * Generates all numbered and action cards of a given color.
      * @param color the color of cards to generate
      * @return a Vector of UnoCard with numbers 0-9, Skip, Reverse, DrawTwo cards
      */
    def coloredCards(color : Color) : Vector[UnoCard] =
        require(color != Color.TBD, "Cannot generate colored cards with TBD color")
        val numbers = (0 to 9).map(n => NumberCard(color,n)).toVector
        val skips = Vector.fill(2)(SkipCard(color))
        val reverses = Vector.fill(2)(ReverseCard(color))
        val drawTwos = Vector.fill(2)(DrawTwoCard(color))
        numbers ++ numbers ++ skips ++ reverses ++ drawTwos

    /** Wild and Wild Draw Four cards (color TBD) */
    val wilds : Vector[UnoCard] = 
        Vector.fill(4)(WildCard(Color.TBD)) ++ Vector.fill(4)(WildDrawFourCard(Color.TBD))

    /** Full UNO deck, including colored cards and wilds */
    val deck : Vector[UnoCard] = 
        Color.all.filter(_!=Color.TBD).flatMap(coloredCards) ++ wilds
    
    /** 
      * Generates a shuffled draw pile (stack) of UNO cards.
      * @return a mutable stack (MStack) of UnoCard in random order
      */
    def drawPile : MStack[UnoCard] = 
      val stack : MStack[UnoCard] = MStack()
      for card <- scala.util.Random.shuffle(deck) do 
        stack.push(card)
      stack
    
    /** Checks if a card is a wild card (Wild or Wild Draw Four) */
    def isWild(card: UnoCard): Boolean = card match
      case WildCard(_) | WildDrawFourCard(_) => true
      case _ => false
    
    /** 
      * Determines if a selected card can be played after the previous card according to UNO rules.
      * Handles penalty situations for Draw Two and Wild Draw Four.
      * @param previous the card currently on top of the pile
      * @param selected the card the player wants to play
      * @param hasPenalty whether a penalty is active
      * @return true if the play is valid
      */
    def isValid(previous: UnoCard, selected: UnoCard, hasPenalty : Boolean): Boolean = 
      if hasPenalty then 
        previous match
          case DrawTwoCard(c) => 
            selected match
              case DrawTwoCard(c) => true
              case WildDrawFourCard(c) => true
              case _ => false 
          case WildDrawFourCard(c) => 
            selected match
              case WildDrawFourCard(c) => true
              case _ => false 
          case _ => false
      else 
        isWild(selected) || previous.color == selected.color || previous.value == selected.value 



// =========================================================================
// II. STATE MACHINE
// =========================================================================

/** 
  * UNO Game logic implemented as a state machine.
  * Handles game initialization, transitions, and projections for clients.
  */
class Logic extends StateMachine[Event, State, View]:

  // Application metadata
  val appInfo: AppInfo = AppInfo(
    id = "ul2025app17",
    name = "UNO Card Game",
    description = "Play the classic UNO card game" + 
    "match colors or numbers, play action cards and be the first to empty your hand",
    year = 2025
  )

  override val wire = ul2025app17.Wire

  private val END_GAME_PAUSE_MS = 2500
  private val INITIAL_HAND_SIZE = 7
  

  // =========================================================================
  // III. INIT FUNCTION
  // =========================================================================

  /** 
    * Initializes the game state for all connected clients.
    * Deals with the initial hands, sets the top card, and creates the draw pile.
    * @param clients sequence of connected user IDs
    * @return initial State of the game
    */
  override def init(clients: Seq[UserId]): State =
    {require(clients.nonEmpty, "There must be at least one player")
    val deck = Logic.drawPile
    val initialHands : Map[UserId, Hand] = clients.map(user => 
      var hand : List[UnoCard] = List.empty
      for i <- (0 until INITIAL_HAND_SIZE) do 
        hand = deck.pop() :: hand
      user -> hand).toMap

    // Draw initial card for play pile, ensuring it's a number card  
    var toPutBack = List[UnoCard]()
    var initialCard : Option[UnoCard] = None
    while (initialCard match
      case Some(value) => false
      case None => true)
     do 
      val c = deck.pop()
      c match
        case NumberCard(color, n) => initialCard = Some(c)
        case _ => toPutBack = c :: toPutBack
    val initialStack = (initialCard.get, 0)
    toPutBack.foreach(deck.push)
    
    State(
      players = clients,
      turnOrder = clients.toVector,
      hands = initialHands,
      selected = clients.map(u => u -> List.empty[Int]).toMap,
      drawPile = deck,
      topCard = initialCard.get,
      penalty = 0,
      phase = Phase.Playing(TurnState(clients.head, false, false, false)),
      rankings = Map (-1 -> clients)
      )
    
    } ensuring(s => s.hands.values.forall(_.nonEmpty) && s.topCard.isInstanceOf[NumberCard], "All players should start with initial hands and the top Card should be a number card")


  // =========================================================================
  // IV. TRANSITION FUNCTION
  // =========================================================================

  /**
    * Handles a game event from a player, producing resulting actions.
    * Includes helper functions for validating moves, updating turns, ranking, and handling penalties.
    */
  override def transition(state: State)(userId: UserId, event: Event): Try[Seq[Action[State]]] = Try : 
    import Action.* 

    // ===================== Helper functions ======================
      
    /** Checks if a sequence of selected cards is valid for play */
    def isValidSequence(selected: List[Int]): Boolean = 
      val hand = state.hands(userId)
      if selected.isEmpty then 
        false 
      else 
        val sequence = selected.map(hand(_))
        val first = hand(selected.last)
        Logic.isValid(state.topCard, first, state.penalty>0) && sequence.forall(_.value == first.value)
      

    /** Finds the next player to play given the current player and number of skips */
    def findNextToPlay(currentPlayer: UserId, reordered: Vector[UserId], skips: Int): UserId = 
      require(reordered.nonEmpty, "Turn order cannot be empty")
      val effectiveSkips = skips % reordered.length
      val index = (reordered.indexOf(currentPlayer) + 1 + effectiveSkips) % reordered.length
      reordered(index)  

    /** Updates turn order considering reverses and finished players */
    def updateTurnOrder(reverses: Int, nextHands : Map[UserId, Hand ], current : UserId): Vector[UserId] = 
      val reordered = if reverses%2==0 then state.turnOrder else state.turnOrder.reverse
      val finishedPlayers = nextHands.collect{case (u, h) if h.isEmpty && u != current=> u}.toList
      val newTurnOrder = reordered.filter(!finishedPlayers.contains(_)) // turnOrder containing only right players
      newTurnOrder

      
    // Helper function that determines the current dynamic ranking
    def rankings(previousRanking: Map[Int, Seq[UserId]], newHands : Map[UserId, Hand]): Map[Int, Seq[UserId]] = {
      val finishedPlayers = newHands.collect{case (u, h) if h.isEmpty => u}.toSet
      val orderedFinishers = previousRanking.toList.sortBy(_._1).flatMap{case (_, users) => users.filter(finishedPlayers.contains)} // Produce a list of ordered finishers
      val lockedSet = orderedFinishers.toSet
      
      val remainingPlayers = newHands.keySet.diff(lockedSet).toList
      val rankedByCards = remainingPlayers.groupBy(u => newHands(u).size).toSeq.sortBy(_._1).map(_._2) // Produced a list of ordered remaining players

      val lockedRanking = orderedFinishers.zipWithIndex.map{case (user, idx) => (idx + 1, Seq(user))} // ranking for finishers

      val newStartRank = lockedRanking.size + 1
      val remainingRanking = rankedByCards.zipWithIndex.map{case (group, idx) => (newStartRank + idx, group)} // ranking for remaining player

      (lockedRanking ++ remainingRanking).toMap // final ranking
    } ensuring(r => r.keySet.subsetOf((1 to state.players.size).toSet), "All ranks must be within valid range")


    /** Checks if a hand has any valid cards to play */
    def canPlay(hand: Hand): Boolean = 
      if hand.isEmpty then false
      else
        val hasPenalty = state.penalty > 0
        hand.exists(card => Logic.isValid(state.topCard, card, hasPenalty))

    /** Draws penalty cards from the draw pile */
    def receivePenalty(hand: Hand, penalty: Int): Hand = 
      if penalty <= 0 || state.drawPile.isEmpty then hand else state.drawPile.pop() :: receivePenalty(hand, penalty -1) // drawPile automatically updated through internal var l

    /** Checks if the game has reached its end */
    def reachedEndgame(hands : Map[UserId, Hand], stack : (UnoCard, Int), turnOrder : Vector[UserId]): Boolean =
      turnOrder.size == 1 || (state.drawPile.isEmpty && state.turnOrder.map(state.hands(_)).filter(!_.isEmpty).forall(h => !canPlay(h)))

    /** Rotates hands when a NumberCard(0) is played */
    def rotateHands(hands : Map[UserId, Hand], turnOrder : Vector[UserId], zeroes : Int) : Map[UserId, Hand] = 
      if turnOrder.size <= 1 then hands 
      else
        val effectiveRotations = zeroes % turnOrder.size
        val rotated = turnOrder.indices.map { idx =>
          val currentPlayer = turnOrder(idx)
          val nextIdx = (idx + effectiveRotations) % turnOrder.length
          val nextPlayer = turnOrder(nextIdx)
          nextPlayer -> hands(currentPlayer) // Next player receives current player's hand
        }.toMap
      
        // Merge: empty hands stay empty, non-empty hands get rotated
        hands ++ rotated
      


  // ====================== State machine transitions ========================
    state.phase match 
      case Phase.Done => Seq(Render(state)) // Game finished. Render end display
  
      case Playing(TurnState(currentPlayer, pressedUno, mustChoseColor, haveDrawnCard)) => 
        require(state.turnOrder.contains(currentPlayer), s"Current player $currentPlayer must be in turn order")
        if userId == currentPlayer then
          if mustChoseColor then 
            event match 
                case ColorChosen(color) => 
                  val determinedColorCard = state.topCard match
                                    case WildCard(c) => WildCard(color)
                                    case WildDrawFourCard(c) => WildDrawFourCard(color)
                                    case _ => throw IllegalMoveException("You cannot chose a color for a non wild card") // Supposedly unreachable case
                  val finalTurnOrder =  
                    if state.hands(currentPlayer).isEmpty then 
                      state.turnOrder.filter(_ != currentPlayer)
                    else 
                      state.turnOrder
                  val state2 = state.copy(topCard = determinedColorCard,
                                turnOrder = finalTurnOrder,
                                phase = Playing(TurnState(findNextToPlay(currentPlayer, state.turnOrder, 0), false, false, false)))
                  if reachedEndgame(state2.hands, (state2.topCard, state2.penalty), finalTurnOrder) then 
                    Seq(Render(state2.copy(phase = Phase.Done)))
                  else
                    Seq(Render(state2))             
                case _ => Seq()
          else 
              event match
                // =================== Same in both ============
                case CardClicked(cardIndex) => 
                  val hand = state.hands(userId)
                  val selectedCard = hand(cardIndex)
                  val selUser = state.selected.getOrElse(userId, List.empty)
                  val previousCard = if selUser.isEmpty then state.topCard else hand(selUser.last)
                  
                  if selUser.contains(cardIndex) then // the player is unselecting a card he wanted to play
                    val state2 = state.copy(selected = state.selected + (userId -> selUser.filterNot(_ == cardIndex)))
                    Seq(Render(state2))
                  
                  else 
                    if selUser.isEmpty then 
                      if Logic.isValid(previousCard, selectedCard, state.penalty>0) then 
                        val state2 = state.copy(selected = state.selected + (userId -> (cardIndex +: selUser)))
                        Seq(Render(state2))
                      else 
                        Seq(Render(state))
                    else 
                      if isValidSequence(cardIndex +: selUser) then
                        val state2 = state.copy(selected = state.selected + (userId -> (cardIndex +: selUser)))
                        Seq(Render(state2))
                      else 
                        Seq(Render(state))

                case UnoButtonPressed =>
                  val state2 = state.copy(phase = Playing(TurnState(currentPlayer, true, false, haveDrawnCard)))
                      Seq(Render(state2)) // display for the uno
                
                // =================== From draw penalty ============

                case AcceptPenalty => 
                  if state.penalty > 0 then
                    val state2 = state.copy(hands = state.hands + (userId -> receivePenalty(state.hands(userId), state.penalty)),
                                            penalty = 0,
                                            phase = Playing(TurnState(findNextToPlay(currentPlayer, state.turnOrder, 0), false, false, false)))
                    Seq(Render(state2))
                  else Seq()

                // ===================== From playing only ===============
                case Pass => 
                  if state.penalty == 0 then
                    if (haveDrawnCard || state.drawPile.isEmpty) then 
                      val state2 = state.copy(phase = Playing(TurnState(findNextToPlay(currentPlayer, state.turnOrder, 0), false, false, false)))
                          Seq(Render(state2)) 
                    else throw IllegalMoveException("You have to draw a card first !")
                  else Seq()

                case Draw => 
                  if state.penalty == 0 && !haveDrawnCard then
                    val newHand = receivePenalty(state.hands(userId), 1)
                    require(newHand.size >= state.hands(userId).size, "Hand size must increase after drawing")
                    if canPlay(newHand) then
                      // Can play after having drawn
                      val state2 = state.copy(
                        hands = state.hands + (userId -> newHand),
                        selected = state.selected + (userId -> List.empty),
                        phase = Playing(TurnState(currentPlayer, false, false, true)))
                      Seq(Render(state2))
                    else
                      // Still can't play, goes to next player
                      val state2 = state.copy(
                        hands = state.hands + (userId -> newHand),
                        selected = state.selected + (userId -> List.empty),
                        phase = Playing(TurnState(findNextToPlay(currentPlayer, state.turnOrder, 0), false, false, false)))
                      Seq(Render(state2))
                  else Seq()

                // ========================= Mix both =============

                case Play =>                                 
                  val hand = state.hands(userId)
                  val userSel = state.selected.getOrElse(userId, List.empty)
                  if isValidSequence(userSel) then 
                    val hand2 = hand.zipWithIndex.collect{case (e, i) if !userSel.contains(i) => e} // cards that we are not playing remain in our hand
                    val isUno = hand2.length == 1
                    val hand3 = if isUno && !pressedUno then receivePenalty(hand2, 2) else hand2 // Did the user say uno ?
                    val selUser = state.selected.getOrElse(userId, List.empty)
                    val reverses = selUser.map(hand(_)).collect{case ReverseCard(c) => 1}.sum
                    val skips = selUser.map(hand(_)).collect{case SkipCard(c) => 1}.sum // These 2 must be = 0 in draw penalty case !
                    val zeroes = selUser.map(hand(_)).collect{case NumberCard(_,0) => 1}.sum
                    val topCard = hand(userSel.head)
                    val newHands = state.hands + (userId -> hand3)
                    val reordered = updateTurnOrder(reverses, newHands, userId)
                    require(reverses == 0 || skips == 0)
                    val effectiveSkips = if reordered.size == 2 && (reverses !=0 || skips!= 0) then 1 else skips
                    val finalTurnOrder = if hand3.isEmpty then reordered.filter(_ != userId) else reordered
                    
                    
                    topCard match
                      case DrawTwoCard(c) => 
                        val drawTwos = state.selected(userId).map(hand(_)).collect{case DrawTwoCard(c) => 2}.sum
                        val newStack = (topCard, state.penalty + drawTwos)
                        val state2 = state.copy(hands = newHands,
                                                selected = state.selected + (userId -> List.empty),
                                                topCard = newStack._1,
                                                penalty = newStack._2,
                                                phase = Playing(TurnState(findNextToPlay(userId, reordered, effectiveSkips), false, false, false)), 
                                                turnOrder = finalTurnOrder,
                                                rankings = rankings(state.rankings, newHands))
                        if reachedEndgame(newHands, newStack, finalTurnOrder) then 
                          if isUno && pressedUno then 
                            Seq(Alert(s"${userId} : Uno!"), Render(state2.copy(phase = Phase.Done)))
                          else 
                            Seq(Render(state2.copy(phase = Phase.Done)))
                        else 
                            if isUno && pressedUno then 
                              Seq(Alert(s"${userId} : Uno!"), Render(state2))
                            else 
                              Seq(Render(state2))
                      //=================================
                      case WildDrawFourCard(c) => 
                        val wildDrawFours = state.selected(currentPlayer).map(hand(_)).collect{case WildDrawFourCard(c) => 4}.sum
                        val newStack = (topCard, state.penalty + wildDrawFours)
                        val state2 = state.copy(hands = newHands,
                                                selected = state.selected + (userId -> List.empty),
                                                topCard = newStack._1,
                                                penalty = newStack._2,
                                                phase = Playing(TurnState(currentPlayer, false, true, false)), 
                                                turnOrder = reordered,
                                                rankings = rankings(state.rankings, newHands))
                        if isUno && pressedUno then 
                          Seq(Alert(s"${userId} : Uno!"),Render(state2))
                        else 
                          Seq(Render(state2))
                      //=================================
                      case WildCard(c) => 
                        if state.penalty == 0 then
                          val newStack = (topCard, 0)
                          val state2 = state.copy(hands = newHands,
                                                  selected = state.selected + (userId -> List.empty),
                                                  topCard = newStack._1,
                                                  penalty = newStack._2,
                                                  phase = Playing(TurnState(currentPlayer, false, true, false)), 
                                                  turnOrder = reordered,
                                                  rankings = rankings(state.rankings, newHands))
                          if isUno && pressedUno then 
                            Seq(Alert(s"${userId} : Uno!"),Render(state2))
                          else 
                            Seq(Render(state2))
                        else Seq()
                      //=================================
                      case NumberCard(_,0) => 
                        val newStack = (topCard,0)
                        val rotatedHands = rotateHands(newHands, finalTurnOrder, zeroes)
                        val state2 = state.copy(hands = rotatedHands,
                                                  selected = state.selected + (userId -> List.empty),
                                                  topCard = newStack._1,
                                                  penalty = newStack._2,
                                                  phase = Playing(TurnState(findNextToPlay(userId, reordered, effectiveSkips), false ,false, false)), 
                                                  turnOrder = finalTurnOrder,
                                                  rankings = rankings(state.rankings, newHands))
                          if reachedEndgame(newHands, newStack, finalTurnOrder) then 
                            if isUno && pressedUno then 
                              Seq(Alert(s"${userId} : Uno!"), Render(state2.copy(phase = Phase.Done)))
                            else 
                              Seq(Render(state2.copy(phase = Phase.Done)))
                          else 
                            if isUno && pressedUno then 
                              Seq(Alert(s"${userId} : Uno!"), Render(state2))
                            else 
                              Seq(Render(state2))
                      case _ => 
                        if state.penalty == 0 then
                          val newStack = (topCard, 0)
                          val state2 = state.copy(hands = state.hands + (userId -> hand3),
                                                  selected = state.selected + (userId -> List.empty),
                                                  topCard = newStack._1,
                                                  penalty = newStack._2,
                                                  phase = Playing(TurnState(findNextToPlay(userId, reordered, effectiveSkips), false ,false, false)), 
                                                  turnOrder = finalTurnOrder,
                                                  rankings = rankings(state.rankings, newHands))
                          if reachedEndgame(newHands, newStack, finalTurnOrder) then 
                            if isUno && pressedUno then 
                              Seq(Alert(s"${userId} : Uno!"), Render(state2.copy(phase = Phase.Done)))
                            else 
                              Seq(Render(state2.copy(phase = Phase.Done)))
                          else 
                            if isUno && pressedUno then 
                              Seq(Alert(s"${userId} : Uno!"), Render(state2))
                            else 
                              Seq(Render(state2))
                        else Seq()

                  else // unvalid sequence of selected cards -> unselct everything
                    val state2 = state.copy(selected = state.selected + (userId -> List.empty))
                    Seq(Render(state2))
                //=================================
                case _ => Seq()
        else throw NotYourTurnException()


  // =========================================================================
  // V. PROJECT FUNCTION
  // =========================================================================

    /** 
    * Projects the internal game state into a client-viewable format.
    * @param state current internal game state
    * @param userId the user for whom the view is generated
    * @return a View showing card statuses, phase, and opponent hand counts
    */
  override def project(state: State)(userId: UserId): View = // the representation of View seems odd (especially the fact that the facedown cards are only represented in the selecting state...)

    val finished =  state.players.map(_ -> false).toMap ++ state.hands.collect{case (u, h) if h.isEmpty => u}.map(_ -> true)   
    val selUser = state.selected.getOrElse(userId, List.empty)
    val selected = selUser.map(state.hands(userId)(_)).map(CardView.Selected(_))
    val unselected = state.hands(userId).toSet.diff(selUser.map(state.hands(userId)(_)).toSet)
    val previousCard = if selUser.isEmpty then state.topCard else state.hands(userId)(selUser.last)
    val otherCardCounts: Map[UserId, Int] = state.hands.view.filterKeys(_ != userId).map { case (uid, hand) => uid -> hand.size }.toMap
    
    state.phase match
      case Phase.Done => 
        View(StateView.Finished(state.rankings), state.hands(userId).map(CardView.UnselectedNotValid(_)).toVector, finished, otherCardCounts)
      
      case Playing(turn) =>  // has to see if what is meant by DrawPenalty view and when/what does is it displayed. Should we add it in the next version ? 
        val cardviews  = if userId == turn.currentPlayer then 
          state.hands(userId).zipWithIndex.map { case (card, index) =>
            if selUser.contains(index) then
              CardView.Selected(card)
            else if (selUser.isEmpty && Logic.isValid(previousCard, card, state.penalty > 0)) || (selUser.nonEmpty && previousCard.value == card.value) then // Use Logic.isValid for quick check
              CardView.UnselectedValid(card)
            else
              CardView.UnselectedNotValid(card)
          }.toVector
          else 
            state.hands(userId).zipWithIndex.map { case (card, index) => CardView.UnselectedNotValid(card)}.toVector

        val phaseView = 
          if turn.currentPlayer == userId then // is this something to be checked ?
            if turn.mustChoseColor then PhaseView.ChoosingColor
            else if state.penalty > 0 then PhaseView.DrawPenalty(state.penalty) 
            else PhaseView.SelectingCards
          else PhaseView.Waiting
        View(StateView.Playing(phaseView, turn.currentPlayer, state.topCard), cardviews, finished, otherCardCounts)

        