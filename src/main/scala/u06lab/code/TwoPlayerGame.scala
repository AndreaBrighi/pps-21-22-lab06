package u06lab.code

trait TwoPlayerGame:

  def bound: Int

  enum Player:
    case X, O

    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)

  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   * 0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(disk => disk.x == x && disk.y == y) match
      case Some(disk) => Some(disk.player)
      case None => None

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    board.count(disk => disk.x == x) match
      case s if s <= bound => Some(s)
      case _ => None

  def placeAnyDisk(board: Board, player: Player): Seq[Board]

  private[code] def computeAnyGameStrategy(player: Player, moves: Int)(win: (game: Game) => Boolean): LazyList[Game] = moves match
    case 0 => LazyList(Seq(Seq.empty))
    case _ =>
      val games = computeAnyGameStrategy(player.other, moves - 1)(win)
      (for
        game <- games
        if isWin(game.head)
      yield game) ++
        (for
          game <- games
          if !isWin(game.head)
          board <- placeAnyDisk(game.head, player)
        yield board +: game)

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = computeAnyGameStrategy(player, moves)(game => false)

  def computeAnyGameWin(player: Player, moves: Int): LazyList[Game] = computeAnyGameStrategy(player, moves)(game => isWin(game.head))

  private[code] def isWin(board: Board): Boolean = isWinRow(board) || isWinColumn(board) || isWinDiagonal(board) || isWinAntiDiagonal(board)

  private[code] def isWinRow(board: Board): Boolean =
    (for
      y <- 0 to bound
      x <- 1 until bound
    yield checkThreeNear(board)(x - 1, y)(x, y)(x + 1, y)).exists(identity)

  private[code] def isWinColumn(board: Board): Boolean =
    (for
      x <- 0 to bound
      y <- 1 until bound
    yield checkThreeNear(board)(x, y - 1)(x, y)(x, y + 1)).exists(identity)

  private[code] def isWinDiagonal(board: Board): Boolean =
    (for
      i <- 1 until bound
    yield checkThreeNear(board)(i - 1, i - 1)(i, i)(i + 1, i + 1)).exists(identity)

  private[code] def isWinAntiDiagonal(board: Board): Boolean =
    (for
      i <- 1 until bound
    yield
      checkThreeNear(board)(i - 1, bound - (i - 1))(i, bound - i)(i + 1, bound - (i + 1))).exists(identity)

  private[code] def checkThreeNear(board: Board)(x1: Int, y1: Int)(x2: Int, y2: Int)(x3: Int, y3: Int): Boolean = (find(board, x1, y1), find(board, x2, y2), find(board, x3, y3)) match
    case (Some(player1), Some(player2), Some(player3)) if player1 == player2 && player3 == player1 => true
    case _ => false

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()
