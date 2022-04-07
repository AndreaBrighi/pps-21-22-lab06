package u06lab.code

object TicTacToe extends App with TwoPlayerGame :

  val bound = 2

  override def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- 0 to bound
      if find(board, x, y).isEmpty
    yield board :+ Disk(x, y, player)


  @main
  def mainTicTacToe(): Unit =
    import Player._

    printBoards(placeAnyDisk(List(), X))
    // ... ... ... ... ..X
    // ... ... ... X.. ...
    // ..X .X. X.. ... ...
    printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
    // ... ... ... ...
    // ..X ... ... .X.
    // ..O .XO X.O ..O
    println("EX 3: ")
    // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
    computeAnyGame(O, 4).foreach { g =>
      printBoards(g)
      println()
    }
  //  .... .... .... .... ...O
  //  .... .... .... ...X ...X
  //  .... .... ...O ...O ...O
  //  .... ...X ...X ...X ...X
  //
  //
  // .... .... .... .... O...
  // .... .... .... X... X...
  // .... .... O... O... O...
  // .... X... X... X... X...


    // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
    println("EX 4: ")
    computeAnyGameWin(O, 6).foreach { g =>
      printBoards(g)
      println()
    }

