package u06lab.code

import java.util.OptionalInt
import scala.annotation.tailrec

object ConnectThree extends App with TwoPlayerGame:
  override val bound = 3

  override def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield board :+ Disk(x, y, player)

  @main
  def main(): Unit =
    import Player._
    // Exercise 1: implement find such that..
    println("EX 1: ")
    println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
    println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
    println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

    // Exercise 2: implement firstAvailableRow such that..
    println("EX 2: ")
    println(firstAvailableRow(List(), 0)) // Some(0)
    println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
    println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
    println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
    println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
    // Exercise 2: implement placeAnyDisk such that..
    printBoards(placeAnyDisk(List(), X))
    // .... .... .... ....
    // .... .... .... ....
    // .... .... .... ....
    // ...X ..X. .X.. X...
    printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
    // .... .... .... ....
    // .... .... .... ....
    // ...X .... .... ....
    // ...O ..XO .X.O X..O
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