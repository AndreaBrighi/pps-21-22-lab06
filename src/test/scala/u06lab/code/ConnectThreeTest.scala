package u06lab.code

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test
import u06lab.code.ConnectThree._
import u06lab.code.ConnectThree.Player._

class ConnectThreeTest:
  @Test
  def findTestX(): Unit =
    assertEquals(Some(X), find(List(Disk(0, 0, X)), 0, 0))

  @Test
  def findTestO(): Unit =
    assertEquals(Some(O), find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1))

  @Test
  def findNone(): Unit =
    assertEquals(None, find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1))

  @Test
  def firstAvailableRowTest0(): Unit =
    assertEquals(Some(0), firstAvailableRow(List(), 0)) // Some(0)

  @Test
  def firstAvailableRowTest1(): Unit =
    assertEquals(Some(1), firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)

  @Test
  def firstAvailableRowTest2(): Unit =
    assertEquals(Some(2), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)

  @Test
  def firstAvailableRowTest3(): Unit =
    assertEquals(Some(3), firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)

  @Test
  def firstAvailableRowTest4(): Unit =
    assertEquals(None, firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None)

  @Test
  def placeAnyTest(): Unit =
    assertEquals(List(List(Disk(0, 0, X)), List(Disk(1, 0, X)), List(Disk(2, 0, X)), List(Disk(3, 0, X))), placeAnyDisk(List(), X))

  @Test
  def placeAnyTest2(): Unit =
    printBoards(placeAnyDisk(List(Disk(0, 0, O)), X))
    assertEquals(List(List(Disk(0, 0, O), Disk(0, 1, X)), List(Disk(0, 0, O), Disk(1, 0, X)), List(Disk(0, 0, O), Disk(2, 0, X)), List(Disk(0, 0, O), Disk(3, 0, X))), placeAnyDisk(List(Disk(0, 0, O)), X))

  @Test
  def TestIsWinRowWin(): Unit =
    assertTrue(isWinRow(List(Disk(0, 0, X), Disk(1, 0, X), Disk(2, 0, X))))

  @Test
  def TestIsWinRowNoWin(): Unit =
    assertFalse(isWinRow(List(Disk(0, 0, X), Disk(1, 0, X), Disk(2, 0, O), Disk(3, 0, X))))

  @Test
  def TestIsWinColumnWin(): Unit =
    assertTrue(isWinColumn(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X))))

  @Test
  def TestIsWinColumnNoWin(): Unit =
    assertFalse(isWinColumn(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, O), Disk(0, 3, X))))

  @Test
  def TestIsWinDiagonalWin(): Unit =
    assertTrue(isWinDiagonal(List(Disk(0, 0, X), Disk(1, 1, X), Disk(2, 2, X))))

  @Test
  def TestIsWinDiagonalNoWin(): Unit =
    assertFalse(isWinDiagonal(List(Disk(0, 0, X), Disk(1, 1, X), Disk(2, 2, O), Disk(3, 3, X))))

  @Test
  def TestIsWinAntiDiagonalWin(): Unit =
    assertTrue(isWinAntiDiagonal(List(Disk(3, 0, X), Disk(2, 1, X), Disk(1, 2, X))))

  @Test
  def TestIsWinAntiDiagonalNoWin(): Unit =
    assertFalse(isWinAntiDiagonal(List(Disk(3, 0, X), Disk(2, 1, X), Disk(1, 2, O), Disk(0, 3, X))))
