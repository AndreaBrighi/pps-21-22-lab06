package u06lab.code

import org.junit.Test
import org.junit.Assert.*

class CombinerTests {

  private val f: Functions = FunctionsImpl
  @Test
  def testSum():Unit =
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001) // 60.1
  @Test
  def testSumEmpty():Unit =
    assertEquals(0.0, f.sum(List()), 0.1) // 0.0

  @Test
  def testConcat():Unit =
    assertEquals("abc", f.concat(Seq("a", "b", "c")))
  @Test
  def testConcatEmpty():Unit =
    assertEquals("", f.concat(Seq()))

  @Test
  def testMax():Unit =
    assertEquals(3, f.max(List(-10, 3, -5, 0)))

  @Test
  def testMaxEmpty():Unit =
    assertEquals(Integer.MIN_VALUE, f.max(List()))
}
