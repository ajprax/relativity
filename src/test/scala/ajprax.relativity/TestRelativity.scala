package ajprax.relativity

import org.junit.Test
import org.junit.Assert
import scala.collection.mutable

class TestRelativity {
  @Test
  def testDistance() {
    val a = Location(0, 0, 0)
    val b = Location(0, 0, 1)
    val c = Location(0, 1, 0)
    val d = Location(0, 1, 1)
    Assert.assertTrue(closeEnough(1, distance(a, b), 0.00001))
    Assert.assertTrue(closeEnough(math.sqrt(2), distance(b, c), 0.00001))
    Assert.assertTrue(closeEnough(math.sqrt(2), distance(a, d), 0.00001))
  }

  @Test
  def testMetersToAU() {
    val a: Meters = 10000000000.0
    val au: AstronomicalUnits = metersToAU(a)
    Assert.assertTrue(closeEnough(a / 149597870700.0, au, 0.00001))
  }

  @Test
  def benchmark() {
    val total: mutable.MutableList[Long] = mutable.MutableList()
    for (i <- Range(0, 1000000)) {
      val a = Location(math.random, math.random, math.random)
      val b = Location(math.random, math.random, math.random)
      val st = System.nanoTime()
      distance(a, b)
      total.+=:(System.nanoTime() - st)
    }
    print(total.sum / total.length)
  }
}
