package ajprax.relativity

import org.junit.Test
import org.junit.Assert
import scala.collection.mutable

class TestRelativity {
  @Test
  def testDistance {
    val a = Location(0, 0, 0)
    val b = Location(0, 0, 1)
    val c = Location(0, 1, 0)
    val d = Location(0, 1, 1)
    Assert.assertTrue(closeEnough(1, distance(a, b), 0.00001))
    Assert.assertTrue(closeEnough(math.sqrt(2), distance(b, c), 0.00001))
    Assert.assertTrue(closeEnough(math.sqrt(2), distance(a, d), 0.00001))
  }

  @Test
  def testMetersToAU {
    val a: Meters = 10000000000.0
    val au: AstronomicalUnits = metersToAU(a)
    Assert.assertTrue(closeEnough(a / 149597870700.0, au, 0.00001))
  }

  @Test
  def testQuadratic {
    val results = quadratic(1, -2, 1)
    Assert.assertEquals(1, results.size)
    val result = results.head
    Assert.assertTrue(closeEnough(result, 1.0, 1e-5))
  }

  @Test
  def testQuadraticTwo {
    val results = quadratic(1, 0, -1)
    Assert.assertEquals(2, results.size)
    val Array(res1, res2) = results.toArray
    Assert.assertTrue(closeEnough(res1, 1.0, 1e-5))
    Assert.assertTrue(closeEnough(res2, -1.0, 1e-5))
  }

//  @Test benchmark test is disabled.
  def benchmark {
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

  @Test
  def testApparentPosition {
    val h1 = HistoryPoint(0, Location(0, 0, 0), Velocity(0, 0, 0))
    val a = RelativisticObject(mutable.Buffer(h1))
    a.changeVelocity(yearsToSeconds(10), Velocity(c * 0.1, 0, 0)) // Location(0, 0, 0)
    a.changeVelocity(yearsToSeconds(20), Velocity(c * 0.5, 0, 0)) // Location(1ly, 0, 0)

    val apparent = a.apparentPosition(yearsToSeconds(40), Location(lightYearsToMeters(1),0,0))
//    println("apparent = %s".format(apparent))
//    println("apparent.x = %s ( = %s ly)".format(apparent.x, metersToLightYears(apparent.x)))
    Assert.assertTrue("expected: %s apparent: %s".format(lightYearsToMeters(23.0 / 3), apparent.x),
      closeEnough(apparent.x, lightYearsToMeters(23.0 / 3), lightYearsToMeters(0.00001)))
  }
}
