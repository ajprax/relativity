package ajprax

import org.jscience.mathematics.number.Real

package object relativity {

  /**
   * Wrapper class around Real to add mathematical operators.
   *
   * @param x the Real to wrap.
   */
  class MyReal(x: Real) {
    def + (y: Real): Real = x.plus(y)
    def - (y: Real): Real = x.minus(y)
    def * (y: Real): Real = x.times(y)
    def / (y: Real): Real = x.divide(y)
    def ^ (y: Int): Real = x.pow(y)
    def > (y: Real): Boolean = x.isGreaterThan(y)
    def < (y: Real): Boolean = x.isLessThan(y)
    def >= (y: Real): Boolean = x.equals(y) || x.isGreaterThan(y)
    def <= (y: Real): Boolean = x.equals(y) || x.isLessThan(y)
    def equals(y: Real): Boolean = x.equals(y)
    def sqrt: Real = x.sqrt()
    def unary_- = x.opposite()
  }

  /** Implicit casts to Real and MyReal for cleaner mathematical operations. */
  implicit def realToMyReal(x: Real): MyReal = new MyReal(x)
  implicit def doubleToReal(x: Double): Real = Real.valueOf(x)
  implicit def longToReal(x: Long): Real = Real.valueOf(x)
  implicit def intToReal(x: Int): Real = Real.valueOf(x)

  /**
   * Calculate the square root of a given Real value.
   *
   * @param x the Real value to square root.
   * @return the square root of the given Real value.
   */
  def sqrt(x: Real): Real = {
    if (x == Zero) {
      return 0
    } else {
      return x.sqrt
    }
  }

  type MetersPerSecond = Real
  type Meters = Real
  type AstronomicalUnits = Real
  type LightYears = Real
  type Seconds = Real
  type Years = Real

  val Zero = Real.valueOf(0)

  /** The speed of light in m/s. */
  val c: MetersPerSecond = 299792458
  val secondsInAYear: Seconds = 60 * 60 * 24 * 365.25
  val metersInAnAU: Meters = 149597870700L
  val metersInALightYear: Meters = secondsInAYear * c

  def secondsToYears(t: Seconds): Years = t / secondsInAYear
  def yearsToSeconds(t: Years) : Seconds = t * secondsInAYear

  def metersToLightYears(d: Meters): LightYears = d / metersInALightYear
  def lightYearsToMeters(d: LightYears): Meters = d * metersInALightYear

  def metersToAU(d: Meters): AstronomicalUnits = d / metersInAnAU
  def AUToMeters(d: AstronomicalUnits): Meters = d * metersInAnAU

  /**
   * Calculate the time light takes to travel d meters.
   *
   * @param d the number of meters which light travels in ? seconds.
   * @return the number of seconds light takes to travel d meters.
   */
  def timeToTravelAtC(d: Meters): Seconds = d / c

  /**
   * Calculate the distance light travels in t seconds.
   *
   * @param t the number of seconds in which light travels ? meters.
   * @return the number of meters light travels in t seconds.
   */
  def distanceTraveledAtC(t: Seconds): Meters = c * t

  /**
   * Calculate the difference between two locations.
   *
   * @param a the first Location.
   * @param b the second Location.
   * @return the distance between the two locations.
   */
  def distance(a: Location, b: Location): Meters =
    sqrt(((a.x - b.x) ^ 2) + ((a.y - b.y) ^ 2) + ((a.z - b.z) ^ 2))

  /**
   * Calculate the new location of an object which has been traveling from a known start location
   * for a known amount of time at a constant velocity.
   *
   * @param start the start location of the object.
   * @param t the time for which the object travelled.
   * @param v the velocity of the object over that period.
   * @return the new location of the object.
   */
  def location(start: Location, t: Seconds, v: Velocity): Location =
    Location((t * v.x) + start.x, (t * v.y) + start.y, (t * v.z) + start.z)

  /**
   * Whether two Real values are the same with a given error.
   *
   * @param x the first Real.
   * @param y the second Real
   * @param epsilon the allowable difference below which the two Reals are close enough.
   * @return whether two Real values are the same with a given error.
   */
  def closeEnough(x: Real, y: Real, epsilon: Real): Boolean = x - y < epsilon

  /**
   * Calculate the quadratic formula for the given coefficients.
   *
   * @param a coefficient of x * x.
   * @param b coefficient of x.
   * @param c constant.
   * @return the set of valid solutions for the quadratic equation.
   */
  def quadratic(a: Real, b: Real, c: Real): Set[Real] = {
    val delta = (b ^ 2) - (a * c * 4)
//    println("Delta(a=%s, b=%s, c=%s) = %s".format(a, b, c, delta))
    if (delta < 0) {
      return Set()
    } else if (delta == Zero) {
      return Set(-b / (a * 2))
    } else {
      val sol1 = (-b + sqrt(delta)) / (a * 2)
      val sol2 = (-b - sqrt(delta)) / (a * 2)
      return Set(sol1, sol2)
    }
  }
}
