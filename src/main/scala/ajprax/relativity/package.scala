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
    def sqrt: Real = x.sqrt()
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
  def sqrt(x: Real): Real = x.sqrt

  type MetersPerSecond = Real
  type Meters = Real
  type AstronomicalUnits = Real
  type Seconds = Real

  /** The speed of light in m/s. */
  val c: MetersPerSecond = 299792458

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
   * Convert a distance in Meters to a distance in AstronomicalUnits.
   *
   * @param d the number of meters.
   * @return the number of AU in d Meters.
   */
  def metersToAU(d: Meters): AstronomicalUnits = d / 149597870700L

  /**
   * Convert a distance in AstronomicalUnits to a distance in Meters.
   * @param d the number of AU.
   * @return the number of Meters in d AU.
   */
  def AUToMeters(d: AstronomicalUnits): Meters = d * 149597870700L

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
    Location(t * v.x, t * v.y, t * v.z)

  /**
   * Whether two Real values are the same with a given error.
   *
   * @param x the first Real.
   * @param y the second Real
   * @param epsilon the allowable difference below which the two Reals are close enough.
   * @return whether two Real values are the same with a given error.
   */
  def closeEnough(x: Real, y: Real, epsilon: Real): Boolean = x - y < epsilon
}


