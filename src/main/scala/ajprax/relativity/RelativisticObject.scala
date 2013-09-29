package ajprax.relativity

import scala.collection.mutable

case class RelativisticObject(history: mutable.Buffer[HistoryPoint]) {

  /**
   * Adds a new HistoryPoint do the history for this object based on its previous velocity and the
   * time between now and when it last changed velocity.
   *
   * @param time the absolute time at which this change in velocity occurred.
   * @param newVelocity the new velocity of the object as of this change.
   * @return the Location of the new history point.
   */
  def changeVelocity(time: Seconds, newVelocity: Velocity): Location = {
    val last = history.head
    require(time >= last.time)
    val loc = location(last.location, time - last.time, last.velocity)
    history.+=:(HistoryPoint(
          time,
          loc,
          newVelocity))
    return loc
  }

  /**
   * Calculates the position of this object as viewed from the specified time/location.
   *
   * @param tobs Reference time from which to view this object.
   * @param lobs Reference location from which to view this object.
   * @return the position of this object, relative to the specified time/location.
   */
  def apparentPosition(tobs: Seconds, lobs: Location): Location = {
//    println("Computing apparent position from tobs=%s lobs=%s".format(tobs, lobs))

    for (hp <- history) {
      if (hp.time <= tobs) {
        apparentPosition(tobs, lobs, hp) match {
          case Some(location) => return location
          case None => ()
        }
      } else {
        println("Discarding history point with time %s > tobs %s".format(hp.time, tobs))
      }
    }
    sys.error("No valid history point.")
  }

  /**
   * Attempt to calculate the apparent position along the line segment starting with hp of this
   * object from the specified observer time and location.
   *
   * @param tobs the absolute time of the observer.
   * @param lobs the absolute location of the observer.
   * @param hp the history point from which to attempt to calculate the apparent position of this
   *           object. hp.time is assumed to be less than tobs.
   * @return an optional location where this object appears along along the line segment started by
   *         hp.
   */
  private def apparentPosition(tobs: Seconds, lobs: Location, hp: HistoryPoint): Option[Location] = {
//    println("Computing apparent position with history point: " + hp)

    /** ti, ri and v are the segment origin and slope of this object. */
    val ti = hp.time
    val ri = hp.location
    val v = hp.velocity

    val dx = ri.x - lobs.x - (ti * v.x)
    val dy = ri.y - lobs.y - (ti * v.y)
    val dz = ri.z - lobs.z - (ti * v.z)

    val A = (v.x * v.x) + (v.y * v.y) + (v.z * v.z) - (c * c)
    val B = ((tobs * (c * c)) + ((v.x * dx) + (v.y * dy) + (v.z * dz))) * 2
    val C = (dx * dx) + (dy * dy) + (dz * dz) - ((c * c) * (tobs * tobs))

    val tcs = quadratic(A, B, C)
//    println("times=%s (=%s y)".format(tcs, tcs.map(secondsToYears)))
    val times = tcs
        .filter(_ >= ti)
        .filter(_ <= tobs)
//    println("filtered times=%s (=%s y)".format(times, times.map(secondsToYears)))
    if (times.isEmpty) return None

    require(times.size == 1, times)
    val tc = times.head
    return Some(location(ri, tc - ti, v))
  }
}

/**
 * A point in the history of an object at which its velocity changed. The velocity is held constant
 * until the next HistoryPoint.
 *
 * @param time the absolute time at which the change in velocity represented by this point occurred.
 * @param location the location the object was at when the change in velocity represented by this
 *                 point occurred.
 * @param velocity the new velocity of the object as of the time at which the velocity changed.
 */
case class HistoryPoint(time: Seconds, location: Location, velocity: Velocity)

/**
 * Absolute location in 3d space.
 *
 * @param x distance from the origin along the x axis.
 * @param y distance from the origin along the y axis.
 * @param z distance from the origin along the z axis.
 */
case class Location(x: Meters, y: Meters, z: Meters)

/**
 * Location relative to a specified origin.
 *
 * @param origin the location to which this location is relatively defined.
 * @param xOff the offset from origin along the x axis.
 * @param yOff the offset from origin along the y axis.
 * @param zOff the offset from origin along the z axis.
 */
case class RelativeLocation(origin: Location, xOff: Meters, yOff: Meters, zOff: Meters)

/**
 * Velocity in x, y, and z as orthogonal vector components.
 *
 * @param x speed along the x axis.
 * @param y speed along the y axis.
 * @param z speed along the z axis.
 */
case class Velocity(x: MetersPerSecond, y: MetersPerSecond, z: MetersPerSecond) {
  val speed: MetersPerSecond = sqrt((x ^ 2) + (y ^ 2) + (z ^ 2))
  require(speed < c)
}
