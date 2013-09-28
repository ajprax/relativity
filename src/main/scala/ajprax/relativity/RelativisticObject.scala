package ajprax.relativity

import scala.collection.mutable

case class RelativisticObject(history: mutable.Buffer[HistoryPoint]) {

  /**
   * Adds a new HistoryPoint do the history for this object based on its previous velocity and the
   * time between now and when it last changed velocity.
   *
   * @param time the absolute time at which this change in velocity occurred.
   * @param newVelocity the new velocity of the object as of this change.
   */
  def changeVelocity(time: Seconds, newVelocity: Velocity) {
    val last = history.head
    require(time >= last.time)
    history.+=:(HistoryPoint(
          time,
          location(last.location, time - last.time, last.velocity),
          newVelocity))
  }

  def apparentPosition(time: Seconds, from: Location): Location = {
    // iterate through the history of this object checking for the distance between from and
    // HistoryPoint.location and comparing that to the distance light travels in that much time.
    // When we switch from < to > use that history point and calculate the exact apparent position.
    null
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
 * Velocity in x, y, and z orthogonal vector components.
 *
 * @param x speed along the x axis.
 * @param y speed along the y axis.
 * @param z speed along the z axis.
 */
case class Velocity(x: MetersPerSecond, y: MetersPerSecond, z: MetersPerSecond) {
  val speed = sqrt((x ^ 2) + (y ^ 2) + (z ^ 2))
  require(speed < c)
}
