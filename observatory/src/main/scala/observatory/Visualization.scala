package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  val EarthRadius = 6371
  val MinDistance = 0.001
  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    //def omega(l: Location): Double = 1/Math.pow(distance(l, location), 3)
    val temperaturesWithDistance = temperatures.map{ case(loc, temp) => (distance(loc, location), temp) }
    val nearest = temperaturesWithDistance.find{ case(dist, _) => dist < MinDistance }
    nearest match  {
      case None => {
        val temperaturesWithOmega = temperaturesWithDistance.map{ case(dist, temp) => (1.0/Math.pow(dist, 3), temp)}
        temperaturesWithOmega.foldLeft(0.0)((sum, pair) => sum + pair._1 * pair._2) / temperaturesWithOmega.map(_._1).sum
      }
      case(_, temp) => temp
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
  }

  /**
  {\displaystyle \delta =\arccos(\sin \varphi _{1}\cdot \sin \varphi _{2}+\cos \varphi _{1}\cdot \cos \varphi _{2}\cdot \cos(\lambda _{2}-\lambda _{1})).} \delta =\arccos(\sin \varphi _{1}\cdot \sin \varphi _{2}+\cos \varphi _{1}\cdot \cos \varphi _{2}\cdot \cos(\lambda _{2}-\lambda _{1})).
  */
  def distance(l1: Location, l2: Location): Double = {
    val phi_1 = Math.toRadians(l1.lat)
    val phi_2 = Math.toRadians(l2.lat)
    val lambda_1 = Math.toRadians(l1.lon)
    val lambda_2 = Math.toRadians(l2.lon)
    Math.acos(
      Math.sin(phi_1) * Math.sin(phi_2) +
      Math.cos(phi_1) * Math.cos(phi_2) * Math.cos(lambda_2 - lambda_1)
    )
  }
}

