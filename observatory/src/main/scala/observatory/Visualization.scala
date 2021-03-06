package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.math._

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
  def predictTemperature2(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    //def omega(l: Location): Double = 1/Math.pow(distance(l, location), 3)
    val temperaturesWithDistance = temperatures.par.map{ case(loc, temp) => (distance(loc, location), temp) }
    //temperaturesWithDistance.head._2
    val nearest = temperaturesWithDistance.par.find{ case(dist, _) => dist == 0.0 }

    nearest match {
      case None => {
        val (weightedSum, invertedSum) = temperaturesWithDistance.aggregate(0.0, 0.0)({
          case ((weightedSum, invertedSum), (dist, temp)) =>
            val cur = 1.0 / (dist*dist*dist)
            (weightedSum + temp * cur, invertedSum + cur)
        }, {
          case ((weightedSumA, invertedSumA), (weightedSumB, invertedSumB)) =>
            (weightedSumA + weightedSumB, invertedSumA + invertedSumB)
        })
        weightedSum / invertedSum
      }
      case Some(t: (Double, Double)) => t._2
    }
  }

  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    //def omega(l: Location): Double = 1/Math.pow(distance(l, location), 3)
    val temperaturesWithDistance = temperatures.map{ case(loc, temp) => (distance(loc, location), temp) }
    val nearest = temperaturesWithDistance.find{ case(dist, _) => dist < MinDistance }
    nearest match  {
      case None => {
        val temperaturesWithOmega = temperaturesWithDistance.map{ case(dist, temp) => (1.0/Math.pow(dist, 3), temp)}
        temperaturesWithOmega.foldLeft(0.0)((sum, pair) => sum + pair._1 * pair._2) / temperaturesWithOmega.map(_._1).sum
      }
      case Some(t: (Double, Double)) => t._2
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    def interColor(points: Iterable[(Double, Color)], value: Double): Color ={
      if(points.size == 1 || value <= points.head._1){
        points.head._2
      }
      else if(value > points.head._1 && value < points.tail.head._1){
        val k = (value - points.head._1)/(points.tail.head._1 - points.head._1)
        val c1 = points.head._2
        val c2 = points.tail.head._2
        Color(
          Math.round(c1.red + k *(c2.red - c1.red)).toInt,
          Math.round(c1.green + k *(c2.green - c1.green)).toInt,
          Math.round(c1.blue + k *(c2.blue - c1.blue)).toInt
        )
      }
      else{
        interColor(points.tail, value)
      }
    }
    if (points.size == 0) throw new IllegalArgumentException
    val sortedPoints = points.toList.sortWith((a, b)=> a._1 < b._1)
    interColor(sortedPoints, value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val totalWidth = 360
    val latStep = 180 * 2 / totalWidth
    val totalHeight = 180
    val lonStep = 90 * 2 / totalHeight
    val lat_0 = 90
    val lon_0 = -180

    val data = (0 until totalWidth * totalHeight).map((n: Int) => {
      val x = n % totalWidth
      val y = n / totalWidth
      val lat = lat_0 - y * latStep
      val lon = lon_0 + x * lonStep
      val location = Location(lat, lon)

      val temperature = predictTemperature(temperatures: Iterable[(Location, Double)], location)
      val color = interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, 255)
    }).toArray

    Image(totalWidth, totalHeight, data)
  }

  /**
  {\displaystyle \delta =\arccos(\sin \varphi _{1}\cdot \sin \varphi _{2}+\cos \varphi _{1}\cdot \cos \varphi _{2}\cdot \cos(\lambda _{2}-\lambda _{1})).} \delta =\arccos(\sin \varphi _{1}\cdot \sin \varphi _{2}+\cos \varphi _{1}\cdot \cos \varphi _{2}\cdot \cos(\lambda _{2}-\lambda _{1})).
  */
  def distance(l1: Location, l2: Location): Double = {
    val Location(lat1, lon1) = l1
    val Location(lat2, lon2) = l2

    val latDist = toRadians(lat2 - lat1)
    val lonDist = toRadians(lon2 - lon1)

    val a = pow(sin(latDist / 2), 2) + pow(sin(lonDist / 2), 2) * cos(toRadians(lat1)) * cos(toRadians(lat2))

    2 * atan2(sqrt(a), sqrt(1 - a))
  }

  def distance2(l1: Location, l2: Location):Double = {
    val Location(lat1, lon1) = l1
    val Location(lat2, lon2) = l2
    val x = (lon2 - lon1) * cos( 0.5*(lat2+lat1) )
    val y = lat2 - lat1
    sqrt( x*x + y*y )
  }
}

