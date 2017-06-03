package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.tileLocation
import org.apache.commons.lang.mutable.Mutable

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
      return d00 * (1-x) * (1 - y) +
        d10 * x * (1 - y) +
        d01 * (1-x) * y +
        d11 * x * y

  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val x_0 = 256 * x
    val y_0 = 256 * y
    val inner_zoom = zoom + 8

    val cache = scala.collection.mutable.Map[(Int, Int), Double]()
    def getTemp(lat: Int, lon: Int): Double = {
      cache.get((lat,lon)) match  {
        case Some(value) => value
        case None => {
          val newValue = grid(lat,lon)
          cache.put((lat,lon), newValue)
          newValue
        }
      }
    }

    val data = (0 until 256 * 256).map((n: Int) => {
      val x_i = n % 256
      val y_i = n / 256
      val location = tileLocation(inner_zoom, x_0 + x_i, y_0 + y_i)
      val lat = location.lat
      val lat_0 = math.floor(lat).toInt
      val lat_1 = math.ceil(lat).toInt
      val lon = location.lon
      val lon_0 = math.floor(lon).toInt
      val lon_1 = math.ceil(lon).toInt
      val temperature = bilinearInterpolation(
        x = lon - lon_0,
        y = lat_1 - lat,
        d00 = getTemp(lat_1, lon_0),
        d10 = getTemp(lat_1, lon_1),
        d01 = getTemp(lat_0, lon_0),
        d11 = getTemp(lat_0, lon_1)
      )
      val color = Visualization.interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, 127)
    }).toArray

    Image(256, 256, data)
  }

}
