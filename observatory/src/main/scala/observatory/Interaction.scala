package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = Math.pow(2, zoom)
    val lonDeg  = ((x / n) * 360.0) - 180.0
    val latRad = Math.atan(Math.sinh(Math.PI * (1 - (2 * y) / n)))
    val latDeg = Math.toDegrees(latRad)
    Location(latDeg, lonDeg)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val x_0 = 256 * x
    val y_0 = 256 * y
    val inner_zoom = zoom + 8

    val data = (0 until 256 * 256).map((n: Int) => {
      val x_i = n % 256
      val y_i = n / 256
      val location = tileLocation(inner_zoom, x_0 + x_i, y_0 + y_i)
      val temperature = Visualization.predictTemperature(temperatures: Iterable[(Location, Double)], location)
      val color = Visualization.interpolateColor(colors, temperature)
      Pixel(color.red, color.green, color.blue, 127)
    }).toArray

    Image(256, 256, data)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    yearlyData.foreach{case(year, data) =>
      (0 to 3).foreach(zoom => {
          val n = Math.round(Math.pow(2, zoom)).toInt
          (0 until n).foreach(x => {
            (0 until n).par.foreach(y => {
              generateImage(year, zoom, x, y, data)
            })
          })
      })
    }
  }


}
