package observatory

import org.apache.spark.sql.Dataset
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers


@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {
  val year = 2015

  val stationsPath:String = "/stations.csv"
  val temperaturePath:String = s"/$year.csv"

  lazy val stations:Dataset[Station] = Extraction.getStations(stationsPath).persist
  lazy val temperatures:Dataset[Temperature] = Extraction.getTemperatures(temperaturePath, year).persist

  lazy val locateTemperatures = Extraction.locateTemperatures(year, stationsPath, temperaturePath)
  lazy val locateAverage = Extraction.locationYearlyAverageRecords(locateTemperatures)

  test("simple tile"){
    //assert(tile(temperatures, colors, 0,0,0))
  }

}
