package observatory

import org.apache.spark.sql.Dataset
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  val year = 1975

  val stationsPath:String = "/stations.csv"
  val temperaturePath:String = s"/$year.csv"

  lazy val stations:Dataset[Station] = Extraction.getStations(stationsPath).persist
  lazy val temperatures:Dataset[Temperature] = Extraction.getTemperatures(temperaturePath, year).persist

  lazy val locateTemperatures = Extraction.locateTemperatures(year, stationsPath, temperaturePath)
  lazy val locateAverage = Extraction.locationYearlyAverageRecords(locateTemperatures)

  test("stations"){
    stations.show()
    assert(stations.filter((station:Station) => station.id=="007005_").count()===0,"id: 007005_")
    assert(stations.filter((station:Station) => station.id=="007018_").count()===0,"id: 007018_")
    assert(stations.filter((station:Station) => station.id=="008268_").count()===1,"id: 008268_")
    assert(stations.filter((station:Station) => station.id=="700001_26492").count()===1,"id: 700001_26492")
    assert(stations.filter((station:Station) => station.id=="_00100").count()===1,"id: _00100")
    assert(stations.count()===27708,"Num stations")
  }

  test("temperatures"){
    temperatures.show()
    assert(temperatures.filter((temperature:Temperature) => temperature.stationId=="007005_").count()===0,"id: 007005_")
    assert(temperatures.filter((temperature:Temperature) => temperature.stationId=="010010_").count()===363,"id: 010010_")
  }

  test("locateTemperatures"){
   locateTemperatures.take(20).foreach(println)
    assert(locateTemperatures.count(_._2==Location(70.933,-8.667)) === 363)
    assert(locateTemperatures.size === 2176493)
  }

  test("locationYearlyAverageRecords"){
    locateAverage.take(20).foreach(println)
    assert(locateAverage.count(_._1==Location(70.933,-8.667)) === 1)
    assert(locateAverage.size === 8251)
  }
}