package observatory

import java.time.LocalDate

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.sql._
import org.apache.spark.sql.functions._
import org.apache.spark.sql.types._

import scala.reflect.internal.ClassfileConstants
/**
  * 1st milestone: data extraction
  */
object Extraction {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Extraction")
      .config("spark.master", "local[4]")
      .getOrCreate()
  spark.sparkContext.setLogLevel("ERROR")
  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */

  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations: Dataset[Station] = getStations(stationsFile)
    val temperatures : Dataset[Temperature] = getTemperatures(temperaturesFile, year)
    stations.join(temperatures, temperatures.col("stationId") === stations.col("id"))
      .collect()
      .par
      .map(
        row => (
          LocalDate.of(row.getAs[Int]("year"), row.getAs[Int]("month"), row.getAs[Int]("day")),
          Location(row.getAs[Double]("lat"), row.getAs[Double]("lon")),
          row.getAs[Double]("temperature")
        )
      ).seq

  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    ???
  }

  def getStations(stationsFile: String): Dataset[Station] = {
    spark
      .read
      .schema(StructType(Array(
        StructField("STN", StringType, true),
        StructField("WBAN", StringType, true),
        StructField("lat", DoubleType, true),
        StructField("lon", DoubleType, true)
      )))
      .csv(getClass.getResource(stationsFile).getPath)
      .na.fill("", Array("STN", "WBAN"))
      .select(
        concat($"STN", lit("_"), $"WBAN").alias("id"),
        $"lat",
        $"lon"
      )
      .where($"lat".isNotNull && $"lat" =!= 0.0 && $"lon".isNotNull && $"lon" =!= 0.0)
      .as[Station]
  }

  def getTemperatures(temperaturesFile: String, year: Int): Dataset[Temperature] = {
    spark
      .read
      .schema(StructType(Array(
        StructField("STN", StringType, true),
        StructField("WBAN", StringType, true),
        StructField("month", IntegerType, true),
        StructField("day", IntegerType, true),
        StructField("temperature", DoubleType, true)
      )))
      .csv(getClass.getResource(temperaturesFile).getPath)
      .na.fill("", Array("STN", "WBAN"))
      .select(
        concat($"STN", lit("_"), $"WBAN").alias("stationId"),
        $"day",
        $"month",
        lit(year).alias("year"),
        (($"temperature" - 32) / 9 * 5).alias("temperature")
      )
      .where($"day".isNotNull && $"day" =!= 0.0 && $"month".isNotNull && $"month" =!= 0.0)
      .as[Temperature]
  }

}
