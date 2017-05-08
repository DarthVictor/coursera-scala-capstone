package observatory

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(id: String, lat: Double, lon: Double)

case class Temperature(stationId: String, day: Int, month: Int, year: Int, temperature: Double)