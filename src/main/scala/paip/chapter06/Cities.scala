package paip.chapter06

import paip.chapter06.Search._

import scala.math._

object Cities {
  case class City(name: String, long: Double, lat: Double)

  object City {
    def cityName(c: City): String = {
      c.name
    }
  }

  implicit val cities = List(
    City("Atlanta", 84.23, 33.45),
    City("Los-Angeles", 118.15, 34.03),
    City("Boston", 71.05, 42.21),
    City("Memphis", 90.03, 35.09),
    City("Chicago", 87.37, 41.50),
    City("New-York", 73.58, 40.47),
    City("Denver", 105.00, 39.45),
    City("Oklahoma-City", 97.28, 35.26),
    City("Eugene", 123.05, 44.03),
    City("Pittsburgh", 79.57, 40.27),
    City("Flagstaff", 111.41, 35.13),
    City("Quebec", 71.11, 46.49),
    City("Grand-Jct", 108.37, 39.05),
    City("Reno", 119.49, 39.30),
    City("Houston", 105.00, 34.00),
    City("San-Francisco", 122.26, 37.47),
    City("Indianapolis", 86.10, 39.46),
    City("Tampa", 82.27, 27.57),
    City("Jacksonville", 81.40, 30.22),
    City("Victoria", 123.21, 48.25),
    City("Kansas-City", 94.35, 39.06),
    City("Wilmington", 77.57, 34.14))

  def city(name: String)(implicit cities: List[City]): City = {
    cities.find((c: City) => name.equals(c.name)).get
  }

  def truncate(deg: Double): Int = {
    Math.floor(deg).toInt
  }

  def remainder(num: Double, divisior: Int): Double = {
    BigDecimal.decimal(num).remainder(BigDecimal.decimal(divisior)).toDouble
  }

  def degToRadians(deg: Double): Double = {
    (truncate(deg) + (remainder(deg, 1) * (100.0 / 60.0))) * Pi * (1.0 / 180.0)
  }

  case class Point(x: Double, y: Double, z: Double) {
    def distance(p: Point): Double = {
      val x2 = pow(x - p.x, 2)
      val y2 = pow(y - p.y, 2)
      val z2 = pow(z - p.z, 2)

      sqrt(x2 + y2 + z2)
    }
  }

  object Point {
    def apply(city: City): Point = {
      val psi = degToRadians(city.lat)
      val phi = degToRadians(city.long)
      val x = cos(psi) * cos(phi)
      val y = cos(psi) * sin(phi)
      val z = sin(psi)

      Point(x, y, z)
    }
  }

  val EarthDiameter = 12765.0

  def airDistance(c1: City, c2: City): Double = {
    val d = Point(c1).distance(Point(c2))
    EarthDiameter * asin(d / 2.0)
  }


  def neighbors(city: City)(implicit cities: List[City]): List[City] = {
    cities.filter((c: City) => !c.equals(city) && airDistance(c, city) < 1000.0)
  }

  def trip(start: City, dest: City): Option[City] = {
    beamSearch[City, Double](
      start,
      Search.is[City](dest),
      neighbors,
      (c: City) => airDistance(c, dest),
      1
    )
  }

  def cityEquals(c1: City, c2: City): Boolean = {
    c1.equals(c2)
  }

  def trip(start: City, dest: City, beamWidth: Int = 1): Option[Path[City]] = {
    beamSearch[Path[City], Double](
      Path(state = start),
      is[City, Path[City]](dest, Path.pathState),
      pathSaver(neighbors, airDistance, (c: City) => airDistance(c, dest)),
      Path.pathTotalCost,
      beamWidth
    )
  }

  def showCityPath(path: Path[City]): Unit = {
    println(s"#<Path ${path.totalCost} km: ${mapPath(City.cityName, path).reverse.mkString(" - ")}>")
  }

  def main(args: Array[String]): Unit = {
    //    showCityPath(trip(city("San-Francisco"), city("Boston"), 1).get)
    //    showCityPath(trip(city("Boston"), city("San-Francisco"), 3).get)
    showCityPath(trip(city("Boston"), city("San-Francisco"), 1).get)
  }
}
