package at.iem.sysson.turbulence

import de.sciss.numbers
import numbers.Implicits._

object Preparations {
  lazy val dymGrid = Vector.tabulate(73) { latIdx =>
    Vector.tabulate(144) { lonIdx =>
      val lat = latIdx.linlin(0,  72, -177.5, +180.0)
      val lon = lonIdx.linlin(0, 143,  -90.0,  +90.0)
      Dymaxion.mapLonLat2(lat = lat, lon = lon)
    }
  }
}
