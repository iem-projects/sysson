val f = openFile(userHome / "IEM" / "SysSon" / "installation" / "data" / "ta_anom_spk2.nc")
val sec = f.variableMap("Temperature") in "Time" select (12 to 144)
val x = sec.read().double1D
x.min
x.max
x.exists(_.isNaN)

///////////////////////////////////

val f = openFile(userHome / "IEM" / "SysSon" / "installation" / "data" / "ta_anom_spk.nc")

import at.iem.sysson.turbulence._
import Turbulence._

val sec = f.variableMap("Temperature") in "Time" select (12 to 144)

val presIdx0=10

val grid = Channels.map { spk =>
  val ptd     = ChannelToMatrixMap(spk).toPoint
  val pt      = ptd.equalize
  //val lat     = LatLonDym.minBy(_._2.equalize distanceTo pt)._1.toLatLon.lat
  //val latIdx  = latitudes.indexOf(latitudes.minBy(_ absdif lat))
  val presIdx = presIdx0 + ptd.y.toInt
  presIdx // (latIdx, presIdx)
}

def collSpk(i: Int) = {
  val j = grid.zipWithIndex.filter(_._1 == i).map(_._2)

  ((0.0, 0.0) /: j) { case ((mn, mx), jj) =>
    val sec0 = f.variableMap("Temperature") in "Time" select (12 to 144)
    val sec  = sec0 in "spk" select jj
    val x = sec.read().double1D
    (math.min(x.min, mn), math.max(x.max, mx))
  }
}

val minMax = (10 to 19).map(collSpk)

minMax.minBy(_._1) // -12.5
minMax.maxBy(_._2) // +17.8


import de.sciss.synth.swing.Plotting.Implicits._

minMax.map(_._1).plot()
minMax.map(_._2).plot()

minMax.map(_._1).zipWithIndex.map { case (m, i) => m * 1.0 / (i+1) } .plot()  // yes!
minMax.map(_._2).zipWithIndex.map { case (m, i) => m * 1.0 / (i+1) } .plot()  // yes!

minMax.map(_._1).zipWithIndex.map { case (m, i) => m * 1.0 / (i+1) } .min // -2.1
minMax.map(_._2).zipWithIndex.map { case (m, i) => m * 1.0 / (i+1) } .max // +2.4

grid.map { pi => 1.0 / (pi - 9) }

// Vector(1.0, 1.0, 1.0, 0.5, 0.5, 0.3333, 0.1429, 0.125, 0.1429, 0.125, 0.1111, 0.1, 0.1429, 0.1111, 0.1, 0.1,
// 0.25, 0.2, 0.1667, 0.125, 0.25, 0.25, 0.2, 0.1429, 0.1667, 0.1429, 0.125, 0.1, 0.25, 0.2, 0.1667, 0.125,
// 0.1429, 0.1111, 0.1, 0.1, 0.25, 0.2, 0.1667, 0.125, 0.25, 0.1429)
