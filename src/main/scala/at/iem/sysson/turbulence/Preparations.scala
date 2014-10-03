package at.iem.sysson.turbulence

import at.iem.sysson.turbulence.Dymaxion.Pt2
import de.sciss.numbers
import numbers.Implicits._

object Preparations {
  lazy val dymGrid = Vector.tabulate(73) { latIdx =>
    Vector.tabulate(144) { lonIdx =>
      val lon = lonIdx.linlin(0, 143, -177.5, +180.0)
      val lat = latIdx.linlin(0,  72,  -90.0,  +90.0)
      Dymaxion.mapLonLat(lat = lat, lon = lon)
    }
  }

  lazy val dymGridF = dymGrid.zipWithIndex.flatMap { case (inLat, latIdx) =>
    inLat.zipWithIndex.map { case (pt, lonIdx) => (latIdx, lonIdx, pt)
    }
  }

  lazy val dymGridAng = dymGrid.zipWithIndex.map { case (inLat, latIdx) =>
    inLat.zipWithIndex.map { case (pt1, lonIdx) =>
      //      val (nnLat, nnLon, nnPt) = dymGridF.minBy { case (_, _, pt2) =>
      //        if (pt1 == pt2) Double.MaxValue else {
      //          val Pt2(x1, y1) = pt1
      //          val Pt2(x2, y2) = pt2
      //          val dx = x2 - x1
      //          val dy = y2 - y1
      //          (dx * DymaxionView.hScale).squared + (dy * DymaxionView.vScale).squared
      //        }
      //      }
      //      // now we have found the nearest neighbour point in dym space
      //      // assert((nnLat == latIdx) != (nnLon == lonIdx), s"nnLat $nnLat, nnLon $nnLon, lat $latIdx, lon $lonIdx")
      //
      //      if (!((nnLat == latIdx) != (nnLon == lonIdx))) println(s"nnLat $nnLat, nnLon $nnLon, lat $latIdx, lon $lonIdx")
      //
      //      // we have to angle relations, one in mercator space, one in dym space;
      //      // for example, with two marc pairs (2.5, 90), (5.0, 90), the second
      //      // point lies east of the first point. If we now subtract the dym angle
      //      // from this angle, we get the relative rotation angle of the dym point,
      //      // i.e. a "compass needle".
      //      // val mAng = if (nnLat < latIdx)

      val latS = latIdx - 1
      val latN = latIdx + 1
      val ptS  = if (latS  <  0) Pt2(-99, -99) else dymGrid(latS)(lonIdx)
      val ptN  = if (latN >= 73) Pt2(-99, -99) else dymGrid(latN)(lonIdx)

      import DymaxionView.{scaledDist, scalePt}
      val useSouth  = scaledDist(pt1, ptS) < scaledDist(pt1, ptN)
      val pt1Sc     = scalePt(pt1)
      val pt2Sc     = scalePt(if (useSouth) ptS else ptN)
      val ang1      = math.atan2(-(pt2Sc.y - pt1Sc.y), pt2Sc.x - pt1Sc.x)
      val ang       = if (useSouth) (ang1 + math.Pi) % (2 * math.Pi) else ang1

      //      if ((lonIdx * 73 + latIdx) % 256 == 0) {
      //        println(f"PT1: lat $latIdx / ${latIdx.linlin(0,  72, -177.5, +180.0)}, lon $lonIdx, ")
      //      }

      (pt1, ang)
    }
  }
}
