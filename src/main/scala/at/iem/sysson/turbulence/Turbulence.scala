package at.iem.sysson
package turbulence

import de.sciss.pdflitz

import java.awt.EventQueue

import scala.swing.Frame

object Turbulence extends Runnable {
  // don't use `App` because body will not be initialized if we don't use it as main entry
  def main(args: Array[String]): Unit = EventQueue.invokeLater(this)

  /** Maps from (x, y) index with respect to the
    * equilateral triangle grid to speaker channel
    * (1-based offset)
    */
  final val MatrixToChannelMap = Map[(Int, Int), Int](
    ( 4, 0) ->  3,
    ( 6, 0) ->  5,
    ( 8, 0) ->  4,
    ( 5, 0) ->  6,
    ( 7, 0) ->  7,
    ( 6, 1) ->  8,
    ( 3, 1) -> 21,
    ( 5, 1) -> 25,
    ( 7, 1) -> 26,
    ( 9, 1) -> 33,
    (11, 1) -> 41,
    (13, 1) -> 45,
    ( 4, 2) -> 22,
    ( 6, 2) -> 27,
    ( 8, 2) -> 34,
    (12, 2) -> 42,
    ( 5, 2) -> 23,
    ( 7, 2) -> 29,
    ( 9, 2) -> 35,
    (11, 2) -> 43,
    ( 0, 3) -> 10,
    ( 2, 3) -> 13,
    ( 4, 3) -> 17,
    ( 6, 3) -> 28,
    ( 8, 3) -> 30,
    (10, 3) -> 37,
    (12, 3) -> 46,
    ( 1, 3) -> 11,
    ( 3, 3) -> 14,
    ( 5, 3) -> 24,
    ( 7, 3) -> 31,
    ( 9, 3) -> 36,
    (11, 3) -> 44,
    ( 2, 4) -> 15,
    ( 4, 4) -> 18,
    (10, 4) -> 38,
    ( 1, 4) -> 16,
    ( 3, 4) -> 19,
    ( 5, 4) -> 20,
    ( 7, 4) -> 32,
    ( 9, 4) -> 39,
    (11, 4) -> 40
  )

  /** Maps from loudspeaker channels to
    * quantized (lat, lon) pairs. Nearest
    * raster points are used except where
    * there are "jumps" in the Dymaxion,
    * in which case we go "away" a little
    * from the gap, so there are no two
    * loudspeakers with the same geo-coordinates.
    */
  final val ChannelToGeoMap = Map[Int, (Double, Double)](
     3 -> ( -40.0,  +35.0),
     6 -> ( -12.5,  +30.0),
     5 -> (  +5.0,    0.0),
     7 -> ( +35.0,  -25.0),
     4 -> ( +27.5,  -60.0),
    21 -> ( -45.0,  +45.0),
    25 -> ( +10.0,  +57.5),
     8 -> ( +27.5,  +22.5),
    26 -> ( +65.0,  +12.5),
    33 -> ( +27.5,  -67.5),
    41 -> ( +17.5,  -65.0),
    45 -> (  +2.5,  -12.5),
    22 -> ( -25.0,  +72.5),
    23 -> ( +10.0,  +97.5),
    27 -> ( +45.0,  +72.5),
    29 -> ( +75.0, +142.5),
    34 -> ( +60.0,  -75.0),
    35 -> ( +25.0, -107.5),
    43 -> ( -10.0,  -82.5),
    42 -> (  -5.0,  -42.5),
    10 -> (  -7.5,   -5.0),
    11 -> ( -35.0,  -10.0),
    13 -> ( -52.5,  +27.5),
    14 -> ( -60.0, +102.5),
    17 -> ( -25.0, +112.5),
    24 -> (  +5.0, +137.5),
    28 -> ( +40.0, +122.5),
    31 -> ( +35.0, +170.0),
    30 -> ( +50.0, -142.5),
    36 -> ( +12.5, -150.0),
    37 -> ( -10.0, -122.5),
    44 -> ( -45.0, -107.5),
    46 -> ( -37.5,  -60.0),
    16 -> ( -45.0,  -50.0),
    15 -> ( -75.0,  -35.0),
    19 -> ( -67.5, -172.5),
    18 -> ( -35.0, +152.5),
    20 -> (  -7.5, +167.5),
    32 -> (  +7.5, +172.5),
    39 -> (  -5.0, +180.0),
    38 -> ( -27.5, -157.5),
    40 -> ( -62.5, -160.0)
  )

  assert(ChannelToGeoMap.size == 42 && ChannelToGeoMap.valuesIterator.toSet.size == 42)

  private val chans = MatrixToChannelMap.valuesIterator.toVector.sorted
  assert(chans == (3 to 8) ++ (10 to 11) ++ (13 to 46), s"ChannelMap does not have expected values: $chans")

  def run(): Unit = {
    Main.run()

    val dyn     = new DymaxionView
    dyn.crosses = Preparations.dymGrid.flatten
    dyn.mouseControl = false
    val merc    = new MercatorView(dyn)

    val fDyn = new Frame {
      contents = dyn
      resizable = false
    }

    new pdflitz.SaveAction(dyn :: Nil).setupMenu(fDyn)
    fDyn.pack()
    fDyn.centerOnScreen()
    fDyn.open()

    new Frame {
      contents = merc
      resizable = false
      pack()
      centerOnScreen()
      open()
    }
  }
}
