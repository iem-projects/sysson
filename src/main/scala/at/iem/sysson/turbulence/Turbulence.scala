package at.iem.sysson
package turbulence

import java.awt.EventQueue

import scala.swing.Frame

object Turbulence extends Runnable {
  // don't use `App` because body will not be initialized if we don't use it as main entry
  def main(args: Array[String]): Unit = EventQueue.invokeLater(this)

  /** Maps from (x, y) index with respect to the
    * equilateral triangle grid to speaker channel
    * (1-based offset)
    */
  final val ChannelMap = Map[(Int, Int), Int](
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

  private val chans = ChannelMap.valuesIterator.toVector.sorted
  assert(chans == (3 to 8) ++ (10 to 11) ++ (13 to 46), s"ChannelMap does not have expected values: $chans")

  def run(): Unit = {
    Main.run()

    val dyn   = new DymaxionView
    val merc  = new MercatorView(dyn)

    new Frame {
      contents = dyn
      resizable = false
      pack()
      centerOnScreen()
      open()
    }

    new Frame {
      contents = merc
      resizable = false
      pack()
      centerOnScreen()
      open()
    }
  }
}
