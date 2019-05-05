/*
 *  BinauralView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson.binaural

import de.sciss.desktop
import de.sciss.desktop.Preferences.Entry
import de.sciss.desktop.WindowHandler
import de.sciss.file.File
import de.sciss.lucre.swing.requireEDT
import de.sciss.mellite.Application
import de.sciss.mellite.Application.userPrefs
import de.sciss.swingplus.GroupPanel

object BinauralView {
  def numRows    : Entry[Int]    = userPrefs("binaural-num-rows"        )
  def numColumns : Entry[Int]    = userPrefs("binaural-num-columns"     )
  def hDistance  : Entry[Int]    = userPrefs("binaural-h-distance"      ) // centimeters
  def vDistance  : Entry[Int]    = userPrefs("binaural-v-distance"      ) // centimeters
  def verbFile   : Entry[File]   = userPrefs("binaural-reverb-file"     )
  def irDirectory: Entry[File]   = userPrefs("binaural-ir-directory"    )
  def maxConv    : Entry[Int]    = userPrefs("binaural-max-convolutions")

  def apply(): BinauralView = {
    requireEDT()

    import de.sciss.desktop.PrefsGUI._

    val lbNumRows     = label("Rows")
    val ggNumRows     = intField(numRows, 5, 1, 100)
    val lbNumColumns  = label("Columns")
    val ggNumColumns  = intField(numRows, 8, 1, 100)

    // ---- panel ----

    val box = new GroupPanel {
      // val lbValue = new Label("Value:", EmptyIcon, Alignment.Right)
      horizontal = Seq(
        Par(lbNumRows, lbNumColumns),
        Par(ggNumRows, ggNumColumns)
      )
      vertical = Seq(
        Par(Baseline)(lbNumRows         , ggNumRows         ),
        Par(Baseline)(lbNumColumns      , ggNumColumns      )
      )
    }

    new desktop.impl.WindowImpl {
      def handler: WindowHandler = Application.windowHandler

      title = "Binaural Mix"
      contents = box
      pack()
      front()
    }

    new BinauralView {}
  }
}
trait BinauralView