/*
 *  package.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

import at.iem.sysson.gui.impl.{SonificationTimelineView, SonificationObjView, DataSourceObjView}

package object gui {
  def registerViews(): Unit = {
    DataSourceObjView
    SonificationObjView
    SonificationTimelineView
  }
}
