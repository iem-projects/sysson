/*
 *  package.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

import at.iem.sysson.gui.impl.{DataSourceObjView, MatrixObjView, PlotObjView, SonificationObjView}

package object gui {
  def registerViews(): Unit = {
    DataSourceObjView  .init()
    SonificationObjView.init()
    PlotObjView        .init()
    MatrixObjView      .init()
  }
}
