/*
 *  AuralSonification.scala
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

package at.iem.sysson
package sound

import at.iem.sysson.sound.impl.{AuralSonificationImpl => Impl}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.{event => evt}
import de.sciss.synth.proc.{AuralContext, AuralObj}

object AuralSonification extends AuralObj.Factory {
  private lazy val _init: Unit = AuralObj.addFactory(this)

  def tpe: Obj.Type = Sonification

  def init(): Unit = _init

  def find[S <: Sys[S]]()(implicit tx: S#Tx): Option[Sonification[S]] = Impl.find()

  type Repr[S <: Sys[S]] = Sonification[S]

  def apply[S <: SSys[S]](obj: Sonification[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralSonification[S] =
    Impl(obj)

  sealed trait Update /* [S <: Sys[S]] */ {
    // def view: AuralSonification[S]
  }
  case class Elapsed(key: graph.Dim, ratio: Float, dimValue: Float)
    extends Update /* [S] */
  case class PlayFailed(cause: Throwable) extends Update
}
trait AuralSonification[S <: SSys[S]] extends AuralObj[S] {
  def sonification(implicit tx: S#Tx): Sonification[S]

  def status: evt.Observable[S#Tx, AuralSonification.Update /* [S] */]
}
