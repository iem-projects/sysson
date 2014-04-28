/*
 *  AuralSonification.scala
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
package sound

import de.sciss.lucre.event.{Observable, Sys}
import impl.{AuralSonificationImpl => Impl}

object AuralSonification {
  sealed trait Update
  case object Preparing extends Update
  case object Playing   extends Update
  case object Stopped   extends Update

  private[sysson] def current(): AuralSonification[_] = Impl.current()

  case class MissingSource         (key : String)
    extends Exception(s"The source for key '$key' is not assigned")

  case class MissingDimension      (key : String)
    extends Exception(s"The dimension for key '$key' is not assigned")

  case class MissingSourceDimension(sourceKey: String, name: String)
    extends Exception(s"The source for key '$sourceKey' does not have dimension '$name'")
}
trait AuralSonification[S <: Sys[S]] extends Observable[S#Tx, AuralSonification.Update] {
  def play()(implicit tx: S#Tx): Unit
  def stop()(implicit tx: S#Tx): Unit

  def state(implicit tx: S#Tx): AuralSonification.Update

  private[sysson] def attributeKey(elem: Any): String
}
