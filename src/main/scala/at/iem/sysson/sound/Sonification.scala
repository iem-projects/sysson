/*
 *  Sonification.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package sound

import de.sciss.lucre.{event => evt}
import evt.{Publisher, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.model
import de.sciss.lucre.expr
import de.sciss.synth.proc.{Elem, AttrMap}
import impl.{SonificationImpl => Impl}
import de.sciss.serial.DataInput
import de.sciss.lucre.matrix.Matrix

object Sonification {
  // ---- implementation forwards ----

  def apply[S <: Sys[S]](implicit tx: S#Tx): Sonification[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Sonification[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Sonification[S]] = Impl.serializer[S]

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](sonif: Sonification[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  /** A state change is either a renaming, a change of graph, or a change of association (map) */
  sealed trait StateChange[S <: Sys[S]] extends Change[S]
  final case class PatchChange[S <: Sys[S]](change: Patch.Change[S]) extends StateChange[S]

  /** An associative change is either adding or removing an association */
  sealed trait AssociativeChange[S <: Sys[S]] extends StateChange[S] {
    def key: String
  }
  final case class AttributeAdded  [S <: Sys[S]](key: String) extends AssociativeChange[S]
  final case class AttributeRemoved[S <: Sys[S]](key: String) extends AssociativeChange[S]

  final case class AttributeChange [S <: Sys[S]](key: String, attribute: Elem[S], change: Any)
    extends Change[S] {
    override def toString = s"AttributeChange($key, $attribute, $change)"
  }

  // -------------------------------------------------------

  object Source {
    sealed trait Update[S <: Sys[S]] { def source: Source[S] }
    final case class DimsChanged[S <: Sys[S]](source: Source[S],
                                              update: expr.Map.Update[S, String, Expr[S, String], model.Change[String]])
      extends Update[S]

    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Source[S]] = Impl.sourceSerializer

    def apply[S <: Sys[S]](matrix: Matrix[S])(implicit tx: S#Tx): Source[S] =
      Impl.applySource(matrix)
  }
  trait Source[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Source.Update[S]] {
    def matrix: Matrix[S]
    /** Maps sonification model/patch dimensions (keys) to source matrix dimensions (values). */
    def dims: expr.Map[S, String, Expr[S, String], model.Change[String]]
  }
}
trait Sonification[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Sonification.Update[S]] {
  def patch: Patch[S]

  def sources : expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]]
  def controls: expr.Map[S, String, Expr[S, Double], model.Change[Double]]

  /** A scalar attribute map */
  def attr: AttrMap.Modifiable[S]
}
