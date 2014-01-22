/*
 *  Patch.scala
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
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{Attribute, Attributes}
import impl.{PatchImpl => Impl}
import de.sciss.serial.DataInput
import de.sciss.model

object Patch {
  // ---- implementation forwards ----

  def apply[S <: Sys[S]](implicit tx: S#Tx): Patch[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Patch[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Patch[S]] = Impl.serializer[S]

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](patch: Patch[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  /** A state change is either a renaming, a change of graph, or a change of association (map) */
  sealed trait StateChange[S <: Sys[S]] extends Change[S]
  final case class GraphChange[S <: Sys[S]](change: model.Change[SynthGraph]) extends StateChange[S]

  /** An associative change is either adding or removing an association */
  sealed trait AssociativeChange[S <: Sys[S]] extends StateChange[S] {
    def key: AssociativeKey
  }
  final case class AssociationAdded  [S <: Sys[S]](key: AssociativeKey) extends AssociativeChange[S]
  final case class AssociationRemoved[S <: Sys[S]](key: AssociativeKey) extends AssociativeChange[S]

  /** An associative key is either a grapheme or a scan key */
  sealed trait AssociativeKey { def name: String }
  final case class ScanKey(name: String) extends AssociativeKey {
    override def toString = s"[scan: $name]"
  }

  final case class AttributeKey(name: String) extends AssociativeKey {
    override def toString = s"[attribute: $name]"
  }

  final case class AttributeChange[S <: Sys[S]](key: String, attribute: Attribute[S], change: Any)
    extends Change[S] {
    override def toString = s"AttributeChange($key, $attribute, $change)"
  }
}
trait Patch[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Patch.Update[S]] {
  def graph: Expr.Var[S, SynthGraph]

  /** A scalar attribute map */
  def attributes: Attributes.Modifiable[S]
}