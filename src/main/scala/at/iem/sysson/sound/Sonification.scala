package at.iem.sysson
package sound

import de.sciss.lucre.{event => evt}
import evt.{Publisher, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.expr
import de.sciss.synth.proc.{Attribute, Attributes}
import impl.{SonificationImpl => Impl}
import de.sciss.serial.DataInput

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

  // -------------------------------------------------------

  object Source {
    sealed trait Update[S <: Sys[S]]

    implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Source[S]] = ???
  }
  trait Source[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Source.Update[S]] {
    def data: DataSource[S]
    // def dims: Map[String, String]
    // XXX TODO: Ops. Perhaps this could be covered by having DataSource behave like an expression?
    def dims: SkipList.Map[S, String, Expr[S, String]]
  }
}
trait Sonification[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Sonification.Update[S]] {
  def patch: Patch[S] // PatchOLD.Source

  def sources: expr.Map[S, String, Sonification.Source[S], Sonification.Source.Update[S]]

  /** A scalar attribute map */
  def attributes: Attributes.Modifiable[S]
}
