package at.iem.sysson
package sound

import de.sciss.lucre.{event => evt}
import evt.{Publisher, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.Attributes

object Patch {
//  object Value {
//    implicit object serializer extends ImmutableSerializer[Value] {
//
//    }
//  }
//  final case class Value(sources: Vec[graph.Var] = Vec.empty, user: Vec[graph.UserValue] = Vec.empty) {
//    def spec : SonificationSpec // XXX TODO: unite with Patch, and make Expr.Var or itself mutable
//    def patch: PatchOLD
//  }
//  object Elem extends BiTypeImpl[Value] {
//    final val typeID = 0x30000
//
//    // 0 reserved for variables
//    private final val elemCookie = 1
//
//    protected def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
//                                        (implicit tx: S#Tx): Elem.ReprNode[S] = ???
//
//    def writeValue(value: Value, out: DataOutput): Unit = ???
//
//    def readValue(in: DataInput): Value = ???
//  }
//  type Elem[S <: Sys[S]] = Expr[S, Value]

  sealed trait Update[S <: Sys[S]]

  object Keys {
    /** Name attribute. Type `String` */
    final val attrName        = "name"
    /** Source code of the graph function. */
    final val attrGraphSource = "graph-source"
  }
}
trait Patch[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Patch.Update[S]] {
  def graph: Expr.Var[S, SynthGraph]

  /** The scalar attributes of the process. */
  def attributes: Attributes.Modifiable[S]
}