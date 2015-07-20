package at.iem.sysson
package sound

import de.sciss.lucre.{event => evt}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.{AuralContext, AuralObj}
import impl.{AuralSonificationImpl => Impl}

object AuralSonification extends AuralObj.Factory {
  private lazy val _init: Unit = AuralObj.addFactory(this)

  def init(): Unit = _init

  def typeID = Sonification.typeID

  type E[S <: evt.Sys[S]] = Sonification.Elem[S]

  def apply[S <: Sys[S]](obj: Sonification.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralSonification[S] =
    Impl(obj)

  sealed trait Update /* [S <: Sys[S]] */ {
    // def view: AuralSonification[S]
  }
  case class Elapsed(key: graph.Dim, ratio: Float, dimValue: Float)
    extends Update /* [S] */
}
trait AuralSonification[S <: Sys[S]] extends AuralObj[S] {
  def status: evt.Observable[S#Tx, AuralSonification.Update /* [S] */]
}
