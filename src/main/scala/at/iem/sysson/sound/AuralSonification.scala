package at.iem.sysson
package sound

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.{AuralContext, AuralObj}
import impl.{AuralSonificationImpl => Impl}

object AuralSonification extends AuralObj.Factory {
  private lazy val _init: Unit = AuralObj.addFactory(this)

  def init(): Unit = _init

  def typeID = Sonification.typeID

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
  def status: evt.Observable[S#Tx, AuralSonification.Update /* [S] */]
}
