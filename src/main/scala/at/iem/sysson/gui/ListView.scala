package at.iem.sysson
package gui

import swing.Component
import de.sciss.lucre.{stm, expr, event => evt}
import evt.Sys
import stm.{Cursor, Disposable}
import expr.List
import impl.{ListViewImpl => Impl}
import de.sciss.serial.Serializer

object ListView {
  def apply[S <: Sys[S], Elem, U](list: List[S, Elem, U])(show: Elem => String)
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, List[S, Elem, U]])
  : ListView[S, Elem, U] = Impl(list)(show)

  def empty[S <: Sys[S], Elem, U](show: Elem => String)
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, List[S, Elem, U]])
  : ListView[S, Elem, U] = Impl.empty(show)

  sealed trait Update
  final case class SelectionChanged(current: Vec[Int]) extends Update
}
trait ListView[S <: Sys[S], Elem, U] extends Disposable[S#Tx] {
  def component: Component

  def guiReact(pf: PartialFunction[ListView.Update, Unit]): Removable

  def guiSelection: Vec[Int]

  def list(implicit tx: S#Tx): Option[List[S, Elem, U]]
  def list_=(list: Option[List[S, Elem, U]])(implicit tx: S#Tx): Unit
}
