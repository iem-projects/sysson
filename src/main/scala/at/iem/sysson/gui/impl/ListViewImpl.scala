/*
 *  ListViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import de.sciss.lucre.{stm, expr, event => evt}
import evt.Sys
import stm.{Source, Cursor, Disposable}
import expr.List
import swing.{ScrollPane, Component}
import javax.swing.DefaultListModel
import concurrent.stm.{Ref => STMRef}
import swing.event.ListSelectionChanged
import de.sciss.serial.Serializer
import GUI.{fromTx => guiFromTx, requireEDT}
import at.iem.sysson.gui.ListView.Handler
import de.sciss.model.impl.ModelImpl
import de.sciss.swingplus.Implicits._

object ListViewImpl {
  def empty[S <: Sys[S], Elem, U, Data](handler: Handler[S, Elem, U, Data])
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, List[S, Elem, U]]): ListView[S, Elem, U] = {
    val view = new Impl[S, Elem, U, Data](handler)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  def apply[S <: Sys[S], Elem, U, Data](list: List[S, Elem, U], handler: Handler[S, Elem, U, Data])
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, List[S, Elem, U]]): ListView[S, Elem, U] = {
    val view = empty[S, Elem, U, Data](handler)
    view.list_=(Some(list))
    view
  }

  private final class Impl[S <: Sys[S], Elem, U, Data](handler: Handler[S, Elem, U, Data])
                                                (implicit cursor: Cursor[S], listSer: Serializer[S#Tx, S#Acc, List[S, Elem, U]])
    extends ListView[S, Elem, U] with ComponentHolder[Component] with ModelImpl[ListView.Update] {
    view =>

    private var ggList: swing.ListView[Data] = _
    private val mList   = new DefaultListModel
    private val current = STMRef(Option.empty[(Source[S#Tx, List[S, Elem, U]], Disposable[S#Tx])])

    def view = ggList

    def list(implicit tx: S#Tx): Option[List[S, Elem, U]] =
      current.get(tx.peer).map {
        case (h, _) => h()
      }

    def list_=(newOption: Option[List[S, Elem, U]])(implicit tx: S#Tx): Unit = {
      current.get(tx.peer).foreach {
        case (_, obs) =>
          disposeObserver(obs)
      }
      val newValue = newOption.map {
        case ll =>
          val obs = createObserver(ll)
          (tx.newHandle(ll), obs)
      }
      current.set(newValue)(tx.peer)
    }

    private def disposeObserver(obs: Disposable[S#Tx])(implicit tx: S#Tx): Unit = {
      obs.dispose()
      guiFromTx {
        view.clear()
      }
    }

    private def createObserver(ll: List[S, Elem, U])(implicit tx: S#Tx): Disposable[S#Tx] = {
      val items = ll.iterator.map(handler.data).toIndexedSeq
      guiFromTx {
        view.addAll(items)
      }
      ll.changed.react { implicit tx => upd => upd.changes.foreach {
        case List.Added(  idx, elem)  => val item = handler.data(elem); guiFromTx(view.add(idx, item))
        case List.Removed(idx, elem)  => guiFromTx(view.remove(idx))
        case List.Element(elem, eu )  =>
          val idx = upd.list.indexOf(elem)
          if (idx >= 0) {
            handler.dataUpdate(elem, eu).foreach { item =>
              guiFromTx(view.update(idx, item))
            }
          }
        }
      }
    }

    private def notifyViewObservers(current: Vec[Int]): Unit = {
      val evt = ListView.SelectionChanged(current)
      dispatch(evt)
    }

    def guiSelection: Vec[Int] = {
      requireEDT()
      ggList.selection.indices.toIndexedSeq
    }

    def guiInit(): Unit = {
      requireEDT()
      //         val rend = new DefaultListCellRenderer {
      //            override def getListCellRendererComponent( c: JList, elem: Any, idx: Int, selected: Boolean, focused: Boolean ) : awt.Component = {
      //               super.getListCellRendererComponent( c, showFun( elem.asInstanceOf[ Elem ]), idx, selected, focused )
      //            }
      //         }
      ggList = new swing.ListView[Data] {
        this.dragEnabled = true
        peer.setModel(mList)
        listenTo(selection)
        reactions += {
          case l: ListSelectionChanged[_] => notifyViewObservers(l.range)
        }
      }

      component = new ScrollPane(ggList)
    }

    def clear(): Unit = mList.clear()

    def addAll(items: Vec[Data]): Unit = {
      mList.clear()
      items.foreach(mList.addElement)
    }

    def add(idx: Int, item: Data): Unit =
      mList.add(idx, item)

    def remove(idx: Int): Unit =
      mList.remove(idx)

    def update(idx: Int, newItem: Data): Unit = {
      mList.set(idx, newItem)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      list_=(None)
      guiFromTx {
        releaseListeners()
      }
    }
  }
}
