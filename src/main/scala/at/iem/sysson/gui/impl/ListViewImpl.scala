package at.iem.sysson
package gui
package impl

import de.sciss.lucre.{stm, expr}
import stm.{Source, Cursor, Disposable, Sys}
import expr.LinkedList
import swing.{ScrollPane, Component}
import javax.swing.DefaultListModel
import concurrent.stm.{Ref => STMRef}
import swing.event.ListSelectionChanged
import de.sciss.serial.Serializer
import GUI.{fromTx => guiFromTx, requireEDT}

object ListViewImpl {
  def empty[S <: Sys[S], Elem, U](show: Elem => String)
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, LinkedList[S, Elem, U]]): ListView[S, Elem, U] = {
    val view = new Impl[S, Elem, U](show)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  def apply[S <: Sys[S], Elem, U](list: LinkedList[S, Elem, U])(show: Elem => String)
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, LinkedList[S, Elem, U]]): ListView[S, Elem, U] = {
    val view = empty[S, Elem, U](show)
    view.list_=(Some(list))
    view
  }

  private final class Impl[S <: Sys[S], Elem, U](show: Elem => String)
                                                (implicit cursor: Cursor[S], listSer: Serializer[S#Tx, S#Acc, LinkedList[S, Elem, U]])
    extends ListView[S, Elem, U] {
    view =>

    @volatile private var comp: Component = _
    @volatile private var ggList: swing.ListView[_] = _

    private val mList = new DefaultListModel

    private var viewObservers = Vec.empty[Observer]

    // private val current = STMRef( Option.empty[ (S#Acc, LinkedList[ S, Elem, U ], Disposable[ S#Tx ])])
    private val current = STMRef(Option.empty[(Source[S#Tx, LinkedList[S, Elem, U]], Disposable[S#Tx])])

    def list(implicit tx: S#Tx): Option[LinkedList[S, Elem, U]] = {
      current.get(tx.peer).map {
        case (h, _) => h()
      }
    }

    def list_=(newOption: Option[LinkedList[S, Elem, U]])(implicit tx: S#Tx): Unit = {
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

    private final class Observer(fun: PartialFunction[ListView.Update, Unit]) extends Removable {
      obs =>

      def remove(): Unit =
        viewObservers = viewObservers.filterNot(_ == obs)

      def tryApply(evt: ListView.Update): Unit =
        try {
          if (fun.isDefinedAt(evt)) fun(evt)
        } catch {
          case e: Exception => e.printStackTrace()
        }
    }

    private def disposeObserver(obs: Disposable[S#Tx])(implicit tx: S#Tx): Unit = {
      obs.dispose()
      guiFromTx {
        view.clear()
      }
    }

    private def createObserver(ll: LinkedList[S, Elem, U])(implicit tx: S#Tx): Disposable[S#Tx] = {
      val items = ll.iterator.toIndexedSeq
      guiFromTx {
        view.addAll(items.map(show))
      }
      ll.changed.react { implicit tx => upd => upd.changes.foreach {
        case LinkedList.Added(  idx, elem)  => guiFromTx(view.add(idx, show(elem)))
        case LinkedList.Removed(idx, elem)  => guiFromTx(view.remove(idx))
        case LinkedList.Element(elem, _  )  =>
          val idx = upd.list.indexOf(elem)
          if (idx >= 0) {
            val str = show(elem)
            guiFromTx(view.update(idx, str))
          }
        }
      }
    }

    private def notifyViewObservers(current: Vec[Int]): Unit = {
      val evt = ListView.SelectionChanged(current)
      viewObservers.foreach(_.tryApply(evt))
    }

    def component: Component = {
      requireEDT()
      val res = comp
      if (res == null) sys.error("Called component before GUI was initialized")
      res
    }

    def guiReact(fun: PartialFunction[ListView.Update, Unit]): Removable = {
      requireEDT()
      val obs = new Observer(fun)
      viewObservers :+= obs
      obs
    }

    def guiSelection: Vec[Int] = {
      requireEDT()
      ggList.selection.indices.toIndexedSeq
    }

    def guiInit(): Unit = {
      requireEDT()
      require(comp == null, "Initialization called twice")
      //         val rend = new DefaultListCellRenderer {
      //            override def getListCellRendererComponent( c: JList, elem: Any, idx: Int, selected: Boolean, focused: Boolean ) : awt.Component = {
      //               super.getListCellRendererComponent( c, showFun( elem.asInstanceOf[ Elem ]), idx, selected, focused )
      //            }
      //         }
      ggList = new swing.ListView {
        peer.setModel(mList)
        listenTo(selection)
        reactions += {
          case l: ListSelectionChanged[_] => notifyViewObservers(l.range)
        }
      }

      comp = new ScrollPane(ggList)
    }

    def clear(): Unit = mList.clear()

    def addAll(items: Vec[String]): Unit = {
      mList.clear()
      items.foreach(mList.addElement)
    }

    def add(idx: Int, item: String): Unit =
      mList.add(idx, item)

    def remove(idx: Int): Unit =
      mList.remove(idx)

    def update(idx: Int, item: String): Unit =
      mList.set(idx, item)

    def dispose()(implicit tx: S#Tx): Unit = {
      list_=(None)
      guiFromTx {
        viewObservers = Vec.empty
      }
    }
  }
}
