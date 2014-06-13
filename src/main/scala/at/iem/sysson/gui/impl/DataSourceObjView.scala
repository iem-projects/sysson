package at.iem.sysson
package gui
package impl

import de.sciss.file._
import de.sciss.lucre.swing.View
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.ObjView
import de.sciss.lucre.stm.{Cursor, Source}
import de.sciss.synth.proc.{Elem, Obj, Folder}
import de.sciss.desktop.{FileDialog, Window}
import javax.swing.undo.UndoableEdit
import de.sciss.mellite.gui.impl.ObjViewImpl
import de.sciss.icons.raphael
import de.sciss.lucre.{event => evt, stm}
import de.sciss.synth.proc
import de.sciss.serial.DataInput
import de.sciss.lucre.matrix.DataSource
import de.sciss.synth.proc.impl.ElemImpl
import de.sciss.lucre.event.{EventLike, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import proc.Implicits._

import scala.swing.{Label, Component}

object DataSourceObjView extends ObjView.Factory {
  ObjView.addFactory(this)

  type E[S <: Sys[S]] = DataSourceElem[S]
  final val prefix  = "DataSource"
  final val icon    = ObjViewImpl.raphaelIcon(raphael.Shapes.Database)
  final val typeID  = DataSourceElem.typeID

  def apply[S <: SSys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx): ObjView[S] = {
    val name      = obj.attr.name
    val ds        = obj.elem.peer
    val f         = ds.file
    val vr        = ds.variables
    val rank      = if (vr.isEmpty) 1 else vr.map(_.reducedRank).max
    val multiDim  = vr.collect {
      case v if v.reducedRank == rank => v.name -> v.reducedShape
    }
    new DataSourceObjView.Impl(tx.newHandle(obj), name = name, value = new Value(file = f, multiDim = multiDim))
  }

  def initDialog[S <: SSys[S]](workspace: Workspace[S], folderH: Source[S#Tx, Folder[S]], window: Option[Window])
                             (implicit cursor: Cursor[S]): Option[UndoableEdit] = {
    val dlg = FileDialog.open(title = "Add Data Source")
    dlg.setFilter(util.NetCdfFileFilter)
    dlg.show(window).map { f =>
      val edit = cursor.step { implicit tx =>
        implicit val ws       = workspace
        implicit val resolver = WorkspaceResolver[S]
        val ds  = DataSource[S](f)
        val obj = Obj(DataSourceElem(ds))
        obj.attr.name = ds.file.base
        ObjViewImpl.addObject[S](prefix, folderH(), obj)
      }
      edit
    }
  }

  private final class Value(file: File, multiDim: List[(String, Vec[Int])]) {
    private val multiS = multiDim.map {
      case (name, shape) => s"$name ${shape.mkString("[", "][", "]")}"
    } .mkString(", ")

    override def toString = multiS // s"$multiS - ${file.base}"
  }

  private final class Impl[S <: SSys[S]](val obj: stm.Source[S#Tx, Obj.T[S, DataSourceElem]],
                                var name: String, val value: Value)
    extends /* ObjView.AudioGrapheme[S] with */ ObjViewImpl.Impl[S] with ObjViewImpl.NonEditable[S] {

    def prefix  = DataSourceObjView.prefix
    def icon    = DataSourceObjView.icon

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false

    def isViewable = true

    def openView(workspace: Workspace[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[View[S]] = {
      implicit val ws = workspace
      val frame = DataSourceWindow(obj())
      Some(frame.view)
    }

    def configureRenderer(label: Label): Component = {
      val txt    = value.toString
      label.text = txt
      label
    }
  }
}

object DataSourceElem extends ElemImpl.Companion[DataSourceElem] {
  final val typeID = 0x30005  // DataSource.typeID

  Elem.registerExtension(this)

  def apply[S <: Sys[S]](peer: DataSource[S])(implicit tx: S#Tx): DataSourceElem[S] = {
    val targets = evt.Targets[S]
    new Impl[S](targets, peer)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSourceElem[S] =
    serializer[S].read(in, access)

  // ---- Elem.Extension ----

  /** Read identified active element */
  def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): DataSourceElem[S] with evt.Node[S] = {
    val peer = DataSource.read(in, access)
    new Impl[S](targets, peer)
  }

  /** Read identified constant element */
  def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): DataSourceElem[S] =
    sys.error("Constant DataSource not supported")

  // ---- implementation ----

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        val peer: DataSource[S])
    extends DataSourceElem[S]
    with ElemImpl.Basic[S] with evt.Node[S] {

    def typeID = DataSourceElem.typeID
    def prefix = "DataSource"

    override def toString() = s"$prefix$id"

    override def changed: EventLike[S, Elem.Update[S, PeerUpdate]] = evt.Dummy[S, Elem.Update[S, PeerUpdate]]

    def select(slot: Int): evt.Event[S, Any, Any] = sys.error("No event")

    def mkCopy()(implicit tx: S#Tx): DataSourceElem[S] = DataSourceElem(peer) // XXX TODO
  }

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, DataSourceElem]] =
      if (obj.elem.isInstanceOf[DataSourceElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, DataSourceElem]])
      else None
  }
}
trait DataSourceElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = DataSource[S] // Expr[S, DataSource]
  type PeerUpdate = Unit // model.Change[String]
}
