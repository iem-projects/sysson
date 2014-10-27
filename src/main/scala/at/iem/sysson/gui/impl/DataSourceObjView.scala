/*
 *  DataSourceObjView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import de.sciss.file._
import de.sciss.lucre.swing.Window
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.{ActionArtifactLocation, ObjView}
import de.sciss.lucre.stm.{Cursor, Source}
import de.sciss.synth.proc.{Elem, Obj, Folder}
import de.sciss.desktop
import desktop.FileDialog
import javax.swing.undo.UndoableEdit
import de.sciss.mellite.gui.impl.ObjViewImpl
import de.sciss.icons.raphael
import de.sciss.lucre.{event => evt, stm}
import de.sciss.synth.proc
import de.sciss.serial.DataInput
import de.sciss.lucre.matrix.DataSource
import de.sciss.synth.proc.impl.{BasicElemImpl, ElemCompanionImpl}
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
    val name      = obj.name
    val ds        = obj.elem.peer
    val f         = ds.artifact.value
    val vr        = ds.variables
    val rank      = if (vr.isEmpty) 1 else vr.map(_.reducedRank).max
    val multiDim  = vr.collect {
      case v if v.reducedRank == rank => v.name -> v.reducedShape
    }
    new DataSourceObjView.Impl(tx.newHandle(obj), name = name, value = new Value(file = f, multiDim = multiDim))
  }

  final case class Config[S <: Sys[S]](file: File, location: ActionArtifactLocation.QueryResult[S],
                                       workspace: Workspace[S])

  def initDialog[S <: SSys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                             (implicit cursor: Cursor[S]): Option[Config[S]] = {
    val dlg = FileDialog.open(title = "Add Data Source")
    dlg.setFilter(util.NetCdfFileFilter)
    val fOpt = dlg.show(window)

    fOpt.flatMap { f =>
      ActionArtifactLocation.query[S](workspace.rootH, file = f, window = window).map { location =>
        Config(file = f, location = location, workspace = workspace)
      }
    }
  }

  def make[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
    val (list0: List[Obj[S]], loc) = config.location match {
      case Left(source) => (Nil, source())
      case Right((name, directory)) =>
        val objLoc  = ActionArtifactLocation.create(name = name, directory = directory)
        (objLoc :: Nil, objLoc)
    }
    loc.elem.peer.modifiableOption.fold(list0) { locM =>
      implicit val ws       = config.workspace
      implicit val resolver = WorkspaceResolver[S]
      val artifact          = locM.add(config.file)
      val ds                = DataSource[S](artifact)
      val obj               = Obj(DataSourceElem(ds))
      obj.name              = config.file.base
      obj :: list0
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
    def typeID  = DataSourceObjView.typeID

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false

    def isViewable = true

    def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val frame = DataSourceFrame(obj())
      Some(frame)
    }

    def configureRenderer(label: Label): Component = {
      val txt    = value.toString
      label.text = txt
      label
    }
  }
}

object DataSourceElem extends ElemCompanionImpl[DataSourceElem] {
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
    with BasicElemImpl[S] with evt.Node[S] {

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
