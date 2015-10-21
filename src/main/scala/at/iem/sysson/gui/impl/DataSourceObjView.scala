/*
 *  DataSourceObjView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.desktop
import de.sciss.desktop.FileDialog
import de.sciss.file._
import de.sciss.icons.raphael
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.matrix.DataSource
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Obj, Sys}
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.impl.{ListObjViewImpl, ObjViewImpl}
import de.sciss.mellite.gui.{ActionArtifactLocation, ListObjView}
import de.sciss.synth.proc.Implicits._

import scala.swing.{Component, Label}

object DataSourceObjView extends ListObjView.Factory {
  private lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = _init

  def tpe: Obj.Type = ???

  type E[S <: Sys[S]] = DataSource[S]
  final val prefix    = "DataSource"
  final val humanName = "Data Source"
  final val icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.Database)
  final val typeID    = DataSource.typeID
  def category        = SwingApplication.categSonification

  def hasMakeDialog: Boolean = true

  def mkListView[S <: SSys[S]](ds: DataSource[S])(implicit tx: S#Tx): ListObjView[S] = {
    val f         = ds.artifact.value
    val vr        = ds.variables
    val rank      = if (vr.isEmpty) 1 else vr.map(_.reducedRank).max
    val multiDim  = vr.collect {
      case v if v.reducedRank == rank => v.name -> v.reducedShape
    }
    new DataSourceObjView.Impl(tx.newHandle(ds), value = new Value(file = f, multiDim = multiDim)).initAttrs(ds)
  }

  final case class Config[S <: Sys[S]](file: File, location: ActionArtifactLocation.QueryResult[S],
                                       workspace: Workspace[S])

  def initMakeDialog[S <: SSys[S]](workspace: Workspace[S], window: Option[desktop.Window])
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

  def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
    val (list0: List[Obj[S]], loc) = config.location match {
      case Left(source) => (Nil, source())
      case Right((name, directory)) =>
        val objLoc  = ActionArtifactLocation.create(name = name, directory = directory)
        (objLoc :: Nil, objLoc)
    }
    implicit val ws       = config.workspace
    implicit val resolver = WorkspaceResolver[S]
    val artifact          = Artifact(loc, config.file) // locM.add(config.file)
    val ds                = DataSource[S](artifact)
    val obj               = ds // Obj(DataSourceElem(ds))
    obj.name              = config.file.base
    obj :: list0
  }

  private final class Value(file: File, multiDim: List[(String, Vec[Int])]) {
    private val multiS = multiDim.map {
      case (name, shape) => s"$name ${shape.mkString("[", "][", "]")}"
    } .mkString(", ")

    override def toString = multiS // s"$multiS - ${file.base}"
  }

  private final class Impl[S <: SSys[S]](val objH: stm.Source[S#Tx, DataSource[S]], val value: Value)
    extends ObjViewImpl.Impl[S] with ListObjViewImpl.NonEditable[S] with ListObjView[S] {

    def factory = DataSourceObjView
    def prefix  = DataSourceObjView.prefix

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false

    def isViewable = true

    override def obj(implicit tx: S#Tx): DataSource[S] = objH()

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val frame = DataSourceFrame(obj)
      Some(frame)
    }

    def configureRenderer(label: Label): Component = {
      val txt    = value.toString
      label.text = txt
      label
    }
  }
}

//object DataSourceElem extends ElemCompanionImpl[DataSourceElem] {
//  final val typeID = 0x30005  // DataSource.typeID
//
//  private lazy val _init: Unit = Elem.registerExtension(this)
//
//  def init(): Unit = _init
//
//  def apply[S <: Sys[S]](peer: DataSource[S])(implicit tx: S#Tx): DataSourceElem[S] = {
//    val targets = evt.Targets[S]
//    new Impl[S](targets, peer)
//  }
//
//  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSourceElem[S] =
//    serializer[S].read(in, access)
//
//  // ---- Elem.Extension ----
//
//  /** Read identified active element */
//  def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                 (implicit tx: S#Tx): DataSourceElem[S] with evt.Node[S] = {
//    val peer = DataSource.read(in, access)
//    new Impl[S](targets, peer)
//  }
//
//  /** Read identified constant element */
//  def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): DataSourceElem[S] =
//    sys.error("Constant DataSource not supported")
//
//  // ---- implementation ----
//
//  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
//                                        val peer: DataSource[S])
//    extends DataSourceElem[S]
//    with BasicElemImpl[S] with evt.Node[S] {
//
//    def typeID = DataSourceElem.typeID
//    def prefix = "DataSource"
//
//    override def toString() = s"$prefix$id"
//
//    override def changed: EventLike[S, Elem.Update[S, PeerUpdate]] = evt.Dummy[S, Elem.Update[S, PeerUpdate]]
//
//    def select(slot: Int): evt.Event[S, Any, Any] = sys.error("No event")
//
//    def mkCopy()(implicit tx: S#Tx): DataSourceElem[S] = DataSourceElem(peer) // XXX TODO
//  }
//
//  object Obj {
//    def unapply[S <: Sys[S]](obj: proc.Obj[S]): Option[Obj[S]] =
//      if (obj.elem.isInstanceOf[DataSourceElem[S]]) Some(obj.asInstanceOf[Obj[S]])
//      else None
//  }
//
//  type Obj[S <: Sys[S]] = proc.Obj.T[S, DataSourceElem]
//}
//trait DataSourceElem[S <: Sys[S]] extends Elem[S] {
//  type Peer       = DataSource[S] // Expr[S, DataSource]
//  type PeerUpdate = Unit // model.Change[String]
//  type This       = DataSourceElem[S]
//}
