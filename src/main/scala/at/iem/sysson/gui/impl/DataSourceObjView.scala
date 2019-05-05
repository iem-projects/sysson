/*
 *  DataSourceObjView.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.gui.impl.objview.{ListObjViewImpl, ObjViewImpl}
import de.sciss.mellite.gui.{ActionArtifactLocation, ListObjView, ObjView}
import de.sciss.processor.Processor.Aborted
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Universe, Workspace}
import javax.swing.Icon

import scala.swing.{Component, Label}
import scala.util.{Failure, Success}

object DataSourceObjView extends ListObjView.Factory {
  private lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = _init

  def tpe: Obj.Type = DataSource

  type E[S <: Sys[S]]   = DataSource[S]
  final val prefix      = "DataSource"
  final val humanName   = "Data Source"
  final val icon: Icon  = ObjViewImpl.raphaelIcon(raphael.Shapes.Database)
  final val typeId: Int = DataSource.typeId
  def category: String  = SwingApplication.categSonification

  def canMakeObj: Boolean = true

  def initMakeCmdLine[S <: SSys[S]](args: List[String])(implicit universe: Universe[S]): MakeResult[S] = ???

  def mkListView[S <: SSys[S]](ds: DataSource[S])(implicit tx: S#Tx): ListObjView[S] = {
    val f         = ds.artifact.value
    val vr        = ds.variables
    val rank      = if (vr.isEmpty) 1 else vr.map(_.reducedRank).max
    val multiDim  = vr.collect {
      case v if v.reducedRank == rank => v.name -> v.reducedShape
    }
    new DataSourceObjView.Impl(tx.newHandle(ds), value = new Value(file = f, multiDim = multiDim)).initAttrs(ds)
  }

  final case class Config[S <: Sys[S]](files: List[File], location: ActionArtifactLocation.QueryResult[S],
                                       workspace: Workspace[S])

  def initMakeDialog[S <: SSys[S]](window: Option[desktop.Window])(done: MakeResult[S] => Unit)
                                 (implicit universe: Universe[S]): Unit = {
    val dlg = FileDialog.open(title = "Add Data Source")
    dlg.setFilter(util.NetCdfFileFilter)
    dlg.multiple  = true
    val fOpt      = dlg.show(window)
    import universe.workspace
    val res: Option[Config[S]] = fOpt.flatMap { f0 =>
      val q = ActionArtifactLocation.query[S](file = f0, window = window)(implicit tx => workspace.root)
      q.map { location =>
        Config(files = dlg.files, location = location, workspace = workspace)
      }
    }

    val tr = res match {
      case Some(loc)  => Success(loc)
      case None       => Failure(Aborted())
    }

    done(tr)
  }

  def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
    val (list0: List[Obj[S]], loc) = config.location match {
      case (Left(source), _) => (Nil, source())
      case (Right(name), directory) =>
        val objLoc  = ActionArtifactLocation.create(name = name, directory = directory)
        (objLoc :: Nil, objLoc)
    }
    implicit val ws: Workspace[S] = config.workspace
    implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]
    val obj = config.files.map { f =>
      val artifact = Artifact(loc, f)
      val ds       = DataSource[S](artifact)
      ds.name      = f.base
      ds
    }
    list0 ++ obj
  }

  private final class Value(file: File, multiDim: List[(String, Vec[Int])]) {
    private val multiS = multiDim.map {
      case (name, shape) => s"$name ${shape.mkString("[", "][", "]")}"
    } .mkString(", ")

    override def toString: String = multiS // s"$multiS - ${file.base}"
  }

  private final class Impl[S <: SSys[S]](val objH: stm.Source[S#Tx, DataSource[S]], val value: Value)
    extends ObjViewImpl.Impl[S] with ListObjViewImpl.NonEditable[S] with ListObjView[S] {

    def factory: ObjView.Factory  = DataSourceObjView
    def prefix: String            = DataSourceObjView.prefix

//    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false

    def isViewable = true

    override def obj(implicit tx: S#Tx): DataSource[S] = objH()

    def openView(parent: Option[Window[S]])(implicit tx: S#Tx, universe: Universe[S]): Option[Window[S]] = {
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