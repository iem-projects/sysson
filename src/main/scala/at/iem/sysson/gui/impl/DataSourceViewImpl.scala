/*
 *  DataSourceViewImpl.scala
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

import java.awt.datatransfer.Transferable

import at.iem.sysson
import javax.swing.table.AbstractTableModel
import javax.swing.tree.DefaultTreeCellRenderer
import javax.swing.{JComponent, JTree, TransferHandler}
import at.iem.sysson.gui.DragAndDrop.MatrixDrag
import de.sciss.equal
import de.sciss.file._
import de.sciss.icons.raphael
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm.{Source, Sys}
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.{expr, stm}
import de.sciss.mellite.gui.GUI
import de.sciss.model.impl.ModelImpl
import de.sciss.swingtree.event.TreeNodeSelected
import de.sciss.swingtree.{ExternalTreeModel, Tree}
import de.sciss.synth.proc.{ObjKeys, Universe, Workspace}
import ucar.nc2

import scala.annotation.switch
import scala.swing.Swing._
import scala.swing.event.TableRowsSelected
import scala.swing.{Action, BorderPanel, BoxPanel, Component, Orientation, ScrollPane, SplitPane, Table}

object DataSourceViewImpl {
  import at.iem.sysson.Implicits._

  def apply[S <: Sys[S]](source: DataSource[S])(implicit tx: S#Tx, universe: Universe[S]): DataSourceView[S] = {
    import universe.workspace
    implicit val resolver: DataSource.Resolver[S] = WorkspaceResolver[S]
    val data  = source.data()
    val docH  = tx.newHandle(source)
    val res   = new Impl[S](docH, source.artifact.value, data)
    deferTx(res.guiInit())
    res
  }

  private final class GroupModel(root: nc2.Group)
    extends ExternalTreeModel[nc2.Group](root :: Nil, _.children) {
  }

  private object GroupRenderer extends DefaultTreeCellRenderer {
    override def getTreeCellRendererComponent(tree: JTree, value: Object, isSelected: Boolean, isExpanded: Boolean,
                                              isLeaf: Boolean, row: Int, hasFocus: Boolean) : java.awt.Component = {
      val v2 = value match {
        case g: nc2.Group =>
          val n = g.name
          import equal.Implicits._
          if (n === "") "<untitled group>" else n
        case _ => value
      }
      super.getTreeCellRendererComponent(tree, v2, isSelected, isExpanded, isLeaf, row, hasFocus)
    }
  }

  private trait TableModel[A] extends AbstractTableModel {
    def data: Vec[A]
    def getRowCount: Int = data.size
  }

  private final class AttrsModel(val data: Vec[nc2.Attribute]) extends TableModel[nc2.Attribute] {
    def getColumnCount  = 2 // name, value representation
    override def getColumnName(col: Int): String = col match {
      case 0 => "Name"
      case 1 => "Value"
    }

    def getValueAt(row: Int, col: Int): String = {
      val attr = data(row)
      col match {
        case 0 => attr.name
        case 1 =>
          //          val typ = attr.dataType
          if (attr.size == 1) {
            attr.values.getObject(0).toString
          } else {
            s"${attr.size} elements of type ${attr.dataType}"
          }
      }
    }
  }

  private final class VarsModel(val data: Vec[nc2.Variable]) extends TableModel[nc2.Variable] {
    def getColumnCount = 5 // name, /* full-name, */ description, data-type, shape /* , size */
    override def getColumnName(col: Int): String = (col: @switch) match {
      case 0 => "Name"
      case 1 => "Description"
      case 2 => "Data Type"
      case 3 => "Shape"
      case 4 => "Units"
    }

    def getValueAt(row: Int, col: Int): AnyRef = {
      val vr = data(row)
      (col: @switch) match {
        case 0 => vr.name
//        case 1 => attr.fullName
        case 1 /* 2 */ => vr.description.getOrElse("")
        case 2 /* 3 */ => vr.dataType
        case 3 /* 4 */ => (vr.dimensions zip vr.shape).map {
          case (dim, sz) => dim.nameOption match {
            case Some(name) => s"$name:$sz"
            case _ => sz.toString
          }
        } mkString ("[", ", ", "]")

        case 4 => vr.units.getOrElse("")
      }
    }
  }

  private final class Impl[S <: Sys[S]](sourceH: stm.Source[S#Tx, DataSource[S]],
                                        val file: File, data: nc2.NetcdfFile)
                                       (implicit val universe: Universe[S])
    extends DataSourceView[S] with ComponentHolder[Component] with ModelImpl[DataSourceView.Update] {
    impl =>

    type C = Component
    
    private[this] var _selVar     = Option.empty[nc2.Variable]

    private[this] var mGroupAttrs = new AttrsModel(Vec.empty)
    private[this] var mGroupVars  = new VarsModel (Vec.empty)

    private[this] var tGroupAttrs: Table = _
    private[this] var tGroupVars : Table = _

    private[this] var tGroups    : Tree[nc2.Group]  = _

    def source(implicit tx: S#Tx): DataSource[S] = sourceH()

    private def performPlot(prefix: String)(fun: S#Tx => Plot[S] => Unit): Unit =
      selectedVariable.foreach { vr =>
        cursor.step { implicit tx =>
          val key     = vr.name
          val attrKey = s"$prefix-$key"
          val src     = source
          val srcAttr = src.attr
          val plotOpt = srcAttr.$[Plot](attrKey) // .flatMap(Plot.Obj.unapply)
          val plot    = plotOpt.orElse {
            mkMatrix(vr).map { mr =>
              val p         = Plot[S](mr)
              val varName   = key: StringObj[S]
              val plotName  = srcAttr.$[StringObj](ObjKeys.attrName).fold(varName) { srcName =>
                import expr.Ops._
                srcName ++ " > " ++ varName
              }
              p.attr.put(ObjKeys.attrName, plotName)
              val po  = p // Obj(Plot.Elem(p))
              src.attr.put(attrKey, po)
              po
            }
          }
          plot.foreach(fun(tx)(_))
        }
      }

    def guiInit(): Unit = {
      requireEDT()

      val mGroups = new GroupModel(data.rootGroup)
      tGroups = new Tree(mGroups) {
        selection.mode  = Tree.SelectionMode.Single
        renderer        = Tree.Renderer.wrap(GroupRenderer)
        listenTo(selection)
        reactions += {
          case TreeNodeSelected(g: nc2.Group) => groupSelected(g)
        }
        peer.setVisibleRowCount(3)
      }

      tGroupAttrs = {
        val res                     = new Table()
        res.selection.intervalMode  = Table.IntervalMode.Single
        res.model                   = mGroupAttrs
        res
      }

      tGroupVars = new Table() {
        selection.intervalMode  = Table.IntervalMode.Single
        model                   = mGroupVars
        listenTo(selection)
        reactions += {
          case TableRowsSelected(_, range, adjusting) if !adjusting =>
            val vro: Option[nc2.Variable] = if (range.isEmpty) None else {
              val data = mGroupVars.data
              // val row   = range.end // weird scala-swing convention! screw the range, let's use the leadIndex instead
              val row = selection.rows.leadIndex
              if (row >= 0 && data.size > row) Some(data(row)) else None
            }
            deafTo(selection) // avoid feedback
            try {
              selectedVariable = vro
            } finally {
              listenTo(selection)
            }
        }
        
        peer.setDragEnabled(true)
        peer.setTransferHandler(new TransferHandler {
          // ---- export ----

          // dragging only works when MOVE _and_ COPY are included. Why?
          override def getSourceActions(c: JComponent): Int =
            TransferHandler.LINK | TransferHandler.COPY | TransferHandler.MOVE

          override def createTransferable(c: JComponent): Transferable = {
            mkSelectedMatrix().map { varH =>
              DragAndDrop.Transferable(DragAndDrop.MatrixFlavor) {
                new MatrixDrag {
                  type S1 = S
                  val workspace : Workspace[S]            = impl.universe.workspace
                  val matrix    : Source[S#Tx, Matrix[S]] = varH
                }
              }
            } .orNull
          }
        })
      }

      val actionPlot   = Action("Plot" )(performPlot("plot" ) { implicit tx => plot => PlotFrame            (plot) })
      val actionSpread = Action("Table")(performPlot("table") { implicit tx => plot => PlotFrame.spreadsheet(plot) })

      tGroups.selectInterval(0, 0)

      val ggPlot    = GUI.toolButton(actionPlot  , raphael    .Shapes.LineChart   , tooltip = "Create 2D Plot"  )
      val ggSpread  = GUI.toolButton(actionSpread, sysson.gui .Shapes.Spreadsheet , tooltip = "Open Data Table" )
      tGroupAttrs.preferredViewportSize = tGroupAttrs.preferredSize // WTF? why do we even have to do this explicitly?
      tGroupVars .preferredViewportSize = tGroupVars .preferredSize // WTF? why do we even have to do this explicitly?
      val ggSplit = new SplitPane(Orientation.Horizontal, new ScrollPane(tGroupAttrs), new ScrollPane(tGroupVars))
      // println(s"tGroupAttrs: ${tGroupAttrs.preferredSize}")
      // println(s"tGroupVars : ${tGroupVars .preferredSize}")

      component = new BorderPanel {
        if (data.rootGroup.children.nonEmpty) {
          add(new ScrollPane(tGroups), BorderPanel.Position.North)
        }
        add(ggSplit, BorderPanel.Position.Center)
        add(new BoxPanel(Orientation.Horizontal) {
          contents += HGlue
          contents += ggSpread
          contents += ggPlot
          contents += HGlue
        }, BorderPanel.Position.South)
      }
    }

    def mkSelectedMatrix(): Option[stm.Source[S#Tx, Matrix[S]]] =
      selectedVariable.flatMap { vr =>
        val varHOpt = impl.cursor.step { implicit tx =>
          mkMatrix(vr).map(tx.newHandle(_))
        }
        varHOpt
      }

    private def mkMatrix(vr: nc2.Variable)(implicit tx: S#Tx): Option[Matrix[S]] = {
      val ds      = source // .elem.peer
      val p       = vr.parents
      val n       = vr.name
      val dsvOpt  = ds.variables.find(dsv => dsv.name == n && dsv.parents == p)
      dsvOpt
    }

    def dispose()(implicit tx: S#Tx): Unit = () // : Unit = GUI.fromTx(disposeGUI())

    private def groupSelected(g: nc2.Group): Unit = {
      mGroupAttrs       = new AttrsModel(g.attributes)
      mGroupVars        = new VarsModel (g.variables )
      tGroupAttrs.model = mGroupAttrs
      tGroupVars .model = mGroupVars
    }

//    private def selectGroup(group: nc2.Group): Unit = {
//      @tailrec def loop(path: Tree.Path[nc2.Group], g: nc2.Group): Tree.Path[nc2.Group] = {
//        val p2 = g +: path
//        g.parentOption match {
//          case Some(p)  => loop(p2, p)
//          case _        => p2
//        }
//      }
//      val fullPath = loop(Tree.Path.empty[nc2.Group], group)
//      tGroups.expandPath(fullPath)
//      tGroups.selection.paths += fullPath
//      tGroups.peer.makeVisible(tGroups.pathToTreePath(fullPath))
//      // the following in fact kicks in automatically due to the event dispatch;
//      // but the caller needs to make selections to tGroupVars for example,
//      // so that update must happen synchronously. eventually we could
//      // add a filter to `groupSelected` not to do the work twice.
//      groupSelected(group)    // XXX TODO - problematic -- resets table column widths
//    }

    def selectedVariable: Option[nc2.Variable] = {
      requireEDT()
      _selVar
    }

    def selectedVariable_=(opt: Option[nc2.Variable]): Unit = {
      requireEDT()
      if (_selVar != opt) {
        _selVar = opt
        val sel = tGroupVars.selection.rows
        opt match {
          case Some(vr) =>
            // the following line is not needed, we only have one group in all
            // the common files; the problem of `selectGroup` is that it
            // re-adjusts all the table column widths
            //  selectGroup(vr.group)
            val row = mGroupVars.data.indexOf(vr)
            if (row >= 0 && sel.leadIndex != row) sel += row

          case None =>
            if (sel.size != 0) sel.clear()
        }
        dispatch(DataSourceView.VariableSelection(opt))
      }
    }
  }
}