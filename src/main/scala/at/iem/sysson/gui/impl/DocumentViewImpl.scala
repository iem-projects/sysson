package at.iem.sysson
package gui
package impl

import swing.{ScrollPane, Orientation, BoxPanel, Table, Component, Frame}
import de.sciss.swingtree.tree.{Tree, ExternalTreeModel}
import ucar.nc2
import javax.swing.tree.DefaultTreeCellRenderer
import javax.swing.JTree
import de.sciss.swingtree.event.TreeNodeSelected
import collection.immutable.{IndexedSeq => IIdxSeq}
import javax.swing.table.AbstractTableModel
import annotation.{tailrec, switch}

object DocumentViewImpl {
  import Implicits._

  def apply(doc: Document): DocumentView = new Impl(doc)

  private final class GroupModel(root: nc2.Group)
    extends ExternalTreeModel[nc2.Group](root :: Nil, _.children) {

//    def test(t: Tree[nc2.Group]) {
//      // t.expandPath()
//      // t.expandsSelectedPaths
//      // t.getRowForPath()
//      // t.model.pathToTreePath()
//      // t.treePathToPath()
//      // t.selection
//    }
  }

  private object GroupRenderer extends DefaultTreeCellRenderer {
    override def getTreeCellRendererComponent(tree: JTree, value: Object, isSelected: Boolean, isExpanded: Boolean,
                                              isLeaf: Boolean, row: Int, hasFocus: Boolean) : java.awt.Component = {
      val v2 = value match {
        case g: nc2.Group =>
          val n = g.name
          if (n == "") "<untitled group>" else n
        case _ => value
      }
      super.getTreeCellRendererComponent(tree, v2, isSelected, isExpanded, isLeaf, row, hasFocus)
    }
  }

  private trait TableModel[A] extends AbstractTableModel {
    def data: IIdxSeq[A]
    def getRowCount = data.size
  }

  private final class AttrsModel(val data: IIdxSeq[nc2.Attribute]) extends TableModel[nc2.Attribute] {
    def getColumnCount  = 2 // name, value representation
    override def getColumnName(col: Int) = (col: @switch) match {
      case 0 => "Name"
      case 1 => "Value"
    }

    def getValueAt(row: Int, col: Int) = {
      val attr = data(row)
      (col: @switch) match {
        case 0 => attr.name
        case 1 => {
//          val typ = attr.dataType
          if (attr.size == 1) {
            attr.values.getObject(0).toString
          } else {
            s"${attr.size} elements of type ${attr.dataType}"
          }
        }
      }
    }
  }

  private final class VarsModel(val data: IIdxSeq[nc2.Variable]) extends TableModel[nc2.Variable] {
    def getColumnCount  = 4 // name, /* full-name, */ description, data-type, shape /* , size */
    override def getColumnName(col: Int) = (col: @switch) match {
      case 0 => "Name"
      case 1 => "Description"
      case 2 => "Data Type"
      case 3 => "Shape"
//      case 4 => "Size"
    }

    def getValueAt(row: Int, col: Int) = {
      val vr = data(row)
      (col: @switch) match {
        case 0 => vr.name
//        case 1 => attr.fullName
        case 1 /* 2 */ => vr.description.getOrElse("")
        case 2 /* 3 */ => vr.dataType
        case 3 /* 4 */ => (vr.dimensions zip vr.shape) map {
          case (dim, sz) => dim.name match {
            case Some(name) => s"${name}:${sz}"
            case _ => sz.toString
          }
        } mkString ("[", ", ", "]")
//        case 4 /* 5 */ => vr.size.asInstanceOf[AnyRef]
      }
    }
  }

  private final class Impl(val document: Document)
    extends DocumentView {

    private var _selVar = Option.empty[nc2.Variable]
    private val mGroups = new GroupModel(document.data.rootGroup)
    private val tGroups = new Tree(mGroups) {
      selection.mode = Tree.SelectionMode.Single
      renderer = Tree.Renderer.wrap(GroupRenderer)
      listenTo(selection)
      reactions += {
        case TreeNodeSelected(g: nc2.Group) => groupSelected(g)
      }
      peer.setVisibleRowCount(3)
    }

    private var mGroupAttrs = new AttrsModel(IIdxSeq.empty)
    private var mGroupVars  = new VarsModel(IIdxSeq.empty)

    private val tGroupAttrs = {
      val res                     = new Table()
      res.selection.intervalMode  = Table.IntervalMode.Single
      res.model                   = mGroupAttrs
      res
    }

    private val tGroupVars = {
      val res                     = new Table()
      res.selection.intervalMode  = Table.IntervalMode.Single
      res.model                   = mGroupVars
      res
    }

    val component = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(
        new ScrollPane(tGroups),
        new ScrollPane(tGroupAttrs),
        new ScrollPane(tGroupVars)
      )
    }

    val f = new Frame {
      title     = document.path
      contents  = component
      centerOnScreen()
      pack()
      open()
    }

    private def groupSelected(g: nc2.Group) {
      mGroupAttrs       = new AttrsModel(g.attributes)
      mGroupVars        = new VarsModel(g.variables)
      tGroupAttrs.model = mGroupAttrs
      tGroupVars.model  = mGroupVars
    }

    private def selectGroup(group: nc2.Group) {
      @tailrec def loop(path: Tree.Path[nc2.Group], g: nc2.Group): Tree.Path[nc2.Group] = {
        val p2 = g +: path
        g.parent match {
          case None => p2
          case Some(p) => loop(p2, p)
        }
      }
      val fullPath = loop(Tree.Path.empty[nc2.Group], group)
      tGroups.expandPath(fullPath)
      tGroups.selection.paths += fullPath
      tGroups.peer.makeVisible(tGroups.pathToTreePath(fullPath))
      // the following in fact kicks in automatically due to the event dispatch;
      // but the caller needs to make selections to tGroupVars for example,
      // so that update must happen synchronously. eventually we could
      // add a filter to `groupSelected` not to do the work twice.
      groupSelected(group)
    }

    def selectedVariable: Option[nc2.Variable] = {
      GUI.requireEDT()
      _selVar
    }

    def selectedVariable_=(opt: Option[nc2.Variable]) {
      GUI.requireEDT()
      _selVar = opt
      opt match {
        case Some(vr) =>
          vr.group.foreach(selectGroup)
          val row = mGroupVars.data.indexOf(vr)
          if (row >= 0) {
            tGroupVars.selection.rows += row
          }

        case None =>
          tGroupVars.selection.rows.clear()
      }
    }
  }
}