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
import annotation.switch

object DocumentViewImpl {
  import Implicits._

  def apply(doc: Document): DocumentView = new Impl(doc)

  private final class GroupModel(root: nc2.Group)
    extends ExternalTreeModel[nc2.Group](root :: Nil, _.children) {
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

  private final class AttrsModel(attrs: IIdxSeq[nc2.Attribute]) extends AbstractTableModel {
    def getRowCount     = attrs.size

    def getColumnCount  = 2 // name, value representation
    override def getColumnName(col: Int) = (col: @switch) match {
      case 0 => "Name"
      case 1 => "Value"
    }

    def getValueAt(row: Int, col: Int) = {
      val attr = attrs(row)
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

  private final class VarsModel(vrs: IIdxSeq[nc2.Variable]) extends AbstractTableModel {
    def getRowCount     = vrs.size

    def getColumnCount  = 4 // name, /* full-name, */ description, data-type, shape /* , size */
    override def getColumnName(col: Int) = (col: @switch) match {
      case 0 => "Name"
      case 1 => "Description"
      case 2 => "Data Type"
      case 3 => "Shape"
//      case 4 => "Size"
    }

    def getValueAt(row: Int, col: Int) = {
      val vr = vrs(row)
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

  private final class Impl(doc: Document)
    extends DocumentView {

    private val mGroup  = new GroupModel(doc.data.rootGroup)
    private val tGroup  = new Tree(mGroup) {
      renderer = Tree.Renderer.wrap(GroupRenderer)
      listenTo(selection)
      reactions += {
        case TreeNodeSelected(g: nc2.Group) => groupSelected(g)
      }
      peer.setVisibleRowCount(3)
    }

    val tGroupAttrs = {
      val res = new Table()
      res
    }

    val tGroupVars = {
      val res = new Table()
      res
    }

    val f = new Frame {
      title     = doc.path
      contents  = new BoxPanel(Orientation.Vertical) {
        contents ++= Seq(
          new ScrollPane(tGroup),
          new ScrollPane(tGroupAttrs),
          new ScrollPane(tGroupVars)
        )
      }
//      centerOnScreen()
      pack()
      open()
    }

    private def groupSelected(g: nc2.Group) {
      tGroupAttrs.model = new AttrsModel(g.attributes)
      tGroupVars.model  = new VarsModel(g.variables)
    }

    def component: Component = tGroup
  }
}