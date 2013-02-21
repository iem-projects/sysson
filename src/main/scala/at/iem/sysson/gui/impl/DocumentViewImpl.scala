package at.iem.sysson
package gui
package impl

import swing.{ScrollPane, Orientation, BoxPanel, Table, Frame}
import de.sciss.swingtree.tree.{Tree, ExternalTreeModel}
import ucar.nc2
import javax.swing.tree.DefaultTreeCellRenderer
import javax.swing.{WindowConstants, JTree}
import de.sciss.swingtree.event.TreeNodeSelected
import collection.immutable.{IndexedSeq => IIdxSeq}
import javax.swing.table.AbstractTableModel
import annotation.{tailrec, switch}
import de.sciss.audiowidgets.Transport
import swing.event.{WindowClosing, TableRowsSelected}
import sound.AudioSystem
import java.io.File
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFileSpec, AudioFile}
import de.sciss.synth

private[impl] object DocumentViewImpl {
  import Implicits._

  def apply(doc: Document): DocumentView with Disposable = new Impl(doc)

  trait Disposable { def dispose(): Unit }

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
            case Some(name) => s"$name:$sz"
            case _ => sz.toString
          }
        } mkString ("[", ", ", "]")
//        case 4 /* 5 */ => vr.size.asInstanceOf[AnyRef]
      }
    }
  }

  private final class Impl(val document: Document)
    extends DocumentView with Disposable {

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

    private val tGroupVars = new Table() {
      selection.intervalMode  = Table.IntervalMode.Single
      model                   = mGroupVars
      listenTo(selection)
      reactions += {
        case TableRowsSelected(_, range, adjusting) if !adjusting =>
          val vro   = if (range.isEmpty) None else {
            val data  = mGroupVars.data
//            val row   = range.end // weird scala-swing convention! screw the range, let's use the leadIndex instead
            val row = selection.rows.leadIndex
//            println("IDX =" + row)
            if (row >= 0 && data.size > row) Some(data(row)) else None
          }
          deafTo(selection) // avoid feedback
          try {
//            println("SELECTING" + vro.map(_.name))
            selectedVariable = vro
          } finally {
            listenTo(selection)
          }
      }
    }

    private val transport = Transport.makeButtonStrip {
      import Transport._
      Seq(
        GoToBegin {
          println("Go-to-begin")
        },
        Rewind {
          println("Rewind")
        },
        Stop {
          println("Stop")
        },
        Play {
          play()
        },
        FastForward {
          println("Fast-forward")
        }
      )
    }

    val component = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(
        new ScrollPane(tGroups),
        new ScrollPane(tGroupAttrs),
        new ScrollPane(tGroupVars),
        transport
      )
    }

    val f = new Frame {
      title     = document.path
      contents  = component
      peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
      listenTo(this)
      reactions += {
        case WindowClosing(_) =>
           // this will be recognized by the DocumentViewHandler which invokes dispose() on this view subsequently:
          document.close()
      }
      menuBar   = {
        val r = MenuFactory.root
//        r("file")("close").actionFor(this) { ... }
        r.create(this)
      }
      pack()
      centerOnScreen()
      open()
    }

    def dispose() {
      MenuFactory.root.destroy(f)
      f.dispose()
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
      val sel = tGroupVars.selection.rows
      opt match {
        case Some(vr) =>
          vr.group.foreach(selectGroup)
          val row = mGroupVars.data.indexOf(vr)
          if (row >= 0 && sel.leadIndex != row) sel += row

        case None =>
          if (sel.size != 0) sel.clear()
      }
    }

    private def play() {
      selectedVariable.foreach { vr =>
//        println("rank = " + vr.rank)
        if (vr.dataType.isFloatingPoint) {
//          (vr.rank: @switch) match {
//            case 1 =>
//            case 2 =>
//            case 3 =>
//            case _ =>
//          }

          // just for testing purposes
          val data1d    = vr.read.f1d
          val dataLen   = data1d.size
          if (dataLen > 0) {
            val duration  = 5.0
            val frameRate = duration / dataLen
//            val noise     = frameRate < 200
            AudioSystem.instance.server match {
              case Some(server: synth.Server) =>
                val fTmp  = File.createTempFile("sysson", ".aif")
                fTmp.deleteOnExit()
                val afTmp = AudioFile.openWrite(fTmp,
                  AudioFileSpec(AudioFileType.AIFF, SampleFormat.Float, numChannels = 1, sampleRate = 44100, numFrames = dataLen))
                try {
                  val dataN = Array(data1d.dropNaNs.normalize.toArray)
                  afTmp.write(dataN)
                  afTmp.close()
                  import synth._
                  import ugen._
                  import Ops._
                  val df = SynthDef("temp") {
                    val disk  = VDiskIn.ar(1, "buf".ir, "speed".ir(1), loop = 0)
  //                  val n     = "noise".ir
  //                  val noise = LFNoise1.
  //                  val sig   = Select.ar(n, Seq(disk, disk * noise))
                    val sig   = SinOsc.ar(disk.linexp(0, 1, 300, 3000))
                    Line.kr(0, 0, dur = "dur".ir, doneAction = freeSelf)
                    Out.ar(0, Pan2.ar(sig))
                  }
                  Buffer.cue(server, fTmp.getAbsolutePath, numChannels = 1, completion = { (b: Buffer) =>
                    val synth = df.play(server, Seq("buf" -> b.id, "speed" -> frameRate, "dur" -> duration))
                    synth.onEnd { b.close(b.freeMsg()) }
                  })

                } finally {
                  if (afTmp.isOpen) afTmp.close()
                }

              case _ =>
            }
          }
        }
      }
    }
  }
}