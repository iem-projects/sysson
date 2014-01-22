/*
 *  WorkspaceViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import scala.swing.{Swing, FlowPanel, Component, BoxPanel, Orientation, Action}
import de.sciss.desktop
import desktop.impl.UndoManagerImpl
import desktop.{UndoManager, FileDialog}
import de.sciss.file._
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.List
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import javax.swing.undo.{CannotRedoException, CannotUndoException, AbstractUndoableEdit}
import scala.concurrent.ExecutionContext
import at.iem.sysson.sound.{Keys, Sonification}
import de.sciss.synth.proc.{Attribute, ExprImplicits}
import de.sciss.lucre.synth.expr.Strings
import de.sciss.model.Change
import de.sciss.swingplus.Separator
import at.iem.sysson.gui.DragAndDrop.{DataSourceDrag, LibraryNodeDrag, LibraryNodeFlavor}
import javax.swing.{JComponent, TransferHandler}
import java.awt.datatransfer.Transferable

// XXX TODO: factor out frame
object WorkspaceViewImpl {
  import Implicits._

  def apply[S <: Sys[S]](workspace: Workspace[S])(implicit tx: S#Tx): WorkspaceView[S] = {
    import workspace.cursor
    val undoMgr = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }

    implicit val ws         = workspace
    implicit val llSrcSer   = List.serializer[S, DataSource  [S]]
    implicit val llSpcSer   = List.serializer[S, Sonification[S], Sonification.Update[S]]
    val dataSourcesHndl     = ListView.Handler[S, DataSource[S]](implicit tx => _.file.base)

    val untitled = "<untitled>"

    def sonifName(son: Sonification[S])(implicit tx: S#Tx): Option[String] =
      son.attributes[Attribute.String](Keys.attrName).map(_.value)

    val sonificationsHndl   = ListView.Handler[S, Sonification[S], Sonification.Update[S]] {
      implicit tx => sonifName(_).getOrElse(untitled)
    } {
      implicit tx => { (son, upd) =>
        (Option.empty[String] /: upd.changes) {
          case (_, Sonification.AttributeAdded  (Keys.attrName)) => sonifName(son)
          case (_, Sonification.AttributeRemoved(Keys.attrName)) => Some(untitled)
          case (_, Sonification.AttributeChange (Keys.attrName, _, Change(_, name: String))) => Some(name)
          case (x, _) => x
        }
      }
    }
    val dataSources   = ListView[S, DataSource  [S], Unit                  , String](workspace.dataSources  , dataSourcesHndl  )
    val sonifications = ListView[S, Sonification[S], Sonification.Update[S], String](workspace.sonifications, sonificationsHndl)
    val res = new Impl[S](undoMgr, dataSources, sonifications)(workspace)
    workspace.addDependent(res)
    GUI.fromTx(res.guiInit())
    res
  }

  // trait Disposable { def dispose(): Unit }

  private final val txtAddDataSource    = "Add Data Source"
  private final val txtRemoveDataSource = "Remove Data Source"
  private final val txtViewDataSource   = "View Data Source"

  private final val txtAddSonif         = "Add Sonification"
  private final val txtRemoveSonif      = "Remove Sonification"
  private final val txtViewSonif        = "View Sonification"

  private final class Impl[S <: Sys[S]](val undoManager: UndoManager, 
                                        dataSources  : ListView[S, DataSource  [S], Unit],
                                        sonifications: ListView[S, Sonification[S], Sonification.Update[S]])
                                       (implicit val workspace: Workspace[S])
    extends WorkspaceView[S] {

    impl =>

    var component: Component = _

    import workspace.cursor

    def file: File = workspace.file

    // XXX TODO: DRY (LibraryViewImpl)
    // direction: true = insert, false = remove
    private class EditNode[A](direction: Boolean,
                              parentFun: S#Tx => List.Modifiable[S, A, _],
                              index: Int,
                              childH: stm.Source[S#Tx, A])
      extends AbstractUndoableEdit {

      override def undo(): Unit = {
        super.undo()
        cursor.step { implicit tx =>
          val success = if (direction) remove() else insert()
          if (!success) throw new CannotUndoException()
        }
      }

      override def redo(): Unit = {
        super.redo()
        cursor.step { implicit tx => perform() }
      }

      override def die(): Unit = {
        val hasBeenDone = canUndo
        super.die()
        if (!hasBeenDone) {
          // XXX TODO: dispose()
        }
      }

      private def insert()(implicit tx: S#Tx): Boolean = {
        val parent = parentFun(tx)
        if (parent.size >= index) {
          val child = childH()
          parent.insert(index, child)
          true
        } else false
      }

      private def remove()(implicit tx: S#Tx): Boolean = {
        val parent = parentFun(tx)
        if (parent.size > index) {
          parent.removeAt(index)
          true
        } else false
      }

      def perform()(implicit tx: S#Tx): Unit = {
        val success = if (direction) insert() else remove()
        if (!success) throw new CannotRedoException()
      }
    }

    private class EditInsertSource(index: Int, childH: stm.Source[S#Tx, DataSource[S]])
      extends EditNode(true, workspace.dataSources(_), index, childH) {

      override def getPresentationName = txtAddDataSource
    }

    private class EditRemoveSource(index: Int, childH: stm.Source[S#Tx, DataSource[S]])
      extends EditNode(false, workspace.dataSources(_), index, childH) {

      override def getPresentationName = txtRemoveDataSource
    }

    private class EditInsertSonif(index: Int, childH: stm.Source[S#Tx, Sonification[S]])
      extends EditNode(true, workspace.sonifications(_), index, childH) {

      override def getPresentationName = txtAddSonif
    }

    private class EditRemoveSonif(index: Int, childH: stm.Source[S#Tx, Sonification[S]])
      extends EditNode(false, workspace.sonifications(_), index, childH) {

      override def getPresentationName = txtRemoveSonif
    }

    def openSourceDialog(): Unit = {
      val dlg = FileDialog.open(title = txtAddDataSource)
      dlg.setFilter(util.NetCdfFileFilter)
      dlg.show(GUI.windowOption(component)).foreach { f =>
        val edit = cursor.step { implicit tx =>
          val idx     = workspace.dataSources.size
          val ds      = DataSource[S](f.path)
          val childH  = tx.newHandle(ds)
          val _edit   = new EditInsertSource(idx, childH)
          _edit.perform()
          _edit
        }
        undoManager.add(edit)
      }
    }

    // creates and adds a Sonification instance
    def addSonification(patch: PatchOLD): Unit = {
      val imp   = ExprImplicits[S]
      import imp._
      val edit = cursor.step { implicit tx =>
        val idx   = workspace.sonifications.size
        val sonif = Sonification[S]
        sonif.patch.graph() = patch.graph
        sonif.attributes.put(Keys.attrName, Attribute.String.apply(Strings.newVar(patch.name)))
        val childH  = tx.newHandle(sonif)
        val _edit   = new EditInsertSonif(idx, childH)
        _edit.perform()
        _edit
      }
      undoManager.add(edit)
    }

    def guiInit(): Unit = {
      GUI.requireEDT()

      val actionAddSource = new Action(null) {
        def apply(): Unit = openSourceDialog()
      }
      val ggAddSource = GUI.toolButton(actionAddSource, raphael.Shapes.Plus, tooltip = s"$txtAddDataSource...")

      val actionRemoveSource = new Action(null) {
        enabled = false

        def apply(): Unit = {
          val indices = dataSources.guiSelection
          indices.headOption.foreach { idx => // XXX TODO: compound removal of multiple selection
            val edit = cursor.step { implicit tx =>
              val childH  = tx.newHandle(workspace.dataSources.apply(idx))
              val _edit   = new EditRemoveSource(idx, childH)
              _edit.perform()
              _edit
            }
            undoManager.add(edit)
          }
        }
      }
      val ggRemoveSource = GUI.toolButton(actionRemoveSource, raphael.Shapes.Minus, tooltip = txtRemoveDataSource)

      val actionViewSource = new Action(null) {
        enabled = false

        def apply(): Unit = {
          val indices = dataSources.guiSelection
          if (indices.nonEmpty) cursor.step { implicit tx =>
            indices.foreach { idx =>
              dataSources.list.foreach { ll =>
                val ds = ll(idx)
                DataSourceWindow(ds)
              }
            }
          }
        }
      }
      val ggViewSource = GUI.toolButton(actionViewSource, raphael.Shapes.View, tooltip = txtViewDataSource)

      dataSources.view.peer.setTransferHandler(new TransferHandler {
        // ---- export ----

        // dragging only works when MOVE _and_ COPY are included. Why?
        override def getSourceActions(c: JComponent): Int =
          TransferHandler.LINK | TransferHandler.COPY | TransferHandler.MOVE

        override def createTransferable(c: JComponent): Transferable = {
          // println("createTransferable")
          dataSources.guiSelection.headOption.flatMap { idx =>
            val hOpt = cursor.step { implicit tx =>
              dataSources.list.flatMap { li => li.get(idx).map(tx.newHandle(_)) }
            }
            hOpt.map { sourceH =>
              // println("AQUI")
              DragAndDrop.Transferable(DragAndDrop.DataSourceFlavor) {
                new DataSourceDrag {
                  type S1 = S
                  val workspace = impl.workspace
                  val source    = sourceH
                }
              }
            }
          } .orNull
        }
      })

      val flowSource  = new FlowPanel(ggAddSource, ggRemoveSource, ggViewSource)

      // note: listener is released upon `dataSources.dispose()`
      /* val liSources = */ dataSources.addListener {
        case ListView.SelectionChanged(indices) =>
          val selected = indices.nonEmpty
          ggRemoveSource.enabled = selected
          ggViewSource  .enabled = selected
      }

      val pageSources = new BoxPanel(Orientation.Vertical) {
        contents += dataSources.component
        contents += flowSource
        border    = Swing.TitledBorder(Swing.EmptyBorder(4), "Data Sources")
      }

      val actionRemoveSonif = new Action(null) {
        enabled = false

        def apply(): Unit = {
          val indices = sonifications.guiSelection
          indices.headOption.foreach { idx => // XXX TODO: compound removal of multiple selection
            val edit = cursor.step { implicit tx =>
              val childH  = tx.newHandle(workspace.sonifications.apply(idx))
              val _edit   = new EditRemoveSonif(idx, childH)
              _edit.perform()
              _edit
            }
            undoManager.add(edit)
          }
        }
      }
      val ggRemoveSonif = GUI.toolButton(actionRemoveSonif, raphael.Shapes.Minus, tooltip = txtRemoveSonif)

      val actionViewSonif = new Action(null) {
        enabled = false

        def apply(): Unit = {
          val indices = sonifications.guiSelection
          if (indices.nonEmpty) {
            cursor.step { implicit tx =>
              indices.foreach { idx =>
                val sonif = workspace.sonifications.apply(idx)
                SonificationWindow(workspace, sonif)
              }
            }
          }
        }
      }
      val ggViewSonif = GUI.toolButton(actionViewSonif, raphael.Shapes.View, tooltip = txtViewSonif)

      // note: listener is released upon `sonifications.dispose()`
      /* val liSonif = */ sonifications.addListener {
        case ListView.SelectionChanged(indices) =>
          val selected = indices.nonEmpty
          ggRemoveSonif.enabled = selected
          ggViewSonif  .enabled = selected
      }

      val ggBusy    = new SpinningProgressBar
      val butSonif  = DropButton[LibraryNodeDrag](LibraryNodeFlavor, "Sonification Patch From the Library") { drag =>
        val sourceOpt = drag.cursor.step { implicit tx =>
          drag.node() match {
            case TreeLike.IsLeaf(l) => Some(PatchOLD.Source(l.name.value, l.source.value))
            case _ => None
          }
        }

        sourceOpt.exists { case source =>
          import ExecutionContext.Implicits.global
          val fut         = Library.compile(source)
          ggBusy.spinning = true
          fut.onComplete(_ => ggBusy.spinning = false)
          fut.foreach(addSonification)
          true
        }
      }

      val flowSpec  = new FlowPanel(ggBusy, butSonif, ggRemoveSonif, ggViewSonif)

      val pageSpecs = new BoxPanel(Orientation.Vertical) {
        contents += sonifications.component
        contents += flowSpec
        border    = Swing.TitledBorder(Swing.EmptyBorder(4), "Sonifications")
      }

      val ggTab = new BoxPanel(Orientation.Vertical) {
        contents += pageSources
        contents += Separator()
        contents += pageSpecs
      }

      component = ggTab
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      workspace.removeDependent(this)
      dataSources  .dispose()
      sonifications.dispose()
    }
  }
}