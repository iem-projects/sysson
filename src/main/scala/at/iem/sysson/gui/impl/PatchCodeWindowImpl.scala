/*
 *  PatchCodeWindowImpl.scala
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

import de.sciss.desktop.{UndoManager, OptionPane}
import de.sciss.lucre.stm
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import de.sciss.swingplus.Implicits._
import de.sciss.lucre.swing._
import at.iem.sysson.sound.{Keys, Patch}
import de.sciss.lucre.expr.{Expr, String => StringEx}
import de.sciss.synth.proc.{Obj, StringElem, Elem}
import scala.concurrent.ExecutionContext
import de.sciss.desktop

object PatchCodeWindowImpl {
  def apply[S <: Sys[S]](entry: Library.Leaf[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): PatchCodeWindow[S] = {
    val view    = PatchCodeView(entry)
    mkWindow(view, entry.name)
  }

  def apply[S <: Sys[S]](patch: Obj.T[S, Patch.Elem])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): PatchCodeWindow[S] = {
    val source  = patch.attr.expr[String](Keys.attrGraphSource).fold {
      val res = StringEx.newVar[S](StringEx.newConst("// No source code found for patch!\n"))
      patch.attr.put(Keys.attrGraphSource, StringElem(res))
      res
    } {
      case Expr.Var(vr) => vr
      case ex           =>
        val res = StringEx.newVar(ex)
        patch.attr.put(Keys.attrGraphSource, StringElem(res))
        res
    }
    val view = PatchCodeView[S](source, graph = Some(patch.elem.peer.graph))
    val name = patch.attr.expr[String](Keys.attrName).getOrElse(StringEx.newConst("<Untitled>"))
    mkWindow(view, name)
  }

  private def mkWindow[S <: Sys[S]](view: PatchCodeView[S], nameEx: Expr[S, String])
                                   (implicit tx: S#Tx, cursor: stm.Cursor[S],
                                    undoManager: UndoManager): PatchCodeWindow[S] = {
    val name0   = nameEx.value
    val res     = new Impl[S](view, name0 = name0, contextName = Code.SynthGraph.name) {
      val observer = nameEx.changed.react { implicit tx => ch =>
        deferTx {
          name = ch.now
        }
      }
    }
    res.init()
    res
  }

  private def mkTitle(name: String, contextName: String) = s"$name : $contextName Code"

  private abstract class Impl[S <: Sys[S]](val view: PatchCodeView[S], name0: String, contextName: String)
                                          (implicit cursor: stm.Cursor[S])
    extends WindowImpl[S](title0 = mkTitle(name0, contextName)) with PatchCodeWindow[S] {

    // import code.{contextName, id => codeID}

    override protected def style = desktop.Window.Auxiliary

    protected def observer: Disposable[S#Tx]

    private var _name = ""

    def name = _name
    def name_=(value: String): Unit = {
      _name = value
      title = mkTitle(name, contextName)
    }

    override def checkClose(): Boolean = {
      if (view.isCompiling) return false
      if (!view.dirty     ) return true

      val message = "The code has been edited.\nDo you want to save the changes?"
      val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
        messageType = OptionPane.Message.Warning)
      opt.title = "Close Code Editor" // s"Close Code Editor - $name"
      opt.show(Some(component)) match {
        case OptionPane.Result.No  => true
        case OptionPane.Result.Yes =>
          val fut = view.save()
          fut.value.fold {
            import ExecutionContext.Implicits.global
            fut.onSuccess { case _ => defer(disposeFromGUI()) }
            false

          } (_.isSuccess)

        case OptionPane.Result.Cancel | OptionPane.Result.Closed => false
      }
    }

    private def disposeFromGUI(): Unit = {
      cursor.step { implicit tx =>
        disposeData()
      }
      component.dispose()
    }

    private def disposeData()(implicit tx: S#Tx): Unit = {
      observer.dispose()
    }

    view.addListener {
      case PatchCodeView.DirtyChange(value) => dirty = value
    }

    override def undoRedoActions = Some(view.undoAction -> view.redoAction)


    //    private class WindowImpl2(top: Component, actionApply: Action) extends WindowImpl {
    //      frame =>
    //
    //      def setDirty(value: Boolean): Unit = {
    //        dirty               = value
    //        actionApply.enabled = value
    //      }
    //      def getDirty = dirty
    //
    //    }
    //  }
  }
}