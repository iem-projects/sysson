/*
 *  SonificationObjView.scala
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

import javax.swing.undo.UndoableEdit

import at.iem.sysson.sound.Sonification
import at.iem.sysson.sound.impl.SonificationImpl.SonificationElemImpl
import de.sciss.desktop
import desktop.OptionPane
import de.sciss.icons.raphael
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Source}
import de.sciss.lucre.swing.{Window, View}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.ObjView
import de.sciss.mellite.gui.impl.ObjViewImpl
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{ExprImplicits, Folder, Obj}

import scala.swing.{Component, Label}

object SonificationObjView extends ObjView.Factory {
  type E[S <: Sys[S]] = Sonification.Elem[S]
  final val prefix  = "Sonification"
  final val icon    = ObjViewImpl.raphaelIcon(raphael.Shapes.Feed)
  final val typeID  = SonificationElemImpl.typeID

  ObjView.addFactory(this)

  def apply[S <: SSys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx): ObjView[S] = {
    val name      = obj.attr.name
    val son       = obj.elem.peer
    val procName  = son.proc.attr.name
    new SonificationObjView.Impl(tx.newHandle(obj), name = name, value = new Value(procName))
  }

  def initDialog[S <: SSys[S]](workspace: Workspace[S], folderH: Source[S#Tx, Folder[S]],
                               window: Option[desktop.Window])
                              (implicit cursor: Cursor[S]): Option[UndoableEdit] = {
    val opt = OptionPane.textInput(message = "Enter Sonification Name:",
      messageType = OptionPane.Message.Question, initial = "Sonification")
    opt.title = "Add Sonification"
    val res = opt.show(window)
    res.map { name =>
      cursor.step { implicit tx =>
        val elem  = Sonification.Elem(Sonification[S])
        val obj   = Obj(elem)
        val imp   = ExprImplicits[S]
        import imp._
        obj.attr.name = name
        ObjViewImpl.addObject(prefix, folderH(), obj)
      }
    }
  }

  private final class Value(procName: String) {
    override def toString = if (procName == "<unnamed>") "" else s"proc: $procName"
  }

  private final class Impl[S <: SSys[S]](val obj: stm.Source[S#Tx, Obj.T[S, Sonification.Elem]],
                                         var name: String, var value: Value)
    extends /* ObjView.AudioGrapheme[S] with */ ObjViewImpl.Impl[S] with ObjViewImpl.NonEditable[S] {

    def prefix  = SonificationObjView.prefix
    def icon    = SonificationObjView.icon
    def typeID  = SonificationObjView.typeID

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = update match {
      case Sonification.Update(_, ch) => false
        // XXX TODO - I don't see how the patch obj's name changes are propagated
        //        ch.exists {
        //          case Sonification.PatchChange(Change())
        //        }
    }

    def isViewable = true

    def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val frame = SonificationFrame(obj())
      Some(frame)
    }

    def configureRenderer(label: Label): Component = {
      val txt    = value.toString
      label.text = txt
      label
    }
  }
}