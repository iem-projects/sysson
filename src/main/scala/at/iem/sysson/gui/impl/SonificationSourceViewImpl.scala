/*
 *  SonificationSourceViewImpl.scala
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

import java.awt.datatransfer.Transferable
import javax.swing.undo.UndoableEdit

import at.iem.sysson.gui.DragAndDrop.SonificationSourceMappingDrag
import at.iem.sysson.sound.Sonification
import de.sciss.desktop.UndoManager
import de.sciss.icons.raphael
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.matrix.{Matrix, Reduce}
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.stm.{Sys, Disposable}
import de.sciss.lucre.swing.View
import de.sciss.lucre.swing.edit.EditMutableMap
import de.sciss.lucre.{expr, stm}
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.GUI
import de.sciss.serial.Serializer
import de.sciss.synth.proc.ObjKeys

import scala.annotation.tailrec
import scala.swing.{Action, Component, FlowPanel}

object SonificationSourceViewImpl {
  def apply[S <: Sys[S]](parent: SonificationView[S],
                         key: String, keyDimNames: Vec[String])
                        (implicit tx: S#Tx, workspace: Workspace[S],
                         undoManager: UndoManager, cursor: stm.Cursor[S]): SonificationSourceView[S] = {
    val res = new Impl[S](parent = parent, key = key, _keys = keyDimNames).init1()
    res
  }

  private final class Impl[S <: Sys[S]](parent: SonificationView[S], key: String, _keys: Vec[String])
                                       (implicit workspace: Workspace[S], undoManager: UndoManager, cursor: stm.Cursor[S])
    extends MatrixAssocViewImpl[S](_keys) with SonificationSourceView[S] {
    impl =>

    private var sourceObserver: Disposable[S#Tx] = _
    private var mapHOpt: Option[stm.Source[S#Tx, evt.Map.Modifiable[S, String, Sonification.Source]]] = _

    type Source[S1 <: Sys[S1]] = Sonification.Source[S1]

    def init1()(implicit tx: S#Tx): this.type = {
      val sonif = parent.sonification
      val map   = sonif.sources
      mapHOpt   = map.modifiableOption.map(tx.newHandle(_))
      sourceObserver = map.changed.react { implicit tx => upd =>
        // println(s"OBSERVE CHANGES $upd")
        upd.changes.foreach {
          case evt.Map.Added  (`key`, source) => updateSource(Some(source)) // .data.file.base))
          case evt.Map.Removed(`key`, source) => updateSource(None)
          // case evt.Map.Element(`key`, source, sourceUpdate) =>
          // XXX TODO -- this is not seen yet somehow

          // println(s"MAP.ELEMENT $sourceUpdate")
          // res.update(now       )
          case _ =>
          // println(s"OBSERVED OTHER $other (MY KEY $key")
        }
      }
      init()
      updateSource(map.get(key))
      this
    }

    override def dispose()(implicit tx: S#Tx): Unit = {
      super.dispose()
      sourceObserver.dispose()
    }

    protected def mkAssocView(source: Source[S], key: String)(implicit tx: S#Tx): View[S] =
      SonificationAssocView(source, key)

    protected def sourceSerializer: Serializer[S#Tx, S#Acc, Source[S]] = Sonification.Source.serializer[S]

    protected def canRemoveMatrix: Boolean = mapHOpt.isDefined

    protected def canSetMatrix   : Boolean = mapHOpt.isDefined

    protected def matrix(source: Source[S])(implicit tx: S#Tx): Matrix[S] = source.matrix

    protected def editRemoveMatrix()(implicit tx: S#Tx): Option[UndoableEdit] = mapHOpt.map { mapH =>
      val map = mapH()
      EditMutableMap[S, String, Sonification.Source]("Remove Matrix", map, key, None)
    }

    protected def editDropMatrix(m: Matrix[S])(implicit tx: S#Tx): Option[UndoableEdit] = mapHOpt.map { mapH =>
      val map     = mapH()
      val source  = Sonification.Source(m)
      val edit    = EditMutableMap("Assign Matrix", map, key, Some(source))
      edit
    }

    protected def mkDimAssocTransferable(src: stm.Source[S#Tx, Source[S]], key0: String): Transferable =
      DragAndDrop.Transferable(DragAndDrop.SonificationSourceMappingFlavor)(new SonificationSourceMappingDrag {
        type S1 = S
        def source: stm.Source[S#Tx, Sonification.Source[S]] = src
        def key: String = key0
        def workspace: Workspace[S] = impl.workspace
      })

    @tailrec private def findRoot(m: Matrix[S])(implicit tx: S#Tx): Matrix[S] = m match {
      case Matrix.Var(vr)   => findRoot(vr())
      case Reduce(in, _, _) => findRoot(in)
      case _ => m
    }

    private object actionPlot extends Action(null) {
      def apply(): Unit = {
        cursor.step { implicit tx =>
          matrixView.matrix.foreach { m =>
            val sonif   = parent.sonification
            val attrKey = s"plot-$key"
            val sonifA  = sonif.attr
            val plotOpt = sonifA.$[Plot](attrKey)
            val mr      = findRoot(m)
            val plot    = plotOpt.getOrElse {
              val p         = Plot[S](mr)
              val varName   = m.name: StringObj[S]
              val plotName  = sonifA.$[StringObj](ObjKeys.attrName).fold(varName) { srcName =>
                import expr.Ops._
                srcName ++ " > " ++ varName
              }
              p.attr.put(ObjKeys.attrName, plotName)
              // sonifA.$[StringObj](ObjKeys.attrName).foreach(v => p.attr.put(ObjKeys.attrName, v))
              sonifA.put(attrKey, p)
              p
            }
            val m1  = plot.matrix
            val m1r = findRoot(m1)
            if (mr != m1r) {  // sonif editor matrix was replaced
              Matrix.Var.unapply(m1).foreach { vr =>
                vr() = mr
              }
            }
            PlotFrame(plot, parent)
          }
        }
      }
    }

    override protected def mkTopComponent(c: Component): Component = {
      val ggPlot = GUI.toolButton(actionPlot, raphael.Shapes.LineChart)
      new FlowPanel(FlowPanel.Alignment.Leading)(c, ggPlot)
    }
  }
}