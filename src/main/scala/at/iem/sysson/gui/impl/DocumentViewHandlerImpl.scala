/*
 *  DocumentViewHandlerImpl.scala
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

import de.sciss.model.impl.ModelImpl
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import scala.concurrent.stm.TMap
import de.sciss.desktop.Desktop

private[gui] object DocumentViewHandlerImpl {
  import DocumentHandler.Document

  def instance: DocumentViewHandler = impl

  private lazy val impl = new Impl

  def mkWindow[S <: Sys[S]](doc: Workspace[S])(implicit tx: S#Tx): WorkspaceWindow[S] = impl.mkWindow(doc)

  private final class Impl extends DocumentViewHandler with ModelImpl[DocumentViewHandler.Update] {
    override def toString = "DocumentViewHandler"

    private val map     = TMap  .empty[Document, WorkspaceWindow[_]]
    private var _active = Option.empty[Document]

    Desktop.addListener {
      case Desktop.OpenFiles(_, files) =>
        // println(s"TODO: $open; EDT? ${java.awt.EventQueue.isDispatchThread}")
        files.foreach { f =>
          ActionOpenWorkspace.perform(f)
        }
    }

    def activeDocument = _active
    def activeDocument_=[S <: Sys[S]](value: Option[Workspace[S]]): Unit = {
      GUI.requireEDT()
      if (_active != value) {
        _active = value
        value.foreach { doc =>
          dispatch(DocumentViewHandler.Activated(doc))
        }
      }
    }

    def getWindow[S <: Sys[S]](doc: Workspace[S]): Option[WorkspaceWindow[S]] = {
      GUI.requireEDT()
      map.single.get(doc).asInstanceOf[Option[WorkspaceWindow[S]]]
    }

    def mkWindow[S <: Sys[S]](doc: Workspace[S])(implicit tx: S#Tx): WorkspaceWindow[S] =
      map.get(doc)(tx.peer).asInstanceOf[Option[WorkspaceWindow[S]]].getOrElse {
        val w = WorkspaceWindow(doc)
        map.put(doc, w)(tx.peer)
        doc.addDependent(new Disposable[S#Tx] {
          def dispose()(implicit tx: S#Tx): Unit = GUI.fromTx {
            logInfo(s"Remove view map entry for ${doc.name}")
            map.single.remove(doc)
            if (_active == Some(doc)) activeDocument = None
          }
        })
        w
      }
  }
}