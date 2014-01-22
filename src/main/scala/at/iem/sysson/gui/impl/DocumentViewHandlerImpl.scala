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

private[gui] object DocumentViewHandlerImpl {
  import DocumentHandler.Document

  def instance: DocumentViewHandler = impl

  private lazy val impl = new Impl

  def mkView[S <: Sys[S]](doc: Workspace[S])(implicit tx: S#Tx): WorkspaceView[S] = impl.mkView(doc)

  private final class Impl extends DocumentViewHandler with ModelImpl[DocumentViewHandler.Update] {
    override def toString = "DocumentViewHandler"

    private var map       = Map.empty[Document, WorkspaceView[_]]
    private var _active  = Option.empty[Document]

    def activeDocument = _active
    def activeDocument_=[S <: Sys[S]](value: Option[Workspace[S]]): Unit =
      if (_active != value) {
        _active = value
        value.foreach { doc =>
          dispatch(DocumentViewHandler.Activated(doc))
        }
      }

    def getView[S <: Sys[S]](doc: Workspace[S]): Option[WorkspaceView[S]] = {
      GUI.requireEDT()
      map.get(doc).asInstanceOf[Option[WorkspaceView[S]]]
    }

    def mkView[S <: Sys[S]](doc: Workspace[S])(implicit tx: S#Tx): WorkspaceView[S] = {
      getView(doc).getOrElse {
        val view = WorkspaceView(doc)
        map += doc -> view
//        doc.addListener {
//          case DataSource.Closed(_) => GUI.defer {
//            view.dispose()
//            map -= doc
//          }
//        }
        view
      }
    }
  }
}