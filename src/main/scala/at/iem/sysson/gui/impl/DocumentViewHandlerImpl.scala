/*
 *  DocumentViewHandlerImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import de.sciss.model.impl.ModelImpl

private[gui] object DocumentViewHandlerImpl {
  def instance: DocumentViewHandler = impl

  private lazy val impl = new Impl

  def mkView(doc: Document): DocumentView = impl.mkView(doc)

  private final class Impl extends DocumentViewHandler with ModelImpl[DocumentViewHandler.Update] {
    override def toString = "DocumentViewHandler"

    private var map       = Map.empty[Document, DocumentView]
    private var _active  = Option.empty[Document]

    def activeDocument = _active
    def activeDocument_=(value: Option[Document]): Unit =
      if (_active != value) {
        _active = value
        value.foreach { doc =>
          dispatch(DocumentViewHandler.Activated(doc))
        }
      }

    def getView(doc: Document): Option[DocumentView] = {
      GUI.requireEDT()
      map.get(doc)
    }

    def mkView(doc: Document): DocumentView = {
      getView(doc).getOrElse {
        val view = DocumentViewImpl(doc)
        map += doc -> view
        doc.addListener {
          case Document.Closed(_) => GUI.defer {
            view.dispose()
            map -= doc
          }
        }
        view
      }
    }
  }
}