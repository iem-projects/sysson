/*
 *  DocumentHandlerImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
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
package impl

import de.sciss.model.impl.ModelImpl
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.Disposable
import scala.concurrent.stm.{Ref => STMRef, TMap}
import at.iem.sysson.gui.GUI
import de.sciss.file.File

private[sysson] object DocumentHandlerImpl {
  import DocumentHandler.Document

  def apply(): DocumentHandler = new Impl

  private final class Impl extends DocumentHandler with ModelImpl[DocumentHandler.Update] {
    override def toString = "DocumentHandler"

    private val all   = STMRef(Vec.empty[Document])
    private val map   = TMap.empty[File, Document] // path to document

    // def openRead(path: String): Document = ...

    def addDocument[S <: Sys[S]](doc: Workspace[S])(implicit tx: S#Tx): Unit = {
      // doc.addListener(docListener)
      val p = doc.file
      require(!map.contains(p)(tx.peer), s"Workspace for path '$p' is already registered")
      all.transform(_ :+ doc)(tx.peer)
      map.+=(p -> doc)(tx.peer)

      doc.addDependent(new Disposable[S#Tx] {
        def dispose()(implicit tx: S#Tx): Unit = removeDoc(doc)
      })

      GUI.fromTx(dispatch(DocumentHandler.Opened(doc)))
    }

    private def removeDoc[S <: Sys[S]](doc: Workspace[S])(implicit tx: S#Tx): Unit = {
      all.transform { in =>
        val idx = in.indexOf(doc)
        require(idx >= 0, s"Workspace for path '${doc.path}' was not registered")
        in.patch(idx, Nil, 1)
      } (tx.peer)
      map.-=(doc.file)(tx.peer)

      GUI.fromTx(dispatch(DocumentHandler.Closed(doc)))
    }

    def allDocuments: Iterator[Document] = all.single().iterator
    def getDocument(file: File): Option[Document] = map.single.get(file)

    def isEmpty: Boolean = map.single.isEmpty
  }
}