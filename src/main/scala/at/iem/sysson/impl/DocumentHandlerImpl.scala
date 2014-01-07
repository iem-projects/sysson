/*
 *  DocumentHandlerImpl.scala
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
package impl

import de.sciss.model.impl.ModelImpl

private[sysson] object DocumentHandlerImpl {
  def apply(): DocumentHandler = new Impl

  private final class Impl extends DocumentHandler with ModelImpl[DocumentHandler.Update] {
    override def toString = "DocumentHandler"

    private val sync  = new AnyRef
    private var all   = Vec.empty[DataSourceLike]
    private var map   = Map.empty[String, DataSourceLike] // path to document

    //    private val docListener: DataSource.Listener = {
    //      case DataSource.Closed(doc) => removeDoc(doc)
    //    }

    def openRead(path: String): DataSourceLike = {
      ???
//      val doc = DataSourceImpl.openRead(path)
//      // doc.addListener(docListener)
//      sync.synchronized {
//        all :+= doc
//        map  += path -> doc
//      }
//      dispatch(DocumentHandler.Opened(doc))
//      doc
    }

    private def removeDoc(doc: DataSourceLike): Unit = {
      sync.synchronized {
        val idx = all.indexOf(doc)
        assert(idx >= 0)
        // doc.removeListener(docListener)
        all  = all.patch(idx, Nil, 1)
        map -= doc.path
      }
      dispatch(DocumentHandler.Closed(doc))
    }

    def allDocuments: Iterator[DataSourceLike] = sync.synchronized( all.iterator )

    def getDocument(path: String): Option[DataSourceLike] = sync.synchronized(map.get(path))
  }
}