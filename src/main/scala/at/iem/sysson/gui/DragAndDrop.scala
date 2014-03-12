/*
 *  DragAndDrop.scala
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

import java.awt.datatransfer.{UnsupportedFlavorException, Transferable, DataFlavor}
import collection.breakOut
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.lucre.matrix.{Matrix, DataSource}

object DragAndDrop {
  sealed trait Flavor[+A] extends DataFlavor

  def internalFlavor[A](implicit ct: reflect.ClassTag[A]): Flavor[A] =
    new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=\"" + ct.runtimeClass.getName + "\"") with Flavor[A]

  // ---- specific flavors ----

  val LibraryNodeFlavor = internalFlavor[LibraryNodeDrag]

  trait LibraryNodeDrag {
    type S1 <: Sys[S1]
    def cursor: stm.Cursor[S1]
    def node: stm.Source[S1#Tx, Either[Library.Branch[S1], Library.Leaf[S1]]]
  }

  val DataSourceFlavor = internalFlavor[DataSourceDrag]

  trait DataSourceDrag {
    type S1 <: Sys[S1]
    def workspace: Workspace[S1]
    def source: stm.Source[S1#Tx, DataSource[S1]]
  }

  val MatrixFlavor = internalFlavor[MatrixDrag]

  trait MatrixDrag {
    type S1 <: Sys[S1]
    def workspace: Workspace[S1]
    def matrix: stm.Source[S1#Tx, Matrix[S1]]
  }

  // ----

  object Transferable {
    /** Creates a transferable for one particular flavor. */
    def apply[A](flavor: Flavor[A])(data: A): Transferable = new Transferable {
      override def toString = s"Transferable($data)"

      // private val flavor = internalFlavor[A]
      // println(s"My flavor is $flavor")
      def getTransferDataFlavors: Array[DataFlavor] = Array(flavor) // flavors.toArray
      def isDataFlavorSupported(_flavor: DataFlavor): Boolean = {
        // println(s"is $flavor the same as ${this.flavor} ? ${flavor == this.flavor}")
        _flavor == flavor
        // flavors.contains(flavor)
      }
      def getTransferData(_flavor: DataFlavor): AnyRef  = {
        if (!isDataFlavorSupported(_flavor)) throw new UnsupportedFlavorException(flavor)
        data  /* .getOrElse(throw new IOException()) */ .asInstanceOf[AnyRef]
      }
    }

    /** Creates a transferable by wrapping a sequence of existing transferable objects. */
    def seq(xs: Transferable*): Transferable = new Transferable {
      def getTransferDataFlavors: Array[DataFlavor] = xs.flatMap(_.getTransferDataFlavors)(breakOut)
      def isDataFlavorSupported(_flavor: DataFlavor): Boolean = xs.exists(_.isDataFlavorSupported(_flavor))
      def getTransferData(_flavor: DataFlavor): AnyRef = {
        val peer = xs.find(_.isDataFlavorSupported(_flavor)).getOrElse(throw new UnsupportedFlavorException(_flavor))
        peer.getTransferData(_flavor)
      }
    }
  }
}