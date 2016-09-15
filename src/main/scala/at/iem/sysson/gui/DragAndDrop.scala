/*
 *  DragAndDrop.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui

import java.awt.datatransfer.{DataFlavor, Transferable, UnsupportedFlavorException}
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.{JComponent, TransferHandler}

import at.iem.sysson.sound.Sonification
import de.sciss.equal
import de.sciss.lucre.matrix.{DataSource, Matrix}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.Workspace

import scala.collection.breakOut
import scala.language.higherKinds

object DragAndDrop {
  sealed trait Flavor[+A] extends DataFlavor

  def internalFlavor[A](implicit ct: reflect.ClassTag[A]): Flavor[A] =
    new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=\"" + ct.runtimeClass.getName + "\"") with Flavor[A]

  // ---- specific flavors ----

  //  val LibraryNodeFlavor = internalFlavor[LibraryNodeDrag]
  //
  //  trait LibraryNodeDrag {
  //    type S1 <: Sys[S1]
  //    def cursor: stm.Cursor[S1]
  //    def node: stm.Source[S1#Tx, Either[Library.Branch[S1], Library.Leaf[S1]]]
  //  }

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

  val SonificationSourceMappingFlavor = internalFlavor[SonificationSourceMappingDrag]
  val PlotMappingFlavor               = internalFlavor[PlotMappingDrag]

  trait MappingDrag {
    type S1 <: Sys[S1]
    def workspace: Workspace[S1]
    type Source[S <: Sys[S]]
    def source: stm.Source[S1#Tx, Source[S1]]
    def key: String
  }

  trait SonificationSourceMappingDrag extends MappingDrag {
    type Source[S <: Sys[S]] = Sonification.Source[S]
  }

  trait PlotMappingDrag extends MappingDrag {
    type Source[S <: Sys[S]] = Plot[S]
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
        import equal.Implicits._
        _flavor === flavor
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

  abstract class Button extends swing.Button() {

    protected def sourceActions: Int

    protected def sourceAction(modifiers: Int): Int

    protected def export(): Option[Transferable]

    private object Transfer extends TransferHandler {
      override def getSourceActions(c: JComponent): Int = sourceActions

      override def createTransferable(c: JComponent): Transferable = export().orNull
    }

    peer.setTransferHandler(Transfer)
    focusable = false

    private var dndInitX    = 0
    private var dndInitY    = 0
    private var dndPressed  = false
    private var dndStarted  = false
    private object Mouse extends MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit = {
        dndInitX	  = e.getX
        dndInitY    = e.getY
        dndPressed  = true
        dndStarted	= false
      }

      override def mouseReleased(e: MouseEvent): Unit = {
        dndPressed  = false
        dndStarted	= false
      }

      override def mouseDragged(e: MouseEvent): Unit =
        if (dndPressed && !dndStarted && ((math.abs(e.getX - dndInitX) > 5) || (math.abs(e.getY - dndInitY) > 5))) {
          Transfer.exportAsDrag(peer, e, sourceAction(e.getModifiers))
          dndStarted = true
        }
    }

    peer.addMouseListener      (Mouse)
    peer.addMouseMotionListener(Mouse)
  }
}