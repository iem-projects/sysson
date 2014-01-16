package at.iem.sysson
package gui
package impl

import scala.swing.Button
import javax.swing.TransferHandler
import javax.swing.TransferHandler.TransferSupport
import java.awt.datatransfer.DataFlavor

object DropButton {
  def apply[A](flavor: DataFlavor, tooltip: String)(imp: A => Boolean): Button = {
    val but        = new Button(null: String)
    but.icon       = Icons.Target(24)
    but.focusable  = false
    but.tooltip    = s"Drop $tooltip Here"

    but.peer.setTransferHandler(new TransferHandler(null) {
      // how to enforce a drop action: https://weblogs.java.net/blog/shan_man/archive/2006/02/choosing_the_dr.html
      override def canImport(support: TransferSupport): Boolean = but.enabled && {
        val res =
          if (support.isDataFlavorSupported(flavor) &&
            ((support.getSourceDropActions & TransferHandler.LINK) != 0)) {
            support.setDropAction(TransferHandler.LINK)
            true
          } else
            false
  
        // println(s"canImport? $res")
        res
      }
  
      override def importData(support: TransferSupport): Boolean = {
        val t         = support.getTransferable
        val drag      = t.getTransferData(flavor).asInstanceOf[A]
        imp(drag)
      }
    })

    but
  }
}
