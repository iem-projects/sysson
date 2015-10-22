- moving time slider in simple surface plot doesn't update GUI,
  perhaps Reduce doesn't propagate event (not connected?)
- increasing stride seems incredibly slow (but doesn't overflow)
- cannot drag from attributes editor to folder

# exceptions

    java.lang.NullPointerException
    	at de.sciss.mellite.gui.impl.document.FolderViewTransferHandler$FolderTransferHandler$.canImport(FolderViewTransferHandler.scala:79)
    	at javax.swing.TransferHandler$DropHandler.handleDrag(TransferHandler.java:1459)
    	at javax.swing.TransferHandler$DropHandler.dragEnter(TransferHandler.java:1478)
    	at java.awt.dnd.DropTarget.dragEnter(DropTarget.java:357)
    	at javax.swing.TransferHandler$SwingDropTarget.dragEnter(TransferHandler.java:1238)
    	at de.sciss.treetable.j.TreeTable$DT.dragEnter(TreeTable.java:3100)

while performing some dnd in a folder

