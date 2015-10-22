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

    java.lang.IllegalStateException: Cyclic object graph involving DataSource<27>
    	at de.sciss.lucre.stm.impl.CopyImpl.apply(CopyImpl.scala:54)
    	at de.sciss.lucre.matrix.impl.DataSourceImpl$DimensionImpl.copy(DataSourceImpl.scala:251)
    	at de.sciss.lucre.stm.impl.CopyImpl.apply(CopyImpl.scala:57)
    	at de.sciss.lucre.matrix.impl.DataSourceImpl$Impl$$anonfun$7.apply(DataSourceImpl.scala:347)
    	at de.sciss.lucre.matrix.impl.DataSourceImpl$Impl$$anonfun$7.apply(DataSourceImpl.scala:347)
    	at scala.collection.immutable.List.map(List.scala:273)
    	at de.sciss.lucre.matrix.impl.DataSourceImpl$Impl.copy(DataSourceImpl.scala:347)
    	at de.sciss.lucre.stm.impl.CopyImpl.apply(CopyImpl.scala:57)
    	at de.sciss.lucre.matrix.impl.DataSourceImpl$VariableImpl.copy(DataSourceImpl.scala:224)
    	at de.sciss.lucre.stm.impl.CopyImpl.apply(CopyImpl.scala:57)
    	at de.sciss.lucre.matrix.impl.ReduceImpl$Impl.copy(ReduceImpl.scala:680)
    	at de.sciss.lucre.stm.impl.CopyImpl.apply(CopyImpl.scala:57)
    	at de.sciss.lucre.matrix.impl.MatrixVarImpl$Impl.copy(MatrixVarImpl.scala:63)
    	at de.sciss.lucre.stm.impl.CopyImpl.apply(CopyImpl.scala:57)
    	at at.iem.sysson.impl.PlotImpl$Impl.copy(PlotImpl.scala:106)
    	at de.sciss.lucre.stm.impl.CopyImpl.apply(CopyImpl.scala:57)
    	at de.sciss.mellite.gui.impl.document.FolderViewTransferHandler$FolderTransferHandler$$anonfun$7.apply(FolderViewTransferHandler.scala:200)

while trying to dnd a plot across workspaces, same with data-source

