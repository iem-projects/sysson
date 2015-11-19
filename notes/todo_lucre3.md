# bugs or problems

# features

# alleviated or less important

- sonif-editor: controls - should allow tuning of spec (improved: now shows more digits)
- cannot drag from attributes editor to folder - low priority
- matrix-editor: row-header baseline

# exceptions

    java.lang.NullPointerException
    	at de.sciss.mellite.gui.impl.document.FolderViewTransferHandler$FolderTransferHandler$.canImport(FolderViewTransferHandler.scala:79)
    	at javax.swing.TransferHandler$DropHandler.handleDrag(TransferHandler.java:1459)
    	at javax.swing.TransferHandler$DropHandler.dragEnter(TransferHandler.java:1478)
    	at java.awt.dnd.DropTarget.dragEnter(DropTarget.java:357)
    	at javax.swing.TransferHandler$SwingDropTarget.dragEnter(TransferHandler.java:1238)
    	at de.sciss.treetable.j.TreeTable$DT.dragEnter(TreeTable.java:3100)

while performing some dnd in a folder. (cannot reproduce)

# workshop

- 26.11., 13:30h. 1 hour presentation, then individual work
- three examples: 
    - 1-variable ("augmented")
    - 2-variables ("thresholds" = Regen = CMFJ)
    - QBO
- each documented on the wiki
- fix most annoying things, e.g. prevent matrices to become too large for playback

## Notes 18-Nov

- rsdt historical; 200 hz / 0.25 c; min/max nach statistik
- 1d augmented : -1 dB gain, clip min/max sicherheitshalber
- convert audio to netcdf fuer 1d augmented? 2d-to-1d scanning function?
- new uservalue labels: 'data min', 'data max', c = 'pitch modulation {octaves}', df = 'frequency shift {Hz}',
  'speed {1/s}'
- https://en.wikibooks.org/wiki/Designing_Sound_in_SuperCollider - actually pretty cheesy stuff
- Reduce.min/max fuer longitude panning
- zwei kontrastierende Texturen
- sichtbare parameter minimieren

- presentation: sound+beamer+beispiele vom server?

--------------

QBO-Tests : time kann nicht geslict werden -- warum auch immer:

Exception in thread "AWT-EventQueue-0" scala.NotImplementedError: an implementation is missing
	at scala.Predef$.$qmark$qmark$qmark(Predef.scala:225)
	at de.sciss.lucre.matrix.impl.ReduceImpl$ReaderFactory$Cloudy.reader(ReduceImpl.scala:632)
	at at.iem.sysson.gui.impl.PlotChartImpl$Impl.at$iem$sysson$gui$impl$PlotChartImpl$Impl$$updateData(PlotChartImpl.scala:123)
	at at.iem.sysson.gui.impl.PlotChartImpl$Impl$$anonfun$init$7$$anonfun$apply$12.apply(PlotChartImpl.scala:356)
	at at.iem.sysson.gui.impl.PlotChartImpl$Impl$$anonfun$init$7$$anonfun$apply$12.apply(PlotChartImpl.scala:354)
	at de.sciss.lucre.event.Observer$Impl.apply(Observer.scala:40)
	at de.sciss.lucre.event.Push$Reaction$$anonfun$apply$1.apply(Push.scala:40)
	at de.sciss.lucre.event.Push$Reaction$$anonfun$apply$1.apply(Push.scala:40)
	at scala.collection.immutable.List.foreach(List.scala:381)
	at de.sciss.lucre.event.Push$Reaction.apply(Push.scala:40)
	at de.sciss.lucre.event.Push$Impl$$anonfun$pull$1.apply(Push.scala:95)
	at de.sciss.lucre.event.Push$Impl$$anonfun$pull$1.apply(Push.scala:95)
	at scala.collection.immutable.List.foreach(List.scala:381)
	at de.sciss.lucre.event.Push$Impl.pull(Push.scala:95)
	at de.sciss.lucre.event.Push$.apply(Push.scala:29)
	at de.sciss.lucre.event.impl.Generator$class.fire(Generator.scala:22)
	at de.sciss.lucre.matrix.impl.VarImpl$changed$.fire(VarImpl.scala:78)
	at de.sciss.lucre.matrix.impl.VarImpl$class.update(VarImpl.scala:48)
	at de.sciss.lucre.matrix.impl.MatrixVarImpl$Impl.update(MatrixVarImpl.scala:52)