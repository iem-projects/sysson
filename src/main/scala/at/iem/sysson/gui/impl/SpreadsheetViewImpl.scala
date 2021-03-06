/*
 *  SpreadsheetViewImpl.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package gui
package impl

import at.iem.sysson.gui.impl.AbstractPlotViewImpl.PlotData
import de.sciss.desktop.UndoManager
import de.sciss.lucre.matrix.gui.DimensionIndex
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.swingplus.ListView
import de.sciss.synth.proc.Universe
import javax.swing.table.{AbstractTableModel, DefaultTableColumnModel, TableColumn}

import scala.swing.ScrollPane.BarPolicy
import scala.swing.Table.{AutoResizeMode, ElementMode}
import scala.swing.{ScrollPane, Table}

object SpreadsheetViewImpl {
  def apply[S <: Sys[S]](plot: Plot[S], stats: PlotStatsView[S])(implicit tx: S#Tx, universe: Universe[S],
                                                                 undoManager: UndoManager): View[S] = {
    new Impl[S](stats).init(plot)
  }

  private final class Impl[S <: Sys[S]](statsView: PlotStatsView[S])(implicit val universe: Universe[S],
                                                                     val undoManager: UndoManager)
    extends AbstractPlotViewImpl[S] {

    override def init(plot: Plot[S])(implicit tx: S#Tx): Impl.this.type = {
      super.init(plot)
      deferTx(guiInit())
      this
    }

    private[this] var _plotData = new PlotData(
      "", "", new Array[Float](0), "", "", new Array[Float](0), "", "", new Array[Array[Float]](0), is1D = false)

    // called on EDT
    protected def updatePlot(data: PlotData): Unit = {
      val colSizeChanged  = data.hData.length != _plotData.hData.length
      val colDataChanged  = colSizeChanged || !data.hData.sameElements(_plotData.hData)
      val rowSizeChanged  = data.vData.length != _plotData.vData.length
      val rowDataChanged  = rowSizeChanged || !data.vData.sameElements(_plotData.vData)
      _plotData = data
      if (colDataChanged) {
        mTableColumn.updateHeader()
        mTable.fireTableStructureChanged()
      } else {
        mTable.fireTableDataChanged()
      }
      if (rowDataChanged) {
        updateRows()
      }
    }

    private[this] object mTable extends AbstractTableModel {
      def getRowCount   : Int = _plotData.vData.length
      def getColumnCount: Int = if (_plotData.is1D) 1 else _plotData.hData.length

      def getValueAt(rowIdx: Int, colIdx: Int): AnyRef = {
//        val f = if (_plotData.mData.length > rowIdx) {
//          val row = _plotData.mData(rowIdx)
//          if (row.length > colIdx) row(colIdx) else Float.NaN
//        } else Float.NaN
        val f = if (_plotData.is1D) _plotData.mData(0)(rowIdx) else _plotData.mData(rowIdx)(colIdx)
        f.toString  // XXX TODO
      }
    }

    private[this] object mTableColumn extends DefaultTableColumnModel {
      def updateHeader(): Unit =
        if (_plotData.is1D) updateHeader1D() else updateHeader2D()

      private def mkColumn(colIdx: Int, name: String): Unit = {
        val col = new TableColumn(colIdx)
        col.setHeaderValue("")
        col.setMinWidth      (80)
        col.setPreferredWidth(80)
        addColumn(col)
      }

      private def updateHeader1D(): Unit = {
        val oldNum  = getColumnCount
        val newNum  = 1
        val stop1   = math.min(oldNum, newNum)
        var colIdx  = 0
        while (colIdx < stop1) {
          val col = getColumn(colIdx)
          col.setHeaderValue("")
          colIdx += 1
        }
        while (colIdx < newNum) {
          mkColumn(colIdx, "")
          colIdx += 1
        }
        while (colIdx < oldNum) {
          val col = getColumn(newNum)
          removeColumn(col)
          colIdx += 1
        }
      }

      private def updateHeader2D(): Unit = {
        import DimensionIndex.{shouldUseUnitsString, unitsStringFormatter}
        val units   = _plotData.hUnits
        val lbUnits = shouldUseUnitsString(units)
        val data    = _plotData.hData
        val labels  = if (lbUnits) {
          val fmt = unitsStringFormatter(units)
          data.map(fmt(_))
        } else {
          data.map(_.toString)
        }

        val oldNum  = getColumnCount
        val newNum  = data.length
        val stop1   = math.min(oldNum, newNum)
        var colIdx  = 0
        while (colIdx < stop1) {
          val col = getColumn(colIdx)
          col.setHeaderValue(labels(colIdx))
          colIdx += 1
        }
        while (colIdx < newNum) {
          mkColumn(colIdx, labels(colIdx))
          colIdx += 1
        }
        while (colIdx < oldNum) {
          val col = getColumn(newNum)
          removeColumn(col)
          colIdx += 1
        }
      }
    }

    private[this] val mList = ListView.Model.empty[String]

    private def updateRows(): Unit = {
      import DimensionIndex.{shouldUseUnitsString, unitsStringFormatter}
      val units   = _plotData.vUnits
      val lbUnits = shouldUseUnitsString(units)
      val data    = _plotData.vData
      val labels  = if (lbUnits) {
        val fmt = unitsStringFormatter(units)
        data.map(fmt(_))
      } else {
        data.map(_.toString)
      }

      val oldNum  = mList.size
      val newNum  = data.length
      val stop1   = math.min(oldNum, newNum)
      var colIdx  = 0
      while (colIdx < stop1) {
        mList.update(colIdx, labels(colIdx))
        colIdx += 1
      }
      while (colIdx < newNum) {
        mList  += labels(colIdx)
        colIdx += 1
      }
      if (oldNum > newNum) {
        mList.remove(newNum, oldNum - newNum)
      }
    }

    private def guiInit(): Unit = {
      val ggTable                   = new Table
      ggTable.peer.setAutoCreateColumnsFromModel(false)
      ggTable.peer.setColumnModel(mTableColumn)
      ggTable.autoResizeMode        = AutoResizeMode.Off
      ggTable.model                 = mTable
      ggTable.selection.elementMode = ElementMode.Cell

      // cf. http://www.java2s.com/Code/Java/Swing-Components/TableRowHeaderExample.htm
      // XXX TODO -- this looks nicer:
      // http://stackoverflow.com/questions/8187639/jtable-with-titled-rows-and-columns#8187799
      val ggRows  = new ListView[String](mList)
      ggRows.fixedCellHeight  = ggTable.rowHeight
      ggRows.enabled          = false
//        fixedCellWidth    = 160 // maxRow.toString.length * 13
//        fixedCellHeight   =  24 // rowHeightIn
//        visibleRowCount   =  12 // inVisRows
//      }
      val ggScroll = new ScrollPane(ggTable)
      ggScroll.horizontalScrollBarPolicy  = BarPolicy.Always
      ggScroll.verticalScrollBarPolicy    = BarPolicy.Always
      ggScroll.rowHeaderView = Some(ggRows)

      component = ggScroll
    }
  }
}
