/*
 *  ActionConvertSpreadsheet.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2015 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2015 Hanns Holger Rutz. All rights reserved.
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

import javax.swing.table.DefaultTableModel

import de.sciss.desktop.{OptionPane, DialogSource, FileDialog}
import de.sciss.file._
import de.sciss.mellite
import de.sciss.sheet.{BooleanCell, StringCell, Cell, StyledCell, NumericCell, Sheet, Workbook}
import de.sciss.swingplus.ListView

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.swing.{FlowPanel, Swing, Label, BoxPanel, Orientation, ScrollPane, Table, Action}
import scala.util.{Success, Failure, Try}

object ActionConvertSpreadsheet extends Action("Spreadsheet To NetCDF...") {
  private def title0 = title.dropRight(3)

  def apply(): Unit = {
    val fDlg = FileDialog.open(title = "Select .xls/.xlsx Spreadsheet")
    fDlg.setFilter { f =>
      val ext = f.ext.toLowerCase
      ext == "xls" || ext == "xlsx"
    }
    fDlg.show(None).foreach { fIn =>
      val fInName = fIn.name
      Try(Workbook.fromFile(fIn.path)) match {
        case Failure(ex) =>
          DialogSource.Exception(ex -> s"Open Spreadsheet '$fInName'").show(None)

        case Success(book) =>
          val sheets = book.sortedSheets
          sheets match {
            case sheetHead +: sheetTail if sheetTail.nonEmpty =>
              OptionPane.comboInput(message = "Select Sheet (Page):", options = sheets.map(_.name),
                initial = sheetHead.name).show(None, title = title0).foreach { name =>

                perform(book.sheetMap(name), fIn)
              }
            case sheet +: _ =>
              perform(sheet, fIn)
            case _ =>
              OptionPane.message(s"No sheets (pages) found in '$fInName'", OptionPane.Message.Error).show(None)
          }
      }
    }
  }

  private def perform(sheet: Sheet, fIn: File): Unit = {
    val tableRows = sheet.sortedRows
    val maxRow    = tableRows.map(_.index).max
    val maxCol    = tableRows.map(r => r.sortedCells.map(_.index).max).max

    @tailrec def cellToString(c: Cell): Any = c match {
      case StringCell (_, data) => data
      case NumericCell(_, data) => data
      case BooleanCell(_, data) => data
      case StyledCell(c1, _) => cellToString(c1)
    }

    @tailrec def colToNameR(i: Int, post: String): String = {
      val m = i % 26
      val d = i / 26
      val c = (m + 'A').toChar
      val s = s"$c$post"
      if (d == 0) s else colToNameR(d - 1, s)
    }

    def colToName(i: Int): String = colToNameR(i, "")

    @tailrec def nameToColR(i: Int, m: Int, s: String): Int =
      if (s.isEmpty) i - 1 else {
        val j = i + ((s.charAt(s.length - 1) - 'A') + 1) * m
        nameToColR(j, m * 26, s.substring(0, s.length - 1))
      }

    def nameToCol(s: String): Int = nameToColR(0, 1, s)

    val rowData = Array.tabulate(maxRow) { ri =>
      Array.tabulate(maxCol) { ci =>
        sheet.rowMap.get(ri).fold[Any]("")(_.cellMap.get(ci).fold[Any]("")(cellToString))
      }
    }

    val colNames = (0 until maxCol).map(colToName)

    val inVisRows = math.min(16, maxRow)

    val ggTableIn   = new Table(rowData, colNames) {
      selection.elementMode = Table.ElementMode.Cell
      // autoResizeMode = Table.AutoResizeMode.Off
    }
    val rowHeightIn = ggTableIn.rowHeight // + ggTableIn.peer.getRowMargin

    ggTableIn.preferredViewportSize = {
      val dim = ggTableIn.preferredViewportSize
      dim.width   = math.min(800, dim.width)
      dim.height  = math.min(inVisRows * rowHeightIn, dim.height)
      dim
    }

    // cf. http://www.java2s.com/Code/Java/Swing-Components/TableRowHeaderExample.htm
    // XXX TODO -- this looks nicer:
    // http://stackoverflow.com/questions/8187639/jtable-with-titled-rows-and-columns#8187799
    val ggRows = new ListView[Int](1 to maxRow) {
      enabled           = false
      fixedCellWidth    = maxRow.toString.length * 13
      fixedCellHeight   = rowHeightIn
      visibleRowCount   = inVisRows
    }
    val ggScrollIn = new ScrollPane(ggTableIn)
    ggScrollIn.rowHeaderView = Some(ggRows)

    val ggTableOut    = new Table(0, 0) {
      selection.intervalMode = Table.IntervalMode.Single
    }

    sealed trait VarDef {
      def name: String
      def flat: Array[Double]
      def rank: Int
    }

    case class TableVarDef(name: String, rows: Range, cols: Range, description: Option[String], unit: Option[String],
                           dim0: Option[VarDef], dim1: Option[VarDef]) extends VarDef {
      def rank = if (rows.isEmpty || cols.isEmpty) 0 else if (rows.size == 1 || cols.size == 1) 1 else 2

      def flat: Array[Double] = rows.flatMap { ri =>
        sheet.rowMap.get(ri).fold(Vec.fill(cols.size)(Double.NaN)) { row =>
          cols.map { ci =>
            @tailrec def getNumber(c: Cell): Double = c match {
              case NumericCell(_, v) => v
              case StyledCell(c1, _) => getNumber(c1)
              case _ => Double.NaN
            }

            row.cellMap.get(ci).fold(Double.NaN)(getNumber)
          } (breakOut)
        }
      } (breakOut)
    }

    case class SyntheticDim(name: String, flat: Array[Double]) extends VarDef { def rank = 1 }

    val mTableOut     = new DefaultTableModel(Array[AnyRef]("Name", "Shape", "Description", "Unit", "Row Dim", "Col Dim"), 0)
    ggTableOut.model  = mTableOut
    val rowHeightOut = ggTableOut.rowHeight // + ggTableOut.peer.getRowMargin

    ggTableOut.preferredViewportSize = {
      val dim = ggTableOut.preferredViewportSize
      dim.height = math.min(4 * rowHeightOut, dim.height)
      dim
    }

    val ggScrollOut = new ScrollPane(ggTableOut)

    def selectionToShape(rows: Range, cols: Range): String =
      s"[${colToName(cols.head)}${rows.head + 1}:${colToName(cols.last)}${rows.last + 1}]"

    val ShapeReg = """\[([A-Z]+)(\d+)\:([A-Z]+)(\d+)\]""".r

    def shapeToSelection(shape: String): Option[(Range, Range)] =
      shape.trim match {
        case ShapeReg(firstColS, firstRowS, lastColS, lastRowS) =>
          val firstCol = nameToCol(firstColS)
          val lastCol  = nameToCol(lastColS )
          for {
            firstRow <- Try(firstRowS.toInt).toOption
            lastRow  <- Try(lastRowS .toInt).toOption
          }
          yield (firstRow to lastRow, firstCol to lastCol)

        case _ => None
      }

    val aAddVar = Action(null) {
      val rows    = ggTableIn.selection.rows
      val cols    = ggTableIn.selection.columns
      if (rows.nonEmpty && cols.nonEmpty) {
        val rowR  = rows.min to rows.max
        val colR  = cols.min to cols.max

        val shape   = selectionToShape(rowR, colR)
        val rowData = Array[AnyRef]("untitled", shape, "", "", "", "")
        mTableOut.addRow(rowData)
      }
    }

    val aRemoveVar = Action(null) {
      ggTableIn.selection.rows.headOption.foreach { ri =>
        mTableOut.removeRow(ri)
      }
    }

    val ggAddVar    = mellite.gui.GUI.addButton   (aAddVar   , "Add New Variable From Table Selection")
    val ggRemoveVar = mellite.gui.GUI.removeButton(aRemoveVar, "Remove Selected Variable")
    val pAddRemove  = new FlowPanel(ggAddVar, ggRemoveVar)

    val pane = new BoxPanel(Orientation.Vertical) {
      contents += ggScrollIn
      contents += Swing.VStrut(4)
      contents += new Label("Output Variables:")
      contents += ggScrollOut
      contents += pAddRemove
    }

    def buildVar(in: List[TableVarDef], row: Int): Either[String, TableVarDef] = {
      def getCol(col: Int): Option[String] = {
        val s = mTableOut.getValueAt(row, col).toString.trim
        if (s.isEmpty) None else Some(s)
      }

      val varName = getCol(0).getOrElse("untitled")
      val desc    = getCol(2)
      val unit    = getCol(3)

      if (varName == "untitled") Left(s"Variable in row ${row + 1} must be named")
      else {
        val shapeS  = mTableOut.getValueAt(row, 1).toString
        shapeToSelection(shapeS).fold[Either[String, TableVarDef]](
          Left(s"Shape in row ${row + 1} is invalid $shapeS")
        ) { case (rows, cols) =>
          val rDimSO  = getCol(4)
          val rank    = if (rows.size == 1 || cols.size == 1) 1 else 2
          val rSize   = if (rows.size == 1) cols.size else rows.size
          val rDimOpt = rDimSO.fold[Option[VarDef]](
            Some(SyntheticDim(s"${varName}_dim0", Array.tabulate(rSize)(_.toDouble)))
          ) { rDimName =>
            in.find(_.name == rDimName)
          }
          rDimOpt.fold[Either[String, TableVarDef]](Left(s"Cannot find dimension $rDimSO")) { rDim =>
            val cDimOptE: Either[String, Option[VarDef]] = if (rank == 1) Right(None) else {
              val cSize   = cols.size
              val cDimSO  = getCol(5)
              val cDimOpt = cDimSO.fold[Option[VarDef]](
                Some(SyntheticDim(s"${varName}_dim1", Array.tabulate(cSize)(_.toDouble)))
              ) { cDimName =>
                in.find(_.name == cDimName)
              }
              cDimOpt.fold[Either[String, Option[VarDef]]](Left(s"Cannot find dimension $cDimSO")) { cDim =>
                Right(Some(cDim))
              }
            }
            cDimOptE.right.map { cDimOpt =>
              TableVarDef(name = varName, rows = rows, cols = cols, description = desc, unit = unit,
                dim0 = Some(rDim), dim1 = cDimOpt)
            }
          }
        }
      }
    }

    @tailrec def showPane(): List[TableVarDef] = {
      val res = OptionPane(message = pane, OptionPane.Options.OkCancel, OptionPane.Message.Plain).show(None, title0)
      if (res == OptionPane.Result.Yes) {
        val numVars = mTableOut.getRowCount

        @tailrec def loop(ri: Int, res: List[TableVarDef]): Either[String, List[TableVarDef]] =
          if (ri == numVars) Right(res) else {
            buildVar(in = res, row = ri) match {
              case Right(vr) => loop(ri + 1, res :+ vr)
              case Left(err) => Left(err)
            }
          }

        loop(0, Nil) match {
          case Left(err) =>
            OptionPane.message(err, OptionPane.Message.Error).show(None, title0)
            showPane()

          case Right(Nil) =>
            OptionPane.message("No variables were defined", OptionPane.Message.Error).show(None, title0)
            showPane()

          case Right(vars0) => vars0
        }

      } else Nil
    }

    val vars = showPane()
    if (vars.isEmpty) return

    vars.foreach(println)
  }
}