/*
 *  ActionConvertSpreadsheet.scala
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

import javax.swing.table.DefaultTableModel

import de.sciss.desktop.{DialogSource, FileDialog, OptionPane}
import de.sciss.equal
import de.sciss.file._
import de.sciss.mellite
import de.sciss.sheet.{BooleanCell, Cell, NumericCell, Sheet, StringCell, StyledCell, Workbook}
import de.sciss.swingplus.ListView
import ucar.nc2.constants.CDM
import ucar.{ma2, nc2}

import scala.annotation.tailrec
import scala.collection.{JavaConverters, breakOut}
import scala.swing.{Action, BoxPanel, FlowPanel, Label, Orientation, ScrollPane, Swing, Table}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object ActionConvertSpreadsheet extends Action("Spreadsheet To NetCDF...") {
  private def title0 = title.dropRight(3)

  def apply(): Unit = {
    val fDlg = FileDialog.open(title = "Select .xls/.xlsx Spreadsheet")
    fDlg.setFilter { f =>
      import equal.Implicits._
      val ext = f.ext.toLowerCase
      ext === "xls" || ext === "xlsx"
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

                query(book.sheetMap(name), fIn)
              }
            case sheet +: _ =>
              query(sheet, fIn)
            case _ =>
              OptionPane.message(s"No sheets (pages) found in '$fInName'", OptionPane.Message.Error).show(None)
          }
      }
    }
  }

  private def selectionToShape(rows: Range, cols: Range): String =
    s"[${colToName(cols.head)}${rows.head + 1}:${colToName(cols.last)}${rows.last + 1}]"

  private def shapeToSelection(shape: String): Option[(Range, Range)] = {
    val ShapeReg = """\[([A-Z]+)(\d+)\:([A-Z]+)(\d+)\]""".r
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
  }

  private sealed trait VarDef {
    def name: String
    def flat(sheet: Sheet): Array[Float]
    def native(sheet: Sheet): ma2.Array
    def rank: Int
    def size: Long
    def dimMap: Map[Int, VarDef]
    def units: Option[String]
    def description: Option[String]
  }

  private final case class TableVarDef(name: String, rows: Range, cols: Range, description: Option[String],
                                       units: Option[String], dim0: Option[VarDef], dim1: Option[VarDef])
    extends VarDef {

    def rank: Int = if (rows.isEmpty || cols.isEmpty) 0 else if (rows.size == 1 || cols.size == 1) 1 else 2
    def size: Long = rows.size.toLong * cols.size

    def dimMap: Map[Int, VarDef] = {
      val m1 = dim0.fold(Map.empty[Int, VarDef])(v => Map(0       -> v))
      val m2 = dim1.fold(m1                    )(v => Map(m1.size -> v))
      m2
    }

    override def toString: String = {
      val shape = selectionToShape(rows, cols)
      val dim0N = dim0.map(_.name)
      val dim1N = dim1.map(_.name)
      s"$productPrefix($name, $shape, description = $description, unit = $units, dim0 = $dim0N, dim1 = $dim1N)"
    }

    def flat(sheet: Sheet): Array[Float] = mkFloatArray(sheet).flatten

    def native(sheet: Sheet): ma2.Array = {
      val arr2D = mkFloatArray(sheet)
      val arrJ  = if (rank == 2) arr2D else arr2D.flatten
      ma2.Array.factory(arrJ)
    }

    private def mkFloatArray(sheet: Sheet): Array[Array[Float]] = rows.map { ri =>
      sheet.rowMap.get(ri).fold(Array.fill(cols.size)(Float.NaN)) { row =>
        cols.map { ci =>
          @tailrec def getNumber(c: Cell): Float = c match {
            case NumericCell(_, v) => v.toFloat
            case StyledCell(c1, _) => getNumber(c1)
            case _ => Float.NaN
          }

          row.cellMap.get(ci).fold(Float.NaN)(getNumber)
        } (breakOut)
      }
    } (breakOut)
  }

  private final case class SyntheticDim(name: String, data: Array[Float]) extends VarDef {
    def rank = 1
    def size: Long  = data.length
    def dimMap      = Map.empty[Int, VarDef]
    def units       = Option.empty[String]
    def description = Option.empty[String]

    def flat  (sheet: Sheet): Array[Float]  = data
    def native(sheet: Sheet): ma2.Array     = ma2.Array.factory(flat(sheet))
  }

  @tailrec private def cellToString(c: Cell): Any = c match {
    case StringCell (_, data) => data
    case NumericCell(_, data) => data
    case BooleanCell(_, data) => data
    case StyledCell(c1, _) => cellToString(c1)
  }

  @tailrec private def colToNameR(i: Int, post: String): String = {
    val m = i % 26
    val d = i / 26
    val c = (m + 'A').toChar
    val s = s"$c$post"
    if (d == 0) s else colToNameR(d - 1, s)
  }

  private def colToName(i: Int): String = colToNameR(i, "")

  @tailrec private def nameToColR(i: Int, m: Int, s: String): Int =
    if (s.isEmpty) i - 1 else {
      val j = i + ((s.charAt(s.length - 1) - 'A') + 1) * m
      nameToColR(j, m * 26, s.substring(0, s.length - 1))
    }

  private def nameToCol(s: String): Int = nameToColR(0, 1, s)

  private def query(sheet: Sheet, fIn: File): Unit = {
    val tableRows = sheet.sortedRows
    val maxRow    = tableRows.map(_.index).max
    val numRows   = maxRow + 1
    val maxCol    = tableRows.map(r => r.sortedCells.map(_.index).max).max
    val numCols   = maxCol + 1

    val rowData = Array.tabulate(numRows) { ri =>
      Array.tabulate(numCols) { ci =>
        sheet.rowMap.get(ri).fold[Any]("")(_.cellMap.get(ci).fold[Any]("")(cellToString))
      }
    }

    val colNames = (0 until numCols).map(colToName)

    val inVisRows = math.min(16, numRows)

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
    val ggRows = new ListView[Int](1 to numRows) {
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

    val mTableOut     = new DefaultTableModel(Array[AnyRef]("Name", "Shape", "Description", "Unit", "Row Dim", "Col Dim"), 0)
    ggTableOut.model  = mTableOut
    val rowHeightOut = ggTableOut.rowHeight // + ggTableOut.peer.getRowMargin

    ggTableOut.preferredViewportSize = {
      val dim = ggTableOut.preferredViewportSize
      dim.height = math.min(4 * rowHeightOut, dim.height)
      dim
    }

    val ggScrollOut = new ScrollPane(ggTableOut)

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
      import equal.Implicits._
      if (varName === "untitled") Left(s"Variable in row ${row + 1} must be named")
      else {
        val shapeS  = mTableOut.getValueAt(row, 1).toString
        shapeToSelection(shapeS).fold[Either[String, TableVarDef]](
          Left(s"Shape in row ${row + 1} is invalid $shapeS")
        ) { case (rows, cols) =>
          val rDimSO    = getCol(4)
          val cDimSO    = getCol(5)
          val isVector  = rows.size == 1 || cols.size == 1
          val rSize     = if (rows.size == 1) cols.size else rows.size
          val rDimOptE  = rDimSO.fold[Either[String, Option[VarDef]]](
            Right(if (isVector) None else Some(SyntheticDim(s"${varName}_dim0", Array.tabulate(rSize)(_.toFloat))))
          ) { rDimName =>
            in.find(_.name === rDimName).fold[Either[String, Option[VarDef]]](
              Left(s"Cannot find dimension $rDimSO"))(rDim => Right(Some(rDim)))
          }
          rDimOptE.right.flatMap { rDimOpt =>
            val cDimOptE: Either[String, Option[VarDef]] = if (isVector) Right(None) else {
              val cSize   = cols.size
              val cDimOpt = cDimSO.fold[Option[VarDef]](
                Some(SyntheticDim(s"${varName}_dim1", Array.tabulate(cSize)(_.toFloat)))
              ) { cDimName =>
                in.find(_.name === cDimName)
              }
              cDimOpt.fold[Either[String, Option[VarDef]]](Left(s"Cannot find dimension $cDimSO")) { cDim =>
                Right(Some(cDim))
              }
            }
            cDimOptE.right.map { cDimOpt =>
              TableVarDef(name = varName, rows = rows, cols = cols, description = desc, units = unit,
                dim0 = rDimOpt, dim1 = cDimOpt)
            }
          }
        }
      }
    }

    @tailrec def showPane(): List[TableVarDef] = {
      val res = OptionPane(pane, OptionPane.Options.OkCancel, OptionPane.Message.Plain).show(None, title0)
      import equal.Implicits._
      if (res === OptionPane.Result.Yes) {
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

    FileDialog.save(init = Some(fIn.replaceExt(".nc")), title = "Select Output NetCDF File")
      .show(None).foreach { fOut =>
        try {
          create(vars, sheet, fOut) // XXX TODO -- could run in a `Future` or `Processor`
        } catch {
          case NonFatal(ex) => DialogSource.Exception(ex -> title0).show(None)
        }
      }

    //    vars.foreach(println)
  }

  private def create(vars: List[TableVarDef], sheet: Sheet, fOut: File): Unit = {
    import Implicits._
    val location  = fOut.path
    val writer    = nc2.NetcdfFileWriter.createNew(nc2.NetcdfFileWriter.Version.netcdf3, location, null)

    // define dimensions and variables

    val synthVars = vars.flatMap { v =>
      v.dimMap.values collect {
        case synth: SyntheticDim => synth
      }
    }
    val allVars = vars ++ synthVars

    val (_ /* dimMap */, varMap) = ((Map.empty[String, nc2.Dimension], Map.empty[String, nc2.Variable]) /: allVars) {
      case ((dimMap0, varMap0), v) =>
        val dimMap1 = v.dimMap
        val (dimMap2, dims1) = ((dimMap0, Vec.empty[nc2.Dimension]) /: (0 until v.rank)) { case ((dimMap3, dims0), dimIdx) =>
          val dim = dimMap1.get(dimIdx).fold[nc2.Dimension] {
            writer.addDimension(null, v.name, v.size.toInt)
          } { vd =>
            dimMap3.getOrElse(vd.name, {
              writer.addDimension(null, vd.name, vd.size.toInt)
            })
          }
          (dimMap3 + (dim.name -> dim), dims0 :+ dim)
        }
        import JavaConverters._
        val vNC = writer.addVariable(null, v.name, ma2.DataType.FLOAT, dims1.asJava)
        v.units      .foreach(units => vNC.addAttribute(new nc2.Attribute(CDM.UNITS      , units)))
        v.description.foreach(desc  => vNC.addAttribute(new nc2.Attribute(CDM.DESCRIPTION, desc )))

        (dimMap2, varMap0 + (v.name -> vNC))
    }

    // end definition mode
    writer.create()

    allVars.foreach { v =>
      val vNC = varMap(v.name)
      val arr = v.native(sheet)
      writer.write(vNC, arr)
    }

    writer.close()

    //    val (outDims, outDimsV) = inDims.map { inDim =>
    //      val size    = inDim.size
    //      val outDim  = writer.addDimension(null, inDim.name, size)
    //      val inVar   = in.variableMap(inDim.name)
    //      val outVarD = dupVar(writer, inVar, Seq(outDim))
    //      (outDim, outVarD)
    //    } .unzip
  }
}