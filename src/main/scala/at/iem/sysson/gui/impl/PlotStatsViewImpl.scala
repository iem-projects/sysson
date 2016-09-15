/*
 *  PlotStatsViewImpl.scala
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
package impl

import java.awt
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer}
import javax.swing.{JTable, SwingConstants}

import at.iem.sysson.Implicits._
import de.sciss.equal
import de.sciss.file.File
import de.sciss.lucre.matrix.{DataSource, Matrix, Reduce}
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{defer, deferTx}
import de.sciss.model.impl.ModelImpl
import de.sciss.synth.proc.Workspace
import ucar.nc2

import scala.annotation.tailrec
import scala.concurrent.stm.Ref
import scala.swing.Swing._
import scala.swing.{BoxPanel, Component, Label, Orientation, Table}
import scala.util.{Failure, Success}

object PlotStatsViewImpl {
  def apply[S <: Sys[S]](plot: Plot[S])(implicit tx: S#Tx, workspace: Workspace[S]): PlotStatsView[S] = {
    implicit val resolver = WorkspaceResolver[S]
    new Impl[S].init(plot)
  }

  private final class Impl[S <: Sys[S]](implicit resolver: DataSource.Resolver[S])
    extends PlotStatsView[S]
    with ComponentHolder[Component]
    with ModelImpl[Stats.Variable] {

    private var observer: Disposable[S#Tx] = _

    def init(plot: Plot[S])(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      observer = plot.changed.react { implicit tx => u => u.changes.foreach {
        case Plot.MatrixChange(mu) => checkMatrix(mu.matrix)
        case _ =>
      }}
      checkMatrix(plot.matrix)
      this
    }

    private val fileRef = Ref(Option.empty[File])

    private def checkMatrix(m: Matrix[S])(implicit tx: S#Tx): Unit =
      findVariable(m).foreach(spawnStats)

    @tailrec private def findVariable(m: Matrix[S])(implicit tx: S#Tx): Option[nc2.Variable] = m match {
      case Matrix.Var(vr)     => findVariable(vr())
      case Reduce(peer, _, _) => findVariable(peer)
      case v: DataSource.Variable[S] =>
        val v1 = v.data()
        Some(v1)

      case _ => None
    }

    private var tab: Table = _

    private def guiInit(): Unit = {
      tab = new Table(
        // use `.toString` for now, because default renderer applies some bad truncation
        Array[Array[Any]](
          Array("size"   , "?" /* tot.num    */),
          Array("min"    , "?" /* tot.min    */), // format tot.min   ),
          Array("max"    , "?" /* tot.max    */), // format tot.max   ),
          Array("mean"   , "?" /* tot.mean   */), // format tot.mean  ),
          Array("std-dev", "?" /* tot.stddev */)  // format tot.stddev)
        ),
        List("Key", "Value"))

      val colKey = tab.peer.getColumnModel.getColumn(0)
      colKey.setPreferredWidth(80)
      val colVal = tab.peer.getColumnModel.getColumn(1)
      tab.peer.setDefaultRenderer(classOf[java.lang.Double], new DefaultTableCellRenderer {
        setHorizontalAlignment(SwingConstants.RIGHT)

        private def formatValue(in: Any): Any = {
          // println("Aqui")
          if (in == null) return null
          // fmt.format(in)
          // better support for exponential formatting actually
          in match {
            case d: Double => d.toFloat.toString
            case _ => in
          }
        }

        override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean,
                                                   hasFocus: Boolean, row: Int,
                                                   column: Int): awt.Component =
          super.getTableCellRendererComponent(table, formatValue(value), isSelected, hasFocus, row, column)
      })
      colVal.setPreferredWidth(140)
      component = new BoxPanel(Orientation.Vertical) {
        border = EmptyBorder(0, 16, 0, 0)
        contents += new Label("<html><body><b>Overall Statistics</b></body>")
        contents += VStrut(6)
        contents += tab
      }
    }

    private def updateStatsTable(sv: Stats.Variable): Unit = {
      val tot = sv.total
      val m   = tab.model.asInstanceOf[AbstractTableModel]
      m.setValueAt(tot.num   , 0, 1)
      m.setValueAt(tot.min   , 1, 1)
      m.setValueAt(tot.max   , 2, 1)
      m.setValueAt(tot.mean  , 3, 1)
      m.setValueAt(tot.stddev, 4, 1)
    }

    private var _stats = Option.empty[Stats.Variable]

    def stats: Option[Stats.Variable] = _stats

    private def spawnStats(in: nc2.Variable)(implicit tx: S#Tx): Unit = {
      val nf  = in.file
      val f   = nf.file
      val fOpt= Some(f)
      val of  = fileRef.swap(fOpt)(tx.peer)
      import equal.Implicits._
      if (of === fOpt) return  // same file as before

      import at.iem.sysson.Stats.executionContext
      val fut = Stats.get(nf)(tx.peer)
      tx.afterCommit(fut.onComplete {
        case Success(Stats(map)) =>
          if (fileRef.single.get === fOpt) defer {
            // see if stats are available for the plotted variable
            val s = map.get(in.name)
            s.foreach { sv =>
              _stats = s
              updateStatsTable(sv)
              dispatch(sv)
            }
          }

        case Failure(ex) =>
          Console.err.println("Statistics could not be calculated:")
          ex.printStackTrace()
      })
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      fileRef.set(None)(tx.peer)
    }
  }
}