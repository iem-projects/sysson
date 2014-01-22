/*
 *  DataSource.scala
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

import ucar.nc2
import java.io.File
import impl.{DataSourceImpl => Impl}
import de.sciss.serial.{DataInput, Writable, Serializer}
import de.sciss.lucre.event.Sys

object DataSource {
  def apply[S <: Sys[S]](path: String)(implicit tx: S#Tx): DataSource[S] = Impl(path)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DataSource[S]] =
    Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): DataSource[S] = Impl.read(in, access)

  object Variable {
    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Variable[S]] = Impl.varSerializer

    def apply[S <: Sys[S]](source: DataSource[S], name: String)(implicit tx: S#Tx): Variable[S] =
      Impl.variable(source, name)

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Variable[S] = Impl.readVariable(in, access)
  }
  trait Variable[S <: Sys[S]] extends Writable {
    def source: DataSource[S]
    def name: String
    def data(workspace: Workspace[S])(implicit tx: S#Tx): nc2.Variable
  }
}
/** A document represents one open data file. */
trait DataSource[S <: Sys[S]] extends Writable {
  /** Path to the document's underlying file (NetCDF). */
  def path: String

  def file: File

  def data(workspace: Workspace[S])(implicit tx: S#Tx): nc2.NetcdfFile
}