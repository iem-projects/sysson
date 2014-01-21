/*
 *  SonificationSourceOLD.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

// OBSOLETE
sealed trait SonificationSourceOLD {
  def numColumns:   Int
  def numRows:      Int
  def section:      VariableSection
  def isTransposed: Boolean
}

case class ColumnSource(section: VariableSection) extends SonificationSourceOLD {
  require(section.reducedRank == 1, s"Reduced rank must be 1 for a column source: $section")

  lazy val size     = section.reducedShape.head
  def numRows       = size
  def numColumns    = 1
  def isTransposed  = false

  override def toString = s"$section.asColumn"
}

case class RowSource(section: VariableSection) extends SonificationSourceOLD {
  require(section.reducedRank == 1, s"Reduced rank must be 1 for a row source: $section")

  lazy val size     = section.reducedShape.head
  def numRows       = 1
  def numColumns    = size
  def isTransposed  = false

  override def toString = s"$section.asRow"
}

case class MatrixSource(section: VariableSection, rowDim: Int, columnDim: Int) extends SonificationSourceOLD {
  import Implicits._

  require(section.reducedRank == 2, s"Reduced rank must be 2 for a matrix source: $section")
  require(section.dimensions(rowDim).size > 1 && section.dimensions(columnDim).size > 1,
    s"Row ($rowDim) or column ($columnDim) dimension will be reduced")

  def numRows       = section.dimensions(rowDim).size
  def numColumns    = section.dimensions(columnDim).size
  def isTransposed  = rowDim < columnDim  // XXX TODO: test if this is correct

  override def toString = {
    val dims    = section.reducedDimensions
    val rowName = dims(rowDim   ).nameOption.getOrElse(rowDim   )
    val colName = dims(columnDim).nameOption.getOrElse(columnDim)
    s"""$section.asMatrix(row: "$rowName", col: "$colName")"""
  }
}