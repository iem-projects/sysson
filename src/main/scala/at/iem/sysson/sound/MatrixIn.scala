///*
// *  MatrixIn.scala
// *  (SysSon)
// *
// *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
// *  Written by Hanns Holger Rutz.
// *
// *	This software is published under the GNU General Public License v2+
// *
// *
// *	For further information, please contact Hanns Holger Rutz at
// *	contact@sciss.de
// */
//
//package at.iem.sysson
//package sound
//
//import de.sciss.synth._
//
//// OBSOLETE
//object MatrixIn {
//  def ar(key: String): MatrixIn = apply(audio, key)
//  def kr(key: String): MatrixIn = apply(control, key)
//
//  private def outsideOfContext() = sys.error( "Expansion out of context" )
//
//  private final case class NumRows(m: MatrixIn) extends GE {
//    def rate = scalar
//    def expand: UGenInLike = builder.getMatrixInSource(m).numRows
//  }
//
//  private final case class NumColumns(m: MatrixIn) extends GE {
//    def rate = scalar
//    def expand: UGenInLike = builder.getMatrixInSource(m).numColumns
//  }
//
//  private def builder: UGenGraphBuilderOLD = UGenGraph.builder match {
//    case b: UGenGraphBuilderOLD  => b
//    case _                    => MatrixIn.outsideOfContext()
//  }
//}
//case class MatrixIn(rate: Rate, key: String) extends GE.Lazy {
//  import MatrixIn._
//
//  protected def makeUGens: UGenInLike = builder.addMatrixIn(this)
//
//  def numRows:    GE = NumRows   (this)
//  def numColumns: GE = NumColumns(this)
//}