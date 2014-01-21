package at.iem.sysson

import de.sciss.lucre.event.Sys

object Matrix {
  case class Nc[S <: Sys[S]](source: DataSource[S], name: String, shape: Vec[(Int, String)]) extends Matrix[S]
}
sealed trait Matrix[S <: Sys[S]] {
  def name  : String
  def shape : Vec[(Int, String)]

  def rank  : Int   = shape.size
  def size  : Long  = (1L /: shape)((prod, elem) => prod * elem._1)
}
