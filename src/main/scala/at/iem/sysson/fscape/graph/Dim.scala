/*
 *  Dim.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2017 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package fscape.graph

import de.sciss.fscape.lucre.{UGenGraphBuilder => UGB}
import de.sciss.fscape.{GE, Lazy, UGenGraph, UGenInLike}

object Dim {
  object Values {
    def apply(dim: Dim): Values = apply(dim = dim, maxSize = 1000)
  }
  final case class Values(dim: Dim, maxSize: Int)
    extends GE.Lazy with UGB.Input /* MatrixPrepare.DimGE with MatrixPrepare.ValuesGE */ {

    private[sysson] def maxNumChannels = maxSize

    type Key    = Dim
    type Value  = Nothing  // XXX TODO

    def key: Key = dim

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = ???

    override def productPrefix  = s"Dim$$Values"
    override def toString       = s"$dim.values"
  }

  final case class Size(dim: Dim)
    extends GE.Lazy with UGB.Input with UGB.Key {

    override def productPrefix  = s"Dim$$Size"
    override def toString       = s"Dim.Size($dim)"

    type Key      = Size
    type Value    = UGB.Unit
    def key: Key  = this

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub = UGB.get(b)
      ???
    }
  }

  final case class SuccSize(dim: Dim)
    extends GE.Lazy with UGB.Input with UGB.Key {

    override def productPrefix  = s"Dim$$SuccSize"
    override def toString       = s"Dim.SuccSize($dim)"

    type Key      = SuccSize
    type Value    = UGB.Unit
    def key: Key  = this

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub = UGB.get(b)
      ???
    }
  }
}
/** Specification of a data source dimension
  *
  * @param variable Data source to which this dimension refers
  * @param name     Logical name by which the dimension is referred to
  */
final case class Dim(variable: Var, name: String)
  extends Lazy.Expander[Unit] /* UserInteraction */ with UGB.Key {

  def values              : Dim.Values = Dim.Values(this)
  def values(maxSize: Int): Dim.Values = Dim.Values(this, maxSize = maxSize)

  /** Produces a graph element reflecting the number of samples (`stopIndex - startIndex`) covered by
    * this selection.
    */
  def size: Dim.Size = Dim.Size(this) // stopIndex - startIndex

  def succSize: Dim.SuccSize = Dim.SuccSize(this)

  protected def makeUGens(implicit b: UGenGraph.Builder): Unit = ()
}