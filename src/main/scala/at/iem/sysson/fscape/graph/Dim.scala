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

import de.sciss.fscape.UGen.Aux
import de.sciss.fscape.graph.ConstantL
import de.sciss.fscape.lucre.{UGenGraphBuilder => UGB}
import de.sciss.fscape.{GE, Lazy, UGenGraph, UGenInLike}
import de.sciss.serial.DataOutput

object Dim {
  object Values {
    def apply(dim: Dim): Values = apply(dim = dim, maxSize = 1000)
  }
  final case class Values(dim: Dim, maxSize: Int)
    extends GE.Lazy with UGB.Input /* MatrixPrepare.DimGE with MatrixPrepare.ValuesGE */ {

    private[sysson] def maxNumChannels = maxSize

    type Key    = Dim
    type Value  = Matrix.ValueSeq.Value

    def key: Key = dim

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = ???

    override def productPrefix: String  = s"Dim$$Values"
    override def toString               = s"$dim.values"
  }

  sealed trait InfoGE extends GE.Lazy with UGB.Input with UGB.Key {
    type Key      = InfoGE
    type Value    = Info

    def dim: Dim

    final def key: Key  = this
  }

  final case class Size(dim: Dim) extends InfoGE {

    override def productPrefix: String  = s"Dim$$Size"
    override def toString               = s"$dim.size"

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub    = UGB.get(b)
      val value = ub.requestInput(this)
      ConstantL(value.size)
    }
  }

  final case class SuccSize(dim: Dim) extends InfoGE {

    override def productPrefix: String  = s"Dim$$SuccSize"
    override def toString               = s"$dim.succSize"

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub    = UGB.get(b)
      val value = ub.requestInput(this)
      val succ  = value.variable.shape.drop(value.index + 1)
      val sz    = (1L /: succ)(_ * _)
      ConstantL(sz)
    }
  }

  trait Info extends Matrix.InfoLike with UGB.Value with Aux {
    def variable: Matrix.Info
    def index   : Int

    // final def shape: Vec[Int] = Vector(variable.shape(index))

    final def write(out: DataOutput): Unit = {
      // we don't need to write our own `matrix` key
      // because that information is redundant wrt
      // `variable`. We only need to write the index
      out.writeByte(104)
      variable.write(out)
      out.writeShort(index)
    }
  }
}
/** Specification of a data source dimension
  *
  * @param variable Data source to which this dimension refers
  * @param name     Logical name by which the dimension is referred to
  */
final case class Dim(variable: Matrix, name: String)
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