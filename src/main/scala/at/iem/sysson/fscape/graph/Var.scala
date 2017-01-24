/*
 *  Var.scala
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
package fscape
package graph

import de.sciss.fscape.UGen.Aux
import de.sciss.fscape.lucre.{UGenGraphBuilder => UGB}
import de.sciss.fscape.stream.{StreamIn, StreamOut, VarPlayLinear, Builder => SBuilder}
import de.sciss.fscape.{GE, Lazy, UGen, UGenGraph, UGenIn, UGenInLike, UGenSource}
import de.sciss.lucre.matrix.Matrix
import de.sciss.serial.DataOutput

object Var {
  object PlayLinear {
    trait Value extends UGB.Value with Aux {
      def matrix: Matrix.Key
      def reader: Matrix.Reader

      def write(out: DataOutput): Unit = {
        out.writeByte(100)
        matrix.write(out)
      }
    }

    final case class WithRef(ref: Value) extends UGenSource.SingleOut {

      protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike =
        makeUGen(Vector.empty)

      protected def makeUGen(args: Vec[UGenIn])(implicit b: UGenGraph.Builder): UGenInLike =
        UGen.SingleOut(this, args, aux = ref :: Nil)

      def makeStream(args: Vec[StreamIn])(implicit b: SBuilder): StreamOut = {
        val in      = args.map(_.toDouble)
        val reader  = ref.reader
        VarPlayLinear(matrix = reader)
      }

      override def productPrefix: String = classOf[WithRef].getName
    }
  }
  final case class PlayLinear(variable: Var) extends GE.Lazy with UGB.Input {
    type Key    = Var
    type Value  = PlayLinear.Value

    def key: Key = variable

    override def productPrefix: String  = classOf[PlayLinear].getName
    override def toString               = s"$variable.playLinear()"

    protected def makeUGens(implicit b: UGenGraph.Builder): UGenInLike = {
      val ub    = UGB.get(b)
      val value = ub.requestInput(this)
      PlayLinear.WithRef(value)
    }
  }
}
final case class Var(name: String) extends Lazy.Expander[Unit] with UGB.Key {
  protected def makeUGens(implicit b: UGenGraph.Builder): Unit = ()

  /** Unrolls all dimensions in time. */
  def playLinear(): Var.PlayLinear = Var.PlayLinear(this)
}