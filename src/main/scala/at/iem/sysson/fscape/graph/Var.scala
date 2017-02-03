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
import de.sciss.serial.{DataOutput, ImmutableSerializer}

object Var {
  object PlayLinear {
    trait Value extends UGB.Value with Aux {
      def matrix: Matrix.Key
      def reader: Matrix.Reader

      final def write(out: DataOutput): Unit = {
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
        val reader = ref.reader
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

  object Op {
    final case class Drop(dim: Dim) extends Op {
      override def productPrefix: String  = getClass.getName
      override def toString               = s"Drop($dim)"
    }
  }
  sealed trait Op

  object Spec {
    final case class Dim(name: String, units: String, values: Vec[Double]) extends Aux {
      def write(out: DataOutput): Unit = {
        out.writeByte(103)
        out.writeUTF(name )
        out.writeUTF(units)
        ImmutableSerializer.indexedSeq[Double].write(values, out)
      }
    }

    final case class Value(name: String, units: String, dimensions: Vec[Spec.Dim])
      extends UGB.Value with Aux {

      lazy val shape : Vec[Int]  = dimensions.map(_.values.size)
      lazy val rank  : Int       = shape.size
      lazy val size  : Long      = if (shape.isEmpty) 0L else (1L /: shape)(_ * _)

      def write(out: DataOutput): Unit = {
        out.writeByte(102)
        out.writeUTF(name )
        out.writeUTF(units)
        out.writeShort(dimensions.size)
        dimensions.foreach(_.write(out))
      }
    }
  }
  final case class Spec(variable: Var, ops: Vec[Op]) extends UGB.Input with UGB.Key {
    type Key    = Spec
    type Value  = Spec.Value

    def key: Key = this

    def drop(dim: Dim): Spec = copy(ops = ops :+ Op.Drop(dim))

    private def unCapitalize(s: String): String =
      if (s.isEmpty || s.charAt(0).isLower) s
      else {
        val chars = s.toCharArray
        chars(0) = chars(0).toLower
        new String(chars)
      }

    override def productPrefix: String  = getClass.getName
    override def toString     : String  = (s"$variable.spec" /: ops)((res, op) => s"$res.${unCapitalize(op.toString)}")
  }

  trait InfoLike {
    def matrix: Matrix.Key

    def shape : Vec[Int]
    def name  : String
    def units : String

    final def rank: Int   = shape.size
    final def size: Long  = if (shape.isEmpty) 0L else (1L /: shape)(_ * _)
  }

  trait Info extends InfoLike with UGB.Value with Aux {
    final def write(out: DataOutput): Unit = {
      out.writeByte(101)
      matrix.write(out)
    }
  }
}
final case class Var(name: String) extends Lazy.Expander[Unit] with UGB.Key {
  protected def makeUGens(implicit b: UGenGraph.Builder): Unit = ()

  /** Unrolls all dimensions in time. */
  def playLinear(): Var.PlayLinear = Var.PlayLinear(this)

  def spec: Var.Spec = Var.Spec(this, Vector.empty)
}