/*
 *  MkMatrix.scala
 *  (FScape)
 *
 *  Copyright (c) 2001-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.fscape
package stream

import akka.stream.{Attributes, FlowShape}
import at.iem.sysson.fscape.graph.Matrix
import de.sciss.file._
import de.sciss.fscape.lucre.FScape.Output
import de.sciss.fscape.lucre.UGenGraphBuilder.OutputRef
import de.sciss.fscape.stream.impl.{BlockingGraphStage, NodeImpl}
import de.sciss.serial.DataOutput

object MkMatrix {
  def apply(ref: OutputRef, file: File, spec: Matrix.Spec.Value, in: OutD)(implicit b: Builder): OutL = {
    val source  = new Stage(ref, file, spec)
    val stage   = b.add(source)
    b.connect(in, stage.in)
    stage.out
  }

  private final val name = "MkMatrix"

  private type Shape = FlowShape[BufD, BufL]

  private final class Stage(ref: OutputRef, file: File, spec: Matrix.Spec.Value)(implicit protected val ctrl: Control)
    extends BlockingGraphStage[Shape](s"$name($file)") {

    val shape = FlowShape(InD(s"$name.in"), OutL(s"$name.out"))

    def createLogic(attr: Attributes): NodeImpl[Shape] =
      new Logic(shape, ref, file, spec)
  }

  private final class Logic(shape: Shape, ref: OutputRef, protected val file: File,
                            protected val spec: Matrix.Spec.Value)(implicit ctrl: Control)
    extends NodeImpl(s"$name($file)", shape) with MatrixOut.AbstractLogic {

    override protected def stopped(): Unit = {
      super.stopped()
      if (isSuccess) ref.complete(new Output.Writer {
        def write(out: DataOutput): Unit = out.writeUTF(file.path)
      })
    }
  }
}