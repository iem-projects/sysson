/*
 *  Library.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013 Institute of Electronic Music and Acoustics, Graz.
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

import scala.concurrent.{blocking, Future}
import scala.collection.mutable

object Library {
  sealed trait Node { def name: String }
  case class Branch(name: String, children: Vec[Node]) extends Node
  case class Child(patch: Patch.Source) extends Node {
    def name = patch.name
  }

  // sealed trait Update

  private val sync    = new AnyRef
  private val codeMap = new mutable.WeakHashMap[Patch.Source, Patch]

  def compile(source: Patch.Source): Future[Patch] = sync.synchronized(codeMap.get(source)).fold {
    Code.future {
      val graph = blocking { Code.SynthGraph(source.code).execute() }
      val res   = Patch(source, graph)
      sync.synchronized(codeMap.put(source, res))
      res
    }

  } (Future.successful)
}
trait Library /* extends Model[Library.Update] */ {
  def root: Library.Branch
}

object TestLibrary extends Library {
  import Library.{Child, Branch}
  // import de.sciss.synth.{GE, SynthGraph, ugen}
  // import ugen._
  // import graph._

  val root = Branch("root",
    Vec(
      Child(Patch.Source("Test-Static-Range",
        """val plevRange = SelectedRange(Pressure)
          |plevRange.values.poll(Impulse.kr(0), label = "plev")
          |""".stripMargin
      )),

      Child(Patch.Source("Test-Dynamic-Range",
        """val timeRange = SelectedRange(Time)
          |val freq      = 1.0 // speed.kr
          |val time: GE  = timeRange.play(freq)
          |time.poll(1)
          |""".stripMargin
      )),

      Child(Patch.Source("Test-Sonif",
        """val latRange  = SelectedRange(Latitude)
          |val lonRange  = SelectedRange(Longitude)
          |val timeRange = SelectedRange(Time)
          |val plev      = SelectedRange(Pressure)
          |// val speed       = RotaryKnob(speedSpec)   // --> position, label etc. via view-map ?
          |
          |val speed     = UserValue(key = "speed", default = 1.0)
          |val pitch     = UserValue(key = "freq-factor", default = 1.0)
          |
          |val sel       = Var().select(latRange, lonRange, timeRange, plev) // .average(Longitude)
          |val freq      = speed.value
          |// freq.poll(Impulse.kr(0), label = "freq")
          |val time      = timeRange.play(freq)
          |val data      = sel.play(time)
          |// val sig       = WhiteNoise.ar // sel.ar(time)
          |
          |val latAxis   = data.axis(Latitude).values: GE
          |// latAxis.values.poll(Impulse.kr(0), label = "lat")
          |// NOT YET WORKING:
          |// val latAxisN  = (latAxis.values: GE).linlin(latAxis.startValue, latAxis.endValue, -1, 1)
          |
          |// latAxis.poll(Impulse.kr(0), "lat")
          |
          |val latMin    = Reduce.min(latAxis)
          |val latMax    = Reduce.max(latAxis)
          |val latAxisN  = latAxis.linlin(-90 /* latMin */, 90 /* latMax */, -1, 1)
          |
          |latMin.poll(0, "lat-min")
          |latMax.poll(0, "lat-max")
          |
          |val osc: GE = SinOsc.ar(data * pitch.value)
          |//WrapOut(Pan2.ar(Mix.mono(osc) * 0.1))
          |val pan = Pan2.ar(osc * 0.1, latAxisN)
          |WrapOut(Mix(pan))
          |
          |// Pan2.ar(osc, sig.axisValues(Latitude).linlin(latRange.startValue, latRange.endValue, -1, 1))
          |""".stripMargin
      )),

      Child(Patch.Source("With-Altitude",
        """SelectedRange(Altitude)
          |""".stripMargin
      ))
    )
  )
}