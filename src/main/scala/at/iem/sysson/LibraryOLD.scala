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
import de.sciss.model
import model.Model
import de.sciss.model.impl.ModelImpl

object LibraryOLD {
  sealed trait Node {
    var name: String
  }

  sealed trait Update {
    def node: Node
  }

  object Branch {
    sealed trait Update extends LibraryOLD.Update {
      def branch: Branch
      def node = branch
    }
    sealed trait ChildUpdate extends Update {
      def index: Int
      def child: Node
    }
    case class Inserted   (branch: Branch, index: Int, child: Node) extends ChildUpdate
    case class Removed    (branch: Branch, index: Int, child: Node) extends ChildUpdate
    case class NodeChanged(branch: Branch, index: Int, child: Node, change: LibraryOLD.Update) extends ChildUpdate
    case class Renamed    (branch: Branch, change: model.Change[String])  extends Update

    def apply(name: String, children: Node*): Branch = {
      val res = new Impl(name)
      children.foreach(res.insert(-1, _))
      res
    }
  }
  trait Branch extends Node with Model[Branch.Update] {
    def children: Vec[LibraryOLD.Node]
    def insert(index: Int, child: LibraryOLD.Node): Unit
    def remove(index: Int): Unit
  }

  object Leaf {
    sealed trait Update extends LibraryOLD.Update {
      def leaf: Leaf
      def node = leaf
    }

    // case class Renamed(leaf: Leaf, change: Change[String]) extends Update
    case class Changed(leaf: Leaf, source: model.Change[Patch.Source]) extends Update

    def apply(source: Patch.Source): Leaf = new LeafImpl(source)
  }
  trait Leaf extends Node with Model[Leaf.Update] {
    def source: Patch.Source
    def name = source.name
  }

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

  def apply(name: String, children: Node*): LibraryOLD = {
    val res = new Impl(name)
    children.foreach(res.insert(-1, _))
    res
  }

  private final class Impl(name0: String) extends LibraryOLD with ModelImpl[Branch.Update] {
    private val sync = new AnyRef
    private var _children = Vec.empty[Node]
    private var _name = name0

    def children = _children

    def insert(index: Int, child: Node): Unit = sync.synchronized {
      if (index >= _children.size) throw new IndexOutOfBoundsException(index.toString)

      val idx = if (index < 0) _children.size else index
      _children = _children.patch(idx, Vec(child), 0)
      dispatch(Branch.Inserted(this, idx, child))
    }

    def remove(index: Int): Unit = sync.synchronized {
      if (index < 0 || index >= _children.size) throw new IndexOutOfBoundsException(index.toString)

      val child = _children(index)
      _children = _children.patch(index, Vec.empty, 1)
      dispatch(Branch.Removed(this, index, child))
    }

    def name = _name
    def name_=(value: String): Unit = sync.synchronized {
      val oldName = _name
      if (oldName != value) {
        _name = value
        dispatch(Branch.Renamed(this, model.Change(oldName, value)))
      }
    }
  }

  private final class LeafImpl(source0: Patch.Source) extends Leaf with ModelImpl[Leaf.Update] {
    private var _source = source0
    private val sync = new AnyRef

    def name_=(value: String): Unit = sync.synchronized {
      if (name != value) {
        source = source.copy(name = value)
      }
    }

    def source = _source
    def source_=(value: Patch.Source): Unit = sync.synchronized {
      val oldSource = _source
      if (oldSource != value) {
        _source = value
        dispatch(Leaf.Changed(this, model.Change(oldSource, value)))
      }
    }
  }
}
trait LibraryOLD extends LibraryOLD.Branch

object TestLibrary {
  // import de.sciss.synth.{GE, SynthGraph, ugen}
  // import ugen._
  // import graph._

  import LibraryOLD.Leaf

  def apply(): LibraryOLD = {
    val root = LibraryOLD("test")
    val children = Vec(
      Leaf(Patch.Source("Test-Static-Range",
        """val plevRange = SelectedRange(Pressure)
          |plevRange.values.poll(Impulse.kr(0), label = "plev")
          |""".stripMargin
      )),

      Leaf(Patch.Source("Test-Dynamic-Range",
        """val timeRange = SelectedRange(Time)
          |val freq      = 1.0 // speed.kr
          |val time: GE  = timeRange.play(freq)
          |time.poll(1)
          |""".stripMargin
      )),

      Leaf(Patch.Source("Test-Sonif",
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

      Leaf(Patch.Source("With-Altitude",
        """SelectedRange(Altitude)
          |""".stripMargin
      ))
    )
    children.foreach(root.insert(-1, _))
    root
  }
}