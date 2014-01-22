/*
 *  Patch.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
 *  Written by Hanns Holger Rutz.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson

import de.sciss.synth.SynthGraph

object PatchOLD {
  val empty = withoutSource("<empty>", SynthGraph {})

  case class Source(name: String, code: String)

  def withoutSource(name: String, graph: SynthGraph): PatchOLD =
    PatchOLD(source = PatchOLD.Source(name = name, code = ""), graph = graph)
}
case class PatchOLD(source: PatchOLD.Source, graph: SynthGraph) {
  def name = source.name
}