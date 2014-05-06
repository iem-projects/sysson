package de.sciss.synth
package ugen

case class Size(in: GE) extends GE.Lazy with ScalarRated {
  def makeUGens: UGenInLike = {
    val exp = in.expand.outputs
    exp.size
  }
}
