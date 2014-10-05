///*
// *  AuralSonificationOLD.scala
// *  (SysSon)
// *
// *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
// *  Copyright (c) 2014 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU General Public License v3+
// *
// *
// *	For further information, please contact Hanns Holger Rutz at
// *	contact@sciss.de
// */
//
//package at.iem.sysson
//package sound
//
//import de.sciss.lucre.event.{Observable, Sys}
//import de.sciss.lucre.synth.{Sys => SSys}
//
//object AuralSonificationOLD {
//  sealed trait Update
//  /** The sonification is being prepared. For example, grapheme production starts. */
//  case object Preparing extends Update
//  /** The sonification transport is actually started. */
//  case object Playing   extends Update
//  /** The sonification transport is stopped. */
//  case object Stopped   extends Update
//
//  /** The sonification reported an elapsed state.
//    *
//    * @param dim      the logical dimension key
//    * @param ratio    the percentage between zero and one
//    * @param value    the dimension's current value
//    */
//  case class Elapsed(dim: String, ratio: Float, value: Float) extends Update
//
//  // private[sysson] def current(): AuralSonificationOLD[_, _] = Impl.current()
//
//  case class MissingSource         (key : String)
//    extends Exception(s"The source for key '$key' is not assigned")
//
//  case class MissingDimension      (key : String)
//    extends Exception(s"The dimension for key '$key' is not assigned")
//
//  case class MissingSourceDimension(sourceKey: String, name: String)
//    extends Exception(s"The source for key '$sourceKey' does not have dimension '$name'")
//}
//trait AuralSonificationOLD[S <: Sys[S], I <: SSys[I]] extends Observable[S#Tx, AuralSonificationOLD.Update] {
//  def play()(implicit tx: S#Tx): Unit
//  def stop()(implicit tx: S#Tx): Unit
//
//  def state(implicit tx: S#Tx): AuralSonificationOLD.Update
//
//  // def auralPresentation: AuralPresentation[I]
//
//  /* Attribute keys are used to link a graph element to an automatically generated attribute
//   * such as a control value.
//   */
//  private[sysson] def attributeKey(elem: Any): String
//}
