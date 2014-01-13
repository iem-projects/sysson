//package at.iem.sysson
//package sonif
//
//import scala.swing.Frame
//import de.sciss.guiflitz.AutoView
//import at.iem.sysson.sound.SonificationSpec
//
//sealed trait UserScalar {
//  /** Logical name by which the value is referred to */
//  def name: String
//}
//
//case class RangeMinimum(range: String, name: String) extends UserScalar
//case class RangeMaximum(range: String, name: String) extends UserScalar
//case class RangeMarker (range: String, name: String) extends UserScalar
//case class DomainMarker(range: String, name: String) extends UserScalar
//
//case class UserConstant(name: String) extends UserScalar // XXX TODO: continue here with numeric spec
//
//object DeclarationEditor {
//  def apply(): Unit = {
//    val gui = AutoView(SonificationSpec())
//
//    new Frame {
//      contents = gui.component
//      pack().centerOnScreen()
//      open()
//    }
//  }
//}