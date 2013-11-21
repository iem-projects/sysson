package de.sciss.lucre
package stm

object Extensions {
  implicit class RichSource[Tx, A](val peer: stm.Source[Tx, A]) {
    def map[B](fun: A => B): stm.Source[Tx, B] = new stm.Source[Tx, B] {
      def apply()(implicit tx: Tx): B = fun(peer())
    }
  }
}
