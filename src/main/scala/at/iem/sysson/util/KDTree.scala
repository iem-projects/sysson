/*
 *  KDTree.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2016 Institute of Electronic Music and Acoustics, Graz.
 *  Copyright (c) 2014-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v3+
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 */

package at.iem.sysson
package util

object KDTree {
  type Point[K] = Vec[K]

  def apply[K : Ordering, V](entries: (Point[K], V)*): KDTree[K, V] = {
    if (entries.isEmpty) return Nil
    val dim = entries.head._1.size
    require(dim >= 1)

    def loop(xs: Vec[(Point[K], V)], axis: Int): KDTree[K, V] = {
      val sz = xs.size
      if (sz == 0) Nil
      else {
        val sorted = xs.sortBy(_._1(axis))
        val (left, (point, value) +: right) = sorted.splitAt(sz >> 1)
        val na = (axis + 1) % dim
        Node(point, value, loop(left, na), loop(right, na))
      }
    }

    loop(entries.toIndexedSeq, 0)
  }

  def toUGenLayout(tree: KDTree[Float, Float]): Vector[Float] = {
    // with median splitting, the height of the tree is guaranteed to be O(log n)
    var nodes: List[KDTree[Float, Float]] = tree :: scala.Nil
    val b = Vector.newBuilder[Float]
    var empty = 1 // NearestNN starts at index 1
    while (nodes.nonEmpty) {
      nodes = nodes.flatMap {
        case Node(point, value, left, right) =>
          if (empty > 0) {
            var pad = (point.size + 3) * empty
            while (pad > 0) { b += 0f; pad -= 1 }
            empty = 0
          }
          b += (if (left  == Nil) 1f else 0f)
          b += (if (right == Nil) 1f else 0f)
          point.foreach(b += _)
          b += value
          left :: right :: scala.Nil
        case _ =>
          empty += 1  // 'buffer' them because we can spare trailing zeros
          scala.Nil
      }
    }
    b.result()
  }

  final case class Node[K, V](point: Point[K], value: V, left: KDTree[K, V], right: KDTree[K, V])
    extends KDTree[K, V] {

    override def toString: String = {
      val s1 = s"L = ${left  match { case Node(lp, _, _, _) => lp.mkString("<", ",", ">"); case _ => "Nil"}}"
      val s2 = s"R = ${right match { case Node(rp, _, _, _) => rp.mkString("<", ",", ">"); case _ => "Nil"}}"
      s"KDTree.Node(${point.mkString("<", ",", ">")}, $value, $s1, $s2)"
    }
  }

  case object Nil extends KDTree[Nothing, Nothing]
}
sealed trait KDTree[+K, +V]