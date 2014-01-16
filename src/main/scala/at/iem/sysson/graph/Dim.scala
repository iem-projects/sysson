/*
 *  Dim.scala
 *  (SysSon)
 *
 *  Copyright (c) 2013-2014 Institute of Electronic Music and Acoustics, Graz.
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

package at.iem.sysson.graph

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

object Dim {
  private final val DIM_COOKIE = 0x64696D00 // "dim\0"

  implicit object serializer extends ImmutableSerializer[Dim] {
    def read(in: DataInput): Dim = {
      val cookie = in.readInt()
      require(cookie == DIM_COOKIE,
        s"Unexpected cookie (expected ${DIM_COOKIE.toHexString}, found ${cookie.toHexString})")
      val name    = in.readUTF()
      val minSize = ImmutableSerializer.option[Int].read(in)
      val maxSize = ImmutableSerializer.option[Int].read(in)
      Dim(name, minSize = minSize, maxSize = maxSize)
    }

    def write(dim: Dim, out: DataOutput): Unit = {
      out.writeInt(DIM_COOKIE)
      out.writeUTF(dim.name)
      ImmutableSerializer.option[Int].write(dim.minSize, out)
      ImmutableSerializer.option[Int].write(dim.maxSize, out)
    }
  }
}
/** Specification of a data source dimension
  *
  * @param name     Logical name by which the dimension is referred to
  * @param minSize  Minimum domain size, or none
  * @param maxSize  Maximum domain size, or none
  */
case class Dim(name: String, minSize: Option[Int], maxSize: Option[Int])
