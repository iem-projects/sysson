/*
 *  NcviewSync.scala
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

import de.sciss.osc
import impl.{NcviewSyncImpl => Impl}
import de.sciss.model.Model

object NcviewSync {
  /** Creates a new Ncview sonification server.
    *
    * @param config  the configuration for the server, such as protocol and port.
    *
    * @return  a new unconnected server. Call `start()` to actually launch the server.
    */
  def apply(config: Config = Config().build): NcviewSync = Impl(config)

  sealed trait ConfigLike {
    /** The protocol to use for the OSC connection (`osc.TCP` or `osc.UDP`)
      *
      * The default is `UDP`
      */
    def protocol: osc.Transport.Net

    /** The server port to use for the OSC connection
      *
      * The default is `Config.DEFAULT_PORT` (`21327`)
      */
    def port: Int

    /** Whether to use the loopback adaptor or the regular local IP address.
      *
      * The default is `true`
      */
    def loopback: Boolean
  }
  object Config {
    final val DEFAULT_PORT = 0x534F // 'SO'

    /** Creates a new configuration builder. */
    def apply(): ConfigBuilder = new BuilderImpl

    implicit def build(b: ConfigBuilder): Config = b.build

    private final case class Impl(protocol: osc.Transport.Net, port: Int, loopback: Boolean) extends Config {
      override def toString = "NcviewSync.Config(protocol =" + protocol + ", port = " + port + ")"
    }

    private final class BuilderImpl extends ConfigBuilder {
      override def toString = "NcviewSync.ConfigBuilder@" + hashCode().toHexString
      var protocol: osc.Transport.Net = osc.UDP
      var port: Int = DEFAULT_PORT
      var loopback: Boolean = true

      def build: Config = Impl(protocol, port, loopback)
    }
  }
  /** The configuration of the server, such as OSC protocol and port.
    * To build a configuration, call `Config()` which yields a `ConfigBuilder` instance.
    * After modifying the builder, call `build` or pass the builder directly into
    * `NcviewSync()`.
    */
  sealed trait Config extends ConfigLike
  sealed trait ConfigBuilder extends ConfigLike {
    def protocol: osc.Transport.Net
    def protocol_=(value: osc.Transport.Net): Unit
    def port: Int
    def port_=(value: Int): Unit
    def loopback: Boolean
    def loopback_=(value: Boolean): Unit

    def build: Config
  }

  type Listener = Model.Listener[Update]

  /** A message from an Ncview client. */
  sealed trait Update

  //  /**
  //   * The client has opened a NetCDF file with the given path.
  //   */
  //  final case class Open(path: String) extends Update
}
/** An Open Sound Control server providing synchronized views from Ncview. */
trait NcviewSync extends Model[NcviewSync.Update] {
  /** Starts the server. */
  def start(): this.type

  /** Shuts down the server. */
  def stop(): this.type

  /** Queries whether the server was started or not. */
  def isRunning: Boolean

  /** Turns on or off OSC message dumping. */
  def dump(on: Boolean): this.type
}