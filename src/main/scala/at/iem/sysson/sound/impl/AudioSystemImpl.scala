package at.iem.sysson
package sound
package impl

import at.iem.sysson.impl.ModelImpl

object AudioSystemImpl {
  lazy val instance: AudioSystem = new Impl

  private final class Impl extends AudioSystem with ModelImpl[AudioSystem.Update] {
    override def toString = "AudioSystem"

    def start() {
      ???
    }

    def stop() {
      ???
    }

    def isRunning: Boolean = ???
  }
}