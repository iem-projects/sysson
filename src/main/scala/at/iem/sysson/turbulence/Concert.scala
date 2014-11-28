package at.iem.sysson.turbulence

import com.alee.laf.WebLookAndFeel

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.swing.defer
import de.sciss.lucre.synth.InMemory
import de.sciss.nuages.{NamedBusConfig, ScissProcs, Nuages, Wolkenpumpe}
import de.sciss.synth.Server
import de.sciss.synth.proc.AuralSystem

object Concert extends App {
  implicit val system = InMemory()
  defer(WebLookAndFeel.install())
  (new Concert).run()
}
class Concert extends Wolkenpumpe[InMemory] {
  type S = InMemory

  override protected def configure(sCfg: ScissProcs.ConfigBuilder, nCfg: Nuages.ConfigBuilder,
                                   aCfg: Server.ConfigBuilder): Unit = {
    super.configure(sCfg, nCfg, aCfg)
    sCfg.generatorChannels  = 4 // ?
    sCfg.micInputs          = Vector(
      NamedBusConfig("m-dpa"  ,  2, 1),
      NamedBusConfig("m-at "  ,  0, 2)
    )
    sCfg.lineInputs         = Vector.empty
    sCfg.highPass           = 100
    sCfg.audioFilesFolder   = Some(userHome / "Music" / "tapes")

    // println(s"master max = ${Turbulence.ChannelIndices.max}")
    nCfg.masterChannels     = Some(Turbulence.ChannelIndices)
    nCfg.soloChannels       = Some(0 to 1)
    nCfg.recordPath         = Some("/tmp")

    aCfg.wireBuffers        = 512 // 1024
    aCfg.audioBuffers       = 4096
    aCfg.blockSize          = 128
  }

  override protected def registerProcesses(sCfg: ScissProcs.Config, nCfg: Nuages.Config)
                                          (implicit tx: S#Tx, cursor: stm.Cursor[InMemory],
                                           nuages: Nuages[S], aural: AuralSystem): Unit = {
    super.registerProcesses(sCfg, nCfg)
  }
}