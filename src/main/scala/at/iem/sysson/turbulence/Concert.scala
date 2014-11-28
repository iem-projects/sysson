package at.iem.sysson.turbulence

import com.alee.laf.WebLookAndFeel
import de.sciss.lucre.stm
import de.sciss.lucre.swing.defer
import de.sciss.lucre.synth.InMemory
import de.sciss.nuages.{ScissProcs, Nuages, Wolkenpumpe}
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
  }

  override protected def registerProcesses(sCfg: ScissProcs.Config, nCfg: Nuages.Config)
                                          (implicit tx: S#Tx, cursor: stm.Cursor[InMemory],
                                           nuages: Nuages[S], aural: AuralSystem): Unit = {
    super.registerProcesses(sCfg, nCfg)
  }
}