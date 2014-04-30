package at.iem.sysson

import org.scalatest.{Outcome, Matchers, fixture}
import de.sciss.lucre.event.Durable
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB

trait DurableSpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  type FixtureParam = stm.Cursor[S]

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }
}

