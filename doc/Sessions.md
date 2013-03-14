# Improv Sessions

For experimentation, there is a trait `SessionLike` in the test scope of the project (`src/test/scala/at/iem/sysson`). To see how to use this, have a look at `Session130311`.

Your setup will be like

```scala

    package at.iem.sysson

    import Implicits._
    import de.sciss.synth
    import synth._
    import ugen._
    import synth.Ops._
    import concurrent.duration._

    object MySession extends SessionLike {
      def run() {
        val x = play {
          FreeSelf.kr(MouseButton.kr)   // stop synth upon mouse click
          WhiteNoise.ar(SinOsc.ar(MouseX.kr.linexp(0, 1, 1, 1000)) * 0.5)
        }

        x.onEnd { quit() }  // quit application when synth finishes
      }
    }
```

You have the example NetCDF file bound to symbol `f`. E.g.

```scala

    val myVar = f.variableMap("ta")
    ...
```

To run your session, use `sbt test:run`. You'll get a menu to select from:

     [1] at.iem.sysson.MySession
     [2] at.iem.sysson.DiskStreamTest
     ...

Type the number corresponding to your session and hit enter. Sound system should boot and run your code.

