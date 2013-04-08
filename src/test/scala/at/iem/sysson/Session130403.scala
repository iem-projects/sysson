package at.iem.sysson

import Implicits._

object Session130403 extends SessionLike {
  lazy val supercollider = false   // if `true`, print array data for sclang

  override lazy val useAudio = !supercollider

  // specific humidity !
  override lazy val dataName  = "25_hus_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-210012.nc"
  override lazy val dataFile  = dataDir / "201211" / "gcm" / "RCP45" / "MetOffUK_HadGEM2-ES" / dataName

  val time = 12
  val plev = 5

  def run() {
    // Met office hus 25, variable "hus"
    val v    = f.variableMap("hus")
    val sect = (v in "time" select time) in "plev" select plev
    assert(sect.reducedRank == 2)
    val arr  = sect.read()
    val red  = arr.reduce
    // j(latIdx)(longIdx)
    val j    = red.copyToNDJavaArray.asInstanceOf[Array[Array[Float]]]

    // reduction of 144 longitudes to 9 averaged groups
    // (replaces original array)
    for(lat <- 0 until 72) {
      val lon = j(lat)
      j(lat) = lon.sliding(16,16).toArray.map(_.sum / 16)
    }

    def lonSlice(l: Int) = j.map(_.apply(l))

    def latMax(l: Int) = {
      val slice = lonSlice(l)
      val mx = slice.max
      slice.indexOf(mx) -> mx
    }

    val lm = (0 until 9).map(latMax)

    def bandwidth(l: Int) = {
      val (lmi, lmv)  = lm(l)
      val thresh      = lmv * 0.7
      val slice       = lonSlice(l)
      val low         = slice.take(lmi)
      val lowRange    = low.reverse.indexWhere(_ < thresh) + 1
      val high        = slice.drop(lmi + 1)
      val highRange   = high.indexWhere(_ < thresh) + 1
      lowRange + highRange - 1
    }

    val lb = (0 until 9).map(bandwidth)

    val together = (lm zip lb).map { case ((a, b), c) => List(a,b,c) }
    val scCode = s"~lm = ${together.map(l => l.mkString("[", ",", "]")).mkString("[", ",", "]")}"

    val min = red.float1D.min // 5.2230465E-5
    val max = red.float1D.max // 0.0039939615

    if (supercollider) {
      println(s"At time index $time and plev index $plev:")
      println(s"~min = $min; ~max = $max;")
      println(scCode)
    } else {
      synth()
    }
  }

  def synth() {
//    // normalize stuff. 1st step: humidity value
//    // CHANGE THE INPUT ARRAY HERE!!
//    ~lmn = ~lm_time6.deepCopy;
//    ~lmn.do ({
//      arg x; x[ 1] = x[ 1].linlin(0, ~max, 0, 1)
//    }); // assume 0 is the natural minimum humidity ratio
//
//    // 3nd step: latitude indices (center them around equator + normalize to -1..1)
//    // (input: 0 .. 71
//    ~lmn.do ({
//      arg x; x[ 0] = (x[ 0] / 71 - 0.5) * 2
//    });
//
//    // 3rd step: bandwidth.
//    ~lmn.do (_.postln); nil
//
//    //////////////////
//
//    // controls: 3 multichannel controls (channel = longitude)
//
//    s.options.numWireBufs = 512; // !
//
//    (
//    ~ def = SynthDef(\ sysson_bubbles, {
//      arg pitchcurvelen = 0.1, baseFreq = 400, volume = 0.1, maxDensity = 4;
//      var lat, mag, spread, poly, pool, trig, density, amp, trans, freq, attack, decay,
//      trigs, ts, osc, oscs, cnt;
//      poly = 10;
//      lat = \ lat.kr(0 ! 9);
//      mag = \ mag.kr(0 ! 9);
//      spread = \ spread.kr(0 ! 9);
//
//      attack = \ attack.kr(0.01);
//      decay = \ decay.kr(0.08);
//
//      // first dimension = longitude, second dim. = voice#
//      pool = Array.fill(9, {
//        | lon |
//        density = mag[lon].linlin(0, 1, 0, maxDensity);
//        //density.poll(1, "lon" ++ lon);
//        trig = Dust.kr(density);
//        trigs = trig ! poly;
//        cnt = Stepper.kr(trig, min: 0, max: poly - 1);
//        // cnt.poll(trig);
//        trigs = trigs.collect {
//          arg t, i; t * BinaryOpUGen('== ', i, cnt)
//        };
//        //trigs.poll(trig, label: "lon" ++ lon);
//        freq = lat[lon].linlin(-1, 1, -12, 12).midiratio * baseFreq;
//        oscs = Array.fill(poly, {
//          | vc |
//          ts = trigs[vc];
//          amp = EnvGen.ar(Env.perc(attack, decay).delay(0.003), ts);
//          trans = EnvGen.ar(Env.new ([ 0, 0, 1], [0, 1] ).exprange(1, 2), ts,
//          timeScale: pitchcurvelen);
//          osc = SinOsc.ar(freq * trans) * amp;
//        });
//        //		oscs = DC.ar(0) ! poly;
//        Pan2.ar(Mix.ar(oscs), lon.linlin(0, 8, -1, 1));
//      });
//      //	pool = pool.keep(2);
//      Out.ar(0, HPF.ar(Mix.ar(pool), 500) * volume);
//    });
//    ~ def.load(s);
//    )
//
//    ///////////////
//
//    x = Synth( ~ def.name, [\ lat, ~lmn.collect(_.at(0)), \ mag, ~lmn.collect(_.at(1)),
//    \ spread, ~lmn.collect(_.at(2))] );
//
//    x.set(\ maxDensity, 8);
//    x.set(\ volume, 0.1);
//    x.set(\ baseFreq, 400);
//
//    x.set(\ mag, 0 ! 9);
    ???
  }
}