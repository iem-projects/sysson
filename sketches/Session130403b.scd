// met office 24 hus
// time 0, plev 5
// reduce longitudes from 144 to 9 through averaging

~min = 5.2230465E-5;  // minimum in that particular slice
~max = 0.0039939615;  // maximum ''

// for each downsampled longitude:
// [0] the index of the latitude which is maximal
// [1] the sample value (specific humidity) for that latitude
// [2] the bandwidth in latitude indices, as -3 drop off against maximum
~lm_time0 = [[29,0.0023981533, 5], [39,0.0020948418, 3], [37,0.0020472144, 10], [37,0.00249873, 3], [35,0.0016489016, 7], [29,0.0024894124, 9], [34,0.0027244077, 7], [34,0.0032935464, 7], [34,0.0021819484, 7]];

~lm_time6 = [[37.0,0.0020803397,4.0],[38.0,0.0018434906,3.0],[38.0,0.0027859572,10.0],[37.0,0.0028848622,3.0],[37.0,0.0025149807,4.0],[39.0,0.0016448286,6.0],[48.0,0.0031668704,4.0],[45.0,0.0030612436,15.0],[37.0,0.0031533504,7.0]];


// normalize stuff. 1st step: humidity value
// CHANGE THE INPUT ARRAY HERE!!
~lmn = ~lm_time0.deepCopy;
~lmn.do({ arg x; x[1] = x[1].linlin(0, ~max, 0, 1) }); // assume 0 is the natural minimum humidity ratio

// 3nd step: latitude indices (center them around equator + normalize to -1..1)
// (input: 0 .. 71
~lmn.do({ arg x; x[0] = (x[0]/71 - 0.5) * 2 });

// 3rd step: bandwidth.
~lmn.do(_.postln); nil

//////////////////

// controls: 3 multichannel controls (channel = longitude)

s.options.numWireBufs = 512; // !

(
~def = SynthDef(\sysson_bubbles, { arg pitchcurvelen = 0.2, baseFreq = 400, volume = 0.2, maxDensity = 10;
	var lat, mag, spread, poly, pool, trig, density, amp, trans, freq, attack, decay,
	    trigs, ts, osc, osc1, osc2, oscs, cnt, mm, ll, vv, atk;
	poly   = 10;
	lat    = \lat.kr(0 ! 9);
	mag    = \mag.kr(0 ! 9);
	spread = \spread.kr(0 ! 9);

	attack = \attack.kr(0.01);
	decay  = \decay.kr(0.08);

	// first dimension = longitude, second dim. = voice#
	pool   = Array.fill(9, { |lon|
	    density = mag[lon].linexp(0.1, 1, 1, maxDensity);
		// density = mag[lon].linlin(0, 1, 0, maxDensity);
//density.poll(1, "lon" ++ lon);
		trig  = Dust.kr(density);
		trigs = trig ! poly;
		cnt   = Stepper.kr(trig, min: 0, max: poly - 1);
// cnt.poll(trig);
		trigs = trigs.collect { arg t, i; t * BinaryOpUGen('==', i, cnt) };
//trigs.poll(trig, label: "lon" ++ lon);
		freq  = lat[lon].linlin(-0.5, 0.5, -12, 12).midiratio * baseFreq;

		// reference
		Out.ar(0, Pan2.ar(SinOsc.ar(baseFreq) * SinOsc.ar(1) * 0.001 * volume));

		// mm    = mag[lon].linlin(0, 1, 1, 2);
		// mm    = spread[lon].linlin(0, 10, -1, 1).clip(-1, 1);
		ll    = lat[lon].linexp(-0.5, 0.5, 1, 2);
		vv    = mag[lon].linexp(0, 1, 0.1, 1);
		atk   = spread[lon].linexp(2, 15, 0.005, 0.05); // .clip(-1, 1);
		oscs  = Array.fill(poly, { |vc|
			ts    = trigs[vc];
			amp   = EnvGen.ar(Env.perc(atk, decay).delay(0.003), ts);
			trans = EnvGen.ar(Env.new([0,0,1],[0,1]).exprange(1, ll), ts,
				timeScale: pitchcurvelen * (atk + decay));
			osc1  = SinOsc.ar(freq * trans);
			// osc2  = osc1 * LFTri.ar (freq/8);
			// osc   = LinXFade2.ar(osc1, osc2, mm) * amp;
			osc = osc1 * amp * vv;
		});
//		oscs = DC.ar(0) ! poly;
		Pan2.ar(Mix.ar(oscs), lon.linlin(0, 8, -1, 1));
	});
//	pool = pool.keep(2);
	Out.ar(0, HPF.ar(Mix.ar(pool), 500) * volume);
});
~def.load(s);
)

///////////////

x = Synth(~def.name, [\lat, ~lmn.collect(_.at(0)), \mag, ~lmn.collect(_.at(1)),
	\spread, ~lmn.collect(_.at(2))]);

x.set(\maxDensity, 8);
x.set(\volume, 0.1);
x.set(\baseFreq, 400);

x.set(\mag, 0 ! 9);


~rec = SimpleRecorder.new;
~rec.makeWindow;



///////////////////////
///////////////////////
///////////////////////
///////////////////////
///////////////////////
///////////////////////
///////////////////////
///////////////////////
///////////////////////
///////////////////////
///////////////////////
///////////////////////

// stuff:

ServerOptions

1.octcps

		{ var t = Impulse.kr(2); Stepper.kr(t, 0, 0, 4).poll(t) }.play

SinOsc.ar === SinOsc.ar


	BinaryOpUGen

PulseCount

(
SynthDef(\bubblebub, {  |out=0, /* t_trig=0, */ attack=0.01, decay=0.08, pitchcurvelen=0.1, freq=1000, doneAction=0, amp=0.1, mod = 1|
        var pitch, son, t_trig;
	t_trig = In.ar(~trig);
        amp   = amp  * EnvGen.ar(Env.perc(attack, decay).delay(0.003), t_trig, doneAction: doneAction);
        pitch = freq * EnvGen.ar(Env.new([0,0,1],[0,1]).exprange(1, mod), t_trig, timeScale: pitchcurvelen);
        son = SinOsc.ar(pitch);
        // high-pass to remove any lowpitched artifacts, scale amplitude
        son = HPF.ar(son, 500) * amp * 10;
        Out.ar(out, son);
}).send(s);

////////////////////////////////////////////////////////////////////////////////////////////////

~min = 5.2230465E-5; ~max = 0.0039939615;
~lm_max_time0 = [[28.0,0.0027869574,7.0],[39.0,0.002455111,4.0],[30.0,0.0039939615,10.0],[31.0,0.0037013863,9.0],[32.0,0.002924976,8.0],[28.0,0.0033379921,9.0],[34.0,0.0032654544,11.0],[34.0,0.0034033284,8.0],[34.0,0.0030252712,9.0]];
~lmn = ~lm_max_time0.deepCopy;
