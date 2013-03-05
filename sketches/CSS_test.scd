// ~path = "/Users/hhrutz/Desktop/IEM/SysSon/Research_HH/audio_work/rhizomwhisper2_cutm.aif";
~path = "/Users/hhrutz/Desktop/IEM/SysSon/Research_HH/audio_work/cutup.aif";

// e = SCMIRAudioFile(~path, [[SpecPcile, 0.5], [Loudness, 1]]);
// e = SCMIRAudioFile(~path, [[SpecPcile, 0.25], [SpecPcile, 0.75]]);
e = SCMIRAudioFile(~path, [[SpecCentroid], [SpecPcile, 0.75]]);
{e.extractFeatures()}.fork;
e.plotFeatures(800,500,40,true); // x, y, border, useLines

e.featuredata.select { arg i, j; j.even } .plot // SpecPcile
e.featuredata.select { arg i, j; j.odd } .plot  // Loudness

x = e.featuredata.clump(2);
// // seems they are already clipped to 0...1
// ~f1   = x.collect(_.at(0));
// ~f1mn = ~f1.minItem;
// ~f1mx = ~f1.maxItem;
// ~f2   = x.collect(_.at(1));
// ~f2mn = ~f2.minItem;
// ~f2mx = ~f2.maxItem;
x = x.collect { arg i, j; i ++ [ j ]};
t = KDTree(x, lastIsLabel: true);
// t.find(x[0].keep(2)).label;
// t.find(x[1].keep(2)).label; // ok, ca marche

// ~spec = SoundFile.openRead(~path); ~spec.close;
~winStep = 1024;

b = Buffer.read(s, ~path);
c = NearestN.makeBufferData(t);
d = Buffer.sendCollection(s, c.flat, numChannels: 5); // why is numChannels constituted?

/*
z = play { arg amp = 1.0;
	var x, y, nn, idx, pos, trig, dur, sig;
	x = MouseX.kr;
	y = MouseY.kr;
	nn = NearestN.kr(treebuf: d.bufnum, in: [x,y], num: 1);
	idx = nn[2];
	dur = ~winStep * SampleDur.ir;
	pos = idx * dur;
	trig = Impulse.kr(dur.reciprocal * 2);
//	dur.poll(2);
    sig = TGrains.ar(numChannels: 2, trigger: trig, bufnum: b.bufnum, rate: 1,
		centerPos: pos, dur: dur, pan: 0, amp: amp, interp: 1);
	Out.ar(0, sig);
};

*/

z = play { arg amp = 1.0;
	var x, y, nn, idx, pos, trig, dur, sig, frameDur;
	x = MouseX.kr;
	y = MouseY.kr;
	nn = NearestN.kr(treebuf: d.bufnum, in: [x,y], num: 1);
	idx = nn[2];
	frameDur = ~winStep * SampleDur.ir;
	trig = Impulse.kr(frameDur.reciprocal * WhiteNoise.kr.linexp(-1,1, 1.41, 2.82));
	dur = frameDur * TExpRand.kr(lo: 1.41, hi: 2.82, trig: trig);
	pos = (idx + TRand.kr(lo: -0.5, hi: 0.5, trig: trig)) * frameDur;
//	dur.poll(2);
    sig = TGrains.ar(numChannels: 2, trigger: trig, bufnum: b.bufnum, rate: 1,
		centerPos: pos, dur: dur, pan: 0, amp: amp, interp: 1);
	Out.ar(0, sig);
};

// das problem ist die skewedness der feature vektoren
// -- die punkte sollten homogener beieinander liegen.
// d.h. man muesste statt zu 'normalisieren' eher entzerren


~equalize = { arg data, amt = 1.0;
	var v, s, ss, dims = data[0].size;
//	["dims", dims].postln;
	ss = dims.collect { arg d;
		v = data.collect(_.at(d));
		v.sort;
	};
	data.collect { arg frame;
		frame.collect { arg x, d;
			s = ss[d];
			(s.indexOf(x).linlin(0, s.size - 1, 0.0, 1.0) * amt) + (x * (1 - amt))
		}
	}
};

e = SCMIRAudioFile(~path, [[SpecCentroid], [SensoryDissonance]]);
{e.extractFeatures()}.fork;
// e.plotFeatures(800,500,40,true); // x, y, border, useLines

x = e.featuredata.clump(2);
x = ~equalize.(x);
