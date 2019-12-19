// systems ∿ encounter example setup file
// 2019, Till Bovermann
// http://tai-studio.org 	(interaction)
// http://lfsaw.de 			(music)


// this file should help to get you started.
// copy this file to a separate directory.
// state saving from within the gui will write
// def-files parallel to this file (same directory)


Server.killAll

(
// 1 of 3

// s.options.device_("Z1 + cameras");
s.options.device_("BlackHole");
// s.options.blockSize = 32;
// s.options.hardwareBufferSize = 256;
s.options.numOutputBusChannels = 2;
s.options.numInputBusChannels = 2;
s.reboot;
s.doWhenBooted{
	s.scope.style_(2);
		// load buffers
	q = q ? ();

	//////////////////////
	// load many buffers
	// q.fileNames = "/monoFiles/*.aiff".pathMatch;
	// q.fileNames = [
	// 	"path1/1.aiff",
	// 	"path2/2.aiff"
	// ];

	q.fileNames.notNil.if{
		q.buffers = q.fileNames.collect{|name, i|
			Buffer.readChannel(s, name,
				channels: 0,
				bufnum: i + 10,
				action: {"loading %/% done.\n".postf(i+1, q.fileNames.size)}
			)
		};
	}
}
)




(
// 2 of 3

Routine{
	var numChans = 2;

	// buses for external sensors
	var ctlBuses = (
		\lights: Bus.control(s, 64),
		// temp: Bus.control(s, 2),
		// rgb: Bus.control(s, 8)
		\ain: Bus.new(\audio, s.options.numOutputBusChannels, s.options.numInputBusChannels, s)
	);

	t = SystemsEncounter(
		numChannels: numChans,
		expand: true,
		server: s,
		defaultDefPath: thisProcess.nowExecutingPath.dirname,
		ctlBuses: ctlBuses,
// which letters to be associated with the quellen
		myQuellen: "aeiou",
// which letters to be associated with filters
		myFilters: "fldngx"
	);

	s.sync;
	t.verbosity_(0);
	x = t.synthParts;

	// set fadetime here
	t.setGlobal(\fadeTime, 10, \attack, 10);
	t.set('[', \fadeTime, 0, \attack, 0);
	t.set(']', \fadeTime, 0, \attack, 0);
	t.set('(', \fadeTime, 0, \attack, 0);
	t.set(')', \fadeTime, 0, \attack, 0);


	// set offset (e.g. for ADAT) here
	// first ADAT pair, (FF-UC)
	// t.bus = Bus.new(\audio, 10, numChans, s);
	q.micIdx = 0;

	s.sync;
	0.5.wait;

	(
		t.myQuellen.do{|c, i|
			"t.quelle('%', {|in, c|
				SinOsc.ar(20 + (50 * %+1) * (c.index+1))
			});".format(c.asSymbol, i).interpret
		};
		t.myFilters.do{|c|
			t.filter(c.asSymbol, {|in, c|
				in
			});
		}
	);

	s.sync;
	0.5.wait;
	// add variables for feedback
	t.declareFBVars([\0, \1, \2, \3]);
	s.sync;
	0.5.wait;
	s.sync;
	0.5.wait;
	t.myQuellen.do{|c|
		t.set(c.asSymbol, \assignment, 2, \mix, 0.5);
		0.1.wait;
	};
	t.myFilters.do{|c|
		t.set(c.asSymbol, \assignment, 2, \mix, 0.5);
		0.1.wait;
	};
	[\0, \1, \2, \3].do{|c|
		t.set(c, \assignment, 2, \feedback, 0, \mix, 1);
		t.set(c, \fadeTime, 0, \attack, 0);
		0.1.wait;
	};
	s.sync;
	t.push;
}.play;
)

// available params for c; see Steno helpfile for details
// ( \synthIndex, \nestingDepth, \numChannels, \env, \gate, \feedback, \index )

(
// 3 of 3

// open GUI
t.window;
// // add controller, requires MKtl quark
//SystemsEncounterControl.launchControl(t, MKtl(\lc, "novation-launchcontrol"));
//SystemsEncounterControl.nanoKontrol2(t, MKtl(\nc, "korg-nanokontrol2"));

// // folder with custom MKtl Description for liine lemur,
// // this also contains the .jzml lemur layout file
//MKtlDesc.addFolder(SystemsEncounterControl.mktlDescDir);
//SystemsEncounterControl.lemur(t, MKtl(\lemur, "liine-SystemsEncounter"));
)



/////////////// helpers ///////////////////////////////////////

s.queryAllNodes

t.window.skipJack

t.dump
t.set(\a, \mix, 0.7)

// all mix params
"aeioufldngx".as(Array).collect(_.asSymbol).collect{|c|
	var mix = t.settings.synthSettings[c].mix;
	"t.set(\\mix, %);".format(mix)
};

t.rawCmdLine
// write definitions to file
t.writeDefs(thisProcess.nowExecutingPath.dirname +/+ "defs%.scd".format((Date.getDate.rawSeconds * 4).asInteger));

