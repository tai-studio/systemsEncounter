SystemsEncounter : Steno {
	var <definitions, window;
	var <>defaultDefPath;
	var <>myQuellen = "aeiou";
	var <>myFilters = "fldngx";
	var <myKeys;
	var <>myVars = "0123";
	var <>synthParts;
	var <>ctlBuses; // buses reserved for external controls (cameras, sensors, manta, microphones...)

	*new { |numChannels = 2, expand = false, maxBracketDepth = 8, server, defaultDefPath, ctlBuses, myQuellen, myFilters|
		^super.new(
			numChannels, expand, maxBracketDepth, server ? Server.default
		)
		.initBuses(ctlBuses)
		.initDefs(defaultDefPath, myQuellen, myFilters)
		.initSynthparts;
	}

	quelle { |name, func, multiChannelExpand, update = true, numChannels|
		definitions[name] = [\quelle, func, multiChannelExpand, update, numChannels];
		super.quelle(name, func, multiChannelExpand, update, numChannels)
	}
	filter { |name, func, multiChannelExpand, update = true, numChannels|
		definitions[name] = [\filter, func, multiChannelExpand, update, numChannels];
		super.filter(name, func, multiChannelExpand, update, numChannels)
	}

	value {|string|
		window.notNil.if {
			window.cmdView.string = string;
		};
		super.value(string);
	}
	valueNoUpdate {|string|
		super.value(string);
	}
	setFB {|key, val|
		this.set(key, \mix, 1/(val.abs+1), \feedback, val)
	}
	getDef {|name|
		var type, func, multiChannelExpand, update, numChannels;

		definitions[name].notNil.if({
			#type, func, multiChannelExpand, update, numChannels = definitions[name];
		}, {
			myQuellen.includes(name.asString.first).if({
				type = \quelle;
			}, {
				type = \filter;
			});
			func = {|in, ctl|};
		});
		// ^(".%(\%, %, %, %, %)".format(
		// 	type,
		// 	name,
		// 	func.asCompileString,
		// 	multiChannelExpand.asCompileString,
		// 	update.asCompileString,
		// 	numChannels.asCompileString)
		// );
		^".%(\%, %)".format(
			type,
			name.asSymbol.asCompileString,
			func.asCompileString,
		);
	}

	writeDefs {|path, varName = "t", comment|
		var mixs, defs, file;

		path.isNil.if{
			// write to default location
			path = defaultDefPath +/+ "%-defs_%.scd".format(this.class, Date.getDate.asSortableString)
		};


		// defs.postln;
		file = File(path,"w");
		file.write("// %\n//\n".format(comment));
		file.write("// %\n\n".format(this.rawCmdLine));
		file.write("%.value(\"%\");\n\n".format(varName, this.rawCmdLine ? ""));

		// get all mix params
		mixs = myKeys.as(Array).collect(_.asSymbol).collect{|c|
			var mix = this.settings.synthSettings[c].mix;
			"%.set(%, \\mix, %);".format(varName, c.asCompileString, mix)
		};

		file.write("\n\n");

		mixs.do{|d|
			file.write(d);
			file.write("\n");
		};

		/// get all definitions
		defs = myKeys.as(Array).collect{|c|
			"%%;".format(varName, this.getDef(c.asSymbol).asString)
		};
		defs.do{|d|
			file.write(d);
			file.write("\n");
		};
		file.close;
		"SystemsEncounter: written defs to %".format(path).inform;

	}
	initBuses {|argCtlBuses|
		ctlBuses = argCtlBuses;
	}
	initDefs {|argDefaultDefPath, argMyQuellen, argMyFilters|
		myQuellen = argMyQuellen ? myQuellen;
		myFilters = argMyFilters ? myFilters;
		myKeys = myQuellen ++ myFilters;

		definitions = ();
		defaultDefPath = argDefaultDefPath ?? {"~/Desktop".standardizePath};


		this.addSynthDef(\monitor, { |out, in, amp = 0.1, level = 0.9|
			Out.ar(out,
				Limiter.ar(
					LeakDC.ar(In.ar(in, numChannels)) * amp.lag(0.1),
					level,
					0.05
				)
			)
		}, force:true);

	}

	// synth part shortcuts

	initSynthparts {
		synthParts = SystemsEncounterSynthParts(this);
	}

	// feedback variables

	declareFBVars { |names|
		names.do { |name|
			name = name.asSymbol;
			if(variables[name].isNil) {
				if(verbosity > 0) {
					"new FB variable as ".post;
				};
				this.filter(name, { |input, controls|
					// Bus declaration inside synth func restores busses with
					// correct channel numbers, e.g. when number of channels changed on the fly
					var bus = Bus.audio(server, numChannels).postln;
					var read = LinXFade2.ar(
						inA: In.ar(bus, numChannels),
						inB: Limiter.ar(InFeedback.ar(bus, numChannels), 8, 0.01),
						// only do InFeedback for first appearance of variable
						pan: (controls.feedback.abs * (controls.index < 1) * 2 - 1)
					)
					* Select.kr(controls.feedback.abs > 0, [1, controls.feedback.sign]);
					// * controls.feedback.sign;

					// \assignment can be increased for feeding in more than one signal
					Out.ar(bus, input * (controls.index < \assignment.kr(1)));

					read * controls[\env] + input
				});
				variables[name] = bus;
			} {
				"Variable '%' already declared".format(name).warn;
			}
		};
		myVars = names.inject("", {|c, n| c ++ n})
	}

	////////////// GUI

	window {
		window.isNil.if({
			window = SystemsEncounterGUI(this);
			^window;
		});
		window.isClosed.if({
			window = SystemsEncounterGUI(this);
			^window;
		});

		^window;

	}

}

SystemsEncounterGUI {
	var <isClosed, window, model;
	var codeView, <cmdView, <defSliders, vSlider, width = 425, height = 950, elemExt, elemExtHalf, textView, randData, decorator;
	var skipJack;

	*new {|model|
		^super.new.init(model)
	}
	init {|argModel|

		isClosed = false;
		model = argModel;

		// pseudo randomness used for colors
		randData = thisThread.randData;
		thisThread.randSeed = 2017;

		window = Window(
			"systems ∿ encounter",
			Rect(0, 0, width, height),
			false
		).decorate;
		window.onClose({isClosed = true});

		decorator   = window.view.decorator;
		elemExt     = (width - window.view.decorator.margin.x) - 30;
		elemExtHalf = (width/2 - window.view.decorator.margin.x - window.view.decorator.gap.x) - 5;

		cmdView = TextView(window, (width - (2*window.view.decorator.margin.x))@40)
		.string_(model.rawCmdLine)
		.keyDownAction_{|me, c ... f|
			(c == $().if{
				(me.selectionSize == 0).if({
					me.selectedString = "()";
				}, {
					me.selectedString = "(" ++ me.selectedString ++ ")";
				});
				me.editable = false;
			};
			(c == ${).if{
				(me.selectionSize == 0).if({
					me.selectedString = "{}";
				}, {
					me.selectedString = "{" ++ me.selectedString ++ "}";
				});
				me.editable = false;
			};
			(c == $[).if{
				(me.selectionSize == 0).if({
					me.selectedString = "[]";
				}, {
					me.selectedString = "[" ++ me.selectedString ++ "]";
				});
				me.editable = false;
			};

			// prevent newline
			(f.last == 16777220).if{
				me.editable = false;
			}
		}
		.keyUpAction_{|me, c ... f|
			// print, backspace or delete
			(c.isPrint || #[16777219, 16777223].includes(f.last)).if{
				model.valueNoUpdate(me.string.asString);
			};
			model.myKeys.includes(c).if{
				codeView.string_("t%".format(model.getDef(c.asSymbol)))
			};
			me.editable = true;
		}
		.font_(Font(Font.defaultMonoFace, 18));
		decorator.nextLine;

		codeView = TextView(window, (width - (2*window.view.decorator.margin.x))@150)
		.enterInterpretsSelection_(false)
		.font_(Font(Font.defaultMonoFace, 12))
		.keyDownAction_{|view ... b|
			(b.last == 16777220 and: {b[1] == 524288 or: {b[1] == 131072}}).if{
				view.string.interpret;
			}
		}
		.tabWidth_(21);
		decorator.nextLine;

		// set general amplitude
		vSlider = EZSmoothSlider(
			window,
			(width - (2*window.view.decorator.margin.x))@20,
			// "%".format(c).asSymbol,
			nil,
			[ 0, 1].asSpec,
			{|me| model.monitor.set(\amp, me.value)}
		).setColors(hiliteColor: Color.red);

		decorator.nextLine;
		decorator.nextLine;

		defSliders = ();


		model.myKeys.do{|c|
			var color = Color.gray(rrand(0.5, 0.8)).alpha_(0.5);
			var displayFunc = {
				codeView
				.string_("t%".format(model.getDef(c.asSymbol)))
				// .syntaxColorize;
				//.stringColor_(color.copy.alpha_(1));
			};
			var button = SmoothButton(window, 20@20)
			.states_([[ c.asString ]] )
			.action_(displayFunc)
			.background_(color);
			var slider = EZSmoothSlider(
				window,
				elemExt@20,
				// "".format(c).asString,
				nil,
				[ 0, 1].asSpec,
				{|me| model.set(c.asSymbol,      \mix, me.value)},
				model.get(c.asSymbol, \mix) ? 0
			)
			.setColors(hiliteColor: color);
			slider.sliderView.mouseDownAction_(displayFunc);
			slider.numberView.mouseDownAction_(displayFunc);
			decorator.nextLine;
			defSliders[c.asSymbol] = slider;
		};
		model.myVars.do{|c|
			var color = Color.rand.alpha_(0.5);
			var slider;
			SmoothButton(window, 20@20)
			.states_([[ c.asString ]] )
			// .action_({ this. })
			.background_(color);
			slider = EZSmoothSlider(
				window,
				elemExt@20,
				// "%".format(c).asSymbol,
				nil,
				[ -1, 1].asSpec,
				{|me| model.setFB(c, me.value)},
				model.get(c.asSymbol, \feedback)
			)
			.setColors(hiliteColor: color);
			decorator.nextLine;
			defSliders[c.asSymbol] = slider;

		};

		decorator.nextLine;
		"f".do{|c|
			var color = Color.blue;
			// write def file
			// description text
			var descTextView = TextView(window, elemExt@20);

			SmoothButton(window, 20@20)
			.states_([[ c.asString ]] )
			.action_({ model.writeDefs(comment: descTextView.string) })
			.background_(color);

			decorator.nextLine;
		};

		thisThread.randData = randData;
		// window.alwaysOnTop_(true);
		window.front;
		this.initSkipjack;
	}

	initSkipjack {
		skipJack = SkipJack({
			defSliders.keysValuesDo{|key, slider|
				(model.myVars.contains(key.asString).not).if({
					// non-vars
					slider.value = model.get(key, \mix);
				}, {
					// vars
					slider.value = model.get(key, \feedback);
				})
			};
			model.monitor.get(\amp, {|v| vSlider.value = v});

		}, 0.1, {this.isClosed})
	}
}

SystemsEncounterControl {
	classvar <>mantaBinary = "/localvol/sound/src/libmanta/MantaOSC/build/MantaOSC 0 31417 57120";

	*launchControl {|model, mktl|
		var n = mktl;
		var quellen, filter, vars, main;

		main = mktl.elAt(0, \kn, \sndA, 5);
		quellen = MKtlElementGroup(\quellen, n, [
			\a -> mktl.elAt(0, \kn, \sndA, 0),
			\e -> mktl.elAt(0, \kn, \sndA, 1),
			\i -> mktl.elAt(0, \kn, \sndA, 2),
			\o -> mktl.elAt(0, \kn, \sndA, 3),
			\u -> mktl.elAt(0, \kn, \sndA, 4),
		]);
		filter = MKtlElementGroup(\filter, n, [
			\f -> mktl.elAt(0, \kn, \sndB, 0),
			\l -> mktl.elAt(0, \kn, \sndB, 1),
			\d -> mktl.elAt(0, \kn, \sndB, 2),
			\n -> mktl.elAt(0, \kn, \sndB, 3),
			\g -> mktl.elAt(0, \kn, \sndB, 4),
			\x -> mktl.elAt(0, \kn, \sndB, 5),
		]);
		vars = MKtlElementGroup(\vars, n, [
			\0 -> mktl.elAt(0, \kn, \sndA, 6),
			\1 -> mktl.elAt(0, \kn, \sndA, 7),
			\2 -> mktl.elAt(0, \kn, \sndB, 6),
			\3 -> mktl.elAt(0, \kn, \sndB, 7),
		]);

		mktl.addNamed(\quellen, quellen);
		mktl.addNamed(\filter, filter);
		mktl.addNamed(\vars, vars);


		main.action = {|el|
			model.monitor.set(\amp, el.value);
		};

		model.myQuellen.collectAs(_.asSymbol, Array).do{|key|
			mktl.elAt(\quellen, key).action = {|el|
				model.set(key, \mix, el.value)
			}
		};
		model.myFilters.collectAs(_.asSymbol, Array).do{|key|
			mktl.elAt(\filter, key).action = {|el|
				model.set(key, \mix, el.value)
			}
		};
		model.myVars.collectAs(_.asSymbol, Array).do{|key|
			mktl.elAt(\vars, key).action = {|el|
				model.setFB(key, el.value.linlin(0, 1, -1, 1))
			}
		};

	}

	*mantaStart {
		mantaBinary.runInTerminal;
	}
	*manta {|model, mktl|
		var pBus, sBus, cons, sliders;
		mktl.addNamed(\cons, mktl.elAt(\pad).collect(_[\con]));
		mktl.addNamed(\sliders, mktl.elAt(\sl).collect(_[\con]));

		cons = mktl.elAt(\cons);
		sliders = mktl.elAt(\sliders);

		pBus = Bus.control(model.server, cons.deviceValue.size);
		model.ctlBuses.put(\mtp, pBus);

		cons.action = { |el|
			var idx = cons.elemIndexOf(el);
			pBus.setAt(idx, el.value)
		};


		sBus = Bus.control(model.server, sliders.deviceValue.size);
		model.ctlBuses.put(\mts, sBus);

		sliders.action = { |el|
			var idx = sliders.elemIndexOf(el);
			(el.deviceValue < 65535).if{
				sBus.setAt(idx, el.value)
			}
		};
	}

	*xosc {|model, mktl|
		var semantics, specs, computeTemperature, computeHumidity;
		var bus; // where data is written to

		// var elems; // named MKtlElementGroup containing all used values


		semantics = (
			\light1: 12,
			\light2: 13,
			\rTemperature: 14,
			\humidity: 15
		);

		specs = (
		// measured
			\light1: [0.056891709566116, 0.94933462142944].asSpec,
			\light2: [0.033085092902184, 0.92284214496613].asSpec,
		// guesstimate
			\temperature: [0, 35].asSpec,
			\humidity: [15, 100].asSpec
		);

		computeTemperature = {|val|
			var temperature_resistor = 10000; // resistor value
			var thermistor_nominal = 10000; // resistance at 25 degrees C
			var temperature_nominal = 25;   // temp. for nominal resistance (almost always 25 C)
			var beta_coeff = 3950;	        // beta coefficient of thermistor (usually 3000-4000)

			val = ((
				log(((temperature_resistor / (val.reciprocal  - 1.0))) / thermistor_nominal)/beta_coeff
			) + (temperature_nominal + 273.15).reciprocal).reciprocal - 273.15;


			//// supposedly the same as
			// val = (1 / val)  - 1.0;
			// val = temperature_resistor / val;  // 10K / (1023/ADC - 1)
			//
			// val = val / thermistor_nominal;     // (R/Ro)
			// val = log(val);                  // ln(R/Ro)
			// val = val / beta_coeff;                   // 1/B * ln(R/Ro)
			// val = val + (1.0 / (temperature_nominal + 273.15)); // + (1/To)
			// val = 1.0 / val;                 // Invert (reading upside-down)
			// val = val - 273.15;

			// return
			val
		};
		computeHumidity = {|val, temperature|
			// input voltage (should be in range 4.5-6V but apparrently also works with 3V)
			var hih4030_supply = 3;

			// convert to voltage value
			val = val * hih4030_supply;
			val = ((val / (0.0062 * hih4030_supply)) - 25.81);
			val = val / (1.0546 - (0.00216 * temperature));
			val
		};


		bus = Bus.control(model.server, semantics.size);
		model.ctlBuses.put(\envir, bus);

		// read values, compute temperature and humidity, set bus
		mktl.collAt('aIn').groupAction = { |coll|
			var vals = coll.asArray;
			var light = [vals[semantics[\light1]].value, vals[semantics[\light2]].value];
			var temperature = computeTemperature.(vals[semantics[\rTemperature]].value);
			var humidity = computeHumidity.(vals[semantics[\humidity]].value, temperature);

			bus.setn([
				specs[\light1].unmap(light[0]),
				specs[\light2].unmap(light[1]),
				specs[\temperature].unmap(temperature),
				specs[\humidity].unmap(humidity)
			]);

				// bus.setAt(0, specs[\light1].unmap(light[0]);
				// bus.setAt(1, specs[\light2].unmap(light[1]);
				// bus.setAt(2, specs[\temperature].unmap(temperature));
				// bus.setAt(3, specs[\humidity].unmap(humidity));
			// light.round(0.01).postln;

		};

	}
}


SystemsEncounterSynthParts {
	var <dict;
	var model;
	*new{|model|
		^super.new.initSystemsEncounterSynthParts(model)
	}
	initSystemsEncounterSynthParts {|argModel|
		model = argModel;
		dict = ();

		// backward compatibility
		dict[\l2o] = {|idx = 0, size = 1, step = 1, wrap|
			this.in(\lights, idx, size, step, wrap)
		};
		dict[\lights] = {|idx = 0, size = 1, step = 1, wrap|
			this.in(\lights, idx, size, step, wrap)
		};
		// dict[\ain] = {|idx = 0, size = 1, step = 1, wrap|
		// 	this.in(\ain, idx, size, step, wrap)
		// };
		dict[\manta] = {|key, idx = 0, size = 1, step = 1, wrap|
			(key == \sl).if({
				this.in(\mts, idx, size, step, wrap)
			}, {
				this.in(\mtp, idx, size, step, wrap)
			})
		};
		dict[\envir] = {|idx = 0, size = 1, step = 1, wrap|
			this.in(\envir, idx, size, step, wrap)
		};

	}
	prUnfold {|obj|
		var wrap, bus, rate, idx;

		bus = model.ctlBuses[obj];
		wrap = bus.numChannels;
		idx = bus.index;
		rate = bus.rate;

		// stupidity
		rate = (rate == \control).if({\kr}, {\ar});

		^[wrap, idx, rate]
	}
	in {|key, idx = 0, size = 1, step = 1, wrap|
		var hardWrap, offset, rate, result;
		#hardWrap, offset, rate = this.prUnfold(key);
		wrap = wrap ? hardWrap;

		^In.perform(rate, (
			offset + (((Array.series(size, 0, step) % wrap) + idx) % hardWrap)
		));
	}
	doesNotUnderstand {|selector ... args|
		dict[selector].notNil.if{
			^dict[selector].value(*args)
		};
		^this.superPerformList(\doesNotUnderstand, selector, args);
	}
}

/*
x.l2o(0, 2)
x.dict[\lights] = {|idx = 0, size = 1, step = 1, wrap|
x.in(\lights, idx, size, step, wrap)
};

*/