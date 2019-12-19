
(
deviceName: "liine-SystemsEncounter",
protocol: \osc,
elementTypes: [\knob, \slider],
deviceType: \ipad,
status: (
	linux: "unknown",
	osx: "tested and working. 2019-12-08, lfsaw",
	win: "unknown"),

idInfo: "manta",
// srcPort is the port external source is listening to, and SC should send to;
// recvPort is the port SC should listen to for messages from the external source.
netAddrInfo: ( srcPort: 8000, ipAddress: "192.168.0.100", recvPort: 9000 ),

// specialMessages: (
// 	enableLEDControl: [
// 		[ "/manta/ledcontrol", "padandbutton", 1 ],
// 		[ "/manta/ledcontrol", "slider", 1 ]
// 	]
// ),
// testCode: {
// 	// start MantaOSC terminal program.
// 	"/<PATH/TO/MANTAOSC>/MantaOSC 0 31417 57120".runInTerminal;
//
// 	// make a manta mktl
// 	m = MKtl(\mt, "*manta");
// 	m.trace;
//
// 	// not working? have a look at where data is coming from...
// 	OSCFunc.trace(true);
//
// 	// update accordingly
// 	m.device.updateSrcAddr("127.0.0.1", 31417);
// 	m.device.updateRecvPort(57120);
//
// 	OSCFunc.trace(false);
//
// 	m.elAt.action = { |el| [el.name, el.value.round(0.0001)].postln };
//
// 	// enable LED control and set one light
// 	m.sendSpecialMessage(\enableLEDControl);
// 	m.elAt(\pad, 0, \led).deviceValue = "amber";
//
// },

specs: (
	liineElem: [0, 1].asSpec
	// liineLabel: ItemsSpec.new( ["off","amber","red"] )
),

deviceInfo: (
	// vendorURI: "http://www.snyderphonics.com/products.htm",
	// manualURI: "http://www.snyderphonics.com/documentation.htm",
	// description: "",
	// features: [
	// 	"48 touch sensors in a 6 X 8 hexagonal array - each sensor can independently send both continuous control data and note-on/off events with velocity sensitivity simultaneously",
	// 	"2 assignable touch sliders",
	// 	"4 assignable touch-sensing function buttons with LED feedback",
	// 	"68 user-controllable LEDs",
	// ],
	// notes: "This device relies on mantaOSC which is part of [libmantassfr’s libmanta implementation](https://github.com/ssfrr/libmanta).",
	longName: "Liine Lemur – SystemsEncounter"
),
elementsDesc: (
	elements:
	[
		(
			key: \fVol,
			ioType: \inout,
			spec: 'liineElem',
			oscPath: "/main/Fvol/x".asSymbol,
			// argTemplate: [ id ],
			elementType: 'slider',
			style: (
				row: 9,
				column: 0,
				width: 2.8, height: 2.8
			)
		)
	] ++
	['mix'].collect { |gIdx|
		(
			key: gIdx.asSymbol,
			shared: (
				spec: 'liineElem'
			),
			elements: [
				(
					key: \sl,
					shared: (
						ioType: \inout,
					),
					elements:
					[
						\a, \e, \i, \o, \u,
						\f, \l, \d, \n, \g, \x,
						\0, \1, \2, \3
					].collect{|name, i|
						(
							key: name,
							oscPath: "/%/F%/x"
								.format(gIdx, name.asString.toUpper).asSymbol,
							// argTemplate: [ id ],
							elementType: 'slider',
							style: (
								row: i,
								column: 0,
								width: 0.8, height: 2.8
							)
						)
					}
				),
				(
					key: \bt,
					shared: (
						ioType: \inout,
					),
					elements:
					[
						\a, \e, \i, \o, \u,
						\f, \l, \d, \n, \g, \x,
						\0, \1, \2, \3
					].collect{|name, i|
						(
							key: name,
							oscPath: "/%/reset%/x"
								.format(gIdx, name.asString.toUpper).asSymbol,
							// argTemplate: [ id ],
							elementType: 'button',
							style: (
								row: i,
								column: 4,
								width: 0.8, height: 0.8
							)
						)
					}
				),

			]
		)
	}
)
);
