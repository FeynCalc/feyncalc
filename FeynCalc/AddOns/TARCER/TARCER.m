(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: TARCER															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1998-2018 Rolf Mertig
	Copyright (C) 1998-2018 Rainer Scharf
	Copyright (C) 1998-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary: 	Loader for TARCER											*)

(* ------------------------------------------------------------------------ *)

$TARCERVersion::usage=
"$FeynHelpersVersion is the string that represents the version of TARCER";

$TARCERDirectory::usage=
"$TARCERDirectory is the string that represents the full path to the TARCER \
directory";

GenerateTarcerMX::usage =
"GenerateTarcerMX creates the *.mx file for TARCER. This is necessary to use
TARCER and has to be done only once. The evaluation usually takes a
couple of minutes."

TARCER::taerror =
"TARCER*.mx file not found or damaged. Please evaluate the command \
GenerateTarcerMX[] to create it and then restart Mathematica.";

Begin["`Package`"]



End[]

Begin["`TARCER`Private`"];

$TARCERVersion = "2.0";

$TARCERDirectory = ToFileName[{$FeynCalcDirectory, "AddOns", "TARCER"}];

tarcerfilenames = FileNames["tarcer"<> StringReplace[$System,{"-"->"","Microsoft"->"","("->"",")"->""," "->""}] <>"*.mx", $TARCERDirectory,IgnoreCase->True];

TarcerDialogText = "TARCER*.mx file not found or damaged. Creating a new \
file can take couple of minutes, but this has to be done only once. \
After the new file is generated you need to restart Mathematica. Should \
we generate the new TARCER*.mx now?"

GenerateTarcerMX[]:=
	If[	Get@ToFileName[$TARCERDirectory, "TARCERSource.m"]=!= $Failed,
		Print["Succesfully created ", Last@FileNames["*.mx", $TARCERDirectory]]
	]

needNewTarcerMX[]:=
	If[	TrueQ[$Notebooks],

		If[	ChoiceDialog[TarcerDialogText],
			GenerateTarcerMX[]
		]
	];

If[	tarcerfilenames==={},
	Message[TARCER::taerror];
	needNewTarcerMX[]
];

If[	Length[tarcerfilenames]>1,
	Print[Style["Found multiple versions of TARCER: ", "Text"]];
	Print[TableForm[tarcerfilenames]];
	Print[Style["FeynCalc will load ","Text"], Last[tarcerfilenames]]
];

If[ tarcerfilenames=!={},
	If[	Get[Last[tarcerfilenames]]===$Failed,
		Message[TARCER::taerror];
		needNewTarcerMX[]
	];

	FeynCalc`Private`AddToTheContextPath = Join[FeynCalc`Private`AddToTheContextPath,{"Tarcer`"}];


	(* Print startup message *)
	If[ $FeynCalcStartupMessages,
		Print[	Style["TARCER ", "Text", Bold],
			Style[$TARCERVersion <> ", for description see the accompanying ", "Text"],
			Style[DisplayForm@ButtonBox["publication.", BaseStyle -> "Hyperlink",	ButtonFunction :>
				SystemOpen[ToFileName[{$TARCERDirectory},"9801383.pdf"]],
				Evaluator -> Automatic, Method -> "Preemptive"], "Text"],
				Style[" If you use TARCER in your research, please cite","Text"]
		];
		Print [Style[" \[Bullet] R. Mertig and R. Scharf, Comput. Phys. Commun., 111, 265-273, 1998, arXiv:hep-ph/9801383","Text"]];
	]
]

End[]


