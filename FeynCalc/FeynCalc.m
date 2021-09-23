(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalc															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	FeynCalc is a Mathematica package for symbolic evaluation
				of Feynman diagrams and algebraic calculations in quantum
				field theory and elementary particle physics.				*)

(* ------------------------------------------------------------------------ *)


If[ MemberQ[$Packages,"FeynCalc`"],
	Print[Style["FeynCalc is already loaded! If you are trying to reload FeynCalc or load \
FeynArts, TARCER, PHI, FeynHelpers or any other add-on, please restart the kernel.","Text", Red, Bold]];
	Abort[]
];

If[ ($VersionNumber < 8.0) && StringFreeQ[$Version, "Mathics"],
	Print[Style["You need at least Mathematica 8.0 to run FeynCalc. Evaluation aborted.",Red, Bold]];
	Abort[]
];

(*    Set the version number    *)
FeynCalc`$FeynCalcVersion = "10.0.0";

fcGlobalToFeynCalc[varG_String, defaultVal_]:=
	fcGlobalToFeynCalc[varG, varG, defaultVal];

fcGlobalToFeynCalc[varG_String, varFC_String, defaultVal_, defaultCheck_String: "!ValueQ"]:=
	Block[{globalVar,fcVar},
	globalVar	=  "Global`"<>varG;
	fcVar 		=  "FeynCalc`"<>varFC;

	If[ ToExpression[defaultCheck<>"[" <> globalVar <> "]"],
		ToExpression[fcVar <> " = " <> ToString[defaultVal,InputForm]],
		ToExpression[fcVar <> " = " <> globalVar]
	];
	ToExpression["Remove[" <> globalVar <> "]"]
	];

(*    Find out where FeynCalc is installed    *)
fcGlobalToFeynCalc["$FeynCalcDirectory", DirectoryName[$InputFileName]];

If[ FileNames["*",{FeynCalc`$FeynCalcDirectory}] === {},
	Print[Style["Could not find a FeynCalc installation. Evaluation aborted.",Red,Bold]];
	Clear[FeynCalc`$FeynCalcDirectory];
	Abort[];
];

fcGlobalToFeynCalc["$FeynCalcStartupMessages",	True];
fcGlobalToFeynCalc["$LoadAddOns",				{}];
fcGlobalToFeynCalc["$FAPatch",					True];
fcGlobalToFeynCalc["$FCAdvice",					True];
fcGlobalToFeynCalc["$VeryVerbose",				0];
fcGlobalToFeynCalc["$RenameFeynCalcObjects",	{}];
fcGlobalToFeynCalc["$FCCloudTraditionalForm",	True];
fcGlobalToFeynCalc["$FCTraditionalFormOutput",	False];

fcGlobalToFeynCalc["$LoadFeynArts",				False]
fcGlobalToFeynCalc["$LoadTARCER",				False]
fcGlobalToFeynCalc["$LoadPhi",					False]

If[	(FeynCalc`$LoadFeynArts===True) && FreeQ[FeynCalc`$LoadAddOns,"FeynArtsLoader"|"FeynArts"],
	AppendTo[FeynCalc`$LoadAddOns,"FeynArtsLoader"]
];

If[	(FeynCalc`$LoadTARCER===True) && FreeQ[FeynCalc`$LoadAddOns,"TARCER"],
	AppendTo[FeynCalc`$LoadAddOns,"TARCER"]
];

If[	(FeynCalc`$LoadPhi===True) && FreeQ[FeynCalc`$LoadAddOns,"PHI"],
	AppendTo[FeynCalc`$LoadAddOns,"PHI"]
];


If[ !ValueQ[FeynCalc`$FeynArtsDirectory],
	FeynCalc`$FeynArtsDirectory = FileNameJoin[{FeynCalc`$FeynCalcDirectory, "FeynArts"}]
];

If[ FeynCalc`$FeynCalcStartupMessages=!=False,
	PrintTemporary[Style["Loading FeynCalc from " <> FeynCalc`$FeynCalcDirectory, "Text"]]
];

If[	TrueQ[FileExistsQ[FileNameJoin[{FeynCalc`$FeynCalcDirectory, ".testing"}]]],
	FeynCalc`$FeynCalcDevelopmentVersion = True,
	FeynCalc`$FeynCalcDevelopmentVersion = False
];

If[ !ValueQ[Global`$FCCheckContext],
	If[	TrueQ[FeynCalc`$FeynCalcDevelopmentVersion],
		FeynCalc`$FCCheckContext = True,
		FeynCalc`$FCCheckContext = False
	],
	FeynCalc`$FCCheckContext = Global`$FCCheckContext
];
Remove[Global`$FCCheckContext];


Remove[Global`fcGlobalToFeynCalc];
Global`globalContextBeforeLoadingFC = Names["Global`*"];

BeginPackage["FeynCalc`"];

FCDeclareHeader::usage =
"FCDeclareHeader is an internal FeynCalc function to declare objects inside an
.m file in the same manner as it is done in the JLink package. It may be used
by FeynCalc addons.";

Begin["`Private`"]

FCDeclareHeader[file_, type_String:"file"] :=
	Module[ {strm, einput, moreLines = True},

		Switch[
			type,
			"file",
				strm = OpenRead[file],
			"string",
				strm = StringToStream[file],
			_,
			Print["FeynCalc: FCDeclareHeader: Unknown input type. Evaluation aborted"];
			Abort[];
		];

		If[ Head[strm] =!= InputStream,
			Return[$Failed]
		];
		While[
			moreLines,
			einput = Read[strm, Hold[Expression]];
			ReleaseHold[einput];
			If[ einput === $Failed || MatchQ[einput, Hold[_End]],
				moreLines = False
			]
		];
		Close[strm]
	];

End[]

(*    Load the configuration file    *)
If[ FileExistsQ[FileNameJoin[{FeynCalc`$FeynCalcDirectory,"FCConfig.m"}]],
	Get[FileNameJoin[{FeynCalc`$FeynCalcDirectory,"FCConfig.m"}]]
];

(* need to do this first, otherwise $NonComm and $FCTensorList won't get built correctly *)
boostrappingList = Join[
	Map[FileNameJoin[{$FeynCalcDirectory,"Shared",#}]&, {"SharedTools.m", "DataType.m"}],
	Map[FileNameJoin[{$FeynCalcDirectory,"NonCommAlgebra",#}]&, {"NonCommutative.m"}],
	Map[FileNameJoin[{$FeynCalcDirectory,"Lorentz",#}]&, {"DeclareFCTensor.m"}]
];

mainList = {FileNameJoin[{$FeynCalcDirectory, "FCMain.m"}]};

allList = {
	Select[FileNames[{"*.m"}, FileNameJoin[{$FeynCalcDirectory, "Shared"}]], StringFreeQ[#, {"LegacyObjects","SharedTools.m", "DataType.m"}] &],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"NonCommAlgebra"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"Lorentz"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"Dirac"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"Pauli"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"SUN"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"LoopIntegrals"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"Feynman"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"QCD"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"Tables"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"ExportImport"}]],
	FileNames[{"*.m"},FileNameJoin[{$FeynCalcDirectory,"Misc"}]],
	{FileNameJoin[{$FeynCalcDirectory, "Shared", "LegacyObjects.m"}]}
};

fcSelfPatch[file_String]:=
	Block[{originalCode,repList},

		repList = Map[{
				Rule[RegularExpression["\\b" <> First[#] <> "\\b"], Last[#]],
				Rule[RegularExpression["\\_" <> First[#] <> "\\b"], "_" <> Last[#]],
				Rule[RegularExpression[First[#] <> "\\_\\b"], Last[#] <> "_"]} &, $RenameFeynCalcObjects] // Flatten;
		originalCode = Import[file, "Text"];
		StringReplace[originalCode, repList, MetaCharacters -> Automatic]
	];


AppendTo[$ContextPath, "FeynCalc`Package`"];

If[	$VeryVerbose>0,
	Print["FeynCalc.m: This is allList: "];
	Map[Print[(FileNameTake /@ #)] &, allList]
];

If[	$VeryVerbose>0,
	Print["FeynCalc.m: Applying fcSelfPatch to mainList."]
];
patchedMain =(fcSelfPatch/@mainList);

If[	$VeryVerbose>0,
	Print["FeynCalc.m: Applying fcSelfPatch to boostrappingList."]
];
patchedBoostrap	=(fcSelfPatch/@boostrappingList)


patchedList = Map[Function[argList, fcSelfPatch /@ argList], allList];

If[	$VeryVerbose>0,
	Print["FeynCalc.m: Applying FCDeclareHeader to patchedMain."]
];
FCDeclareHeader[#,"string"]&/@(patchedMain);

If[	$VeryVerbose>0,
	Print["FeynCalc.m: Loading patchedMain."]
];
ToExpression/@patchedMain;

If[	$VeryVerbose>0,
	Print["FeynCalc.m: Applying FCDeclareHeader to patchedList."]
];
Map[Function[argList, FCDeclareHeader[#,"string"]& /@ argList], patchedList];

If[	$VeryVerbose>0,
	Print["FeynCalc.m: Loading patchedBoostrap."]
];
ToExpression/@patchedBoostrap;

If[	$VeryVerbose>0,
	Print["FeynCalc.m: Loading the rest."]
];

Map[Function[argList, ToExpression /@ argList], patchedList];

If[	$VeryVerbose>0,
	Print["FeynCalc.m: Loading stage done."]
];

EndPackage[];

Remove["FeynCalc`boostrappingList"];
Remove["FeynCalc`allList"];
Remove["FeynCalc`mainList"];
Remove["FeynCalc`argList"];
Remove["FeynCalc`fcSelfPatch"];
Remove["FeynCalc`patched*"];
Remove["FeynCalc`originalCode"];
Remove["FeynCalc`repList"];
Remove["FeynCalc`file"];

(* If necessary, swtich the output format of the current frontend to TraditionalForm *)
If[	$FCTraditionalFormOutput,
	CurrentValue[$FrontEndSession, {CommonDefaultFormatTypes, "Output"}] = TraditionalForm
];

(* From Mathematica 4.0 onwards there is  "Tr" functions; Overload Tr to use TR *)
Unprotect[Tr];
Tr[Pattern[FeynCalc`Private`trarg,BlankSequence[]]] :=
	TR[FeynCalc`Private`trarg] /; !FreeQ[{FeynCalc`Private`trarg}, FeynCalc`Package`TrFeynCalcObjects];
Tr::usage =
"FeynCalc extension: Tr[list] finds the trace of the matrix or tensor list. Tr[list, f] finds a
generalized trace, combining terms with f instead of Plus. Tr[list, f, n] goes down to level n
in list. \n
Tr[ expression ] calculates the DiracTrace, i.e.,  TR[ expression ], if any of DiracGamma,
GA, GAD, GAE, GS, GSD or GAE are present in expression.";
Tr/:Options[Tr]:=Options[TR];
Protect[Tr];


If[ !$Notebooks && $FeynCalcStartupMessages,
	$PrePrint = FeynCalcForm;
	WriteString["stdout",
	"$PrePrint is set to FeynCalcForm. Use FI and FC to change the display format.\n"],
	If[ ($Notebooks =!= True),
		$PrePrint = FeynCalcForm
	];
];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* Print FeynCalc's startup message *)
If[ $FeynCalcStartupMessages =!= False,
	Print[	Style["FeynCalc ", "Text", Bold],
			If[ TrueQ[$FeynCalcDevelopmentVersion],
				Style[$FeynCalcVersion <> " (development version). For help, use the ", "Text"],
				Style[$FeynCalcVersion <> " (stable version). For help, use the ", "Text"]
			],
			Style[DisplayForm@ButtonBox["online documentation", ButtonData :> {URL["https://feyncalc.github.io/referenceDev"], None},BaseStyle -> "Hyperlink",
				ButtonNote -> "https://feyncalc.github.io/referenceDev"], "Text"],
			Style[", check out the ", "Text"],
			Style[DisplayForm@ButtonBox["wiki", ButtonData :> {URL["https://github.com/FeynCalc/feyncalc/wiki"], None},BaseStyle -> "Hyperlink",
				ButtonNote -> "https://github.com/FeynCalc/feyncalc/wiki"],"Text"],
			Style[" or visit the ", "Text"],
			Style[DisplayForm@ButtonBox["forum.", ButtonData :> {URL["https://github.com/FeynCalc/feyncalc/discussions"], None},BaseStyle -> "Hyperlink",
				ButtonNote -> "http://www.feyncalc.org/forum/"],"Text"]];
	Print[Style["Please check our ","Text"], Style[DisplayForm@ButtonBox["FAQ",ButtonData :> {URL["https://github.com/FeynCalc/feyncalc/wiki/FAQ"], None},BaseStyle -> "Hyperlink",
				ButtonNote -> "https://github.com/FeynCalc/feyncalc/wiki"],"Text"] , Style[" for answers to some common FeynCalc questions and have a look at the supplied ","Text"],
				Style[DisplayForm@ButtonBox["examples.", BaseStyle -> "Hyperlink",	ButtonFunction :>
							SystemOpen[FileNameJoin[{$FeynCalcDirectory, "Examples"}]],
							Evaluator -> Automatic, Method -> "Preemptive"], "Text"]];
	Print[Style["If you use FeynCalc in your research, please evaluate FeynCalcHowToCite[] to learn how to cite this software.","Text"]];
	Print[Style["Please keep in mind that the proper academic attribution of our work is crucial to ensure the future development of this package!", {"Text", Italic}]];

	];


(* 	Some addons might need to add new stuff to the $ContextPath. While inside the
	FeynCalc` path they obviously cannot do this by themselves. However, via
	FeynCalc`Private`AddToTheContextPath they can ask FeynCalc to do this for
	them.
*)
FeynCalc`Private`AddToTheContextPath={};

FeynCalc`Private`AddToTheWhiteListedContextAdditions={};

BeginPackage["FeynCalc`"];
If[ $LoadAddOns=!={},
	$LoadAddOns = $LoadAddOns /. {"FeynArts" -> "FeynArtsLoader"};
	FCDeclareHeader/@Map[ToFileName[{$FeynCalcDirectory,  "AddOns",#},#<>".m"] &, $LoadAddOns];
	Get/@Map[ToFileName[{$FeynCalcDirectory,  "AddOns",#},#<>".m"] &, $LoadAddOns]
];
EndPackage[];

If[ FeynCalc`Private`AddToTheContextPath=!={} && ListQ[FeynCalc`Private`AddToTheContextPath],
	$ContextPath = Join[FeynCalc`Private`AddToTheContextPath,$ContextPath]
]

If[ $FCCheckContext,

	Global`globalContextAfterLoadingFC = Names["Global`*"];

	Global`fcContextLowerCase = Select[Names["FeynCalc`*"], LowerCaseQ[StringTake[#, 1]] &];

	If[$RenameFeynCalcObjects=!={},
		Global`fcContextLowerCase = Complement[Global`fcContextLowerCase, Last /@ $RenameFeynCalcObjects]
	];

	Global`whiteListedContextAdditions = {
		"Colour", "CT", "cto", "d", "dD", "eE", "FAChiralityProjector",
		"FADiracMatrix", "FADiracSlash", "FADiracSpinor", "FADiracTrace",
		"FAFourVector", "FAGS", "FAMetricTensor", "FAPolarizationVector",
		"FAScalarProduct", "FASUNF", "FASUNT", "ff", "gA", "gA5", "gA6",
		"gA7", "globalContextAfterLoadingFC", "Gluon", "GraphName","dSUN",
		"Lorentz", "M", "pp", "TJI111e", "$INTC", "$Special",
		"fcContextLowerCase", "newObjectsInTheContext", "MajoranaSpinor",
		"newObjectsInTheGlobalContext", "whiteListedContextAdditions", "GetFlip",
		"Dirac"
	};

	Global`whiteListedContextAdditions = Join[Global`whiteListedContextAdditions,FeynCalc`Private`AddToTheWhiteListedContextAdditions];

	Global`newObjectsInTheGlobalContext = Complement[Global`globalContextAfterLoadingFC, Global`globalContextBeforeLoadingFC]//
		Complement[#,Global`whiteListedContextAdditions]&;

	If[ (Global`fcContextLowerCase=!={} || Global`newObjectsInTheGlobalContext=!={}) && FreeQ[FeynCalc`$LoadAddOns,{"PHI"}],
		Message[FeynCalc::context];
		If[	Global`fcContextLowerCase=!={},
			Print["New lowercase objects in the FeynCalc context: ", Global`fcContextLowerCase]
		];
		If[	Global`newObjectsInTheGlobalContext=!={},
			Print["New lowercase objects in the Global context: ", Global`newObjectsInTheGlobalContext]
		]
	];

	Remove["Global`fcContextLowerCase"];
	Remove["Global`newObjectsInTheGlobalContext"];
	Remove["Global`whiteListedContextAdditions"];
	Remove["Global`globalContextBeforeLoadingFC"];
	Remove["Global`globalContextAfterLoadingFC"];

]





If[	$CloudEvaluation && $FCCloudTraditionalForm,
	$Post = TraditionalForm;
];
