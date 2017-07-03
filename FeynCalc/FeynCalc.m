(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalc															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	FeynCalc is a Mathematica package for symbolic evaluation
				of Feynman diagrams and algebraic calculations in quantum
				field theory and elementary particle physics.				*)

(* ------------------------------------------------------------------------ *)


If[ MemberQ[$Packages,"FeynCalc`"],
	Print["FeynCalc is already loaded! To reload it, please restart the kernel."];
	Abort[]
];

If[ ($VersionNumber < 8.0),
	Print["You need at least Mathematica 8.0 to run FeynCalc. Quitting the Mathematica kernel."];
	Abort[]
];

(*    Find out where FeynCalc is installed    *)
If[ !ValueQ[FeynCalc`$FeynCalcDirectory],
	FeynCalc`$FeynCalcDirectory =
	DirectoryName[$InputFileName]
];

If[ FileNames["*",{FeynCalc`$FeynCalcDirectory}] === {},
	Print["Could not find a FeynCalc installation. Quitting the Mathematica kernel."];
	Clear[FeynCalc`$FeynCalcDirectory];
	Abort[];
];

(*    Set the version number    *)
FeynCalc`$FeynCalcVersion = "9.3.0";

(*    Set defaults here, not in the config file    *)
If[ !ValueQ[Global`$FeynCalcStartupMessages],
	Global`$FeynCalcStartupMessages = True
];

If[ !ValueQ[Global`$LoadTARCER],
	Global`$LoadTARCER = False
];

If[ !ValueQ[Global`$LoadPhi],
	Global`$LoadPhi = False
];

If[ !ValueQ[Global`$LoadFeynArts],
	Global`$LoadFeynArts = False
];

If[ !ValueQ[FeynCalc`$FAPatch],
	FeynCalc`$FAPatch = True
];

If[ !ValueQ[FeynCalc`$FCAdvice],
	FeynCalc`$FCAdvice = True
];

If[ !ValueQ[Global`$LoadAddOns],
	Global`$LoadAddOns = {}
];

If[ !ValueQ[Global`$RenameFeynCalcObjects],
	Global`$RenameFeynCalcObjects = {}
];

If[ !ValueQ[Global`$FCCloudTraditionalForm],
	FeynCalc`$FCCloudTraditionalForm = True;
	Remove[Global`$FCCloudTraditionalForm],
	FeynCalc`$FCCloudTraditionalForm = Global`$FCCloudTraditionalForm;
	Remove[Global`$FCCloudTraditionalForm]
];

If[ !ValueQ[FeynCalc`$FeynArtsDirectory],
	FeynCalc`$FeynArtsDirectory = FileNameJoin[{FeynCalc`$FeynCalcDirectory, "FeynArts"}]
];

(*    Load configuration file    *)
If[ FileExistsQ[FileNameJoin[{FeynCalc`$FeynCalcDirectory,"FCConfig.m"}]],
	Get[FileNameJoin[{FeynCalc`$FeynCalcDirectory,"FCConfig.m"}]]
];

If[ Global`$FeynCalcStartupMessages=!=False,
	PrintTemporary[Style["Loading FeynCalc from "<>
	FeynCalc`$FeynCalcDirectory, "Text"]]
];


BeginPackage["FeynCalc`"];

FCDeclareHeader::usage =
"FCDeclareHeader is an internal FeynCalc function to declare
objects inside an .m file in the same manner as it is done in
the JLink package. It may be used by FeynCalc addons."

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

(* need to do this first, otherwise $NonComm and $FCTensorList do not get built correctly *)
boostrappingList = Join[
	Map[ToFileName[{$FeynCalcDirectory,"Shared"},#]&, {"SharedTools.m", "DataType.m"}],
	Map[ToFileName[{$FeynCalcDirectory,"NonCommAlgebra"},#]&, {"NonCommutative.m"}],
	Map[ToFileName[{$FeynCalcDirectory,"Lorentz"},#]&, {"DeclareFCTensor.m"}]
];

listMain = {ToFileName[{$FeynCalcDirectory}, "FCMain.m"]};
listShared = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Shared"}]];
listNonCommAlgebra = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"NonCommAlgebra"}]];
listLorentz = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Lorentz"}]];
listDirac = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Dirac"}]];
listPauli = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Pauli"}]];
listSUN = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"SUN"}]];
listLoopIntegrals = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"LoopIntegrals"}]];
listFeynman = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Feynman"}]];
listQCD = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"QCD"}]];
listTables = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Tables"}]];
listExportImport = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"ExportImport"}]];
listMisc = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Misc"}]];

fcSelfPatch[file_String]:=
	Block[{originalCode,repList,patchedCode},

		repList = Map[{
				Rule[RegularExpression["\\b" <> First[#] <> "\\b"], Last[#]],
				Rule[RegularExpression["\\_" <> First[#] <> "\\b"], "_" <> Last[#]],
				Rule[RegularExpression[First[#] <> "\\_\\b"], Last[#] <> "_"]} &, Global`$RenameFeynCalcObjects] // Flatten;
		originalCode = Import[file, "Text"];
		StringReplace[originalCode, repList, MetaCharacters -> Automatic]
	];





AppendTo[$ContextPath, "FeynCalc`Package`"];



patchedMain=(fcSelfPatch/@listMain);
patchedShared=(fcSelfPatch/@listShared);

patchedBoostrap			=(fcSelfPatch/@boostrappingList)
patchedNonCommAlgebra	=(fcSelfPatch/@listNonCommAlgebra);
patchedLorentz			=(fcSelfPatch/@listLorentz)
patchedDirac			=(fcSelfPatch/@listDirac)
patchedPauli			=(fcSelfPatch/@listPauli)
patchedSUN				=(fcSelfPatch/@listSUN)
patchedLoopIntegrals	=(fcSelfPatch/@listLoopIntegrals)
patchedFeynman			=(fcSelfPatch/@listFeynman)
patchedQCD				=(fcSelfPatch/@listQCD)
patchedTables			=(fcSelfPatch/@listTables)
patchedExportImport		=(fcSelfPatch/@listExportImport)
patchedMisc				=(fcSelfPatch/@listMisc)


FCDeclareHeader[#,"string"]&/@(patchedMain);
ToExpression/@patchedMain;

FCDeclareHeader[#,"string"]&/@patchedShared;
FCDeclareHeader[#,"string"]&/@patchedNonCommAlgebra;
FCDeclareHeader[#,"string"]&/@patchedLorentz;
FCDeclareHeader[#,"string"]&/@patchedDirac;
FCDeclareHeader[#,"string"]&/@patchedPauli;
FCDeclareHeader[#,"string"]&/@patchedSUN;
FCDeclareHeader[#,"string"]&/@patchedLoopIntegrals;
FCDeclareHeader[#,"string"]&/@patchedFeynman;
FCDeclareHeader[#,"string"]&/@patchedQCD;
FCDeclareHeader[#,"string"]&/@patchedTables;
FCDeclareHeader[#,"string"]&/@patchedExportImport;
FCDeclareHeader[#,"string"]&/@patchedMisc;

ToExpression/@patchedBoostrap;
ToExpression/@patchedShared;
ToExpression/@patchedNonCommAlgebra;
ToExpression/@patchedLorentz;
ToExpression/@patchedDirac;
ToExpression/@patchedPauli;
ToExpression/@patchedSUN;
ToExpression/@patchedLoopIntegrals;
ToExpression/@patchedFeynman;
ToExpression/@patchedQCD;
ToExpression/@patchedTables;
ToExpression/@patchedExportImport;
ToExpression/@patchedMisc;

EndPackage[];

(*Let us check the configuration of Mathematica and give the user some advices, if necessary*)
If[$FCAdvice,
	If[ $Notebooks &&
		Cases[Options[$FrontEndSession, CommonDefaultFormatTypes], Rule["Output", Pattern[FeynCalc`Private`rulopt, Blank[]]] :> FeynCalc`Private`rulopt, Infinity]=!={TraditionalForm},
		Message[FeynCalc::tfadvice]
	]
]

(* From Mathematica 4.0 onwards there is  "Tr" functions;
	Overload Tr to use TR
*)
Unprotect[Tr];
Tr[Pattern[FeynCalc`Private`trarg,BlankSequence[]]] :=
	TR[FeynCalc`Private`trarg] /; !FreeQ[{FeynCalc`Private`trarg}, FeynCalc`Package`TrFeynCalcObjects];
Tr::usage =
"FeynCalc extension: Tr[list] finds the trace of the matrix or tensor list. Tr[list, f] finds a
generalized trace, combining terms with f instead of Plus. Tr[list, f, n] goes down to level n
in list. \n
Tr[ expression ] calculates the DiracTrace, i.e.,  TR[ expression ], if any of DiracGamma,
DiracSlash, GA, GAD, GAE, GS, GSD or GAE are present in expression.";
Tr/:Options[Tr]:=Options[TR];
Protect[Tr];


If[ !$Notebooks && Global`$FeynCalcStartupMessages,
	$PrePrint = FeynCalcForm;
	WriteString["stdout",
	"$PrePrint is set to FeynCalcForm. Use FI and FC to change the display format.\n"],
	If[ ($Notebooks =!= True),
		$PrePrint = FeynCalcForm
	];
];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* Print FeynCalc's startup message *)
If[ Global`$FeynCalcStartupMessages =!= False,
	Print[	Style["FeynCalc ", "Text", Bold], Style[$FeynCalcVersion <> " (development version). For help, use the ",
				"Text"],
			Style[DisplayForm@ButtonBox["documentation center", BaseStyle->"Link", ButtonData :> "paclet:FeynCalc/",
				ButtonNote -> "paclet:FeynCalc/"], "Text"],
			Style[", check out the ", "Text"],
			Style[DisplayForm@ButtonBox["wiki",ButtonData :> {URL["https://github.com/FeynCalc/feyncalc/wiki"], None},BaseStyle -> "Hyperlink",
				ButtonNote -> "https://github.com/FeynCalc/feyncalc/wiki"],"Text"],
			Style[" or write to the ", "Text"],
			Style[DisplayForm@ButtonBox["mailing list.",ButtonData :> {URL["http://www.feyncalc.org/forum/"], None},BaseStyle -> "Hyperlink",
				ButtonNote -> "http://www.feyncalc.org/forum/"],"Text"]];
	Print[ Style["See also the supplied ","Text"],

	Style[DisplayForm@ButtonBox["examples.", BaseStyle -> "Hyperlink",	ButtonFunction :>
							SystemOpen[FileNameJoin[{$FeynCalcDirectory, "Examples"}]],
							Evaluator -> Automatic, Method -> "Preemptive"], "Text"],
	Style[" If you use FeynCalc in your research, please cite","Text"]];
	Print [Style[" \[Bullet] V. Shtabovenko, R. Mertig and F. Orellana, Comput. Phys. Commun., 207, 432-444, 2016, arXiv:1601.01167","Text"]];
	Print [Style[" \[Bullet] R. Mertig, M. B\[ODoubleDot]hm, and A. Denner, Comput. Phys. Commun., 64, 345-359, 1991.","Text"]]
	];

(* Load PHI... *)
If[	$LoadPhi,
	If[ $FeynCalcStartupMessages,
		PrintTemporary[Style["Loading PHI from " <>  FileNameJoin[{$FeynCalcDirectory, "Phi"}], "Text"]];
	];

	If[Get[FileNameJoin[{$FeynCalcDirectory, "Phi","Phi.m"}]] =!=$Failed,
		If[ Global`$FeynCalcStartupMessages,
			Print[	Style["PHI ", "Text", Bold],
					Style[Phi`$PhiVersion <>", for examples visit ",
						"Text"],
					Style[DisplayForm@ButtonBox["www.feyncalc.org/phi.",	ButtonData :> {URL["http://www.feyncalc.org/phi/"], None},
						BaseStyle -> "Hyperlink", ButtonNote -> "http://www.feyncalc.org/phi/"],"Text"]
			]
		],
		Message[FeynCalc::phierror]
	];
];

(* Load FeynArts... *)
If[	$LoadFeynArts,
	If[ $FeynCalcStartupMessages,
		PrintTemporary[Style["Loading FeynArts from " <>  $FeynArtsDirectory, "Text"]];
	];
	Block[ {FeynCalc`Private`loadfa, FeynCalc`Private`fafiles, FeynCalc`Private`strm, FeynCalc`Private`patch=True, FeynCalc`Private`str},
		If[	$FAPatch,
			(* Check if FeynArts needs to be patched *)
			If[(FeynCalc`Private`fafiles = FileNames["FeynArts.m", $FeynArtsDirectory])=!={},
				FeynCalc`Private`strm = OpenRead[First[FeynCalc`Private`fafiles]];
				If[ Head[FeynCalc`Private`strm] =!= InputStream,
					Message[General::noopen, First[FeynCalc`Private`fafiles]];
					Abort[]
				];
				While[	ToString[FeynCalc`Private`str] != "EndOfFile",
						FeynCalc`Private`str = Read[FeynCalc`Private`strm, String];
						If[ StringMatchQ[ToString[FeynCalc`Private`str], "*patched for use with FeynCalc*", IgnoreCase -> True],
							FeynCalc`Private`patch = False
						]
				];
				Close[First[FeynCalc`Private`fafiles]],
				Message[General::noopen, FileNameJoin[{$FeynArtsDirectory, "FeynArts.m"}]];
				Message[FeynCalc::faerror, $FeynArtsDirectory];
				FeynCalc`Private`patch = False
			];
			(* Apply the patch *)
			If[ FeynCalc`Private`patch,
				FAPatch[]
			]
		];
		FeynCalc`Private`loadfa=Block[ {Print= System`Print},Get[FileNameJoin[{$FeynArtsDirectory, "FeynArts.m"}]]];
		If[FeynCalc`Private`loadfa =!=$Failed,
			(* If everything went fine *)
			If[ Global`$FeynCalcStartupMessages,
				Print[	Style["FeynArts ", "Text", Bold],
						Style[ToString[FeynArts`$FeynArts] <>" patched for use with FeynCalc, for documentation use the ",
							"Text"],
						Style[DisplayForm@ButtonBox["manual", BaseStyle -> "Hyperlink",	ButtonFunction :>
							SystemOpen[First@FileNames[{"*.pdf", "*.PDF"}, FileNameJoin[{$FeynArtsDirectory, "manual"}]]],
							Evaluator -> Automatic, Method -> "Preemptive"], "Text"],
						Style[" or visit ", "Text"],
						Style[DisplayForm@ButtonBox["www.feynarts.de.",	ButtonData :> {URL["http://www.feynarts.de/"], None},
							BaseStyle -> "Hyperlink", ButtonNote -> "www.feynarts.de/"],"Text"]
				]
			],
			(* If FeynArts didn't load *)
			Message[FeynCalc::faerror, $FeynArtsDirectory];
		];
	];
];

(* Load TARCER... *)
If[ $LoadTARCER,
	If[ $FeynCalcStartupMessages,
			PrintTemporary[Style["Loading TARCER from " <> FileNameJoin[{$FeynCalcDirectory, "Tarcer"}], "Text"]]
	];
	Block[{FeynCalc`Private`tarcerfilenames},
		FeynCalc`Private`tarcerfilenames =
		FileNames["tarcer"<> StringReplace[$System,{"-"->"","Microsoft"->"","("->"",")"->""," "->""}] <>"*.mx",
		ToFileName[{FeynCalc`$FeynCalcDirectory,"Tarcer"}],IgnoreCase->True];
		If[ FeynCalc`Private`tarcerfilenames=!={},
			(*    If the .mx file of TARCER is found, load it now. *)
			If[	Get[Last[FeynCalc`Private`tarcerfilenames]]=!=$Failed,
				If[ $FeynCalcStartupMessages,
					Print[	Style["TARCER ", "Text", Bold],
						Style[Tarcer`$TarcerVersion <>
							", for description see the preprint hep-ph/9801383 at ", "Text"],
						Style[DisplayForm@ButtonBox["arxiv.org.",	ButtonData :>
							{URL["http://arxiv.org/abs/hep-ph/9801383"], None},
							BaseStyle -> {"Hyperlink"}, ButtonNote -> "http://arxiv.org/abs/hep-ph/9801383"],"Text"]
					]
				],
				Message[FeynCalc::taerror];
				If [$Notebooks,
					If[	ChoiceDialog[FeynCalc`Private`TarcerDialogText],
						GenerateTarcerMX
					]
				]
			],
			Message[FeynCalc::taerror];
			If [$Notebooks,
					If[	ChoiceDialog[FeynCalc`Private`TarcerDialogText],
						GenerateTarcerMX
					]
			]
		];
	];
	(* This seems to be needed for MMA8 *)
	If[FreeQ[$ContextPath,"Tarcer`"],PrependTo[$ContextPath,"Tarcer`"]];
];

BeginPackage["FeynCalc`"];
If[ Global`$LoadAddOns=!={},
	FCDeclareHeader/@Map[ToFileName[{$FeynCalcDirectory,  "AddOns",#},#<>".m"] &, Global`$LoadAddOns];
	Get/@Map[ToFileName[{$FeynCalcDirectory,  "AddOns",#},#<>".m"] &, Global`$LoadAddOns]
];
EndPackage[];


If[	$CloudEvaluation && $FCCloudTraditionalForm,
	$Post = TraditionalForm;
];
