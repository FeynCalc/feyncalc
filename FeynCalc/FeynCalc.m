(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalc															*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
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
FeynCalc`$FeynCalcVersion = "9.0.0";

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

$Abbreviations::usage =
"$Abbreviations is a list of string substitution rules used when \
generating names for storing intermediate results. \
It is used by OneLoop and PaVeReduce.\
The elements of the list should be of the form \"name\" -> \"abbreviation\".";

$AchmedRoss::usage =
"experimental";

$AL::usage =
"$AL is the head of dummy indices which may be introduced by \
Uncontract.";

$BreitMaison::usage =
"The Breitenlohner-Maison gamma5 scheme is currently not supported by \
the Dirac algebra functions. Use Tracer if you need it.";

(*
	"The setting of $BreitMaison determines whether the Breitenlohner-
				Maison scheme is applied. If $BreitMaison=True, the so-called
				naive gamma5 prescription is used, i.e. gamma5 anticommutes in
				all dimensions.  The default is False. The setting should
				be chosen in the file FeynCalc.m BEFORE loading the package.
				Reversion during a session is not possible.";
*)

$Color::usage =
"$Color is False by default. If set to True, some special variables \
will be colored.";

$Containers::usage =
"$FieldContainers is a set of heads over which FieldDerivative should
distribute, in the following sense: Let c be a member of $Containers. Then
FieldDerivative[c[f, g, h][x], x, {mu}] ->
c[FieldDerivative[f[x], x, {mu}], FieldDerivative[f[x], x, {mu}],
FieldDerivative[f[x], x, {mu}]].";

$Covariant::usage =
"The boolean setting of $Covariant determines whether \
lorentz indices are displayed as lower indices (True) or as \
upper ones (False).";

DOT::usage =
"DOT[a, b, ...] is the FeynCalc function for non-commutative \
multiplication. By default it is set to the Mathematica Dot \
functions. By setting  \n
DOT=. \n
this can be disabled. \
Note that then non-commutative products should to be entered \
like DOT[ DiracMatrix[mu], m + DiracSlash[p], DiracMatrix[mu] ], \
etc.";

$DistributiveFunctions::usage =
"$DistributiveFunctions is a set of functions over which FieldDerivative
should be distributed.";

$FAPatch::usage =
"$FAPatch switches on and off checking for \
an unpatched FeynArts installation on FeynCalc startup.  Default value : True.";

FC::usage =
"FC changes the output format to FeynCalcForm. \
To change to InputForm use FI.";

FCPrint::usage =
"FCPrint[level, x] outputs Print[x] if the value of $VeryVerbose
is larger than level.";

$FCS::usage = "$FCS is a list of functions with a short name. \
E.g. GA[nu] can be used instead of DiracGamma[nu].";

FeynCalc::usage =
"For installation notes visit www.feyncalc.org\n
For a list of availabe objects type $FeynCalcStuff, \
which contains a list of all functions and options in StringForm. \
You can get on-line information by ?function, e.g. ?Contract.\n
There are several useful functions for short input, type $FCS for a list of \
short commands. Then type, e.g., ?GA.\n\n
To enable/disable start-up messages, put the line\n
$FeynCalcStartupMessages = True;\n
or\n
$FeynCalcStartupMessages = False;\n
into your \"init.m\" file or into your \"FCConfig.m\" file."

$FeynCalcStuff::usage =
"$FeynCalcStuff is the list of availabe stuff in FeynCalc.";

$FeynCalcVersion::usage =
"$FeynCalcVersion is a string that represents the version of FeynCalc.";

FI::usage =
"FI changes the output format to InputForm. \
This is useful to see the internal representation of FeynCalc \
objects. To change back to FeynCalcForm use FC.";

$Gauge::usage =
"$Gauge(= 1/xi) is a constant specifying the gauge fixing parameter of QED \
in Lorentz gauge.  The usual choice is Feynman gauge, $Gauge=1. \
Notice that $Gauge is used by some functions, the option Gauge by others.";

GenerateTarcerMX::usage =
"GenerateTarcerMX creates the *.mx file for TARCER. This is necessary to use
TARCER and has to be done only once. The evaluation usually takes a
couple of minutes."

$IndexPrefix::usage =
"$IndexPrefix is a list of prefixes for default Lorentz and color indices
used by GluonPropagator and similar functions.";

$Larin::usage =
"If set to True, the Larin-Gorishny-Atkyampo-DelBurgo-scheme for \
gamma5 in D-dimensions is used, i.e. before evaluating traces \
(but after moving gamma5 anticommuting in all dimensions to the \
right of the Dirac string) a product  gamma[mu].gamma5 is \
substituted to  -I/6 Eps[mu,al,be,si] gamma[al,be,si], \
where all indices live in D-dimensions now. \
Especially the Levic-Civita tensor is taken to be \
D-dimensional, i.e., contraction of two Eps's results in D's. \
This has (FOR ONE AXIAL-VECTOR-CURRENT ONLY, it is not so clear \
if this scheme also works for more than one fermion line \
involving gamma5) the same effect as the \
Breitenlohner-Maison-'t Hooft-Veltman scheme.";

$LeviCivitaSign::usage =
"$LeviCivitaSign is by default set to -1 which corresponds to the convention
Tr[LeviCivita[a,b,c,d,5]] = -4*I*Eps[a,b,c,d].
Setting $LeviCivitaSign=I  will switch to the FORM-convention.";

$LimitTo4::usage =
"$LimitTo4 is a global variable with default setting True. \
If set to False no limit Dimension -> 4 is \
performed after tensor integral decomposition.";

$LorentzIndices::usage =
"$LorentzIndices is a global variable. If set to True the dimension \
of LorentzIndex is displayed as an index.";

MakeFeynCalcPrivateContext::usage =
"MakeFeynCalcPrivateContext[val] constructs
FeynCalc`Private`val.";

$MemoryAvailable::usage =
"$MemoryAvailable is  a global variable which is set to an integer \
n, where n is the available amount of main memory in MB. \
The default is 1024. It should be increased if possible. \
The higher $MemoryAvailable can be, the more intermediate \
steps do not have to be repeated by FeynCalc.";

$MU::usage =
"$MU is the head of dummy indices which may be introduced by \
Chisholm, Contract, DiracSimplify, FermionSpinSum and various \
QCD functions. By default it is unset, but can be set to anything.";

$Multiplications::usage =
"$Multiplications is a set functions which should be treated as
(commutative or non-commutative) multiplications by FieldDerivative.";

$NonComm::usage =
"$NonComm contains a list of all non-commutative heads present.";

$OPEWard::usage =
"$OPEWard is experimental.";

OptionsSelect::usage =
"OptionsSelect[function,opts] returns the option settings of opts \
accepted by function.  When an option occurs several times in opts, the first \
setting is selected";

TBox::usage =
"TBox[a, b, ...] produces a RowBox[{a,b, ...}] where \
a,b, ... are boxed in TraditionalForm.";

UseWriteString::usage =
"UseWriteString is an option for FCPrint. If set to True,
the expression is printed via WriteString instead of Print.";

$VeryVerbose::usage =
"$VeryVerbose is a global variable with default setting 0. \
If set to 1, 2, ..., less and more intermediate comments and informations \
are displayed during calculations.";

$FCAdvice::usage =
"If $FCAdvice is set to True, FeynCalc will display some
advices on optimal Mathematica configuration for using FeynCalc."

$West::usage =
"If $West is set to True (which is the default), \
traces involving more than 4 Dirac matrices \
and gamma5 are calculated recursively according to formula (A.5) from \
Comp. Phys. Comm 77 (1993) 286-298, which is based on the Breitenlohner \
Maison gamma5 - scheme.";

WriteStringOutput::usage =
"UseWriteStringOutput an option for FCPrint. It specifies, to which
stream WriteString should output the expression";

FeynCalc::faerror =
"FeynArts not found or damaged. Please download the FeynArts \
tarball from www.feynarts.de, unpack it to `1` and restart FeynCalc.";

FeynCalc::taerror =
"TARCER*.mx file not found or damaged. Please evaluate the command \
GenerateTarcerMX to create it.";

FeynCalc::phierror =
"PHI failed to load. Please try resintalling FeynCalc.";

FeynCalc::tfadvice =
"You are not using TraditionalForm as the default format type of new \
output cells. Without TraditionalForm FeynCalc cannot use buil-in \
typeseting rules that make various objects like Lorentz vectors or \
Dirac matrices look nicer. To change the format type go to \
Edit->Preferences->Evaluation.";

FCMonitor::usage =
"FCMonitor is a simple function that activates Monitor if there
is a notebook interface available and disables it otherwise.";

FCMonitorStub::usage =
"FCMonitorStub is a stub for Monitor when the notebook interface
is not available";

FCDoControl::usage =
"FCDoControl is an option for FCPrint that specifies which variable
is used to control the debugging output of FCPrint. The default value
is $VeryVerbose.";

FCDeclareHeader::usage =
"FCDeclareHeader is an internal FeynCalc function to declare
objects inside an .m file in the same manner as it is done in
the JLink package. It may be used by FeynCalc addons."

Begin["`Private`"]

TarcerDialogText = "TARCER*.mx file not found or damaged. Creating a new \
file can take couple of minutes, but this has to be done only once. \
After the new file is generated you need to restart FeynCalc. Should \
we generate new TARCER*.mx now?"

$AchmedRoss				= False;

$Abbreviations = {
	", "->"",
	"^"->"",
	"{"->"",
	"/" -> "",
	"Subscript"->"su",
	"SmallVariable"->"sma",
	"}"->"",
	"["->"",
	"]"->"",
	"*" -> "",
	" " -> "" ,
	"\n" -> "",
	"\r" -> ""
};

$BreitMaison			= False;
$Color					= False;
$Containers				= {};
$Covariant				= False;
$DistributiveFunctions	= {Conjugate, Transpose};

$FCS = {
	"CDr",
	"FAD",
	"FC",
	"FCE",
	"FCI",
	"FDr",
	"FI",
	"FV",
	"FVD",
	"FVE",
	"GA",
	"GA5",
	"GAD",
	"GAE",
	"GGV",
	"GP",
	"GS",
	"GSD",
	"GSE",
	"LC",
	"LCD",
	"MT",
	"MTD",
	"MTE",
	"QGV",
	"QO",
	"SD",
	"SDF",
	"SOD",
	"SP",
	"SPC",
	"SPD",
	"SPE",
	"SPL"
};

$IndexPrefix		= {"li","ci"};
$Larin				= False;
$LeviCivitaSign		= -1;
$LimitTo4			= True;
$LorentzIndices		= False;
$MemoryAvailable	= 4096;
$Multiplications	= {Times, DOT};
$OPEWard			= False;

If[ !ValueQ[$VeryVerbose],
	$VeryVerbose   = 0
];

If[ !ValueQ[$West],
	$West = True
];

If[ !ValueQ[$NonComm],
	$NonComm = {}
];

$Gauge/:
	MakeBoxes[$Gauge,TraditionalForm]:=
		MakeBoxes[StyleForm["\[Lambda]",FontSlant->"Italic"]];

GenerateTarcerMX :=
	If[	Get@ToFileName[{$FeynCalcDirectory, "Tarcer"}, "TARCER.m"]=!= $Failed,
		Print["Succesfully created ", Last@FileNames["*.mx", FileNameJoin[{$FeynCalcDirectory, "Tarcer"}]]]
	]

DOT = Dot;

(* DOT moved into main context. 32/2-2003. F.Orellana.
	The reason for this is the following: In order to use
	DOT instead of Dot consistenly everywhere, we need to be
	able to do e.g. f[DOT[a_,b_]]:=g[a,b]. Because DOT is immediately
	set to something else (Dot), this works only if DOT is in
	$ContextPath at the moment this definition is evaluated.
	In the context of the packages, $ContextPath is typically
	{"System`", "FeynCalc`"}.
	*)

SetAttributes[FCPrint, HoldAll];

Options[FCPrint] = {
		FCDoControl -> $VeryVerbose,
		UseWriteString -> False,
		WriteStringOutput ->"stdout"
}

FCPrint[level_, x:Except[_?OptionQ].. , OptionsPattern[]] :=
	Block[{flowcontrol=OptionValue[FCDoControl]},
		If[ flowcontrol >= level,
			If[ OptionValue[UseWriteString],
				WriteString[OptionValue[WriteStringOutput],x],
				Print[x]
			]
		]
	];

FCMonitor:=
	If[$Notebooks, Monitor, FCMonitorStub];

FCMonitorStub[x_,__]:=
	x;

FCDeclareHeader[file_] :=
	Module[ {strm, e, moreLines = True},
		strm = OpenRead[file];
		If[ Head[strm] =!= InputStream,
			Return[$Failed]
		];
		While[
			moreLines,
			e = Read[strm, Hold[Expression]];
			ReleaseHold[e];
			If[ e === $Failed || MatchQ[e, Hold[_End]],
				moreLines = False
			]
		];
		Close[file]
	];


FI :=
	(Format[LineBreak[_]] :=
		"";
	$PrePrint = InputForm;);
FC :=
	(Format[LineBreak[_]] :=
		"\n";
	(If[ !$Notebooks,
		$PrePrint = FeynCalc`FeynCalcForm,
		Unset[$PrePrint]
	]););

TBox[] =
	"\[Null]";
TBox[a_] :=
	ToBoxes[a, TraditionalForm];
TBox[a_,b__] :=
	RowBox @ Map[(ToBoxes @@ {#, TraditionalForm})&, {a, b}];

l[w_Integer] :=
	l[w] =
		Block[ {pre},
			If[ !MatchQ[pre = $IndexPrefix,{_String,_String}],
				pre = {ToString[Unique["l"]], ToString[Unique["c"]]}
			];
			ToExpression[pre[[1]]<>ToString[w]]
		];

c[w_Integer] :=
	c[w] =
		Block[ {pre},
			If[ !MatchQ[pre = $IndexPrefix,{_String,_String}],
				pre = {ToString[Unique["l"]], ToString[Unique["c"]]}
			];
			ToExpression[pre[[2]]<>ToString[w]]
		];

(*TODO: Get rid of OptionsSelect everywhere. Use FilterRules[opts, Options[function]] instead *)
OptionsSelect[function_, opts___] :=
	Select[(Cases[{opts}, _Rule|_RuleDelayed, Infinity] //.
	{{a___, b_ -> c_, d___, b_ -> e_, f___} -> {a, b -> c, d, f},
	{a___, b_ :> c_, d___, b_ :> e_, f___} -> {a, b :> c, d, f}}),
	(!FreeQ[#, (Options[function] /.
						{((a_ -> b_) | (a_ :> b_)) -> ((a -> _) | (a :> _))} /.
							List -> Alternatives)])&];

MakeFeynCalcPrivateContext[x_String] :=
	MakeFeynCalcPrivateContext[x] =	ToExpression["FeynCalc`Private`"<>x];

End[];

(* need to do this first, otherwise $NonComm does not get built correctly *)
boostrappingList = Join[Map[ToFileName[{$FeynCalcDirectory,"Shared"},#]&,
			{"SharedTools.m", "DataType.m"}],
			Map[ToFileName[{$FeynCalcDirectory,"NonCommAlgebra"},#]&,
			{"NonCommutative.m"}]
			];

listShared = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Shared"}]];
listNonCommAlgebra = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"NonCommAlgebra"}]];
listLorentz = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Lorentz"}]];
listDirac = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Dirac"}]];
listSUN = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"SUN"}]];
listLoopIntegrals = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"LoopIntegrals"}]];
listFeynman = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Feynman"}]];
listQCD = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"QCD"}]];
listTables = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Tables"}]];
listExportImport = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"ExportImport"}]];
listMisc = FileNames[{"*.m"},ToFileName[{$FeynCalcDirectory,"Misc"}]];


AppendTo[$ContextPath, "FeynCalc`Package`"];

FCDeclareHeader/@listShared;
FCDeclareHeader/@listNonCommAlgebra;
FCDeclareHeader/@listLorentz;
FCDeclareHeader/@listDirac;
FCDeclareHeader/@listSUN;
FCDeclareHeader/@listLoopIntegrals;
FCDeclareHeader/@listFeynman;
FCDeclareHeader/@listQCD;
FCDeclareHeader/@listTables;
FCDeclareHeader/@listExportImport;
FCDeclareHeader/@listMisc;

Get/@boostrappingList;
Get/@listShared;
Get/@listNonCommAlgebra;
Get/@listLorentz;
Get/@listDirac;
Get/@listSUN;
Get/@listLoopIntegrals;
Get/@listFeynman;
Get/@listQCD;
Get/@listTables;
Get/@listExportImport;
Get/@listMisc;

EndPackage[];

(*Let us check the configuration of Mathematica and give the user some advices, if necessary*)
If[$FCAdvice,
	If[ $Notebooks &&
		Cases[Options[$FrontEndSession, CommonDefaultFormatTypes], Rule["Output", b_] :> b, Infinity]=!={TraditionalForm},
		Message[FeynCalc::tfadvice]
	]
]

(* From Mathematica 4.0 onwards there is  "Tr" functions;
	Overload Tr to use TR
*)
Unprotect[Tr];
Tr[x__] :=
	TR[x] /; !FreeQ[{x}, DiracGamma | DiracMatrix | DiracSlash | GA | GAD | GAE | GS | GSD | GSE | Pair];
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
	Print[	Style["FeynCalc ", "Text", Bold], Style[$FeynCalcVersion <> ". For help, type ?FeynCalc, use the ",
				"Text"],
			Style[DisplayForm@ButtonBox["help browser",ButtonData :> {"Short Overview", "intro"}, BaseStyle -> "AddOnsLink",
				ButtonNote -> "FeynCalc"], "Text"],
			Style[" or visit ", "Text"],
			Style[DisplayForm@ButtonBox["www.feyncalc.org.",ButtonData :> {URL["http://www.feyncalc.org/"], None},BaseStyle -> "Hyperlink",
				ButtonNote -> "http://www.feyncalc.org/"],"Text"]]
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
	Block[ {loadfa, fafiles, strm, patch=True, str},
		If[	$FAPatch,
			(* Check if FeynArts needs to be patched *)
			If[(fafiles = FileNames["FeynArts.m", $FeynArtsDirectory])=!={},
				strm = OpenRead[First[fafiles]];
				If[ Head[strm] =!= InputStream,
					Message[General::noopen, First[fafiles]];
					Abort[]
				];
				While[	ToString[str] != "EndOfFile",
						str = Read[strm, String];
						If[ StringMatchQ[ToString[str], "*patched for use with FeynCalc*", IgnoreCase -> True],
							patch = False
						]
				];
				Close[First[fafiles]],
				Message[General::noopen, FileNameJoin[{$FeynArtsDirectory, "FeynArts.m"}]];
				Message[FeynCalc::faerror, $FeynArtsDirectory];
				patch = False
			];
			(* Apply the patch *)
			If[ patch,
				FAPatch[]
			]
		];
		loadfa=Block[ {Print= System`Print},Get[FileNameJoin[{$FeynArtsDirectory, "FeynArts.m"}]]];
		If[loadfa =!=$Failed,
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
	Block[{tarcerfilenames},
		tarcerfilenames =
		FileNames["tarcer"<> StringReplace[$System,{"-"->"","Microsoft"->"","("->"",")"->""," "->""}] <>"*.mx",
		ToFileName[{FeynCalc`$FeynCalcDirectory,"Tarcer"}],IgnoreCase->True];
		If[ tarcerfilenames=!={},
			(*    If the .mx file of TARCER is found, load it now. *)
			If[	Get[Last[tarcerfilenames]]=!=$Failed,
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

If[ Global`$LoadAddOns=!={},

	FCDeclareHeader/@Map[ToFileName[{$FeynCalcDirectory,  "AddOns",#},#<>".m"] &, Global`$LoadAddOns];
	Get/@Map[ToFileName[{$FeynCalcDirectory,  "AddOns",#},#<>".m"] &, Global`$LoadAddOns];

	FCDeclareHeader/@Flatten[Map[FileNames[{"*.m"},
	ToFileName[{$FeynCalcDirectory,  "AddOns", #}],Infinity] &, Global`$LoadAddOns]];

	Get/@Flatten[Map[FileNames[{"*.m"},
	ToFileName[{$FeynCalcDirectory,  "AddOns", #}],Infinity] &, Global`$LoadAddOns]]
];

