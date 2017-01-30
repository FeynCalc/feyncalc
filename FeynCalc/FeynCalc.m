(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalc															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
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
FeynCalc`$FeynCalcVersion = "9.2.0";

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

$DisableMemSet::usage=
"The boolean setting of $DisableMemSet allows to disable \
memoizaion that is activated via MemSet. This can be \
useful for debugging purposes";

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

$FCCheckProgress::usage =
"If set to True, some selected functions will display a progress monitor
during their evaluation. While this can be useful for long computations,
on short expressions the progress bars make evaluating slightly slower.
By default $FCCheckProgress is set to False.";

$FCCloudTraditionalForm::usage=
"$FCCloudTraditionalFormetermines whether the the cell output will be done \
in TraditionalForm when FeynCalc is run in Wolfram Cloud. This is done by setting
$Post=TraditionalForm. The default value of $FCCloudTraditionalForm is True."

$FCTensorList::usage =
"$FCTensorList contains a list of all tensor heads present.";

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

GenerateTarcerMX::usage =
"GenerateTarcerMX creates the *.mx file for TARCER. This is necessary to use
TARCER and has to be done only once. The evaluation usually takes a
couple of minutes."

$KeepLogDivergentScalelessIntegrals::usage =
"$KeepLogDivergentScalelessIntegrals is an experimental global option that forces \
FeynCalc not to set 1-loop integrals of type 1/q^4 to zero. This is useful \
when one has to explicitly distinguish between IR- and UV-divergences in \
dimensional regularization. Notice that OneLoop is not guaranteed to respect this \
option.";

$IndexPrefix::usage =
"$IndexPrefix is a list of prefixes for default Lorentz and color indices
used by GluonPropagator and similar functions.";

$Larin::usage =
"If set to True, the Larin-Gorishny-Atkyampo-DelBurgo-scheme for \
gamma5 in D-dimensions is used, i.e. before evaluating traces \
(but after moving gamma5 anticommuting in D-dimensions to the \
right of the Dirac string inside a trace) a product  gamma[mu].gamma5 is \
substituted to -I/6 Eps[mu,al,be,si] gamma[al,be,si], \
where all indices live in D-dimensions now. \
The Levi-Civita tensor is taken to be \
D-dimensional, i.e., contraction of two Eps's results in D's. \
This scheme is often used for performance reasons and is assumed \
to give the same results as the \
Breitenlohner-Maison-'t Hooft-Veltman (BMHV) scheme. However, gamma5 is \
not anticommuting inside closed fermion loops and it is not so clear
if this scheme works for more than one fermion line involving gamma5. \
When in doubt, it might be better to use BMHV instead.";

$LeviCivitaSign::usage =
"$LeviCivitaSign is a global variable that determines \
the sign in the result of a Dirac trace of four gamma matrices \
and gamma5.  $LeviCivitaSign is by default set to -1 which corresponds \
to the convention Tr[LeviCivita[a,b,c,d,5]] = -4*I*Eps[a,b,c,d]. \
Setting $LeviCivitaSign=-I  will switch to the FORM-convention.";

$LimitTo4::usage =
"$LimitTo4 is a global variable that determines whether \
UV-divergent Passarino-Veltman functions are simplified by \
taking the limit D-4 -> 0. A general UV-finite \
Passarino-Veltman function can be written as \
PaVe = a/(D-4) + b + O(Epsilon), with a being the prefactor \
of the pole and b being the finite part. Therefore, products \
of such functions with coefficients that are rational functions \
of D ( f(D) = f(4) + f'(4) (D-4) + O(Epsilon^2) ) can be simplified \
to f(D) PaVe = f(4) PaVe + a f'(4) + O(Epsilon), whenever such \
products appear in the reduction. This relation is correct only if
the Passarino-Veltman functions have no IR divergences, or if such \
divergences are regulated without using dimensional regularization.
For this reason, even when $LimitTo4 is set to True, the simplifications \
are applied only to A and B functions. Although B functions can exhibit an \
IR divegence, such integrals are zero in dimensional regularization, so no \
mixing of Epsilons from IR and UV can occur. The default value of \
$LimitTo4 is True.";

$LimitTo4IRUnsafe::usage =
"$LimitTo4IRUnsafe is a global variable that determines whether \
the simplifications described in $LimitTo4 are applied also to \
C and D Passarino-Veltman functions. In this case it is assumed \
that such  functions are either IR finite, or the IR divergences \
are regulated  without using dimensional regularization \
(i.e. by introducing  fictitious masses). Otherwise the results \
will be inconsistent. If this option is activated, it is the task \
of the user to ensure that IR divergences are properly regulated, \
such that no mixing of Epsilons from IR and UV can occur. The default \
value of $$LimitTo4IRUnsafe is False.";

$LorentzIndices::usage =
"$LorentzIndices is a global variable. If set to True the dimension \
of LorentzIndex is displayed as an index.";

MakeFeynCalcPrivateContext::usage =
"MakeFeynCalcPrivateContext[val] constructs
FeynCalc`Private`val.";

$MemoryAvailable::usage =
"$MemoryAvailable is  a global variable which is set to an integer \
n, where n is the available amount of main memory in MB. \
The default is 4096. It should be increased if possible. \
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

$ScalarProducts::usage =
"$ScalarProducts contains a list of all vector pairs for which a \
scalar product value has been defined.";

$OPEWard::usage =
"$OPEWard is experimental.";

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
output cells. Without TraditionalForm FeynCalc cannot use built-in \
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
$Containers				= {};
$Covariant				= False;
$DisableMemSet 			= False;
$DistributiveFunctions	= {Conjugate, Transpose};
$FCCheckProgress		= False;

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
$KeepLogDivergentScalelessIntegrals = False;
$Larin				= False;
$LeviCivitaSign		= -1;
$LimitTo4			= False;
$LimitTo4IRUnsafe	= False;
$LorentzIndices		= False;
$MemoryAvailable	= 4096;
$Multiplications	= {Times, DOT};
$OPEWard			= False;

If[ !ValueQ[$VeryVerbose],
	$VeryVerbose   = 0
];

If[ !ValueQ[$NonComm],
	$NonComm = {}
];

If[ !ValueQ[$FCTensorList],
	$FCTensorList = {}
];

If[ !ValueQ[$ScalarProducts],
	$ScalarProducts = {}
];


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

SetAttributes[FCPrint, HoldRest];

Options[FCPrint] = {
		FCDoControl :> $VeryVerbose,
		UseWriteString -> False,
		WriteStringOutput ->"stdout"
}

FCPrint[level_, fcprintx__ /;!OptionQ[{fcprintx}] , OptionsPattern[]] :=
	Block[{flowcontrol=OptionValue[FCDoControl]},
		If[ flowcontrol >= level,
			If[ OptionValue[UseWriteString],
				WriteString[OptionValue[WriteStringOutput],fcprintx],
				Print[fcprintx]
			]
		]
	];

FCMonitor:=
	If[$Notebooks && $FCCheckProgress, Monitor, FCMonitorStub];

FCMonitorStub[x_,__]:=
	x;

FCDeclareHeader[file_] :=
	Module[ {strm, einput, moreLines = True},
		strm = OpenRead[file];
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

MakeFeynCalcPrivateContext[x_String] :=
	MakeFeynCalcPrivateContext[x] =	ToExpression["FeynCalc`Private`"<>x];

End[];

(* need to do this first, otherwise $NonComm and $FCTensorList do not get built correctly *)
boostrappingList = Join[
	Map[ToFileName[{$FeynCalcDirectory,"Shared"},#]&, {"SharedTools.m", "DataType.m"}],
	Map[ToFileName[{$FeynCalcDirectory,"NonCommAlgebra"},#]&, {"NonCommutative.m"}],
	Map[ToFileName[{$FeynCalcDirectory,"Lorentz"},#]&, {"DeclareFCTensor.m"}]
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
		Cases[Options[$FrontEndSession, CommonDefaultFormatTypes], Rule["Output", Pattern[FeynCalc`Private`rulopt, Blank[]]] :> FeynCalc`Private`rulopt, Infinity]=!={TraditionalForm},
		Message[FeynCalc::tfadvice]
	]
]

(* From Mathematica 4.0 onwards there is  "Tr" functions;
	Overload Tr to use TR
*)
Unprotect[Tr];
Tr[Pattern[FeynCalc`Private`trarg,BlankSequence[]]] :=
	TR[FeynCalc`Private`trarg] /; !FreeQ[{FeynCalc`Private`trarg}, DiracGamma | DiracMatrix | DiracSlash | GA | GAD | GAE | GS | GSD | GSE | Pair];
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
	Print [Style[" \[Bullet] V. Shtabovenko, R. Mertig and F. Orellana, Comput. Phys. Commun., 207C, 432-444, 2016, arXiv:1601.01167","Text"]];
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
