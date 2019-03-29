(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCMain															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:	Global FeynCalc functions and objects.						*)

(* ------------------------------------------------------------------------ *)

$Abbreviations::usage =
"$Abbreviations is a list of string substitution rules used when \
generating names for storing intermediate results. \
It is used by OneLoop and PaVeReduce.\
The elements of the list should be of the form \"name\" -> \"abbreviation\".";

$AL::usage =
"$AL is the head of dummy indices which may be introduced by \
Uncontract.";

$BreitMaison::usage =
"The setting of $BreitMaison determines whether the Breitenlohner-Maison \
scheme is applied. If $BreitMaison=False, the so-called	naive gamma5 \
prescription is used, i.e. gamma5 anticommutes in all dimensions. \
The default is False. The setting should be chosen at the beginning of\
a FeynCalc session. Reversion during a session is not possible.";

$Containers::usage =
"$FieldContainers is a set of heads over which FieldDerivative should
distribute, in the following sense: Let c be a member of $Containers. Then
FieldDerivative[c[f, g, h][x], x, {mu}] ->
c[FieldDerivative[f[x], x, {mu}], FieldDerivative[f[x], x, {mu}],
FieldDerivative[f[x], x, {mu}]].";

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

$FCCheckContext::usage =
"If set to True, FeynCalc will try to detect unwanted leakage of internal \
objects into the Global or FeynCalc contexts. The default value is False,
however $FCCheckContext will be automatically enabled in the development
version.";

$FCCloudTraditionalForm::usage=
"$FCCloudTraditionalForm determines whether the the cell output will be done \
in TraditionalForm when FeynCalc is run in Wolfram Cloud. This is done by setting
$Post=TraditionalForm. The default value of $FCCloudTraditionalForm is True."

$FCTraditionalFormOutput::usage=
"The boolean setting of $FCTraditionalFormOutput determines which \
output format type should be used in the notebook front end when \
FeynCalc is loaded. If set to True, FeynCalc will activate the \
TraditionalForm output. Otherwise, the StandardForm output \
(Mathematica's default) will be used. This setting only changes \
the output format of the current notebook, i.e. it is not persistent \
and will not modify the global options of Mathematica. If unsure, it
is recommended to set $FCTraditionalFormOutput to True, so that you \
can benefit from the nice FeynCalc typesetting for various QFT quantities.";

$FeynCalcStartupMessages::usage=
"$FeynCalcStartupMessages specifies whether some additional information about \
FeynCalc should be displayed when the package is loaded. Its value must be set \
before loading FeynCalc. The default value is True.";

$FeynCalcDevelopmentVersion::usage =
"The boolean setting of $FeynCalcDevelopmentVersion determines whether \
the current version is a development or a stable version.";

$FCTensorList::usage =
"$FCTensorList contains a list of all tensor heads present.";

$FCShowIEta::usage =
"The boolean setting of $FCShowIEta detrmines whether I \[Eta] should \
be displayed in the typesetting of GFAD and GenericPropagatorDenominator \
objects or not. This setting affects only the TraditionalForm typesetting \
and has absolutely no influence on the internal handling of propagator \
denominators in FeynCalc.";

FCSetMetricSignature::usage =
"FCSetMetricSignature sets the signature of the Minkowski metric used when \
working with Cartesian objects, like CartesianPair, CartesianIndex, CartesianMomentum etc. \
The default choice is (1,-1,-1,-1) which corresponds to \
FCSetMetricSignature[{1,-1}]";

FCGetMetricSignature::usage =
"FCGetMetricSignature[] returns the signature of the Minkowski metric used when \
working with Cartesian objects, like CartesianPair, CartesianIndex, CartesianMomentum etc. \
{1,-1} corresponds to (1,-1,-1,-1) and {-1,1} means (-1, 1, 1, 1)";

FCPrint::usage =
"FCPrint[level, x] outputs Print[x] if the value of $VeryVerbose
is larger than level.";

$FCS::usage = "$FCS is a list of functions with a short name. \
E.g. GA[nu] can be used instead of DiracGamma[nu].";

FeynCalc::usage =
"For installation notes visit www.feyncalc.github.io. You can get \
on-line information by ?function, e.g. ?Contract.\n
There are several useful functions for short input, type $FCS for a list of \
short commands. Then type, e.g., ?GA.\n\n
To enable/disable start-up messages, put the line\n
$FeynCalcStartupMessages = True;\n
or\n
$FeynCalcStartupMessages = False;\n
into your \"init.m\" file or into your \"FCConfig.m\" file."

$FeynCalcVersion::usage =
"$FeynCalcVersion is a string that represents the version of FeynCalc.";

FI::usage =
"FI changes the output format to InputForm. \
This is useful to see the internal representation of FeynCalc \
objects. To change back to FeynCalcForm use FC.";

$KeepLogDivergentScalelessIntegrals::usage =
"$KeepLogDivergentScalelessIntegrals is an experimental global option that forces \
FeynCalc not to set 1-loop integrals of type 1/q^4 to zero. This is useful \
when one has to explicitly distinguish between IR- and UV-divergences in \
dimensional regularization. Notice that OneLoop is not guaranteed to respect this \
option.";

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
Setting $LeviCivitaSign=-I  will switch to the FORM-convention. \n

In terms of explicit components, $LeviCivitaSign=1 corresponds to setting
eps^{0123} = 1 (as in Peskin and Schroeder), while choosing $LeviCivitaSign=-1
gives us the convention of Bjorken and Drell with eps^{0123} = -1. Two other
possible choices are $LeviCivitaSign=-I to have eps^{0123} = I as in FORM and
$LeviCivitaSign=I which would give eps^{0123} = -I.";

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

$LoadAddOns::usage =
"$LoadAddOns specifies which addons should be loaded with FeynCalc. \
E.g. $LoadAddOns={\"FeynHelpers\"}. The value must be set before \
loading FeynCalc. The default value is False.";

$LoadFeynArts::usage =
"$LoadFeynArts specifices whether FeynArts should be loaded together \
with FeynCalc. Its value must be set before loading FeynCalc. \
The default value is False.";

$LoadPhi::usage =
"$LoadPhi specifices whether PHI should be loaded together \
with FeynCalc. Its value must be set before loading FeynCalc. \
The default value is False.";

$FCMemoryAvailable::usage =
"$FCMemoryAvailable is  a global variable which is set to an integer \
n, where n is the available amount of main memory in MB. \
The default is 4096. It should be increased if possible. \
The higher $FCMemoryAvailable can be, the more intermediate \
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

$RenameFeynCalcObjects::usage =
"$RenameFeynCalcObjects specifies a List of replacement rules that \
allow to rename FeynCalc objects on the run to avoid conflicts with \
other package before FeynCalc is loaded (monkey patching). The value
of $RenameFeynCalcObjects must be specified before loading FeynCalc.";

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

FeynCalc::phierror =
"PHI failed to load. Please try resintalling FeynCalc.";

FeynCalc::tfadvice =
"You are not using TraditionalForm as the default format type of new \
output cells. Without TraditionalForm FeynCalc cannot use built-in \
typeseting rules that make various objects like Lorentz vectors or \
Dirac matrices look nicer. To change the format type go to \
Edit->Preferences->Evaluation.";

FeynCalc::nrfail =
"This function is not yet ready to work with new non-covariant objects \
in FeynCalc.";

FeynCalc::context =
"FeynCalc has detected strange objects in the Global or FeynCalc contexts.";

FCDoControl::usage =
"FCDoControl is an option for FCPrint that specifies which variable
is used to control the debugging output of FCPrint. The default value
is $VeryVerbose.";

FCSetPauliSigmaScheme::usage =
"FCSetPauliSigmaScheme[\"scheme\"] allows you to specify how Pauli matrices \
will be handled in D-1 dimensions. This is mainly related to \
the commutator of two Pauli matrices, which involves a Levi-Civita \
tensor. The latter is not a well-defined quantity in D-1 dimensions. \
Following schemes are supported: \

\"None\" - This is the default value. The anticommutator relation is not \
applied to D-1 dimensional Pauli matrices.

\"Naive\" - Naively apply the commutator relation in D-1-dimensions, i.e. \
CSID[i,j]-CSID[i,j] = 2 i CLCD[i,j,k] SID[k]. The Levi-Civita tensor lives \
in D-1-dimensions, so that a contraction of two such tensors which have all \
indices in common yields (D-3) (D-2) (D-1)."

FCGetPauliSigmaScheme::usage =
"FCGetPauliSigmaScheme[] shows currently used scheme for handling Pauli
matrices in D-1 dimensions.";

FCEnableTraditionalFormOutput::usage =
"FCEnableTraditionalFormOutput[] sets the output format of the current
FrontEnd to TraditionalForm. The setting is not persistent, such that
it does not influence any subequent Mathematica FrontEnd sessions.";

FCDisableTraditionalFormOutput::usage =
"FCDisableTraditionalFormOutput[] sets the output format of the current
FrontEnd to StandardForm. The setting is not persistent, such that
it does not influence any subequent Mathematica FrontEnd sessions.";


(* ------------------------------------------------------------------------ *)
Begin["`Package`"]

End[]

Begin["`Private`"]

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
$DisableMemSet 			= False;
$DistributiveFunctions	= {Conjugate, Transpose};
$FCShowIEta				= True;
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

$KeepLogDivergentScalelessIntegrals = False;
$Larin				= False;
$LeviCivitaSign		= -1;
$LimitTo4			= False;
$LimitTo4IRUnsafe	= False;
$FCMemoryAvailable	= 4096;
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


FeynCalc`Package`MetricT = 1;
FeynCalc`Package`MetricS = -1;
FeynCalc`Package`PauliSigmaScheme = "None";

FCEnableTraditionalFormOutput[]:=
	(CurrentValue[$FrontEndSession, {CommonDefaultFormatTypes, "Output"}] = TraditionalForm;);

FCDisableTraditionalFormOutput[]:=
	(CurrentValue[$FrontEndSession, {CommonDefaultFormatTypes, "Output"}] = StandardForm; );

FCSetPauliSigmaScheme[s_String]:=
	If[	MatchQ[s,"None"|"Naive"],
		FeynCalc`Package`PauliSigmaScheme = s,
		Message[SharedObjects::failmsg, "Unknown scheme for Pauli matrices in D-1 dimensions."];
		Abort[]
	];


FCGetPauliSigmaScheme[]:=
	FeynCalc`Package`PauliSigmaScheme;

FCSetMetricSignature[{t_Integer,s_Integer}]:=
	(
	If[ {s^2,t^2}=!={1,1},
		Message[SharedObjects::failmsg, "The square of each diagonal element of the metric tensor must be unity."];
		Abort[]
	];
	FeynCalc`Package`MetricT = t;
	FeynCalc`Package`MetricS = s;
	);


FCGetMetricSignature[]:=
	{FeynCalc`Package`MetricT,FeynCalc`Package`MetricS};


FCPrint[level_, fcprintx__ /;!OptionQ[{fcprintx}] , OptionsPattern[]] :=
	Block[{flowcontrol=OptionValue[FCDoControl]},
		If[ flowcontrol >= level,
			If[ OptionValue[UseWriteString],
				WriteString[OptionValue[WriteStringOutput],fcprintx],
				Print[fcprintx]
			]
		]
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


FCPrint[1,"FCMain loaded."];
End[]

