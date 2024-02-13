(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCMain															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Global FeynCalc functions and objects.						*)

(* ------------------------------------------------------------------------ *)

$Abbreviations::usage =
"$Abbreviations are a list of string substitution rules used when generating
names for storing intermediate results. It is used by OneLoop and PaVeReduce.
The elements of the list should be of the form \"name\" -> \"abbreviation\".";

$AL::usage =
"$AL is the head for dummy indices which may be introduced by Amputate and
Uncontract. By default it is unset, but may be set to anything.";

$Containers::usage =
"$Containers is a set of heads over which FieldDerivative should distribute, in
the following sense: Let c be a member of $Containers. Then
FieldDerivative[c[f, g, h][x], x, {mu}] -> c[FieldDerivative[f[x], x, {mu}],
FieldDerivative[f[x], x, {mu}], FieldDerivative[f[x], x, {mu}]].";

$DisableMemSet::usage=
"The boolean setting of $DisableMemSet allows to disable memoization that is
activated via MemSet. This can be useful for debugging purposes.";

DOT::usage =
"DOT[a, b, ...] is the FeynCalc function for non-commutative multiplication. By
default it is set to the Mathematica Dot function. This can in principle be
disabled by setting DOT=., but then non-commutative products should to be
entered like DOT[ GA[mu], m + GS[p], GA[mu] ] etc.";

$DistributiveFunctions::usage =
"$DistributiveFunctions is a set of functions over which FieldDerivative should
be distributed.";

$FAPatch::usage =
"$FAPatch switches on and off checking for an unpatched FeynArts installation
on FeynCalc startup.  Default value: True.";

$FeynArtsDirectory::usage =
"$FeynArtsDirectory specifies  the location of FeynArts.";

$FeynCalcDirectory::usage =
"$FeynCalcDirectory specifies the location of FeynCalc.";

FC::usage =
"FC changes the output format to FeynCalcForm. To change to InputForm use FI.";

$FCCheckContext::usage =
"If $FCCheckContext set to True, FeynCalc will try to detect unwanted leakage
of internal objects into the Global or FeynCalc contexts. The default value is
False, however $FCCheckContext will be automatically enabled in the
development version.";

$FCCloudTraditionalForm::usage=
"$FCCloudTraditionalForm determines whether the cell output will be done in
TraditionalForm when FeynCalc is run in Wolfram Cloud. This is done by setting
$Post=TraditionalForm. The default value of $FCCloudTraditionalForm is True.";

$FCTraditionalFormOutput::usage=
"The Boolean setting of $FCTraditionalFormOutput determines which output format
type should be used in the notebook front end when FeynCalc is loaded. If set
to True, FeynCalc will activate the TraditionalForm output. Otherwise, the
StandardForm output (Mathematica's default) will be used.

This setting only changes the output format of the current notebook, i.e. it
is not persistent and will not modify the global options of Mathematica.

If unsure, it is recommended to set $FCTraditionalFormOutput to True, so that
you can benefit from the nice FeynCalc typesetting for various QFT quantities.";


$FCDefaultLightconeVectorN::usage=
"$FCDefaultLightconeVectorN is  a global variable which is set to FCGV[\"n\"].
It denotes the default name for the vector $n$ in the lightcone decomposition
for Lorentz tensors.";

$FCDefaultLightconeVectorNB::usage=
"$FCDefaultLightconeVectorNB is  a global variable which is set to
FCGV[\"nb\"]. It denotes the default name for the vector $\\bar{n}$ in the
lightcone decomposition for Lorentz tensors.";

$FeynCalcStartupMessages::usage=
"$FeynCalcStartupMessages specifies whether some additional information about
FeynCalc should be displayed when the package is loaded. Its value must be set
before loading FeynCalc. The default value is True.";

$FeynCalcDevelopmentVersion::usage =
"The boolean setting of $FeynCalcDevelopmentVersion determines whether the
current version is a development or a stable version.";

$FeynCalcLastCommitDateHash::usage =
"The setting of $FeynCalcLastCommitDateHash provides the date and the hash of
the last commit in the branch from which the current FeynCalc version
originates.";

$ParallelizeFeynCalc::usage =
"$ParallelizeFeynCalc is a global switch that enables FeynCalc to evaluate some
subroutines on using parallel kernels. It should be explicitly activated by
setting $ParallelizeFeynCalc to True. However, before that one should evaluate
LaunchKernels[n] with n being the number of parallel kernels to launch. The
default value is False.";

$FCTensorList::usage =
"$FCTensorList contains a list of all tensor heads present.";

$FCShowIEta::usage =
"The Boolean setting of $FCShowIEta determines whether $i \\eta$ should be
displayed in the typesetting of propagator objects (except for FADs) or not.
This setting affects only the TraditionalForm typesetting and has absolutely
no influence on the internal handling of propagator denominators in FeynCalc.";

FCSetMetricSignature::usage =
"FCSetMetricSignature sets the signature of the Minkowski metric used when
working with Cartesian objects, like CartesianPair, CartesianIndex,
CartesianMomentum etc.

The default choice is $(1,-1,-1,-1)$ which corresponds to
FCSetMetricSignature[{1,-1}].";

FCGetMetricSignature::usage =
"FCGetMetricSignature[] returns the signature of the Minkowski metric used when
working with Cartesian objects, such as CartesianPair, CartesianIndex,
CartesianMomentum etc.

{1,-1} corresponds to $(1,-1,-1,-1)$ and {-1,1} means $(-1, 1, 1, 1)$.";

FCPrint::usage =
"FCPrint[level, x] outputs Print[x] if the value of $VeryVerbose is larger than
level.";

FeynCalc::usage =
"For installation notes visit www.feyncalc.github.io. You can get \
on-line information by ?function, e.g. ?Contract.\n
There are several useful functions for short input, type $FCS for a list of \
short commands. Then type, e.g., ?GA.\n\n
To enable/disable start-up messages, put the line\n
$FeynCalcStartupMessages = True;\n
or\n
$FeynCalcStartupMessages = False;\n
into your \"init.m\" file or into your \"FCConfig.m\" file.";

$FeynCalcVersion::usage =
"$FeynCalcVersion is a string that represents the version of FeynCalc.";

FI::usage =
"FI changes the output format to InputForm. This is useful to see the internal
representation of FeynCalc objects. To change back to FeynCalcForm use FC.";

$KeepLogDivergentScalelessIntegrals::usage =
"$KeepLogDivergentScalelessIntegrals is an experimental global option that
forces FeynCalc not to set 1-loop integrals of type $\\frac{1}/{q^4}$ to zero.
This is useful when one has to explicitly distinguish between IR- and
UV-divergences in dimensional regularization. Notice that OneLoop is not
guaranteed to respect this option.";

$LeviCivitaSign::usage =
"$LeviCivitaSign is a global variable that determines the sign in the result of
a Dirac trace of four gamma matrices and $\\gamma^5$.  $LeviCivitaSign is by
default set to -1 which corresponds to the convention TR[LC[a,b,c,d,5]] =
-4*I*Eps[a,b,c,d]. Setting $LeviCivitaSign=-I will switch to the
FORM-convention.";

$LimitTo4::usage =
"$LimitTo4 is a variable with default setting False. If set to True, the limit
Dimension -> 4 is performed after tensor integral decomposition.

$LimitTo4 is a global variable that determines whether UV-divergent
Passarino-Veltman functions are simplified by taking the limit $D-4 \\to 0$.

A generic IR-finite Passarino-Veltman function $X$ can be written as $X =
\\frac{a}{D-4} + b + \\mathcal{O}(\\varepsilon)$, with $a$ being the prefactor of
the pole and $b$ being the finite part. Therefore, products of such functions
with coefficients that are rational functions of $D$ with
$f(D) = f(4) + (D-4) f'(4)  + \\mathcal{O}(\\varepsilon^2)$ can be simplified to
$f(D) X = f(4) X + a f'(4) + \\mathcal{O}(\\varepsilon)$, whenever such products
appear in the reduction.

This relation is correct only if the Passarino-Veltman functions have no IR
divergences, or if such divergences are regulated without using dimensional
regularization.

For this reason, even when $LimitTo4 is set to True, the simplifications are
applied only to $A$ and $B$ functions. Although $B$ functions can exhibit an
IR divergence, such integrals are zero in dimensional regularization, so that
no mixing of $\\varepsilon$-terms from IR and UV can occur.

The default value of $LimitTo4 is False. Notice that even when the switch is
set to True, it will essentially affect only the Passarino-Veltman reduction
via PaVeReduce.

The modern and more flexible way to simplify amplitudes involving IR-finite
PaVe functions is to use the special routine PaVeLimitTo4.";

$LimitTo4IRUnsafe::usage =
"$LimitTo4IRUnsafe is a global variable that determines whether the
simplifications described in $LimitTo4 are applied also to $C$ and $D$
Passarino-Veltman functions. In this case it is assumed that such  functions
are either IR finite, or the IR divergences are regulated  without using
dimensional regularization (i.e. by introducing  fictitious masses). Otherwise
the results will be inconsistent. If this option is activated, it is the task
of the user to ensure that IR divergences are properly regulated, such that no
mixing of $\\varepsilon$ from IR and UV can occur. The default value is False.";

$LoadAddOns::usage =
"$LoadAddOns[] specifies which addons should be loaded with FeynCalc. E.g.
$LoadAddOns={\"FeynHelpers\"}. The value must be set before loading FeynCalc.
The default value is False.";

$FCMemoryAvailable::usage =
"$FCMemoryAvailable is  a global variable which is set to an integer n, where n
is the available amount of main memory in MB. The default is 1/4 of
$SystemMemory. It should be increased if possible. The higher
$FCMemoryAvailable can be, the more intermediate steps do not have to be
repeated by FeynCalc.";

$MU::usage =
"$MU is the head of dummy indices which may be introduced by Chisholm,
Contract, DiracSimplify, FermionSpinSum and various QCD functions. By default
it is unset, but can be set to anything.";

$Multiplications::usage =
"$Multiplications is a set functions which should be treated as (commutative or
non-commutative) multiplications by FieldDerivative.";

$NonComm::usage =
"$NonComm contains a list of all noncommutative heads present.";

$ScalarProducts::usage =
"$ScalarProducts contains a list of all vector pairs for which a scalar product
value has been defined.";

$OPEWard::usage =
"$OPEWard is experimental.";

$RenameFeynCalcObjects::usage =
"$RenameFeynCalcObjects specifies a list of replacement rules that allow to
rename FeynCalc objects on the fly to avoid conflicts with other package
before FeynCalc is loaded (monkey patching). The value of
$RenameFeynCalcObjects must be specified before loading FeynCalc.";

TBox::usage =
"TBox[a, b, ...] produces a RowBox[{a,b, ...}] where a, b, ... are boxed in
TraditionalForm.

TBox is used internally by FeynCalc to produce the typeset output in
TraditionalForm.";

UseWriteString::usage =
"UseWriteString is an option for FCPrint. If set to True, the expression is
printed via WriteString instead of Print.";

$VeryVerbose::usage =
"$VeryVerbose is a global variable with default setting 0. If set to 1, 2, ...,
less and more intermediate comments and informations are displayed during
calculations.";

$FCAdvice::usage =
"If $FCAdvice is set to True, FeynCalc will display some advices on optimal
Mathematica configuration for using FeynCalc.";

WriteStringOutput::usage =
"UseWriteStringOutput an option for FCPrint. It specifies, to which stream
WriteString should output the expression.";

FeynCalcHowToCite::usage =
"FeynCalcHowToCite[] lists publications that should be cited when mentioning
FeynCalc in scientific works.";

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
"FCDoControl is an option for FCPrint that specifies which variable is used to
control the debugging output of FCPrint. The default value is $VeryVerbose.";

FCSetDiracGammaScheme::usage =
"FCSetDiracGammaScheme[scheme] allows you to specify how Dirac matrices will be
handled in D dimensions. This is mainly relevant to the treatment of the 5th
Dirac matrix $\\gamma^5$, which is not well-defined in dimensional
regularization.

Following schemes are supported:

\"NDR\" - This is the default value. In the naive dimensional regularization
(also known as conventional dimensional regularization or CDR) $\\gamma^5$ is
assumed to anticommute with all Dirac matrices in $D$ dimensions. Hence, every
Dirac trace can be rewritten in such a way, that it contains either just one
or not a single $\\gamma^5$ matrix. The latter traces are obviously
unambiguous. The traces with one $\\gamma^5$ are not well-defined in this
scheme. It usually depends on the physics of the process, whether and how they
can contribute to the final result. Therefore, FeynCalc will keep such traces
unevaluated, leaving it to the user to decide how to treat them. Notice that
traces with an odd number of the usual Dirac matrices and one $\\gamma^5$, that
vanish in $4$ dimensions, will be also put to zero in this scheme.

\"NDR-Discard\" - This is a special version of the NDR scheme. The Dirac
algebra is evaluated in the same way as with \"NDR\", but the remaining traces
with one $\\gamma^5$ are put to zero. This assumes that such traces do not
contribute to the final result, which is obviously true only for specific
calculations.

\"BMHV\" - The Breitenlohner-Maison extension of the t'Hooft-Veltman scheme.
This scheme introduces Dirac and Lorentz tensors living in $4$, $D$ or $D-4$
dimensions, while $\\gamma^5$ is a purely $4$-dimensional object. BMHV is
algebraically consistent but often suffers from nonconservation of currents in
the final results. The conservation must be then enforced by introducing
finite counter-terms. The counter-terms are to be supplied by the user, since
FeynCalc does not do this automatically.

\"Larin\" - Special prescription developed by S. Larin, also known as the
Larin-Gorishny-Atkyampo-DelBurgo scheme. Essentially, it is a shortcut (mostly
used in QCD) for obtaining the same results as in BMHV but without the
necessity to deal with tensors from different dimensions. In this scheme
$\\gamma^5$ is treated as nonanticommuting, while Dirac traces are still
cyclic. If a chain of Dirac matrices contains a single $\\gamma^5$, it is
essentially left untouched. When computing the trace of such a chain, the
cyclicity is used to put $\\gamma^5$ to the very end of the chain. Then, the
trace is evaluated using the Moch-Vermaseren-Vogt formula, Eq.(10) from
[arXiv:1506.04517](https://arxiv.org/pdf/1506.04517.pdf). If a chain contains
more than one $\\gamma^5$, all but one $\\gamma^5$ will be eliminated using the
replacement $\\gamma_\\mu \\gamma^5 \\to i/6 \\varepsilon_{\\mu \\nu \\rho \\sigma}
\\gamma^\\nu \\gamma^\\rho \\gamma^\\sigma$. This way every trace with multiple
occurrences of $\\gamma^5$ can be converted to a linear combination of traces
with a single $\\gamma^5$. Such traces are then treated as described above.
Notice that Levi-Civita tensors generated during the calculation of traces are
$D$-dimensional. For example, a product of two such tensors with all their
indices contracted yields a polynomial in $D$'s. This scheme is often used for
performance reasons and is assumed to give the same results as the BMHV
scheme. However, this is not a rigorous statement and so when in doubt it
might be better to use BMHV instead.";

FCGetDiracGammaScheme::usage =
"FCGetDiracGammaScheme[] shows the currently used scheme for handling Dirac
matrices in $D$ dimensions.";

FCSetPauliSigmaScheme::usage =
"FCSetPauliSigmaScheme[scheme] allows you to specify how Pauli matrices will be
handled in $D-1$ dimensions.

This is mainly related to the commutator of two Pauli matrices, which involves
a Levi-Civita tensor. The latter is not a well-defined quantity in $D-1$
dimensions. Following schemes are supported:

- \"None\" - This is the default value. The anticommutator relation is not
applied to $D-1$ dimensional Pauli matrices.

- \"Naive\" - Naively apply the commutator relation in $D-1$-dimensions, i.e. 
$\{\\sigma^i, \\sigma^j \} = 2 i \\varepsilon^{ijk} \\sigma^k$. The Levi-Civita
tensor lives in $D-1$-dimensions, so that a contraction of two such tensors
which have all indices in common yields $(D-3) (D-2) (D-1)$.";

FCGetPauliSigmaScheme::usage =
"FCGetPauliSigmaScheme[] shows the currently used scheme for handling Pauli
matrices in $D-1$ dimensions. For more details see the documentation of
FCSetPauliSigmaScheme.";

FCEnableTraditionalFormOutput::usage =
"FCEnableTraditionalFormOutput[] sets the output format of the current FrontEnd
to TraditionalForm. The setting is not persistent, such that it does not
influence any subsequent Mathematica FrontEnd sessions.";

FCDisableTraditionalFormOutput::usage =
"FCDisableTraditionalFormOutput[] sets the output format of the current
FrontEnd to StandardForm. The setting is not persistent, such that it does not
influence any subsequent Mathematica FrontEnd sessions.";

FeynCalc::failmsg =
"Error! FeynCalc has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)
Begin["`Package`"];

FCStyle;

End[]

Begin["`Private`"];

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

$ParallelizeFeynCalc				= False;
$Containers							= {};
$DisableMemSet 						= False;
$DistributiveFunctions				= {Conjugate, Transpose};
$FCShowIEta							= True;
$KeepLogDivergentScalelessIntegrals = False;
$LeviCivitaSign						= -1;
$LimitTo4							= False;
$LimitTo4IRUnsafe					= False;
$FCMemoryAvailable					= Floor[$SystemMemory/10^6/4];
$Multiplications					= {Times, DOT};
$OPEWard							= False;


$ParallelizeFeynCalc/:
	Set[$ParallelizeFeynCalc, True]:=
		(
		With[{str = FileNameJoin[{$FeynCalcDirectory, "fc.m"}]},
			ParallelEvaluate[Get[str],Kernels[]]
		];
		OwnValues[$ParallelizeFeynCalc] = {HoldPattern[$ParallelizeFeynCalc] :> True};
		True
		)

(*	Mathematica versions 8 and 9 do not have the $SystemMemory variable,
	so for them we set the available memory for memoization to 4 GiB*)
If[	!MatchQ[$FCMemoryAvailable,_Integer?NonNegative],
	$FCMemoryAvailable=4096
];

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

$FCDefaultLightconeVectorN = FeynCalc`FCGV["n"];
$FCDefaultLightconeVectorNB = FeynCalc`FCGV["nb"];

FeynCalcHowToCite[]:=
	(
	Print [Style[" \[Bullet] V. Shtabovenko, R. Mertig and F. Orellana, arXiv:2312.14089.","Text"]];
	Print [Style[" \[Bullet] V. Shtabovenko, R. Mertig and F. Orellana, Comput.Phys.Commun. 256 (2020) 107478, arXiv:2001.04407.","Text"]];
	Print [Style[" \[Bullet] V. Shtabovenko, R. Mertig and F. Orellana, Comput.Phys.Commun. 207 (2016) 432-444, arXiv:1601.01167.","Text"]];
	Print [Style[" \[Bullet] R. Mertig, M. B\[ODoubleDot]hm, and A. Denner, Comput. Phys. Commun. 64 (1991) 345-359.","Text"]];
	);

SetAttributes[FCPrint, HoldRest];

Options[FCPrint] = {
		FCDoControl :> $VeryVerbose,
		UseWriteString -> False,
		WriteStringOutput ->"stdout"
};


FeynCalc`Package`MetricT = 1;
FeynCalc`Package`MetricS = -1;
FeynCalc`Package`PauliSigmaScheme = "None";
FeynCalc`Package`DiracGammaScheme = "NDR";

FCEnableTraditionalFormOutput[]:=
	(CurrentValue[$FrontEndSession, {CommonDefaultFormatTypes, "Output"}] = TraditionalForm;);

FCDisableTraditionalFormOutput[]:=
	(CurrentValue[$FrontEndSession, {CommonDefaultFormatTypes, "Output"}] = StandardForm; );


FCSetDiracGammaScheme[s_String]:=
	If[	MatchQ[s,"NDR"|"NDR-Discard"|"BMHV"|"Larin"],
		FeynCalc`Package`DiracGammaScheme = s,
		Message[FeynCalc::failmsg, "Unknown scheme for Dirac matrices in D dimensions."];
		Abort[]
	];

FCGetDiracGammaScheme[]:=
	FeynCalc`Package`DiracGammaScheme;

FCSetPauliSigmaScheme[s_String]:=
	If[	MatchQ[s,"None"|"Naive"],
		FeynCalc`Package`PauliSigmaScheme = s,
		Message[FeynCalc::failmsg, "Unknown scheme for Pauli matrices in D-1 dimensions."];
		Abort[]
	];

FCGetPauliSigmaScheme[]:=
	FeynCalc`Package`PauliSigmaScheme;

FCSetMetricSignature[{t_Integer,s_Integer}]:=
	(
	If[ {s^2,t^2}=!={1,1},
		Message[FeynCalc::failmsg, "The square of each diagonal element of the metric tensor must be unity."];
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

FCStyle[text_, style_]:=
	If[	TrueQ[!$Notebooks],
		text,
		Style[text,style]
	];

FCPrint[1,"FCMain loaded."];
End[]

