(* ::Package:: *)

(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SharedTools														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Small helper tools extensively used in FeynCalc			    *)

(* ------------------------------------------------------------------------ *)

$OPEWard::usage =
"$OPEWard is experimental.";

Complement1::usage=
"Complement1[l1, l2] where l1 and l2 are lists returns a list of elements from
l1 not inl2. Multiple occurrences of an element in l1 are kept and multiple
occurrences of an element in l2 are dropped if present in l1.";

FunctionLimits::usage =
"FunctionLimits is an option of ILimit, specifying which functions should be
checked for finiteness.";

ILimit::usage =
"ILimit[exp, a -> b] checks functions specified by the option FunctionLimits
and takes the limit a->b of these functions only if it is finite.  For the
rest of the expression exp, the limit is taken.";

MLimit::usage=
"MLimit[expr, lims] takes multiple limits of expr using the limits lims.";

NumericQ1::usage=
"NumericQ1[x, {a, b, ..}] is like NumericQ, but assumes that {a,b,..} are
numeric quantities.";

PowerFactor::usage=
"PowerFactor[exp] replaces x^a y^a with (x y)^a.";

SelectSplit::usage=
"SelectSplit[l, p] constructs list of mutually exclusive subsets from l in
which every element li satisfies a criterion pj[li] with pj from p and appends
the subset of remaining unmatched elements.";

XYT::usage=
"XYT[exp, x, y] transforms  (x y)^m away.";

OPE::usage =
"OPE is a convenience variable to separate OPE insertions.

OPE is also an option of several input functions like GluonPropagator.";



Combine::usage=
"Combine[expr] puts terms in a sum over a common denominator and cancels
factors in the result. Combine is similar to Together, but accepts the option
Expanding and works usually better than Together for polynomials involving
rationals with sums in the denominator.";



SO::usage =
"SO[q] is a four-dimensional scalar product of OPEDelta with q. It is
transformed into Pair[Momentum[q], Momentum[OPEDelta] by FCI.";

SOD::usage =
"SOD[q] is a $D$-dimensional scalar product of OPEDelta with q. It is
transformed into Pair[Momentum[q,D], Momentum[OPEDelta,D] by FeynCalcInternal.";


ZeroMomentumInsertion::usage=
"ZeroMomentumInsertion is an option of FeynRule, Twist2GluonOperator and
Twist2QuarkOperator.";

Divideout::usage =
"Divideout is an option for OPEInt and OPEIntegrate. The setting is divided out
at the end.";

EpsilonOrder::usage =
"EpsilonOrder is an option of OPEIntegrateDelta and other functions. The
setting determines the order n (Epsilon^n) which should be kept.";


Factorout::usage =
"Factorout is an option for OPEInt and OPEIntegrate.";

NegativeInteger::usage =
"NegativeInteger is a data type. E.g. DataType[n, NegativeInteger] can be set
to True.";

PositiveInteger::usage =
"PositiveInteger is a data type. E.g. DataType[OPEm, PositiveInteger] gives
True.";

GrassmannParity::usage =
"GrassmannParity is a data type.

E.g. DataType[F, GrassmannParity] = 1 declares F to be of bosonic type and
DataType[F, GrassmannParity] = -1 of fermionic one.";

Power2::usage=
"Power2[x, y] represents x^y.  Sometimes Power2 is more useful than the
Mathematica Power. Power2[-a,b] simplifies to (-1)^b Power2[a,b] (if no
Epsilon is in b ...).";

PowerSimplify::usage=
"PowerSimplify[exp] simplifies (-x)^a to (-1)^a x^a and (y-x)^n to (-1)^n
(x-y)^n thus assuming that the exponent is an integer (even if it is
symbolic).

Furthermore, (-1)^(a+n) and I^(a+n) are expanded and (I)^(2 m) -> (-1)^m and
(-1)^(n_Integer?EvenQ m) -> 1 and (-1)^(n_Integer?OddQ m) -> (-1)^m for n even
and odd respectively and (-1)^(-n) -> (-1)^n and Exp[I m Pi] -> (-1)^m.";

PositiveNumber::usage =
"PositiveNumber is a data type. E.g. DataType[Epsilon, PositiveNumber] = True
(by default).";

WriteOut::usage =
"WriteOut is an option for OneLoop. If set to True, the result of OneLoop will
be written to a file called \"name.res\", where name is the first argument of
OneLoop.";

QuarkMass::usage=
"QuarkMass is an option of Amplitude and CounterTerm.";

DeltaFunction::usage =
"DeltaFunction[x] is the Dirac delta-function $\\delta (x)$.

Mathematica also provides a built-in function DiracDelta with comparable
properties.";

DeltaFunctionDoublePrime::usage =
"DeltaFunctionDoublePrime[1 - x] is the second derivative of the Dirac
delta-function $\\delta (x)$.";

DeltaFunctionPrime::usage =
"DeltaFunctionPrime[1 - x] is the derivative of the Dirac delta-function
$\\delta (x)$.";

FeynAmp::usage =
"FeynAmp[q, amp] is the head of a Feynman amplitude, where amp denotes the
analytical expression for the amplitude and q is the integration variable.
FeynAmp[q1, q2, amp] denotes a two-loop amplitude. FeynAmp has no functional
properties and serves just as a head. There are however special typesetting
rules attached.";

FeynAmpList::usage =
"FeynAmpList[info][FeynAmp[...], FeynAmp[...], ...] is a head of a list of
Feynman amplitudes. FeynAmpList has no functional properties and serves just
as a head.";

Integratedx::usage =
"Integratedx[x, low, up] is a variable representing the integration operator
Integrate[#, {x,low,up}]&.";

Loop::usage=
"Loop is an option for functions related to FeynArts integration, indicating
the number of (virtual) loops.";

MLimit::usage=
"MLimit[expr, lims] takes multiple limits of expr using the limits lims.";

PlusDistribution::usage =
"PlusDistribution[1/(1 - x)] denotes a distribution (in the sense of the \"+\"
prescription).";


Abbreviation::usage =
"Abbreviation is a function used by OneLoop and PaVeReduce for generating
smaller files when saving results to the hard disk. The convention is that a
definition like GP = GluonPropagator should be accompanied by the definition
Abbreviation[GluonPropagator] = HoldForm[GP].";


$Abbreviations::usage =
"$Abbreviations are a list of string substitution rules used when generating
names for storing intermediate results. It is used by OneLoop and PaVeReduce.
The elements of the list should be of the form \"name\" -> \"abbreviation\".";

CounterT::usage =
"CounterT is a factor used by GluonPropagator and QuarkPropagator when
CounterTerms is set to All.";

SmallDelta::usage =
"SmallDelta denotes some small positive number.";

SmallEpsilon::usage =
"SmallEpsilon denotes some small positive number.";

DummyIndex::usage =
"DummyIndex is an option of CovariantD specifying an index to use as dummy
summation index. If set to Automatic, unique indices are generated.";

Begin["`Package`"];
End[]

(* ------------------------------------------------------------------------ *)

Begin["`SharedTools`Private`"];


Options[Combine] = {
	Expanding -> False
};

Options[SelectSplit] = {
	Heads -> None
};

Options[ILimit] = {
	FunctionLimits -> {Log -> Log}
};

Options[MLimit] = {
	Limit -> Limit
};

Options[PowerSimplify] = {
	Assumptions	-> True,
	PowerExpand	-> True
};

Options[Power2] = {
	Assumptions	-> True
};

SetOptions[FCE, FinalSubstitutions -> {Power2 :> Power}];

SetOptions[FCI, FinalSubstitutions -> {SO[a_] :> Pair[Momentum[a], Momentum[OPEDelta]],
SOD[a_] :> Pair[Momentum[a,D], Momentum[OPEDelta,D]]}];


DeclareNonCommutative[OPESum];

DataType[Epsilon, PositiveNumber] = True;

Unprotect[Greater];
Greater[Re[Epsilon],-4] = True;
Greater[Re[Epsilon],-3] = True;
Greater[Re[Epsilon],-2] = True;
Greater[Re[Epsilon],-1] = True;
Greater[Re[Epsilon],0] = True;
Protect[Greater];

$Abbreviations /:
	Set[$Abbreviations , val_] :=
		(
		If[	$ParallelizeFeynCalc && ($KernelID===0),
			With[{xxx=val},	ParallelEvaluate[OwnValues[$Abbreviations] = {HoldPattern[$Abbreviations] :> xxx};,DistributedContexts -> None]];
		];

		With[{xxx=val},	OwnValues[$Abbreviations] = {HoldPattern[$Abbreviations] :> xxx}];

		val
		);

$OPEWard /:
	Set[$OPEWard , val_] :=
		(
		If[	$ParallelizeFeynCalc && ($KernelID===0),
			With[{xxx=val},	ParallelEvaluate[OwnValues[$OPEWard] = {HoldPattern[$OPEWard] :> xxx};,DistributedContexts -> None]];
		];

		With[{xxx=val},	OwnValues[$OPEWard] = {HoldPattern[$OPEWard] :> xxx}];

		val
		);


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


$OPEWard							= False;




DeltaFunction[_?((NumericQ[#]===True&&(Positive[#]===True||Negative[#]===True))&)] :=
	0;

DeltaFunction[0] :=
	1;


DataType[] = Join[DataType[],{NegativeInteger,PositiveInteger,PositiveNumber,GrassmannParity}];


SO /:
	MakeBoxes[SO[x_],TraditionalForm]:=
		If[ Head[x] =!= Plus,
			TBox["\[CapitalDelta]",  "\[CenterDot]", x],
			TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
		];

SmallDelta /:
	MakeBoxes[SmallDelta, TraditionalForm]:=
		"\[Delta]";

SmallEpsilon /:
	MakeBoxes[SmallEpsilon, TraditionalForm]:=
		"\[Epsilon]";


SOD /:
	MakeBoxes[SOD[x_],TraditionalForm]:=
		If[ Head[x] =!= Plus,
			TBox["\[CapitalDelta]",  "\[CenterDot]",x],
			TBox["\[CapitalDelta]", "\[CenterDot]", "(",x,")"]
		];

PlusDistribution /:
	MakeBoxes[PlusDistribution[ a_ ], TraditionalForm]:=
		SubscriptBox[RowBox[{"(", MakeBoxes[a,
		TraditionalForm],")"}],"+"];

FeynAmp /:
	MakeBoxes[FeynAmp[q__Symbol, amp_], TraditionalForm ]:=
		RowBox[Join[Map[RowBox[{"\[Integral]",
		RowBox[{SuperscriptBox["\[DifferentialD]", "D"],
		TBox[#]}]}] &, {q}], {"(", TBox[amp], ")"}]];

FeynAmp /:
	MakeBoxes[FeynAmp[_[__], q__Symbol, amp_], TraditionalForm]:=
		ToBoxes[FeynAmp[q,amp], TraditionalForm];


Integratedx /:
	MakeBoxes[Integratedx[x_, low_, up_], TraditionalForm]:=
		RowBox[{SubsuperscriptBox["\[Integral]", TBox[low],
		TBox[up]], "\[DifferentialD]",
		MakeBoxes[TraditionalForm[x]], "\[VeryThinSpace]" }];


DeltaFunction /:
	MakeBoxes[ DeltaFunction[y_], TraditionalForm]:=
		RowBox[{"\[Delta]", "(", TBox[y], ")"}];

DeltaFunctionDoublePrime /:
	MakeBoxes[ DeltaFunctionDoublePrime[y_], TraditionalForm]:=
		RowBox[{SuperscriptBox["\[Delta]","\[DoublePrime]"],
		"(", TBox[y], ")"}];

DeltaFunctionPrime /:
	MakeBoxes[ DeltaFunctionPrime[y_], TraditionalForm]:=
		RowBox[{SuperscriptBox["\[Delta]","\[Prime]"],
		"(", TBox[y], ")"}];


Momentum /:
	MakeBoxes[ Momentum[ OPEDelta, _:4 ], TraditionalForm]:=
		TBox[OPEDelta];

Momentum /:
	MakeBoxes[ Momentum[p:Except[_Subscript | _Superscript | _Plus],dim_:4], TraditionalForm]:=
		FeynCalc`SharedObjectsTypesetting`Private`momentumRep[p,dim]/; p=!=OPEDelta;


OPE /:
	MakeBoxes[OPE, TraditionalForm]:=
		"\[CapitalOmega]";

Format[Power2[a_, b_ /; b, OptionsPattern[]]] :=
	a^b;

Power2 /:
	MakeBoxes[Power2[a_, b_, OptionsPattern[]] , TraditionalForm] :=
		ToBoxes[a^b, TraditionalForm];




OPE /:
	OPE^_Integer?Positive := 0;



Complement1[a_List, b_List] :=
	Block[{len, len1, i, alt, p, drp, ii, go}, p = 0; i = 0; drp = {};
		len = Length[a]; len1 = Length[b]; alt = b;
		While[i < len && p < len1, ++i;
			If[ii = 0; go = True;
				While[ii < Length[alt] && go,
					++ii;
					If[MatchQ[a[[i]],
						alt[[ii]]],
						alt = Drop[alt, {ii}];
						go = False
					]
				];
				Not[go],
				p = p + 1;
				drp = Append[drp, i]
			]
		];
		Part[a, Complement[Range[len], drp]]
	];



ILimit[exp_, lim_Rule, OptionsPattern[]] :=
	Block[{limruls, m, ff, fff, out,res},
		limruls = MapAt[(((If[FreeQ[ff[##], lim[[1]]] ||
		!FreeQ[out = Limit[Limit[ff[##] /. SmallVariable[_?((!MatchQ[#, lim[[1]]])&)] -> 0,
		SmallVariable[lim[[1]]] -> lim[[2]]], lim], DirectedInfinity[___] | Indeterminate | _Limit],
		fff[##], out]&))&) /. {ff -> #[[2]], fff -> #[[1]]}, #, 2]& /@ (OptionValue[FunctionLimits]);
		FCPrint[1, "limruls: ", limruls];
		res = exp /. limruls;
		FCPrint[1, "res: ", res];
		Limit[Limit[res, SmallVariable[lim[[1]]] -> lim[[2]]], lim]
	];

MLimit[x_, l_List, OptionsPattern[]] :=
	Fold[OptionValue[Limit][#1, Flatten[{##2}][[1]]]&, x, l];

NumericQ1[x_, nums_List] :=
	Block[{r, syms, res, ii=0, tag},
		SetAttributes[tag, {NumericFunction,NHoldAll}];
		Off[$MaxExtraPrecision::"meprec"];
		syms = (++ii; tag[ii])& /@ nums;
		Off[$MaxExtraPrecision::"meprec"];
		res = NumericQ[x /. ((Rule @@ #) & /@ Transpose[{nums, syms}])];
		On[$MaxExtraPrecision::"meprec"];
		res
	];

PowerFactor[exp_Plus] :=
	PowerFactor /@ exp;

PowerFactor[exp_] :=
	If[Head[exp] =!= Times,
		exp //. {x_^a_ y_^a_ :> (x y)^a},
		SelectFree[exp, Power] (SelectNotFree[exp,
		Power] //. {x_^a_ y_^a_ :> (x y)^a})
	];

SelectSplit[ex_, p_List, opts___Rule] :=
	Block[{ii, jj, aa, res, exp = List @@ ex, h = Head[ex],
		hh = Heads /. Flatten[{opts}] /. Options[SelectSplit]},
		ii = 0;
		res = (++ii; Select[#, (aa = #;	And @@
		((#[aa]=!=True)& /@ Drop[p, {ii}])) &])& /@ (Select[exp, #]& /@ p);
		If[hh =!= None && hh =!= False && hh =!= {},
			If[Length[hh] < Length[#] - 1,
				hh = Join[hh, Table[hh[[-1]], {Length[#] - 1 - Length[hh]}]]
			];
		Append[Table[hh[[jj]][#[[jj]]], {jj, Length[#] - 1}],
		If[Length[hh] === Length[#],
			hh[[-1]][#[[-1]]], #[[-1]]
		]], #
		] &[(h @@ #) & /@ Append[res, Complement[exp, Join @@ res]]]
	];


(* integral transformation only valid if nonsingular in x, y = 0,1 *)
XYT[exp_, x_, y_] :=
	Block[{z, t, u},
		t = 1/x Factor2[PowerSimplify[Factor2[exp] /. y -> (z/x)]];
		Factor2[PowerSimplify[(1-z) (t /. x :> (1-z) u + z)]/.{u:>y,z:>x}]
	];

LeftNablaD[x__] :=
	LeftNablaD @@ (CartesianIndex /@ {x}) /;FreeQ2[{x},
	{LorentzIndex, ExplicitLorentzIndex, CartesianIndex, Momentum, CartesianMomentum, OPEDelta, RowBox,
	Pattern, Blank}] && (Union[{x}]=!={1});

LeftPartialD[x__] :=
	LeftPartialD @@ (LorentzIndex /@ {x}) /;FreeQ2[{x},
	{LorentzIndex, ExplicitLorentzIndex, CartesianIndex, Momentum, CartesianMomentum, OPEDelta, RowBox,
	Pattern, Blank}] && (Union[{x}]=!={1});

LeftPartialD[c:OPEDelta..] :=
	LeftPartialD @@ (Momentum /@ {c});

LeftRightNablaD[xx__] :=
	LeftRightNablaD@@ (CartesianIndex /@ {xx}) /;
	FreeQ2[{xx}, {LorentzIndex, ExplicitLorentzIndex, CartesianIndex,
		Momentum, CartesianMomentum, OPEDelta, RowBox, Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightNablaD2[xx__] :=
	LeftRightNablaD2@@ (CartesianIndex /@ {xx}) /;
	FreeQ2[{xx}, {LorentzIndex, ExplicitLorentzIndex, CartesianIndex,
		Momentum, CartesianMomentum, OPEDelta, RowBox, Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD[xx__] :=
	LeftRightPartialD@@ (LorentzIndex /@ {xx}) /;
	FreeQ2[{xx}, {LorentzIndex, ExplicitLorentzIndex, CartesianIndex,
		Momentum, CartesianMomentum, OPEDelta, RowBox, Pattern, Blank}] && (Union[{xx}]=!={1});


LeftRightPartialD[c:OPEDelta..] :=
	LeftRightPartialD @@ (Momentum /@ {c});


LeftRightPartialD2[xx__] :=
	LeftRightPartialD2@@ (LorentzIndex /@ {xx}) /;
	FreeQ2[{xx}, {LorentzIndex, ExplicitLorentzIndex, CartesianIndex,
		Momentum, CartesianMomentum, OPEDelta, RowBox, Pattern, Blank}] && (Union[{xx}]=!={1});

LeftRightPartialD2[c:OPEDelta..] :=
	LeftRightPartialD2 @@ (Momentum /@ {c});

LeftRightPartialD2[Momentum[OPEDelta]^n_Integer?Positive] :=
	DOT @@ Map[LeftRightPartialD2, Table[Momentum[OPEDelta],{n}]];

FCPartialD[c:OPEDelta..] :=
	FCPartialD @@ (Momentum /@ {c});

RightNablaD[x__] :=
	RightNablaD @@ (CartesianIndex /@ {x}) /;FreeQ2[{x},
	{LorentzIndex, ExplicitLorentzIndex, CartesianIndex, Momentum, CartesianMomentum, OPEDelta, RowBox,
	Pattern, Blank}] && (Union[{x}]=!={1});

RightPartialD[x__] :=
	RightPartialD @@ (LorentzIndex /@ {x}) /;FreeQ2[{x},
	{LorentzIndex, ExplicitLorentzIndex, CartesianIndex, Momentum, CartesianMomentum, OPEDelta, RowBox,
	Pattern, Blank}] && (Union[{x}]=!={1});


RightPartialD[c:OPEDelta..] :=
	RightPartialD @@ (Momentum /@ {c});

FeynCalc`SharedObjects`Private`lori[OPEDelta] :=
	Momentum[OPEDelta];

FieldStrength[mu___, OPEDelta, nu___] :=
	FieldStrength[mu, Momentum[OPEDelta], nu];

FieldStrength[mu_, Momentum[OPEDelta], a_, {aA_, b_, c_}, g_ /; Head[g] =!= Rule, OptionsPattern[]] :=
	(QuantumField[FCPartialD[LorentzIndex[mu]], aA, Momentum[OPEDelta], SUNIndex[a]] -
	QuantumField[FCPartialD[Momentum[OPEDelta]],aA, LorentzIndex[mu], SUNIndex[a]] +
	g SUNF[a, b, c] DOT[QuantumField[aA, LorentzIndex[mu], SUNIndex[b]],
	QuantumField[aA, Momentum[OPEDelta], SUNIndex[c]]]) /;
	FreeQ2[{mu}, {Momentum, OPEDelta}] && OptionValue[Explicit];


FieldStrength[Momentum[OPEDelta], nu_, a_, {aA_, b_, c_}, g_ /; Head[g] =!= Rule, OptionsPattern[]] :=
	(QuantumField[FCPartialD[Momentum[OPEDelta]], aA, LorentzIndex[nu], SUNIndex[a]] -
	QuantumField[FCPartialD[LorentzIndex[nu]],aA, Momentum[OPEDelta], SUNIndex[a]] +
	g SUNF[a, b, c] DOT[QuantumField[aA, Momentum[OPEDelta], SUNIndex[b]],
	QuantumField[aA, LorentzIndex[nu], SUNIndex[c]]]) /;
	FreeQ2[{nu}, {Momentum, OPEDelta}] && OptionValue[Explicit];



FeynCalc`FeynCalcExternal`Private`pairback[Momentum[OPEDelta], Momentum[b_]] :=
	SO[b];
FeynCalc`FeynCalcExternal`Private`pairback[Momentum[b_], Momentum[OPEDelta] ] :=
	SO[b];
FeynCalc`FeynCalcExternal`Private`pairback[Momentum[OPEDelta, D], Momentum[b_, D]] :=
	SOD[b];
FeynCalc`FeynCalcExternal`Private`pairback[Momentum[OPEDelta, d_], Momentum[b_, d_]] /;d=!=D :=
	Pair[Momentum[OPEDelta,d], Momentum[b,d]];
FeynCalc`FeynCalcExternal`Private`pairback[Momentum[b_, D], Momentum[OPEDelta, D]] :=
	SOD[b];
FeynCalc`FeynCalcExternal`Private`pairback[Momentum[b_, d_], Momentum[OPEDelta, d_]] /;d=!=D :=
	Pair[Momentum[OPEDelta,d], Momentum[b,d]];



Power2 /:
	Power2[-1,OPEm, OptionsPattern[]]^2 :=
		1;
Power2[n_Integer?Positive, em_, OptionsPattern[]] :=
	n^em;
Power2[n_, em_Integer, OptionsPattern[]] :=
	n^em;
Power2[-1,OPEm-2, opts:OptionsPattern[]] =
	Power2[-1,OPEm, opts];
Power2[-a_,b_/;FreeQ2[b, {Epsilon,Epsilon2}], opts:OptionsPattern[]] :=
	PowerSimplify[(-1)^b,Assumptions->OptionValue[Assumptions]] Power2[a,b,opts];

PowerSimplify[x_, OptionsPattern[]] :=
	Block[{nx, qcdsub = False, power3, assumpts,usePowerExpand},
		assumpts = OptionValue[Assumptions];
		usePowerExpand = OptionValue[PowerExpand];
		If[!FreeQ[x, ScaleMu],
			qcdsub = True;
			nx = x /. pow_[any_ /ScaleMu^2,exp_]:> power3[pow][any/ScaleMu^2,exp],
			nx = x
		];
		nx = nx /.
			{	(a_/;Head[a]===Plus || Head[a] === Times)^(w_)/;usePowerExpand :>
					(PowerExpand[Factor2[one*a]^w, Assumptions->assumpts] /. one -> 1),
				Power2[(a_/;Head[a]===Plus || Head[a] === Times),(w_)]/;usePowerExpand :>
					(PowerExpand[Factor2[one*a]^w, Assumptions->assumpts] /.
						(ab_Plus)^v_ :> Power2[ab, v] /. one -> 1)/.(-1)^vv_ :> Power2[-1,vv]	} /.
			{	(-1)^(a_Plus) :> Expand[(-1)^a]	}/.
			{(n_Integer?Negative)^m_ :> (-1)^m (-n)^m}/.
			{	((-1)^OPEm (1+(-1)^OPEm)) :> (1+(-1)^OPEm),
				((1-(-1)^OPEm)(1+(-1)^OPEm)) :> 0,
				((1+(-1)^OPEm)(1+(-1)^OPEm)) :> (2(1+(-1)^OPEm)),
				(-1)^OPEm (1-(-1)^OPEm) :> (-1+(-1)^OPEm),
				bbb_^(c_/;!FreeQ[c,Plus]) :> bbb^Expand[c]	}//.
			{	(-1)^(_Integer?EvenQ _) :> 1,
				(-1)^(_Integer?OddQ m_) :> (-1)^m,
				(-1)^(_Integer?EvenQ _. + i_) :> (-1)^i,
				(-1)^(n_Integer?OddQ m_. + i_) :> (-1)^(m+i) /; n=!=(-1),
				(-1)^(-n_) :> (-1)^n,
				I^(2 m_+i_.) :> (I)^i (-1)^m,
				(I/2)^(m_) I^m_ :> (-1)^m/2^m,
				I^(a_Plus) :> Expand[I^a],
				Exp[I Pi OPEi] :> (-1)^OPEi,
				Exp[I Pi OPEj] :> (-1)^OPEj,
				Exp[I Pi OPEm] :> (-1)^OPEm,
				HoldPattern[E^(em_ + Complex[0,n_] Pi)]  :> (-1)^n Exp[em],
				Power2[I, 2 m_ + i_.] :> I^i (-1)^m,
				Power2[I,(a_Plus)] :> Expand[I^a],
				Power2[(-1),(a_Plus)] :> Expand[(-1)^a]	};
			If[	qcdsub === True,
				nx = nx /. power3[poww_] :> poww
			];
		nx
	];







Combine[x_, OptionsPattern[]] :=
	Block[{combinet1, combinet2, expanding, num, le},
		expanding = OptionValue[Expanding];
		combinet2 = Together[ x /. Plus ->
		(If[FreeQ[{##}, _^_?Negative] && FreeQ[{##}, Rational],
				combinet1[##],
				Plus[##]
		]&)] /. combinet1 -> Plus;
		Which[	expanding === All,
				combinet2 = ExpandNumerator[combinet2 // ExpandDenominator] ,
				expanding === True,
				num = Numerator[combinet2];
				If[Head[num] =!= Plus,
					combinet2 = Expand[num]/Denominator[combinet2],
					If[LeafCount[num]<1000,
							combinet2 = Expand[num]/Denominator[combinet2],
							le = Length[num];
							combinet2 = Sum[FCPrint[2,"expanding ", i," out of ",le];
							Expand[num[[i]]],{i,Length[num]}]/ Denominator[combinet2]
					]
				],
				True,
				combinet2
		];
		combinet2
	];


FCPrint[1, "SharedTools loaded."];
End[]

