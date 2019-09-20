(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: LegacyObjects													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  Legacy symbols scheduled for removal somewhere in the future	*)

(* ------------------------------------------------------------------------ *)

ChiralityProjector::usage =
"ChiralityProjector[+1] denotes DiracGamma[6] (=1/2(1 + DiracMatrix[5])). \
ChiralityProjector[-1] denotes DiracGamma[7] (=1/2(1 - DiracMatrix[5])). \
ChiralityProjector is a legacy symbol that might be removed in a future FeynCalc \
version. For the user input shortcuts use GA[6] and GA[7] instead.";

DiracSpinor::usage =
"DiracSpinor is the same as Spinor. DiracSpinor is a legacy symbol that might be \
removed in a future FeynCalc version..";

DiracMatrix::usage =
"DiracMatrix[m] denotes a Dirac gamma matrix with Lorentz index m. \
DiracMatrix[m1, m2, ..] is a product of gamma matrices with Lorentz \
indices m1, m2, etc. DiracMatrix[5] is g^5. \
DiracMatrix is a legacy symbol that might be removed in a future FeynCalc \
version. For the user input shortcuts use GA or GAD instead.";

DiracSlash::usage =
"DiracSlash[p] is the contraction FourVector[p, mu]*DiracSlash[mu]. \
A product of those can be entered in the form DiracSlash[p1, p2, ..]. \
DiracSlash is a legacy symbol that might be removed in a future FeynCalc \
version. For the user input shortcuts use GS or GSD instead.";

FourVector::usage =
"FourVector[p, mu] is the four dimensional vector p with Lorentz index m. \
A vector with space-time Dimension d is obtained by supplying the option \
Dimension->d. FourVector is a legacy symbol that might be removed in a future FeynCalc \
version. For the user input shortcuts use FV or FVD instead. "

IFPD::usage = "IFPD[p, m] denotes (p^2 - m^2)."

LeviCivita::usage =
"LeviCivita[mu, nu, ro, si] is an input  function for the \
totally antisymmetric Levi-Civita tensor. \
It evaluates automatically \
to the internal representation Eps[ LorentzIndex[mu],  LorentzIndex[nu], \
LorentzIndex[ro], LorentzIndex[si] ] \
(or with a second argument in LorentzIndex for the Dimension, \
if the option Dimension of LeviCivita is changed).  \
LeviCivita[mu, nu ...][ p, ...] evaluates to \
Eps[LorentzIndex[mu], LorentzIndex[nu], ..., Momentum[p], ...]. \
LeviCivita is a legacy symbol that might be removed in a future FeynCalc \
version. For the user input shortcuts use LC or LCD instead.";

MetricTensor::usage =
"MetricTensor[mu, nu] is the metric tensor in 4 dimensions. \
The metric tensor in d dimensions is obtained by supplying the
option Dimension->d.\
MetricTensor is a legacy symbol that might be removed in a future FeynCalc \
version. For the user input shortcuts use MT or MTD instead.";

MetricTensor::usage =
"MetricTensor[mu, nu] is the metric tensor in 4 dimensions. \
The metric tensor in d dimensions is obtained by supplying the
option Dimension->d.\
MetricTensor is a legacy symbol that might be removed in a future FeynCalc \
version. For the user input shortcuts use MT or MTD instead.";

$BreitMaison::usage =
"$BreitMaison is a legacy switch for the Breitenlohner-Maison-t'Hooft-Veltman \
scheme.  The modern way is to use FCSetDiracGammaScheme to specify \
a scheme for handling Dirac matrices in dimensional regularization and \
FCGetDiracGammaScheme to check the current setting.";

$Larin::usage =
"$Larin is a legacy switch for the Larin-Gorishny-Atkyampo-DelBurgo \
scheme. The modern way is to use FCSetDiracGammaScheme to specify \
a scheme for handling Dirac matrices in dimensional regularization and \
FCGetDiracGammaScheme to check the current setting.";

DiracMatrix::noint =
"DiracMatrix[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
arguments can be 0, 1, 2, 3, 5, 6 and 7. Evaluation aborted!";

DiracSlash::noint =
"DiracSlash[`1`] is forbidden in FeynCalc. For consistency reasons, the only allowed integer \
argument can be 0. Evaluation aborted!";


(* ------------------------------------------------------------------------ *)
Begin["`Package`"]



End[]

Begin["`LegacyObjects`Private`"]

DeclareNonCommutative[ChiralityProjector];
DeclareNonCommutative[DiracSpinor];
DeclareNonCommutative[DiracMatrix];
DeclareNonCommutative[DiracSlash];

$BreitMaison = False;
$Larin = False;

Options[ChiralityProjector] = {FCI -> True};
Options[DiracMatrix] = {Dimension -> 4, FCI -> True};
Options[DiracSlash] = {Dimension -> 4, FCI -> True};
Options[FourVector]  = {Dimension -> 4, FCI -> True};
Options[LeviCivita] = {Dimension -> 4, FCI->True};
Options[MetricTensor] = {Dimension -> 4, FCI -> True};

FeynCalc`Package`TrFeynCalcObjects = Join[FeynCalc`Package`TrFeynCalcObjects, DiracMatrix | DiracSlash];


ChiralityProjector[1, OptionsPattern[]] :=
	DiracGamma[6]/; OptionValue[FCI];

ChiralityProjector[-1, OptionsPattern[]] :=
	DiracGamma[7]/; OptionValue[FCI];

DiracMatrix[(a:5|6|7), OptionsPattern[]] :=
	DiracGamma[a]/; OptionValue[FCI] && MatchQ[OptionValue[Dimension],_];

DiracMatrix[a_, OptionsPattern[]] :=
	DiracGamma[LorentzIndex[a, OptionValue[Dimension]],
	OptionValue[Dimension]]/; OptionValue[FCI] && Head[a]=!=DOT && !StringQ[a] && !NumberQ[a];

DiracMatrix[(a:0|1|2|3), OptionsPattern[]] :=
	DiracGamma[ExplicitLorentzIndex[a, OptionValue[Dimension]],
	OptionValue[Dimension]]/; OptionValue[FCI] && Head[a]=!=DOT && !StringQ[a];

DiracMatrix[x_?NumberQ, OptionsPattern[]] :=
	(Message[DiracMatrix::noint, x]; Abort[])/; !MemberQ[{0, 1, 2, 3, 5, 6, 7}, x];

DiracMatrix[DOT[a_,b__], opts:OptionsPattern[]] :=
	DOT@@(DiracMatrix[#,opts]& /@ {a,b});

DiracMatrix[a_,b:Except[_?OptionQ].., opts:OptionsPattern[]] :=
	DOT@@(DiracMatrix[#,opts]& /@ {a,b});

DiracSlash[DOT[a_,b__], opts:OptionsPattern[]] :=
	DOT@@(DiracSlash[#,opts]& /@ {a,b});

DiracSlash[a_,b:Except[_?OptionQ].., opts:OptionsPattern[]] :=
	DOT@@(DiracSlash[#,opts]& /@ {a,b});

DiracSlash[a_, OptionsPattern[]] :=
	DiracGamma[Momentum[a, OptionValue[Dimension]],
	OptionValue[Dimension]]/; OptionValue[FCI] && !NumberQ[a];

DiracSlash[0, OptionsPattern[]] :=
	0;

DiracSlash[x_?NumberQ, OptionsPattern[]] :=
	(Message[DiracSlash::noint, x]; Abort[])/; x=!=0;

DiracSpinor = Spinor;

FourVector[a_,b_, OptionsPattern[]] :=
	Pair[Momentum[a, OptionValue[Dimension]],
	LorentzIndex[b, OptionValue[Dimension]]]/; OptionValue[FCI];


IFPD[Momentum[OPEDelta,___],0] :=
	0;

LeviCivita[x:Except[_?OptionQ].., opts:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]..,
		opts:OptionsPattern[LeviCivita]]/; (Length[{x,y}] =!= 4) && (FCPatternFreeQ[{x,y,opts}]) :=
	Message[LeviCivita::argrx, "LeviCivita["<>ToString[{x,opts}]<>"]["<>ToString[{y,opts}]<>"]", Length[{x,y}], 4];

LeviCivita[x:Except[_?OptionQ] ..., opts:OptionsPattern[]]/; (Length[{x}] > 4) && (FCPatternFreeQ[{x,opts}]) :=
	Message[LeviCivita::argrx, "LeviCivita["<>ToString[{x,opts}]<>"]", Length[{x}], 4];

LeviCivita[ a:Except[_?OptionQ].., opts:OptionsPattern[]] :=
	FCI[LeviCivita[a,Join[{FCI->False},FilterRules[{opts},Except[FCI]]]]]/; Length[{a}] === 4 && OptionValue[FCI];

LeviCivita[x:Except[_?OptionQ]..., opts1:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]..., opts2:OptionsPattern[LeviCivita]] :=
	FCI[LeviCivita[x,Join[{FCI->False},FilterRules[{opts1},Except[FCI]]]][y,Join[{FCI->False},FilterRules[{opts2},Except[FCI]]]]]/;
	Length[{x,y}] === 4 && OptionValue[LeviCivita,{opts1},FCI] && OptionValue[LeviCivita,{opts2},FCI];

MetricTensor[a_, b_, OptionsPattern[]] :=
	Pair[LorentzIndex[a, OptionValue[Dimension]], LorentzIndex[b, OptionValue[Dimension]]]/; OptionValue[FCI];


FourVector /:
	MakeBoxes[FourVector[a_,b_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[Pair[Momentum[a, OptionValue[FourVector, {opts},Dimension]], LorentzIndex[b, OptionValue[FourVector, {opts},Dimension]]],TraditionalForm]/;
			!OptionValue[FourVector, {opts},FCI];


MetricTensor /:
	MakeBoxes[MetricTensor[a_, b_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[Pair[LorentzIndex[a,OptionValue[{opts},Dimension]],LorentzIndex[b,OptionValue[{opts},Dimension]]], TraditionalForm]/; !OptionValue[{opts},FCI];

ChiralityProjector /:
	MakeBoxes[ChiralityProjector[1,OptionsPattern[]], TraditionalForm]:=
		ToBoxes[DiracGamma[6],TraditionalForm];

ChiralityProjector /:
	MakeBoxes[ChiralityProjector[-1,OptionsPattern[]], TraditionalForm]:=
		ToBoxes[DiracGamma[7],TraditionalForm];

DiracMatrix /:
	MakeBoxes[DiracMatrix[x_/;!MemberQ[{5,6,7},x], opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[DiracGamma[LorentzIndex[x,OptionValue[DiracMatrix, {opts},Dimension]],
			OptionValue[DiracMatrix, {opts},Dimension]],TraditionalForm]/; !OptionValue[{opts},FCI];

DiracMatrix /:
	MakeBoxes[DiracMatrix[(x:5|6|7), opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[DiracGamma[x],TraditionalForm]/; !OptionValue[{opts},FCI];


DiracSlash /:
	MakeBoxes[DiracSlash[x_, opts:OptionsPattern[]], TraditionalForm]:=
		ToBoxes[DiracGamma[Momentum[x,OptionValue[DiracSlash, {opts},Dimension]],
			OptionValue[DiracSlash, {opts},Dimension]],TraditionalForm]/; !OptionValue[{opts},FCI];

IFPD /:
	MakeBoxes[IFPD[a_,c_], TraditionalForm]:=
		If[ c === 0,
			TBox[a^2],
			TBox["(", a^2," - ", c^2, ")"]
		];

LeviCivita /:
	MakeBoxes[LeviCivita[(a:Except[_?OptionQ]..)/; Length[{a}] === 4, opts:OptionsPattern[LeviCivita]/;!OptionValue[LeviCivita,{opts},FCI]],
	TraditionalForm]:=
		ToBoxes[Eps[Sequence@@(LorentzIndex[#,OptionValue[LeviCivita,{opts},Dimension]]&/@{a})],TraditionalForm];

LeviCivita /:
	MakeBoxes[LeviCivita[x:Except[_?OptionQ]..., opts1:OptionsPattern[LeviCivita]][y:Except[_?OptionQ]..., opts2:OptionsPattern[LeviCivita]], TraditionalForm]:=
		ToBoxes[Eps[Sequence@@(LorentzIndex[#,OptionValue[LeviCivita,{opts1},Dimension]]&/@{x}),
				Sequence@@(Momentum[#,OptionValue[LeviCivita,{opts2},Dimension]]&/@{y})],TraditionalForm]/;
		Length[{x,y}] === 4 && !OptionValue[LeviCivita,{opts1,opts2},FCI];


$BreitMaison /: Set[$BreitMaison, True] :=
	(
		OwnValues[$BreitMaison] = {HoldPattern[$BreitMaison] :> True};
		OwnValues[$Larin] = {HoldPattern[$Larin] :> False};
		FCSetDiracGammaScheme["BMHV"];
		True
	);

$BreitMaison /: Set[$BreitMaison, False] :=
	(
		OwnValues[$BreitMaison] = {HoldPattern[$BreitMaison] :> False};
		If[	TrueQ[$Larin === False],
			FCSetDiracGammaScheme["NDR"],
			FCSetDiracGammaScheme["Larin"]
		];
		False
	);

$Larin /: Set[$Larin, True] :=
	(
		OwnValues[$Larin] = {HoldPattern[$Larin] :> True};
		OwnValues[$BreitMaison] = {HoldPattern[$BreitMaison] :> False};
		FCSetDiracGammaScheme["Larin"];
		True
	);

$Larin /: Set[$Larin, False] :=
	(
		OwnValues[$Larin] = {HoldPattern[$Larin] :> False};
		If[TrueQ[$BreitMaison === False],
			FCSetDiracGammaScheme["NDR"],
			FCSetDiracGammaScheme["BMHV"]
		];
		False
	);


FCPrint[1,"LegacyObjects loaded."];
End[]

