(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCChargeConjugateTransposed										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary: C x^T C^(-1) for Dirac matrices								*)

(* ------------------------------------------------------------------------ *)

FCCCT::usage =
"FCCCT is an alias for FCChargeConjugateTransposed.";

FCChargeConjugateTransposed::usage =
"FCChargeConjugateTransposed[exp] represents the application of the charge \
conjugation operator to the transposed of exp, i.e. C exp^T C^(-1). Here exp \
is understood to be a single Dirac matrix or a chain thereof. \n
The option setting Explicit determines whether the explicit result \
is returned or whether it is left in the unevaluated form. This form will be \
also maintained if the function does not know how to obtain C exp^T C^(-1) \
from the given exp.";

FCChargeConjugateTransposed::failmsg =
"Error! SpinorChainTranspose has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCChargeConjugateTransposed`Private`"]

fcctVerbose::usage="";

DeclareNonCommutative[FCChargeConjugateTransposed];

Options[FCChargeConjugateTransposed] = {
	DotSimplify->True,
	FCVerbose -> False,
	FCDiracIsolate -> True,
	FCI -> False,
	FCE -> False,
	Explicit -> False
};

FCCCT = FCChargeConjugateTransposed;
Abbreviation[FCChargeConjugateTransposed] = HoldForm[FCCCT];

FCChargeConjugateTransposed /:
	MakeBoxes[FCChargeConjugateTransposed[ex_, OptionsPattern[]], TraditionalForm] :=
		RowBox[{"C", SuperscriptBox[RowBox[{"(", TBox[ex], ")"}], "T"], SuperscriptBox["C", "-1"]}];

FCChargeConjugateTransposed[ex_, OptionsPattern[]]:=
	ex/; NonCommFreeQ[ex] && FCPatternFreeQ[{ex}];


FCChargeConjugateTransposed[expr_, opts:OptionsPattern[]] :=
	Block[ {ex, time,dsHead,nonDsHead,diracObjects,
		nonDiracObjects,diracObjectsEval,nonDiracObjectsEval,null1,null2, repRule1,repRule2,res},

		If [OptionValue[FCVerbose]===False,
			fcctVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fcctVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FCChargeConjugateTransposed. Entering.", FCDoControl->fcctVerbose];
		FCPrint[3, "FCChargeConjugateTransposed: Entering with ", expr, FCDoControl->fcctVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	!FreeQ[ex,DiracSigma],
			ex = ex/. d_DiracSigma/; !FreeQ2[d,{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] :> DiracSigmaExplicit[d,FCI->True]
		];

		If[	OptionValue[FCDiracIsolate],
			(*	Extract all the Dirac structures inside the operator. *)
			ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, Factoring-> nonDsHead];
			ex = ex /. dsHead[z_]/;!FreeQ[z,Spinor] :> nonDsHead[z];
			diracObjects = Cases[ex+null1+null2, dsHead[_], Infinity]//Union;
			nonDiracObjects = Cases[ex+null1+null2, nonDsHead[_], Infinity]//Union,

			(*	The operator contains only Dirac structures (fast mode). *)

			If[	TrueQ[FreeQ[ex,Spinor]],
				ex = dsHead[ex];
				diracObjects = {ex};
				nonDiracObjects = {},

				ex = nonDsHead[ex];
				diracObjects = {};
				nonDiracObjects = {ex}

			];

		];


		If[	nonDiracObjects=!={},
			time=AbsoluteTime[];
			nonDiracObjectsEval = Map[FCChargeConjugateTransposed[#,Explicit->False,Sequence@@FilterRules[{opts}, Except[Explicit]]]&,
				nonDiracObjects /. nonDsHead->Identity];
			FCPrint[1,"FCChargeConjugateTransposed: nonDiracObjectsEval finished, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fcctVerbose];
			FCPrint[3,"FCChargeConjugateTransposed: nonDiracObjectsEval: ", nonDiracObjectsEval, FCDoControl->fcctVerbose];
			repRule2 = MapThread[Rule[#1,#2]&,{nonDiracObjects,nonDiracObjectsEval}];
			FCPrint[3,"FCChargeConjugateTransposed: repRule: ",repRule2 , FCDoControl->fcctVerbose],

			repRule2 = {}
		];

		If[	diracObjects=!={},
			time=AbsoluteTime[];
			diracObjectsEval = Map[cctEvaluateSingle[#]&, diracObjects /. dsHead->Identity /. DOT-> dotHold];
			(*	at this point the chain is already reversed (a.b.c)^T -> c^T.b^T.a^T	*)

			FCPrint[3,"FCChargeConjugateTransposed: diracObjectsEval after cctEvaluateSingle: ", diracObjectsEval, FCDoControl->fcctVerbose];

			diracObjectsEval = diracObjectsEval /. cctEvaluateSingle[dotHold[z__]] :> cctEvaluateMultiple[dotHold@@(Reverse[{z}])];

			FCPrint[3,"FCChargeConjugateTransposed: diracObjectsEval after cctEvaluateMultiple: ", diracObjectsEval, FCDoControl->fcctVerbose];

			diracObjectsEval = diracObjectsEval /. cctEvaluateMultiple -> cctEvaluateSplit;

			FCPrint[3,"FCChargeConjugateTransposed: diracObjectsEval after cctEvaluateSplit: ", diracObjectsEval, FCDoControl->fcctVerbose];

			diracObjectsEval = diracObjectsEval/. cctEvaluateSingle[z_] :>
				FCChargeConjugateTransposed[z,Explicit->False,Sequence@@FilterRules[{opts}, Except[Explicit]]] /. dotHold->DOT;

			FCPrint[1,"FCChargeConjugateTransposed: diracObjectsEval finished, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fcctVerbose];

			If[ OptionValue[DotSimplify],
				diracObjectsEval = DotSimplify[#,FCI->True,Expanding->False]&/@diracObjectsEval
			];

			repRule1 = MapThread[Rule[#1,#2]&,{diracObjects,diracObjectsEval}];
			FCPrint[3,"FCChargeConjugateTransposed: repRule: ",repRule1 , FCDoControl->fcctVerbose],

			repRule1 = {}
		];




		res = ex /. Dispatch[repRule2] /. Dispatch[repRule1];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCChargeConjugateTransposed: Leaving.", FCDoControl->fcctVerbose];
		FCPrint[3, "FCChargeConjugateTransposed: Leaving with", res, FCDoControl->fcctVerbose];

		res

	]/; OptionValue[Explicit];

dotHold[]:=
	1;

dotHold[a___, cctEvaluateMultiple[1], b___]:=
	dotHold[a,b];

cctEvaluateSingle[d_DiracSigma]:=
	- d/; FreeQ2[d,{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

(* C (g^mu)^T C^(-1) = - g^mu *)
cctEvaluateSingle[d_DiracGamma]:=
	- d/; !MatchQ[d,DiracGamma[5|6|7]];

(* C g^(5|6|7)^T C^(-1) = g^(5|6|7)*)
cctEvaluateSingle[(d:DiracGamma[5|6|7])]:=
	d;

(* C (g^mu p_mu +/- m)^T C^(-1) = (-g^mu p_mu +/- m) *)
cctEvaluateSingle[c1_. d_DiracGamma + c2_:0]:=
		(- c1 d + c2)/; NonCommFreeQ[{c1,c2}] && FreeQ[{c1,c2}, dotHold] && !MatchQ[d,DiracGamma[5|6|7]];

cctEvaluateMultiple[dotHold[a_]]:=
	cctEvaluateSingle[a];

cctEvaluateMultiple[dotHold[rest1___, Longest[d: DiracGamma[__]..],
	rest2___]]:=
	((-1)^Length[{d}]) dotHold[cctEvaluateMultiple[dotHold[rest1]], d, cctEvaluateMultiple[dotHold[rest2]]]/; Length[{d}]>1 &&
	FreeQ2[{d},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

cctEvaluateSplit[dotHold[a__]]:=
	dotHold@@(cctEvaluateSingle/@{a});

FCPrint[1,"FCChargeConjugateTransposed.m loaded"];
End[]
