(* ::Package:: *)


(* :Title: Chisholm *)

(* :Title: Chisholm														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Applies the Chisholm identity									*)

(* ------------------------------------------------------------------------ *)

Chisholm::usage =
"Chisholm[exp] substitutes products of three Dirac matrices or slashes by the
Chisholm identity.";

Chisholm::failmsg =
"Error! Chisholm has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`Chisholm`Private`"]

chVerbose::usage="";

Options[Chisholm] = {
	Contract 					-> True,
	DiracSigmaExplicit			-> False,
	DiracSimplify				-> True,
	DiracSpinorNormalization	-> "Relativistic",
	DotSimplify					-> True,
	FCDiracIsolate				-> True,
	FCE							-> False,
	FCI							-> False,
	FCJoinDOTs					-> True,
	FCVerbose					-> False,
	InsideDiracTrace			-> False,
	MaxIterations				-> Infinity,
	Mode						-> 1,
	NonCommutative 				-> True,
	SpinorChainEvaluate			-> True
};

Chisholm[a_ == b_, opts:OptionsPattern[]] :=
	Chisholm[a,opts] == Chisholm[b,opts];

Chisholm[expr_List, opts:OptionsPattern[]]:=
	Chisholm[#, opts]&/@expr;

Chisholm[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, tmp, tmpli, tmpli1, tmpli2, terms,rest,res, holdDOT, eps, freePart, dsPart, diracObjects,
			diracObjectsEval, null1, null2, dsHead, time, repRule, mode, chisholmRule1, chisholmRule2, maxIterations,
			nonComm, chisholmRuleInsideTrace},

		mode = OptionValue[Mode];
		maxIterations = OptionValue[MaxIterations];
		nonComm = OptionValue[NonCommutative];

		chisholmRule1 = {
			dsHead[holdDOT[a___,DiracGamma[(lv1:(LorentzIndex|Momentum)[_])], DiracGamma[(lv2:(LorentzIndex|Momentum)[_])],
				DiracGamma[(lv3:(LorentzIndex|Momentum)[_])]]] :>
					(tmpli= LorentzIndex[$MU[Unique[]]]; (
					Pair[lv1, lv2] dsHead[holdDOT[a,DiracGamma[lv3]]] -
					Pair[lv1, lv3] dsHead[holdDOT[a,DiracGamma[lv2]]] +
					Pair[lv2, lv3] dsHead[holdDOT[a,DiracGamma[lv1]]] -
					$LeviCivitaSign I Eps[lv1,lv2,lv3, tmpli] dsHead[holdDOT[a,DiracGamma[tmpli],DiracGamma[5]]]
					)),
			dsHead[holdDOT[a___, DiracGamma[(lv1:(LorentzIndex|Momentum)[_])], DiracGamma[(lv2:(LorentzIndex|Momentum)[_])],
				DiracGamma[(lv3:(LorentzIndex|Momentum)[_])], DiracGamma[5]]] :>
					(tmpli= LorentzIndex[$MU[Unique[]]]; (
					Pair[lv1, lv2] dsHead[holdDOT[a, DiracGamma[lv3],DiracGamma[5]]] -
					Pair[lv1, lv3] dsHead[holdDOT[a, DiracGamma[lv2],DiracGamma[5]]] +
					Pair[lv2, lv3] dsHead[holdDOT[a, DiracGamma[lv1],DiracGamma[5]]] -
					$LeviCivitaSign I Eps[lv1,lv2,lv3, tmpli] dsHead[holdDOT[a,DiracGamma[tmpli]]])),

			(*The same with the spinors*)
			dsHead[holdDOT[a___,DiracGamma[(lv1:(LorentzIndex|Momentum)[_])], DiracGamma[(lv2:(LorentzIndex|Momentum)[_])],
				DiracGamma[(lv3:(LorentzIndex|Momentum)[_])], b_Spinor]] :>
					(tmpli= LorentzIndex[$MU[Unique[]]]; (
					Pair[lv1, lv2] dsHead[holdDOT[a,DiracGamma[lv3], b]] -
					Pair[lv1, lv3] dsHead[holdDOT[a,DiracGamma[lv2], b]] +
					Pair[lv2, lv3] dsHead[holdDOT[a,DiracGamma[lv1], b]] -
					$LeviCivitaSign I Eps[lv1,lv2,lv3, tmpli] dsHead[holdDOT[a,DiracGamma[tmpli],DiracGamma[5], b]]
					)),
			dsHead[holdDOT[a___, DiracGamma[(lv1:(LorentzIndex|Momentum)[_])], DiracGamma[(lv2:(LorentzIndex|Momentum)[_])],
				DiracGamma[(lv3:(LorentzIndex|Momentum)[_])], DiracGamma[5], b_Spinor]] :>
					(tmpli= LorentzIndex[$MU[Unique[]]]; (
					Pair[lv1, lv2] dsHead[holdDOT[a, DiracGamma[lv3],DiracGamma[5], b]] -
					Pair[lv1, lv3] dsHead[holdDOT[a, DiracGamma[lv2],DiracGamma[5], b]] +
					Pair[lv2, lv3] dsHead[holdDOT[a, DiracGamma[lv1],DiracGamma[5], b]] -
					$LeviCivitaSign I Eps[lv1,lv2,lv3, tmpli] dsHead[holdDOT[a,DiracGamma[tmpli],b]]))
		};

		chisholmRuleInsideTrace = {
			dsHead[holdDOT[x___DiracGamma, DiracGamma[5]]]/; Length[{x}]<4 && FreeQ2[{x},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] :> 0
		};


		chisholmRule2 = {
			dsHead[holdDOT[a___, DiracGamma[(lv1:(LorentzIndex|Momentum)[_])], DiracGamma[(lv2:(LorentzIndex|Momentum)[_])],
				DiracGamma[(lv3:(LorentzIndex|Momentum)[_])], b___]] :>
					(tmpli= LorentzIndex[$MU[Unique[]]]; (
					Pair[lv1, lv2] dsHead[holdDOT[a, DiracGamma[lv3], b]] -
					Pair[lv1, lv3] dsHead[holdDOT[a, DiracGamma[lv2], b]] +
					Pair[lv2, lv3] dsHead[holdDOT[a, DiracGamma[lv1], b]] -
					$LeviCivitaSign I Eps[lv1,lv2,lv3, tmpli] dsHead[holdDOT[a, DiracGamma[tmpli], DiracGamma[5],  b]]))
		};

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,DiracGamma],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			chVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				chVerbose=OptionValue[FCVerbose]
			];
		];


		FCPrint[1, "Chisholm. Entering.", FCDoControl->chVerbose];
		FCPrint[3, "Chisholm: Entering with ", ex, FCDoControl->chVerbose];

		If[	OptionValue[FCDiracIsolate],
			(* This is the normal mode which works well both for large and small expressions *)
			FCPrint[1, "Chisholm: Normal mode.", FCDoControl->chVerbose];
			time=AbsoluteTime[];
			FCPrint[1, "Chisholm: Extracting Dirac objects.", FCDoControl->chVerbose];

			If[	!FreeQ[ex, DiracChain],
				ex  = DiracChainExpand[ex, FCI->True];
			];

			ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, DiracChain->True,
				DotSimplify->OptionValue[DotSimplify], FCJoinDOTs -> OptionValue[FCJoinDOTs], DiracGammaCombine-> False];

			If[	!FreeQ[ex, DiracChain],
				ex = ex /. dsHead[DiracChain[x_,i_,j_]] :> DiracChain[dsHead[x],i,j]
			];

			{freePart,dsPart} = FCSplit[ex,{dsHead}];
			FCPrint[3,"Chisholm: dsPart: ",dsPart , FCDoControl->chVerbose];
			FCPrint[3,"Chisholm: freePart: ",freePart , FCDoControl->chVerbose];

			diracObjects = Cases[dsPart+null1+null2, dsHead[_], Infinity]//DeleteDuplicates//Sort;
			FCPrint[1, "Chisholm: Done extracting Dirac objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->chVerbose];

			time=AbsoluteTime[];
			Which[
				mode===1,
					FCPrint[1, "Chisholm: Applying Chisholm identity (mode 1).", FCDoControl->chVerbose];

					diracObjectsEval = diracObjects /. DOT->holdDOT;

					diracObjectsEval = FixedPoint[(# /. chisholmRule1)&,diracObjectsEval, maxIterations];
					If[	nonComm,
						diracObjectsEval = FixedPoint[(# /. chisholmRule2)&,diracObjectsEval, maxIterations]
					];

					If [OptionValue[InsideDiracTrace],
						diracObjectsEval = diracObjectsEval/.chisholmRuleInsideTrace;
					];

					FCPrint[1, "Done applying Chisholm identity  (mode 1), timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->chVerbose],

				mode===2,
					FCPrint[1, "Chisholm: Applying Chisholm identity (mode 2).", FCDoControl->chVerbose];
					diracObjectsEval = diracObjects /. DOT->holdDOT //. {dsHead[holdDOT[DiracGamma[(lv1:(LorentzIndex|Momentum)[_])],
						DiracGamma[(lv2:(LorentzIndex|Momentum)[_])], DiracGamma[5], b___]] :>
						(tmpli1= LorentzIndex[$MU[Unique[]]]; tmpli2= LorentzIndex[$MU[Unique[]]]; (
						- $LeviCivitaSign/2 Eps[lv1, lv2, tmpli1, tmpli2] dsHead[holdDOT[DiracSigma[DiracGamma[tmpli1], DiracGamma[tmpli2]],b]] +
						Pair[lv1, lv2] dsHead[holdDOT[DiracGamma[5],b]]))} //. {

						dsHead[holdDOT[a___, DiracGamma[(lv1:(LorentzIndex|Momentum)[_])], DiracGamma[(lv2:(LorentzIndex|Momentum)[_])], DiracGamma[5], b___]] :>
						(
						tmpli1= LorentzIndex[$MU[Unique[]]];
						tmpli2= LorentzIndex[$MU[Unique[]]];
						- $LeviCivitaSign/2 Eps[lv1, lv2, tmpli1, tmpli2] dsHead[holdDOT[a,DiracSigma[DiracGamma[tmpli1], DiracGamma[tmpli2]],b]] +
						Pair[lv1, lv2] dsHead[holdDOT[a,DiracGamma[5],b]])
						} //. {
						dsHead[holdDOT[a___, DiracSigma[DiracGamma[lv1_], DiracGamma[lv2_]], DiracGamma[5], b___]] :>
						(
						tmpli1= LorentzIndex[$MU[Unique[]]];
						tmpli2= LorentzIndex[$MU[Unique[]]];
						-I*$LeviCivitaSign (1/2) dsHead[holdDOT[a,DiracSigma[DiracGamma[tmpli1], DiracGamma[tmpli2]],b]] Eps[lv1, lv2, tmpli1, tmpli2]
						)
						};
					FCPrint[1, "Done applying Chisholm identity  (mode 2), timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->chVerbose],
				True,
				Message[Chisholm::failmsg, "Unknown operation mode."];
				Abort[]
			];
			time=AbsoluteTime[];
			FCPrint[1, "Chisholm: Inserting Dirac objects back.", FCDoControl->chVerbose];

			diracObjectsEval = diracObjectsEval /. holdDOT[]->1 /.holdDOT->DOT /. dsHead->Identity;
			repRule = Thread[Rule[diracObjects,diracObjectsEval]];
			FCPrint[3,"Chisholm: repRule: ",repRule , FCDoControl->chVerbose];
			tmp = freePart + (dsPart/. Dispatch[repRule]);
			FCPrint[1, "Chisholm: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->chVerbose];
			FCPrint[3,"Chisholm: Intermediate result: ", tmp, FCDoControl->chVerbose],

			(* This is the fast mode for standalone Dirac chains *)
			FCPrint[1, "Chisholm: Fast mode.", FCDoControl->chVerbose];
			time=AbsoluteTime[];

			Which[
				mode===1,
					FCPrint[1, "Chisholm: Applying Chisholm identity (mode 1).", FCDoControl->chVerbose];
					tmp = ex /. DOT -> holdDOT /. holdDOT[z__] :> dsHead[holdDOT[z]];

					tmp = FixedPoint[(# /. chisholmRule1)&, tmp, maxIterations];
					If[ nonComm,
						tmp = FixedPoint[(# /. chisholmRule2)&, tmp, maxIterations]
					];
					FCPrint[1, "Done applying Chisholm identity  (mode 1), timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->chVerbose],

				mode===2,
					FCPrint[1, "Chisholm: Applying Chisholm identity (mode 2).", FCDoControl->chVerbose];
					tmp = ex /. DOT -> holdDOT //. {holdDOT[DiracGamma[(lv1:(LorentzIndex|Momentum)[_])],
					DiracGamma[(lv2:(LorentzIndex|Momentum)[_])], DiracGamma[5], b___] :>
					(tmpli1= LorentzIndex[$MU[Unique[]]]; tmpli2= LorentzIndex[$MU[Unique[]]]; (
					- $LeviCivitaSign/2 Eps[lv1, lv2, tmpli1, tmpli2] holdDOT[DiracSigma[DiracGamma[tmpli1], DiracGamma[tmpli2]],b] +
					Pair[lv1, lv2] holdDOT[DiracGamma[5],b]))} //. {holdDOT[a___, DiracGamma[(lv1:(LorentzIndex|Momentum)[_])],
					DiracGamma[(lv2:(LorentzIndex|Momentum)[_])], DiracGamma[5], b___] :>
					(tmpli1= LorentzIndex[$MU[Unique[]]]; tmpli2= LorentzIndex[$MU[Unique[]]]; (
					- $LeviCivitaSign/2 Eps[lv1, lv2, tmpli1, tmpli2] holdDOT[a,DiracSigma[DiracGamma[tmpli1], DiracGamma[tmpli2]],b] +
					Pair[lv1, lv2] holdDOT[a,DiracGamma[5],b]))};
					FCPrint[1, "Done applying Chisholm identity  (mode 2), timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->chVerbose],


				True,
				Message[Chisholm::failmsg, "Unknown operation mode."];
				Abort[]
			];
			tmp = tmp/. holdDOT[]->1 /.holdDOT->DOT /. dsHead->Identity
		];

		res = tmp;

		If[	OptionValue[Contract],
				time=AbsoluteTime[];
				FCPrint[1, "Chisholm: Applying Contract.", FCDoControl->chVerbose];
				res = Contract[res, FCI->True];
				FCPrint[1, "Chisholm: Done applying Contract, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->chVerbose]
		];

		If[	OptionValue[DiracSimplify],
				time=AbsoluteTime[];
				FCPrint[1, "Chisholm: Applying DiracSimplify.", FCDoControl->chVerbose];
				res = DiracSimplify[res, FCI->True, DiracSigmaExplicit->OptionValue[DiracSigmaExplicit],
					SpinorChainEvaluate -> OptionValue[SpinorChainEvaluate],
					DiracSpinorNormalization -> OptionValue[DiracSpinorNormalization]];
				FCPrint[1, "Chisholm: Done applying DiracSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->chVerbose]
		];

		FCPrint[1, "Chisholm: Leaving.", FCDoControl->chVerbose];
		FCPrint[3, "Chisholm: Leaving with ", res, FCDoControl->chVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

FCPrint[1,"Chisholm.m loaded"];
End[]
