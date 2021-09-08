(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinorChainChiralSplit													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Simplification rules for products of spinor chains			*)

(* ------------------------------------------------------------------------ *)

SpinorChainChiralSplit::usage =
"SpinorChainChiralSplit[exp] introduces chiral projectors in spinor chains that
contain no $\\gamma^5$.";

SpinorChainChiralSplit::failmsg =
"Error! SpinorChainChiralSplit has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SpinorChainChiralSplit`Private`"]

holdDOT::usage="";
spchchspVerbose::usage="";

Options[SpinorChainChiralSplit] = {
	Collecting 			-> True,
	DiracSubstitute5	-> True,
	FCE 				-> False,
	FCI 				-> False,
	FCVerbose 			-> False,
	Factoring 			-> Factor,
	Head				-> Identity
};


SpinorChainChiralSplit[a_ == b_, opts:OptionsPattern[]] :=
	SpinorChainChiralSplit[a,opts] == SpinorChainChiralSplit[b,opts];

SpinorChainChiralSplit[expr_List, opts:OptionsPattern[]]:=
	SpinorChainChiralSplit[#, opts]&/@expr;

SpinorChainChiralSplit[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, tmp, dsHead, dsIso, freePart,dsPart, null1, null2, time, diracObjects, diracObjectsEval, repRule, res, optHead},

		optHead	= OptionValue[Head];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,Spinor],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			spchchspVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				spchchspVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "SpinorChainChiralSplit. Entering.", FCDoControl->spchchspVerbose];
		FCPrint[3, "SpinorChainChiralSplit: Entering with ", ex, FCDoControl->spchchspVerbose];

		If[	MatchQ[ex/.DOT->holdDOT, holdDOT[a_Spinor,b___,c_Spinor]/;FreeQ[{b},Spinor]],

			FCPrint[1, "SpinorChainChiralSplit: Recognized standalone expression.", FCDoControl->spchchspVerbose];
			tmp = dsHead[ex];
			diracObjects  = {dsHead[ex]},

			FCPrint[1, "SpinorChainChiralSplit: Isolating spinor chains.", FCDoControl->spchchspVerbose];
			time=AbsoluteTime[];
			tmp = FCDiracIsolate[ex,FCI->True, Head->dsHead, Spinor->True, Collecting-> OptionValue[Collecting], DiracGamma->False, Factoring -> OptionValue[Factoring]];
			FCPrint[1, "SpinorChainChiralSplit: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchchspVerbose];
			FCPrint[3, "SpinorChainChiralSplit: After FCDiracIsolate ", tmp, FCDoControl->spchchspVerbose];

			diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;

		];

		FCPrint[3,"SpinorChainChiralSplit: diracObjects: ", diracObjects , FCDoControl->spchchspVerbose];


		FCPrint[1, "SpinorChainChiralSplit: Rewriting products of spinor chains.", FCDoControl->spchchspVerbose];
		time=AbsoluteTime[];
		diracObjectsEval = (diracObjects/.dsHead->Identity/. DOT->holdDOT)/. holdDOT[a_Spinor, b___, c_Spinor]/;
			FreeQ2[{b},{DiracGamma[5],DiracGamma[6],DiracGamma[7],Spinor}] :> optHead[holdDOT[a,b,DiracGamma[6],c]] + optHead[holdDOT[a,b,DiracGamma[7],c]];

		If[	OptionValue[DiracSubstitute5] && !FreeQ[diracObjectsEval,DiracGamma[5]],

			diracObjectsEval = diracObjectsEval/. holdDOT[a_Spinor, b___,DiracGamma[5],c___, d_Spinor]:>
				optHead[holdDOT[a, b,DiracGamma[6],c, d]] - optHead[holdDOT[a, b, DiracGamma[7], c, d]]
		];

		FCPrint[1, "SpinorChainChiralSplit: Done rewriting products of spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchchspVerbose];
		FCPrint[3, "SpinorChainChiralSplit: diracObjectsEval: ", diracObjectsEval, FCDoControl->spchchspVerbose];

		FCPrint[1, "SpinorChainChiralSplit: Inserting Dirac objects back.", FCDoControl->spchchspVerbose];
		time=AbsoluteTime[];
		repRule = Thread[Rule[diracObjects,(diracObjectsEval/.holdDOT->DOT)]];
		FCPrint[3,"SpinorChainChiralSplit: repRule: ",repRule , FCDoControl->spchchspVerbose];
		res =  (tmp /. Dispatch[repRule]);
		FCPrint[1, "SpinorChainChiralSplit: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchchspVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "SpinorChainChiralSplit: Leaving.", FCDoControl->spchchspVerbose];
		FCPrint[3, "SpinorChainChiralSplit: Leaving with ", res, FCDoControl->spchchspVerbose];

		res

	];

FCPrint[1,"SpinorChainChiralSplit.m loaded"];
End[]
