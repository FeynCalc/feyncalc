(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinorChainTrick													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  Simplification rules for products of spinor chains			*)

(* ------------------------------------------------------------------------ *)

SpinorChainTrick::usage =
"SpinorChainTrick[exp] applies several simplifications to products of spinor \
chains.";

SpinorChainTrick::failmsg =
"Error! SpinorChainTrick has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SpinorChainTrick`Private`"]

holdDOT::usage="";
spchtrVerbose::usage="";

Options[SpinorChainTrick] = {
	CartesianIndexNames			-> {},
	Contract 					-> True,
	DiracEquation				-> True,
	DiracGammaCombine			-> False,
	DiracSigmaExplicit			-> True,
	DiracTrick					-> True,
	DotSimplify					-> True,
	FCCanonicalizeDummyIndices	-> True,
	FCE							-> False,
	FCI							-> False,
	FCJoinDOTs					-> True,
	FCVerbose					-> False,
	Factoring					-> {Factor2, 5000},
	LorentzIndexNames			-> {}
};

SpinorChainTrick[expr_, OptionsPattern[]] :=
	Block[{ex, tmp, dsHead, dsIso, freePart,dsPart, null1, null2, time, diracObjects, optDiracSigmaExplicit,
		optDiracGammaCombine, diracObjectsEval, repRule, res},

		optDiracSigmaExplicit	= OptionValue[DiracSigmaExplicit];
		optDiracGammaCombine	= OptionValue[DiracGammaCombine];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,Spinor],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			spchtrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				spchtrVerbose=OptionValue[FCVerbose]
			];
		];


		FCPrint[1, "SpinorChainTrick. Entering.", FCDoControl->spchtrVerbose];
		FCPrint[3, "SpinorChainTrick: Entering with ", ex, FCDoControl->spchtrVerbose];

		FCPrint[1, "SpinorChainTrick: Isolating spinor chains.", FCDoControl->spchtrVerbose];
		time=AbsoluteTime[];
		tmp = FCDiracIsolate[ex,FCI->True, Head->dsHead, DotSimplify->True, DiracGammaCombine->optDiracGammaCombine,
				DiracSigmaExplicit->optDiracSigmaExplicit, LorentzIndex->All, Spinor->Join, DiracGamma->False, FCJoinDOTs-> OptionValue[FCJoinDOTs],
				Isolate->True, IsolateNames->dsIso, Factoring -> False, FCTraceExpand->True](* /. dsHead->Identity*);
		FCPrint[1, "SpinorChainTrick: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
		FCPrint[3, "SpinorChainTrick: After FCDiracIsolate ", tmp, FCDoControl->spchtrVerbose];

		If[	OptionValue[FCCanonicalizeDummyIndices],
			FCPrint[1, "SpinorChainTrick: Canonicalize dummy Lorentz and Cartesian indices.", FCDoControl->spchtrVerbose];
			time=AbsoluteTime[];

			tmp = FCCanonicalizeDummyIndices[tmp , FCI->True, Head->{LorentzIndex,CartesianIndex}, DotSimplify->False,
				LorentzIndexNames-> OptionValue[LorentzIndexNames], CartesianIndexNames-> OptionValue[CartesianIndexNames]];
			FCPrint[1, "SpinorChainTrick: Done canonicalize dummy Lorentz and Cartesian indices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
			FCPrint[3, "SpinorChainTrick: After FCCanonicalizeDummyIndices ", tmp, FCDoControl->spchtrVerbose]
		];


		If[	OptionValue[Contract],
			FCPrint[1, "SpinorChainTrick: Applying Contract.", FCDoControl->spchtrVerbose];
			time=AbsoluteTime[];
			tmp = tmp /. dsHead[x_] :> Contract[x, FCI->True];
			FCPrint[1, "SpinorChainTrick: Contract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
			FCPrint[3, "SpinorChainTrick: After Contract ", tmp, FCDoControl->spchtrVerbose]
		];

		tmp = FRH[tmp, IsolateNames->dsIso];

		FCPrint[1, "SpinorChainTrick: Isolating spinor chains (again).", FCDoControl->spchtrVerbose];
		time=AbsoluteTime[];
		tmp = FCDiracIsolate[tmp,FCI->True,Head->dsHead, DotSimplify->False, DiracGammaCombine->optDiracGammaCombine, FCJoinDOTs-> OptionValue[FCJoinDOTs],
			DiracSigmaExplicit->False, LorentzIndex->All, Spinor->Join, DiracGamma->False, Factoring -> OptionValue[Factoring]];
		FCPrint[1, "SpinorChainTrick: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
		FCPrint[3, "SpinorChainTrick: After second FCDiracIsolate ", tmp, FCDoControl->spchtrVerbose];


		diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
		FCPrint[3,"SpinorChainTrick: diracObjects: ", diracObjects , FCDoControl->spchtrVerbose];


		FCPrint[1, "SpinorChainTrick: Simplifying products of spinor chains.", FCDoControl->spchtrVerbose];
		time=AbsoluteTime[];
		diracObjectsEval = Map[(spinorChainTrickEval[#])&, (diracObjects/.dsHead->Identity/. DOT->holdDOT)] /. spinorChainTrickEval -> Identity /. holdDOT->DOT;
		FCPrint[1, "SpinorChainTrick: Done simplifying products of spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
		FCPrint[3, "SpinorChainTrick: diracObjectsEval: ", diracObjectsEval, FCDoControl->spchtrVerbose];


		If[	OptionValue[DiracEquation],
			FCPrint[1, "SpinorChainTrick: Applying Dirac equation to the products of spinor chains.", FCDoControl->spchtrVerbose];
			time=AbsoluteTime[];
			diracObjectsEval = Map[DiracEquation[#,FCI->True]&, diracObjectsEval];
			FCPrint[1, "SpinorChainTrick: Done applying Dirac equation, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
			FCPrint[3, "SpinorChainTrick: diracObjectsEval: ", diracObjectsEval, FCDoControl->spchtrVerbose];
		];

		If[	OptionValue[DiracTrick],
			FCPrint[1, "SpinorChainTrick: Applying DiracTrick to the products of spinor chains.", FCDoControl->spchtrVerbose];
			time=AbsoluteTime[];
			diracObjectsEval = Map[DiracTrick[#,FCI->True]&, diracObjectsEval];
			FCPrint[1, "SpinorChainTrick: Done applying DiracTrick, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
			FCPrint[3, "SpinorChainTrick: diracObjectsEval: ", diracObjectsEval, FCDoControl->spchtrVerbose];
		];

		If[ !FreeQ2[diracObjectsEval,{spinorChainTrickEval,holdDOT}],
			Message[SpinorChainTrick::failmsg,"Evaluation of isolated objects failed."];
			Abort[]
		];

		FCPrint[1, "SpinorChainTrick: Inserting Dirac objects back.", FCDoControl->spchtrVerbose];
		time=AbsoluteTime[];
		repRule = Thread[Rule[diracObjects, diracObjectsEval]];
		FCPrint[3,"SpinorChainTrick: repRule: ",repRule , FCDoControl->spchtrVerbose];
		res =  ( tmp/. Dispatch[repRule]);
		FCPrint[1, "SpinorChainTrick: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "SpinorChainTrick: Leaving.", FCDoControl->spchtrVerbose];
		FCPrint[3, "SpinorChainTrick: Leaving with ", res, FCDoControl->spchtrVerbose];



		res

	];

(* 4 and D *)
spinorChainTrickEval[c_. holdDOT[a1___, DiracGamma[LorentzIndex[arg_]], b1___] holdDOT[a2___, DiracGamma[LorentzIndex[arg_, dim_Symbol], dim_Symbol], b2___]]:=
	spinorChainTrickEval[c holdDOT[a1, DiracGamma[LorentzIndex[arg]], b1] holdDOT[a2, DiracGamma[LorentzIndex[arg]], b2]];

(* 4 and D-4 *)
spinorChainTrickEval[_. holdDOT[___, DiracGamma[LorentzIndex[arg_]], ___] holdDOT[___, DiracGamma[LorentzIndex[arg_, dim_Symbol-4], dim_Symbol-4], ___]]:=
	0;

(* D and D - 4 *)
spinorChainTrickEval[c_. holdDOT[a1___, DiracGamma[LorentzIndex[arg_, dim_Symbol], dim_Symbol], b1___] holdDOT[a2___, DiracGamma[LorentzIndex[arg_, dim_Symbol-4], dim_Symbol-4], b2___]]:=
	spinorChainTrickEval[c holdDOT[a1, DiracGamma[LorentzIndex[arg, dim-4], dim-4], b1] holdDOT[a2, DiracGamma[LorentzIndex[arg, dim-4], dim-4], b2]];

FCPrint[1,"SpinorChainTrick.m loaded"];
End[]
