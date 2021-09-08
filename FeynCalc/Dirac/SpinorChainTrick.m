(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinorChainTrick													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Simplification rules for products of spinor chains			*)

(* ------------------------------------------------------------------------ *)

SpinorChainTrick::usage =
"SpinorChainTrick[exp] applies several simplifications to products of spinor
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
	Collecting					-> True,
	Contract 					-> True,
	DiracChain					-> True,
	DiracEquation				-> True,
	DiracGammaCombine			-> False,
	DiracOrder					-> False,
	DiracSigmaExplicit			-> True,
	DiracTrick					-> True,
	DotSimplify					-> True,
	FCCanonicalizeDummyIndices	-> True,
	FCDiracIsolate				-> True,
	FCE							-> False,
	FCI							-> False,
	FCJoinDOTs					-> True,
	FCVerbose					-> False,
	Factoring					-> {Factor2, 5000},
	LorentzIndexNames			-> {},
	PairContract 				-> True,
	SirlinSimplify				-> False,
	TimeConstrained 			-> 3
};

SpinorChainTrick[a_ == b_, opts:OptionsPattern[]] :=
	SpinorChainTrick[a,opts] == SpinorChainTrick[b,opts];

SpinorChainTrick[expr_List, opts:OptionsPattern[]]:=
	(SpinorChainTrick[#, opts]&/@expr)/; OptionValue[FCDiracIsolate]=!=List;

SpinorChainTrick[expr_, OptionsPattern[]] :=
	Block[{ex, tmp,  dsIso, freePart,dsPart, null1, null2, time, diracObjects, optDiracSigmaExplicit,
		optDiracGammaCombine, diracObjectsEval, repRule, res, timeAll, dsHead,
		optFCDiracIsolate, optDiracEquation, optDiracOrder, optFCCanonicalizeDummyIndices,
		liPrefix,ciPrefix,liNames,ciNames},

		timeAll=AbsoluteTime[];

		optDiracSigmaExplicit			= OptionValue[DiracSigmaExplicit];
		optDiracGammaCombine			= OptionValue[DiracGammaCombine];
		optFCDiracIsolate 				= OptionValue[FCDiracIsolate];
		optDiracEquation				= OptionValue[DiracEquation];
		optDiracOrder					= OptionValue[DiracOrder];
		optFCCanonicalizeDummyIndices	= OptionValue[FCCanonicalizeDummyIndices];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	OptionValue[FCVerbose]===False,
			spchtrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				spchtrVerbose=OptionValue[FCVerbose]
			];
		];


		FCPrint[1, "SpinorChainTrick. Entering.", FCDoControl->spchtrVerbose];
		FCPrint[3, "SpinorChainTrick: Entering with ", ex, FCDoControl->spchtrVerbose];

		Switch[optFCDiracIsolate,
			True,
			(*Normal mode*)
			time=AbsoluteTime[];
			FCPrint[1, "SpinorChainTrick: Isolating spinor chains.", FCDoControl->spchtrVerbose];
			tmp = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->True, DiracGammaCombine->optDiracGammaCombine,
				DiracSigmaExplicit->optDiracSigmaExplicit, LorentzIndex->All, CartesianIndex->All,
				DiracChain->OptionValue[DiracChain], Split->False];
			diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
			diracObjectsEval = diracObjects /. dsHead->Identity;
			FCPrint[1, "SpinorChainTrick: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
			FCPrint[3, "SpinorChainTrick: After FCDiracIsolate ", diracObjectsEval, FCDoControl->spchtrVerbose],
			False,
			(*Fast mode, no FCDiracIsolate*)
			diracObjects = dsHead[ex];
			diracObjectsEval = ex;
			tmp = dsHead[ex],
			List,
			(*	Special mode, the input is already a list of isolated objects and should be returned in the same form *)
			diracObjectsEval = ex,
			_,
			Message[SpinorChainTrick::failmsg,"Unknown value for the FCDiracIsolate option"];
			Abort[]
		];

		If[ OptionValue[Contract],
				time=AbsoluteTime[];
				FCPrint[1, "SpinorChainTrick: Doing index contractions.", FCDoControl->spchtrVerbose];
				diracObjectsEval = FeynCalc`Package`diracChainContract/@diracObjectsEval;
				FCPrint[1, "SpinorChainTrick: Index contractions done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
				FCPrint[3, "SpinorChainTrick: diracObjectsEval after index contractions: ", diracObjectsEval, FCDoControl->spchtrVerbose];
		];

		If[	!MatchQ[FCGetDimensions[diracObjectsEval],{_}],
			time=AbsoluteTime[];
			FCPrint[1, "SpinorChainTrick: Applying spinorChainTrickEval.", FCDoControl->spchtrVerbose];
			diracObjectsEval =	Map[(spinorChainTrickEval[# /. DOT->holdDOT]/. spinorChainTrickEval->Identity /. holdDOT->DOT)&, diracObjectsEval];
			FCPrint[1, "SpinorChainTrick: Done applying spinorChainTrickEval, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
			FCPrint[3, "SpinorChainTrick: diracObjectsEval after spinorChainTrickEval: ", diracObjectsEval, FCDoControl->spchtrVerbose];
		];

		(*	Dirac equation	*)
		If[	!FreeQ[diracObjectsEval,Spinor] && optDiracEquation,
			time=AbsoluteTime[];
			FCPrint[2,"SpinorChainTrick: Applying DiracEquation.", FCDoControl->spchtrVerbose];
			diracObjectsEval = DiracEquation[#, FCI->True]&/@diracObjectsEval;
			FCPrint[2,"SpinorChainTrick: DiracEquation done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
			FCPrint[3,"SpinorChainTrick: After DiracEquation: ", diracObjectsEval, FCDoControl->spchtrVerbose];
		];

		If [ OptionValue[SirlinSimplify] && FCGetDimensions[diracObjectsEval]==={4},

				FCPrint[1, "SpinorChainTrick: Applying SirlinSimplify.", FCDoControl->spchtrVerbose];
				time=AbsoluteTime[];
				diracObjectsEval = SirlinSimplify[#, FCI->True,DiracGammaCombine->optDiracGammaCombine, DiracSigmaExplicit->False,
						SpinorChainTrick->False]&/@diracObjectsEval;

				FCPrint[1,"SpinorChainTrick: Done applying SirlinSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
				FCPrint[3, "SpinorChainTrick: After SirlinSimplify: ", diracObjectsEval, FCDoControl->spchtrVerbose]
		];

		(* 	Canonical ordering of Dirac matrices.	*)
		If[ optDiracOrder,
				time=AbsoluteTime[];
				FCPrint[1,"SpinorChainTrick: Applying DiracOrder.", FCDoControl->spchtrVerbose];
				diracObjectsEval = DiracOrder[#, FCI->True, FCJoinDOTs->False, DiracTrick->Last]&/@diracObjectsEval;
				FCPrint[1,"SpinorChainTrick: Done applying DiracOrder, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
				FCPrint[3, "SpinorChainTrick: After DiracOrder: ", diracObjectsEval, FCDoControl->spchtrVerbose]
		];

		If[ optFCCanonicalizeDummyIndices,
			time=AbsoluteTime[];
			FCPrint[1,"SpinorChainTrick: Applying FCCanonicalizeDummyIndices.", FCDoControl->spchtrVerbose];
			liPrefix=ToString[Unique["LI"]];
			ciPrefix=ToString[Unique["CI"]];
			liNames=Table[FCGV[liPrefix<>ToString[i]], {i,1,Length[Cases2[diracObjectsEval, LorentzIndex]]}];
			ciNames=Table[FCGV[ciPrefix<>ToString[i]], {i,1,Length[Cases2[diracObjectsEval, CartesianIndex]]}];
			diracObjectsEval = FCCanonicalizeDummyIndices[#, FCI->True,
				LorentzIndexNames->Join[OptionValue[LorentzIndexNames],liNames],
				CartesianIndexNames->Join[OptionValue[CartesianIndexNames],ciNames],
				Head -> {LorentzIndex, CartesianIndex, DiracIndex}, DotSimplify->False]&/@diracObjectsEval;
			FCPrint[1,"SpinorChainTrick: Done applying FCCanonicalizeDummyIndices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose]
		];



		If[	TrueQ[optFCDiracIsolate===List],
			Return[diracObjectsEval],
			FCPrint[1, "SpinorChainTrick: Inserting Dirac objects back.", FCDoControl->spchtrVerbose];
			time=AbsoluteTime[];
			repRule = Thread[Rule[diracObjects, diracObjectsEval]];
			FCPrint[3,"SpinorChainTrick: repRule: ",repRule , FCDoControl->spchtrVerbose];
			res =  ( tmp/. Dispatch[repRule]);
			FCPrint[1, "SpinorChainTrick: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose]
		];

		If[	OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "SpinorChainTrick: Applying Collect2.", FCDoControl->spchtrVerbose];
			res = Collect2[res, FeynCalc`Package`DiracHeadsList, Factoring->OptionValue[Factoring], TimeConstrained->OptionValue[TimeConstrained] ];
			FCPrint[1, "SpinorChainTrick: Collect2 done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spchtrVerbose];
			FCPrint[3, "SpinorChainTrick: After Collect2: ", res, FCDoControl->spchtrVerbose]
		];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "SpinorChainTrick: Leaving.", FCDoControl->spchtrVerbose];
		FCPrint[1, "SpinorChainTrick: Total time: ", N[AbsoluteTime[] - timeAll, 4], FCDoControl->spchtrVerbose];
		FCPrint[3, "SpinorChainTrick: Leaving with ", res, FCDoControl->spchtrVerbose];



		res

	]/; (!MemberQ[{Equal},expr] || (Head[expr]===List && OptionValue[FCDiracIsolate]===List));

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
