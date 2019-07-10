(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracChainSimplify											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Simplifies chains of Dirac matrices with explicit Dirac
				indices														*)

(* ------------------------------------------------------------------------ *)

DiracChainSimplify::usage =
"DiracChainSimplify[exp] simplifies chains of Dirac matrices with explicit \
Dirac indices wrapped with a head DiracChain.";

DiracChainSimplify::failmsg =
"Error! DiracChainSimplify has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracChainSimplify`Private`"]

fchsVerbose::usage="";
li::usage="";
li1::usage="";
li2::usage="";
li3::usage="";
li4::usage="";

holdDOT::usage="";
optTraceOfOne::usage="";

Options[DiracChainSimplify] = {
	Contract -> True,
	DiracGammaCombine -> True,
	DiracSigmaExplicit -> False,
	DiracTrick -> True,
	Factoring -> True,
	FCCanonicalizeDummyIndices->True,
	FCDiracIsolate -> True,
	FCE -> False,
	FCI -> False,
	FCJoinDOTs -> True,
	FCVerbose -> False,
	SpinorChainTrick -> True,
	TraceOfOne -> 4
};

DiracChainSimplify[expr_, OptionsPattern[]] :=
	Block[{ex, tmp,  res, diracObjects, diracObjectsEval, null1, null2, dsHead, time, repRule, mode,
			optDiracSigmaExplicit, optDiracGammaCombine, optContract, optFCCanonicalizeDummyIndices
			},

		optDiracSigmaExplicit			= OptionValue[DiracSigmaExplicit];
		optDiracGammaCombine			= OptionValue[DiracGammaCombine];
		optContract						= OptionValue[Contract];
		optFCCanonicalizeDummyIndices	= OptionValue[FCCanonicalizeDummyIndices];
		optTraceOfOne					= OptionValue[TraceOfOne];

		FCPrint[1, "DiracChainSimplify. Entering.", FCDoControl->fchsVerbose];
		FCPrint[3, "DiracChainSimplify: Entering with ", expr, FCDoControl->fchsVerbose];


		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex,{DiracChain,DiracIndexDelta}],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			fchsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				fchsVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "DiracChainSimplify: Isolating Dirac chains.", FCDoControl->fchsVerbose];
		time=AbsoluteTime[];

		tmp = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->False, DiracGammaCombine->optDiracGammaCombine, FCJoinDOTs-> OptionValue[FCJoinDOTs],
			DiracSigmaExplicit->optDiracSigmaExplicit, LorentzIndex->False, Spinor->False, DiracGamma->False, DiracChain->True,
			Factoring -> OptionValue[Factoring]];

		FCPrint[1, "DiracChainSimplify: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fchsVerbose];
		FCPrint[3, "DiracChainSimplify: After FCDiracIsolate ", tmp, FCDoControl->fchsVerbose];


		diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
		FCPrint[3,"DiracChainSimplify: diracObjects: ", diracObjects , FCDoControl->fchsVerbose];


		FCPrint[1, "DiracChainSimplify: Simplifying Dirac chains.", FCDoControl->fchsVerbose];
		time=AbsoluteTime[];
		diracObjectsEval = Map[(diracChainEval[#])&, (diracObjects/.dsHead->Identity/. DOT->holdDOT)]/.
			diracChainEval -> Identity /. holdDOT->DOT;

		FCPrint[1, "DiracChainSimplify: Done simplifying Dirac chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fchsVerbose];
		FCPrint[3, "DiracChainSimplify: diracObjectsEval: ", diracObjectsEval, FCDoControl->fchsVerbose];

		If[ !FreeQ2[diracObjectsEval,{diracChainEval,holdDOT}],
			Message[DiracChainSimplify::failmsg,"Evaluation of isolated objects failed."];
			Abort[]
		];

		FCPrint[1, "DiracChainSimplify: Inserting Dirac objects back.", FCDoControl->fchsVerbose];
		time=AbsoluteTime[];
		repRule = MapThread[Rule[#1,#2]&,{diracObjects,diracObjectsEval}];
		FCPrint[3,"DiracChainSimplify: repRule: ",repRule , FCDoControl->fchsVerbose];
		res =  ( tmp/.repRule);
		FCPrint[1, "DiracChainSimplify: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fchsVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "DiracChainSimplify: Leaving.", FCDoControl->fchsVerbose];
		FCPrint[3, "DiracChainSimplify: Leaving with ", res, FCDoControl->fchsVerbose];



		res

];

(* sbar_i A_ij*)
diracChainEval[rest_. DiracChain[spinor_Spinor, i_DiracIndex] DiracChain[chain_, i_DiracIndex, j_]]:=
	diracChainEval[rest DiracChain[chain,spinor,j]];

(* sbar_j A_ij - special syntax for FeynArts *)
diracChainEval[rest_. DiracChain[spinor_Spinor, j_DiracIndex] DiracChain[chain_, x_, j_DiracIndex]]:=
	diracChainEval[rest DiracChain[chain,x,spinor]];

(* A_ij s_j *)
diracChainEval[rest_. DiracChain[chain_, i_, j_DiracIndex] DiracChain[j_DiracIndex, spinor_]]:=
	diracChainEval[rest DiracChain[chain,i,spinor]];

(* sbar_i A_ij s'_j *)
diracChainEval[rest_. DiracChain[chain_, spinor1_Spinor, spinor2_Spinor]]:=
	holdDOT[spinor1,chain,spinor2] diracChainEval[rest];

(* (sbar.A)_i (B.s')_i -> sbar.A.B.s' *)
diracChainEval[rest_. DiracChain[spinor1_, i_DiracIndex] DiracChain[i_DiracIndex, spinor2_]]:=
	holdDOT[spinor1,spinor2] diracChainEval[rest];

(* A_ii -> Tr(A) *)
diracChainEval[rest_. DiracChain[chain_/;chain=!=1,i_DiracIndex,i_DiracIndex]]:=
	DiracTrace[chain] diracChainEval[rest];

diracChainEval[rest_. DiracChain[1,i_DiracIndex,i_DiracIndex]]:=
	optTraceOfOne diracChainEval[rest];

(* A_*i* d_ij *)
diracChainEval[rest_. DiracChain[a__,i_DiracIndex,b___] DiracIndexDelta[i_DiracIndex,j_DiracIndex]]:=
	diracChainEval[rest DiracChain[a,j,b]];

(* A_ij B_jk -> (A.B)_ik *)
diracChainEval[rest_. DiracChain[chain1_,a_,i_DiracIndex] DiracChain[chain2_,i_DiracIndex,b_]]:=
	diracChainEval[rest DiracChain[holdDOT[chain1,chain2],a,b]]/; a=!=i && b=!=i;

(* d_ii -> Tr(1) *)
diracChainEval[rest_. DiracIndexDelta[i_DiracIndex,i_DiracIndex]]:=
	optTraceOfOne diracChainEval[rest];

(* d_ij d_jk -> d_ik *)
diracChainEval[rest_. DiracIndexDelta[i_DiracIndex,j_DiracIndex] DiracIndexDelta[j_DiracIndex,k_DiracIndex]]:=
	diracChainEval[rest DiracIndexDelta[i,k]];

(* d_ij^2 -> Tr(1) *)
diracChainEval[rest_. DiracIndexDelta[i_DiracIndex, j_DiracIndex]^2]:=
	optTraceOfOne diracChainEval[rest]/; i=!=j;

holdDOT[a___,1,b___]:=
	holdDOT[a,b];

holdDOT[]:=
	1;

FCPrint[1,"DiracChainSimplify.m loaded"];
End[]
