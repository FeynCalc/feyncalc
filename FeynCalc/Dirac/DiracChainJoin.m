(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracChainJoin													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Joins chains of Dirac matrices with explicit Dirac
				indices into index free chains								*)

(* ------------------------------------------------------------------------ *)

DiracChainJoin::usage =
"DiracChainJoin[exp] joins chains of Dirac matrices with explicit Dirac indices
wrapped with a head DiracChain. Notice that DiracChainJoin is not suitable for
creating closed Dirac chains out of the FeynArts output with explicit Dirac
indices, e.g. when the model contains 4-fermion operators. Use
FCFADiracChainJoin for that.";

DiracChainJoin::failmsg =
"Error! DiracChainJoin has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracChainJoin`Private`"]

dchjVerbose::usage="";
li::usage="";
li1::usage="";
li2::usage="";
li3::usage="";
li4::usage="";

holdDOT::usage="";
optTraceOfOne::usage="";

Options[DiracChainJoin] = {
	Drop			-> True,
	FCDiracIsolate	-> True,
	FCE 			-> False,
	FCI 			-> False,
	FCVerbose		-> False,
	Factoring		-> {Factor2, 5000},
	TraceOfOne		-> 4
};

DiracChainJoin[a_ == b_, opts:OptionsPattern[]] :=
	DiracChainJoin[a,opts] == DiracChainJoin[b,opts];

DiracChainJoin[expr_List, opts:OptionsPattern[]]:=
	DiracChainJoin[#, opts]&/@expr;

DiracChainJoin[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{	ex, tmp,  res, diracObjects, diracObjectsEval, null1, null2,
			dsHead, time, repRule},

		optTraceOfOne	= OptionValue[TraceOfOne];

		If [OptionValue[FCVerbose]===False,
			dchjVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				dchjVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "DiracChainJoin. Entering.", FCDoControl->dchjVerbose];
		FCPrint[3, "DiracChainJoin: Entering with ", expr, FCDoControl->dchjVerbose];


		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex,{DiracChain,DiracIndexDelta}],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			dchjVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				dchjVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "DiracChainJoin: Isolating Dirac chains.", FCDoControl->dchjVerbose];
		time=AbsoluteTime[];

		If[	OptionValue[FCDiracIsolate],

			tmp = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->False, DiracGammaCombine->False, FCJoinDOTs-> False,
				DiracSigmaExplicit->False, LorentzIndex->False, Spinor->False, DiracGamma->False, DiracChain->True,
				Factoring -> OptionValue[Factoring]];
			diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
			FCPrint[1, "DiracChainJoin: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dchjVerbose];

			FCPrint[3, "DiracChainJoin: After FCDiracIsolate ", tmp, FCDoControl->dchjVerbose];
			FCPrint[3,"DiracChainJoin: diracObjects: ", diracObjects , FCDoControl->dchjVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "DiracChainJoin: Checking the spinor syntax.", FCDoControl->dchjVerbose];
			If[	FeynCalc`Package`spinorSyntaxCorrectQ[diracObjects]=!=True,
				Message[DiracChainJoin::failmsg, "The input contains Spinor objects with incorrect syntax."];
				Abort[]
			];
			FCPrint[1,"DiracChainJoin: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->dchjVerbose];


			FCPrint[1, "DiracChainJoin: Simplifying Dirac chains.", FCDoControl->dchjVerbose];
			time=AbsoluteTime[];
			diracObjectsEval = Map[(diracChainEval[#])&, (diracObjects/.dsHead->Identity/. DOT->holdDOT)]/.
				diracChainEval -> Identity /. holdDOT->DOT;

			FCPrint[1, "DiracChainJoin: Done simplifying Dirac chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dchjVerbose];
			FCPrint[3, "DiracChainJoin: diracObjectsEval: ", diracObjectsEval, FCDoControl->dchjVerbose];

			If[ !FreeQ2[diracObjectsEval,{diracChainEval,holdDOT}],
				Message[DiracChainJoin::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];

			FCPrint[1, "DiracChainJoin: Inserting Dirac objects back.", FCDoControl->dchjVerbose];
			time=AbsoluteTime[];
			repRule = Thread[Rule[diracObjects,diracObjectsEval]];
			FCPrint[3,"DiracChainJoin: repRule: ",repRule , FCDoControl->dchjVerbose];
			res =  (tmp/. Dispatch[repRule]);
			FCPrint[1, "DiracChainJoin: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dchjVerbose],

			(*Fast mode*)
			FCPrint[1, "DiracChainJoin: Using the fast mode.", FCDoControl->dchjVerbose];


			time=AbsoluteTime[];
			FCPrint[1, "DiracChainJoin: Checking the spinor syntax.", FCDoControl->dchjVerbose];
			If[	FeynCalc`Package`spinorSyntaxCorrectQ[ex]=!=True,
				Message[DiracChainJoin::failmsg, "The input contains Spinor objects with incorrect syntax."];
				Abort[]
			];
			FCPrint[1,"DiracChainJoin: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->dchjVerbose];


			res = diracChainEval[ex/. DOT->holdDOT] /.  diracChainEval -> Identity /. holdDOT->DOT ;
			FCPrint[3, "DiracChainJoin: Result of the evaluation: ", res, FCDoControl->dchjVerbose]
		];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "DiracChainJoin: Leaving.", FCDoControl->dchjVerbose];
		FCPrint[3, "DiracChainJoin: Leaving with ", res, FCDoControl->dchjVerbose];



		res

];

(* sbar_i A_ij*)
diracChainEval[rest_. DiracChain[spinor_Spinor, i_DiracIndex] DiracChain[chain_, i_DiracIndex, j_]]:=
	diracChainEval[rest DiracChain[chain,spinor,j]];

(* ... A_ij s_j *)
diracChainEval[rest_. DiracChain[chain_, i:(_DiracIndex|_ExplicitDiracIndex|_Spinor), j_DiracIndex] DiracChain[j_DiracIndex, spinor_Spinor]]:=
	diracChainEval[rest DiracChain[chain,i,spinor]];

(* sbar_i A_ij s'_j *)
diracChainEval[rest_. DiracChain[chain_, spinor1_Spinor, spinor2_Spinor]]:=
	holdDOT[spinor1,chain,spinor2] diracChainEval[rest];

(* (sbar.A)_i (B.s')_i -> sbar.A.B.s' *)
diracChainEval[rest_. DiracChain[spinor1_Spinor, i_DiracIndex] DiracChain[i_DiracIndex, spinor2_Spinor]]:=
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
diracChainEval[rest_. DiracChain[chain1_,a:(_DiracIndex|_ExplicitDiracIndex|_Spinor),i_DiracIndex] *
		DiracChain[chain2_,i_DiracIndex,b: (_DiracIndex|_ExplicitDiracIndex|_Spinor)]]:=
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

FCPrint[1,"DiracChainJoin.m loaded"];
End[]
