(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliChainJoin													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Joins chains of Pauli matrices with explicit Pauli
				indices into index free chains								*)

(* ------------------------------------------------------------------------ *)

PauliChainJoin::usage =
"PauliChainJoin[exp] joins chains of Pauli matrices with explicit Pauli indices
wrapped with a head PauliChain.";

PauliChainJoin::failmsg =
"Error! PauliChainJoin has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliChainJoin`Private`"]

pchjVerbose::usage="";
li::usage="";
li1::usage="";
li2::usage="";
li3::usage="";
li4::usage="";

holdDOT::usage="";
optTraceOfOne::usage="";

Options[PauliChainJoin] = {
	Drop			-> True,
	FCPauliIsolate	-> True,
	FCE 			-> False,
	FCI 			-> False,
	FCVerbose		-> False,
	Factoring		-> {Factor2, 5000},
	TraceOfOne		-> 4
};

PauliChainJoin[a_ == b_, opts:OptionsPattern[]] :=
	PauliChainJoin[a,opts] == PauliChainJoin[b,opts];

PauliChainJoin[expr_List, opts:OptionsPattern[]]:=
	PauliChainJoin[#, opts]&/@expr;

PauliChainJoin[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{	ex, tmp,  res, PauliObjects, PauliObjectsEval, null1, null2,
			psHead, time, repRule},

		optTraceOfOne	= OptionValue[TraceOfOne];

		If [OptionValue[FCVerbose]===False,
			pchjVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				pchjVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "PauliChainJoin. Entering.", FCDoControl->pchjVerbose];
		FCPrint[3, "PauliChainJoin: Entering with ", expr, FCDoControl->pchjVerbose];


		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex,{PauliChain,PauliIndexDelta}],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			pchjVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				pchjVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "PauliChainJoin: Isolating Pauli chains.", FCDoControl->pchjVerbose];
		time=AbsoluteTime[];

		If[	OptionValue[FCPauliIsolate],

			tmp = FCPauliIsolate[ex,FCI->True,Head->psHead, DotSimplify->False, PauliSigmaCombine->False, FCJoinDOTs-> False,
				LorentzIndex->False, PauliXi->False, PauliEta->False, PauliSigma->False, PauliChain->True,
				Factoring -> OptionValue[Factoring]];
			PauliObjects = Cases[tmp+null1+null2, psHead[_], Infinity]//Sort//DeleteDuplicates;
			FCPrint[1, "PauliChainJoin: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pchjVerbose];

			FCPrint[3, "PauliChainJoin: After FCPauliIsolate ", tmp, FCDoControl->pchjVerbose];
			FCPrint[3,"PauliChainJoin: PauliObjects: ", PauliObjects , FCDoControl->pchjVerbose];

			(*
			time=AbsoluteTime[];
			FCPrint[1, "PauliChainJoin: Checking the spinor syntax.", FCDoControl->pchjVerbose];
			If[	FeynCalc`Package`spinorSyntaxCorrectQ[PauliObjects]=!=True,
				Message[PauliChainJoin::failmsg, "The input contains Spinor objects with incorrect syntax."];
				Abort[]
			];
			FCPrint[1,"PauliChainJoin: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->pchjVerbose];
			*)

			FCPrint[1, "PauliChainJoin: Simplifying Pauli chains.", FCDoControl->pchjVerbose];
			time=AbsoluteTime[];
			PauliObjectsEval = Map[(PauliChainEval[#])&, (PauliObjects/.psHead->Identity/. DOT->holdDOT)]/.
				PauliChainEval -> Identity /. holdDOT->DOT;

			FCPrint[1, "PauliChainJoin: Done simplifying Pauli chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pchjVerbose];
			FCPrint[3, "PauliChainJoin: PauliObjectsEval: ", PauliObjectsEval, FCDoControl->pchjVerbose];

			If[ !FreeQ2[PauliObjectsEval,{PauliChainEval,holdDOT}],
				Message[PauliChainJoin::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];

			FCPrint[1, "PauliChainJoin: Inserting Pauli objects back.", FCDoControl->pchjVerbose];
			time=AbsoluteTime[];
			repRule = Thread[Rule[PauliObjects,PauliObjectsEval]];
			FCPrint[3,"PauliChainJoin: repRule: ",repRule , FCDoControl->pchjVerbose];
			res =  (tmp/. Dispatch[repRule]);
			FCPrint[1, "PauliChainJoin: Done inserting Pauli objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->pchjVerbose],

			(*Fast mode*)
			FCPrint[1, "PauliChainJoin: Using the fast mode.", FCDoControl->pchjVerbose];

			(*
			time=AbsoluteTime[];
			FCPrint[1, "PauliChainJoin: Checking the spinor syntax.", FCDoControl->pchjVerbose];
			If[	FeynCalc`Package`spinorSyntaxCorrectQ[ex]=!=True,
				Message[PauliChainJoin::failmsg, "The input contains Spinor objects with incorrect syntax."];
				Abort[]
			];
			FCPrint[1,"PauliChainJoin: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->pchjVerbose];
			*)

			res = PauliChainEval[ex/. DOT->holdDOT] /.  PauliChainEval -> Identity /. holdDOT->DOT ;
			FCPrint[3, "PauliChainJoin: Result of the evaluation: ", res, FCDoControl->pchjVerbose]
		];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "PauliChainJoin: Leaving.", FCDoControl->pchjVerbose];
		FCPrint[3, "PauliChainJoin: Leaving with ", res, FCDoControl->pchjVerbose];



		res

];

(* sbar_i A_ij*)
PauliChainEval[rest_. PauliChain[spinor : (_PauliXi | _PauliEta), i_PauliIndex] PauliChain[chain_, i_PauliIndex, j_]]:=
	PauliChainEval[rest PauliChain[chain,spinor,j]];

(* ... A_ij s_j *)
PauliChainEval[rest_. PauliChain[chain_, i:(_PauliIndex|_ExplicitPauliIndex|_PauliXi | _PauliEta), j_PauliIndex] PauliChain[j_PauliIndex, spinor : (_PauliXi | _PauliEta)]]:=
	PauliChainEval[rest PauliChain[chain,i,spinor]];

(* sbar_i A_ij s'_j *)
PauliChainEval[rest_. PauliChain[chain_, spinor1 : (_PauliXi | _PauliEta), spinor2 : (_PauliXi | _PauliEta)]]:=
	holdDOT[spinor1,chain,spinor2] PauliChainEval[rest];

(* (sbar.A)_i (B.s')_i -> sbar.A.B.s' *)
PauliChainEval[rest_. PauliChain[spinor1 : (_PauliXi | _PauliEta), i_PauliIndex] PauliChain[i_PauliIndex, spinor2 : (_PauliXi | _PauliEta)]]:=
	holdDOT[spinor1,spinor2] PauliChainEval[rest];

(* A_ii -> Tr(A) *)
PauliChainEval[rest_. PauliChain[chain_/;chain=!=1,i_PauliIndex,i_PauliIndex]]:=
	PauliTrace[chain] PauliChainEval[rest];

PauliChainEval[rest_. PauliChain[1,i_PauliIndex,i_PauliIndex]]:=
	optTraceOfOne PauliChainEval[rest];

(* A_*i* d_ij *)
PauliChainEval[rest_. PauliChain[a__,i_PauliIndex,b___] PauliIndexDelta[i_PauliIndex,j_PauliIndex]]:=
	PauliChainEval[rest PauliChain[a,j,b]];

(* A_ij B_jk -> (A.B)_ik *)
PauliChainEval[rest_. PauliChain[chain1_,a:(_PauliIndex|_ExplicitPauliIndex|_PauliXi | _PauliEta),i_PauliIndex] *
		PauliChain[chain2_,i_PauliIndex,b: (_PauliIndex|_ExplicitPauliIndex|_PauliXi | _PauliEta)]]:=
	PauliChainEval[rest PauliChain[holdDOT[chain1,chain2],a,b]]/; a=!=i && b=!=i;

(* d_ii -> Tr(1) *)
PauliChainEval[rest_. PauliIndexDelta[i_PauliIndex,i_PauliIndex]]:=
	optTraceOfOne PauliChainEval[rest];

(* d_ij d_jk -> d_ik *)
PauliChainEval[rest_. PauliIndexDelta[i_PauliIndex,j_PauliIndex] PauliIndexDelta[j_PauliIndex,k_PauliIndex]]:=
	PauliChainEval[rest PauliIndexDelta[i,k]];

(* d_ij^2 -> Tr(1) *)
PauliChainEval[rest_. PauliIndexDelta[i_PauliIndex, j_PauliIndex]^2]:=
	optTraceOfOne PauliChainEval[rest]/; i=!=j;

holdDOT[a___,1,b___]:=
	holdDOT[a,b];

holdDOT[]:=
	1;

FCPrint[1,"PauliChainJoin.m loaded"];
End[]
