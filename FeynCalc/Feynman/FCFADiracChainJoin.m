(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFADiracChainJoin												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Rebuilds Dirac chains for 4-fermion operators					*)

(* ------------------------------------------------------------------------ *)

FCFADiracChainJoin::usage =
"FCFADiracChainJoin[exp] processes the output of FeynArts (after FCFAConvert)
with explicit Dirac indices and joins matrices and spinors into closed chains.
This is necessary e. g. for models with 4-fermion operators, where FeynArts
cannot determine the correct relative signs. When two matrices have a common
index but the positions do not match, as in $A_{ij} B_{ik}$, it is assumed
that we can take the charge conjugate transposed of either matrix to obtain,
e.g. $\\left(C A^T C^{-1}\\right)_{ji} B_{ik}$ or $\\left(C B^TC^{-1}\\right)_{ki}
A_{ij}$.";

FCFADiracChainJoin::failmsg =
"Error! FCFADiracChainJoin has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

FCFADiracChainJoin::indexsum =
"Cannot process input expressions with unresolved FeynArts index sums. If this error appears when running FCFAConvert, \
please set the option FCFADiracChainJoin to False and check the output with uncontracted Dirac indices to identify the \
unresolved IndexSum objects.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCFADiracChainJoin`Private`"]

dchjVerbose::usage="";
ordering::usage="";
optHead::usage="";
optFirst::usage="";



Options[FCFADiracChainJoin] = {
	FCE 			-> False,
	FCI 			-> False,
	FCVerbose		-> False,
	First			-> {},
	Head			-> Identity
};

FCFADiracChainJoin[expr_, OptionsPattern[]] :=
	Block[{	ex, tmp,  res, diracObjects, diracObjectsEval, null1, null2,
			dsHead, time, repRule},

		optHead = OptionValue[Head];
		optFirst = FCI[OptionValue[First]];

		If[	Head[optFirst]=!=List,
			Message[FCFADiracChainJoin::failmsg,"The value of the option First must be a list."];
			Abort[]
		];

		If [OptionValue[FCVerbose]===False,
			dchjVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				dchjVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FCFADiracChainJoin. Entering.", FCDoControl->dchjVerbose];
		FCPrint[3, "FCFADiracChainJoin: Entering with ", expr, FCDoControl->dchjVerbose];


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

		FCPrint[1, "FCFADiracChainJoin: Isolating Dirac chains.", FCDoControl->dchjVerbose];
		time=AbsoluteTime[];

		tmp = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->False, DiracGammaCombine->False, FCJoinDOTs-> False,
			DiracSigmaExplicit->False, LorentzIndex->False, Spinor->False, DiracGamma->False, DiracChain->True,
			Factoring -> False, Expanding->False];
		diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
		FCPrint[1, "FCFADiracChainJoin: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dchjVerbose];
		FCPrint[3, "FCFADiracChainJoin: After FCDiracIsolate ", tmp, FCDoControl->dchjVerbose];

		FCPrint[3,"FCFADiracChainJoin: diracObjects: ", diracObjects , FCDoControl->dchjVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FermionSpinSum: Checking the spinor syntax.", FCDoControl->dchjVerbose];
		If[	FeynCalc`Package`spinorSyntaxCorrectQ[diracObjects]=!=True,
			Message[FCFADiracChainJoin::failmsg, "The input contains Spinor objects with incorrect syntax."];
			Abort[]
		];
		FCPrint[1,"FermionSpinSum: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->dchjVerbose];

		If[	!FreeQ[diracObjects,FeynArts`IndexSum],
			Message[FCFADiracChainJoin::indexsum];
			Abort[]
		];


		FCPrint[1, "FCFADiracChainJoin: Joining Dirac chains.", FCDoControl->dchjVerbose];
		time=AbsoluteTime[];
		diracObjectsEval = Map[(diracChainEvalM[#])&, (diracObjects/.dsHead->Identity)];

		FCPrint[3, "FCFADiracChainJoin: After diracChainEvalM: ", diracObjectsEval, FCDoControl->dchjVerbose];

		diracObjectsEval = diracObjectsEval /. diracChainEvalM -> diracChainEvalS /. diracChainEvalS -> diracChainEvalSign;

		FCPrint[1, "FCFADiracChainJoin: Done simplifying Dirac chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dchjVerbose];
		FCPrint[3, "FCFADiracChainJoin: diracObjectsEval: ", diracObjectsEval, FCDoControl->dchjVerbose];

		If[ !FreeQ2[diracObjectsEval,{diracChainEvalSign,FCChargeConjugateTransposed,DiracChain,DiracIndex}],
			Message[FCFADiracChainJoin::failmsg,"Evaluation of isolated objects failed."];
			Abort[]
		];

		FCPrint[1, "FCFADiracChainJoin: Inserting Dirac objects back.", FCDoControl->dchjVerbose];
		time=AbsoluteTime[];
		repRule = Thread[Rule[diracObjects,diracObjectsEval]];
		FCPrint[3,"FCFADiracChainJoin: repRule: ",repRule , FCDoControl->dchjVerbose];
		res =  ( tmp/. Dispatch[repRule]);
		FCPrint[1, "FCFADiracChainJoin: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dchjVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FCFADiracChainJoin: Leaving.", FCDoControl->dchjVerbose];
		FCPrint[3, "FCFADiracChainJoin: Leaving with ", res, FCDoControl->dchjVerbose];



		res

];

(* A_*i* d_ij *)
diracChainEvalM[rest_. DiracChain[a__,i_DiracIndex,b___] DiracIndexDelta[i_DiracIndex,j_DiracIndex]]:=
	diracChainEvalM[rest DiracChain[a,j,b]];

(* A_ij B_jk -> (A.B)_ik *)
diracChainEvalM[rest_. DiracChain[chain1_,a_DiracIndex,i_DiracIndex] DiracChain[chain2_,i_DiracIndex,b_DiracIndex]]:=
	diracChainEvalM[rest DiracChain[DOT[chain1,chain2],a,b]]/; a=!=i && b=!=i;

(* A_ai B_bi -> (A. C B^T C^-1)_ab *)
diracChainEvalM[rest_. DiracChain[chain1_,a_DiracIndex,i_DiracIndex] DiracChain[chain2_,b_DiracIndex,i_DiracIndex]]:=
	diracChainEvalM[rest DiracChain[DOT[chain1,FCCCT[chain2, Explicit->True, FCDiracIsolate->True, FCI->True]],a,b]]/; a=!=i && b=!=i;

(* A_ia B_ib -> (C A^T C^-1)_ab *)
diracChainEvalM[rest_. DiracChain[chain1_,i_DiracIndex,a_DiracIndex] DiracChain[chain2_,i_DiracIndex,b_DiracIndex]]:=
	diracChainEvalM[rest DiracChain[DOT[FCCCT[chain1, Explicit->True, FCDiracIsolate->True, FCI->True],chain2],a,b]]/; a=!=i && b=!=i;

(* A_ii -> Tr(A) *)
diracChainEvalM[rest_. DiracChain[chain_/;chain=!=1,i_DiracIndex,i_DiracIndex]]:=
	DiracTrace[chain] diracChainEvalM[rest];

diracChainEvalM[rest_. DiracChain[1,i_DiracIndex,i_DiracIndex]]:=
	DiracTrace[1] diracChainEvalM[rest];

(* d_ii -> Tr(1) *)
diracChainEvalM[rest_. DiracIndexDelta[i_DiracIndex,i_DiracIndex]]:=
	DiracTrace[1] diracChainEvalM[rest];

(* d_ij d_jk -> d_ik *)
diracChainEvalM[rest_. DiracIndexDelta[i_DiracIndex,j_DiracIndex] DiracIndexDelta[j_DiracIndex,k_DiracIndex]]:=
	diracChainEvalM[rest DiracIndexDelta[i,k]];

(* d_ij^2 -> Tr(1) *)
diracChainEvalM[rest_. DiracIndexDelta[i_DiracIndex, j_DiracIndex]^2]:=
	DiracTrace[1] diracChainEvalM[rest]/; i=!=j;

(* u_i v_j A_ij -> ubar.A.v or vbar.A.u *)
diracChainEvalS[rest_. DiracChain[S: Spinor[_. m1_Momentum, ___], a_DiracIndex] DiracChain[z_, a_DiracIndex, b_DiracIndex] DiracChain[Spinor[s_. m2_Momentum, r___], b_DiracIndex]]:=
	diracChainEvalS[rest ordering[First[m1],First[m2]]] DOT[S, z, Spinor[-s m2, r]]/; optFirst==={} || MemberQ[optFirst, S];

(* u_i v_j A_ij -> ubar.A.v or vbar.A.u *)
diracChainEvalS[rest_. DiracChain[S: Spinor[_. m1_Momentum, ___], a_DiracIndex] DiracChain[z_, a_DiracIndex, b_DiracIndex] DiracChain[Spinor[s_. m2_Momentum, r___], b_DiracIndex]]:=
	diracChainEvalS[rest ordering[First[m2],First[m1]]] FCCCT[DOT[S, z, Spinor[-s m2, r]], Explicit->True, FCDiracIsolate->True, FCI->True]/; MemberQ[optFirst, Spinor[s m2, r]];

diracChainEvalS[rest_. ordering[a__] ordering[b__]]:=
	diracChainEvalS[rest ordering[a,b]];

(*fixes the relative sign checking the orderings of the external fermions. This
essentially reproduces the corresponding code in FormCalc. *)
diracChainEvalSign[ordering[a__]]:=
	optHead[Signature[{a}] (-1)^(Length[{a}]/2)];

FCPrint[1,"FCFADiracChainJoin.m loaded"];
End[]
