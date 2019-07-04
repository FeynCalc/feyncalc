(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FermionicChainSimplify											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Simplifies chains of Dirac matrices with explicit Dirac
				indices														*)

(* ------------------------------------------------------------------------ *)

FermionicChainSimplify::usage =
"FermionicChainSimplify[exp] simplifies chains of Dirac matrices with explicit \
Dirac indices wrapped with a head FermionicChain.";

FermionicChainSimplify::failmsg =
"Error! FermionicChainSimplify has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FermionicChainSimplify`Private`"]

fchsVerbose::usage="";
li::usage="";
li1::usage="";
li2::usage="";
li3::usage="";
li4::usage="";

holdDOT::usage="";
optTraceOfOne::usage="";

Options[FermionicChainSimplify] = {
	Contract 					-> True,
	DiracGammaCombine			-> True,
	DiracSigmaExplicit			-> False,
	DiracTrick					-> True,
	FCCanonicalizeDummyIndices	->True,
	FCDiracIsolate 				-> True,
	FCE 						-> False,
	FCI 						-> False,
	FCJoinDOTs 					-> True,
	FCVerbose 					-> False,
	Factoring 					-> True,
	SpinorChainTrick			-> True,
	TraceOfOne					-> 4
};

FermionicChainSimplify[expr_, OptionsPattern[]] :=
	Block[{ex, tmp,  res, diracObjects, diracObjectsEval, null1, null2, dsHead, time, repRule, mode,
			optDiracSigmaExplicit, optDiracGammaCombine, optContract, optFCCanonicalizeDummyIndices
			},

		optDiracSigmaExplicit			= OptionValue[DiracSigmaExplicit];
		optDiracGammaCombine			= OptionValue[DiracGammaCombine];
		optContract						= OptionValue[Contract];
		optFCCanonicalizeDummyIndices	= OptionValue[FCCanonicalizeDummyIndices];
		optTraceOfOne					= OptionValue[TraceOfOne];

		FCPrint[1, "FermionicChainSimplify. Entering.", FCDoControl->fchsVerbose];
		FCPrint[3, "FermionicChainSimplify: Entering with ", expr, FCDoControl->fchsVerbose];


		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex,{FermionicChain,DiracIndexDelta}],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			fchsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fchsVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FermionicChainSimplify: Isolating fermionic chains.", FCDoControl->fchsVerbose];
		time=AbsoluteTime[];

		tmp = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->False, DiracGammaCombine->optDiracGammaCombine, FCJoinDOTs-> OptionValue[FCJoinDOTs],
			DiracSigmaExplicit->optDiracSigmaExplicit, LorentzIndex->False, Spinor->False, DiracGamma->False, FermionicChain->True,
			Factoring -> OptionValue[Factoring]];

		FCPrint[1, "FermionicChainSimplify: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fchsVerbose];
		FCPrint[3, "FermionicChainSimplify: After FCDiracIsolate ", tmp, FCDoControl->fchsVerbose];


		diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
		FCPrint[3,"FermionicChainSimplify: diracObjects: ", diracObjects , FCDoControl->fchsVerbose];


		FCPrint[1, "FermionicChainSimplify: Simplifying fermionic chains.", FCDoControl->fchsVerbose];
		time=AbsoluteTime[];
		diracObjectsEval = Map[(fermionicChainEval[#])&, (diracObjects/.dsHead->Identity/. DOT->holdDOT)]/.
			fermionicChainEval -> Identity /. holdDOT->DOT;

		FCPrint[1, "FermionicChainSimplify: Done simplifying fermionic chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fchsVerbose];
		FCPrint[3, "FermionicChainSimplify: diracObjectsEval: ", diracObjectsEval, FCDoControl->fchsVerbose];

		If[ !FreeQ2[diracObjectsEval,{fermionicChainEval,holdDOT}],
			Message[FermionicChainSimplify::failmsg,"Evaluation of isolated objects failed."];
			Abort[]
		];

		FCPrint[1, "FermionicChainSimplify: Inserting Dirac objects back.", FCDoControl->fchsVerbose];
		time=AbsoluteTime[];
		repRule = Thread[Rule[diracObjects,diracObjectsEval]];
		FCPrint[3,"FermionicChainSimplify: repRule: ",repRule , FCDoControl->fchsVerbose];
		res =  (tmp/. Dispatch[repRule]);
		FCPrint[1, "FermionicChainSimplify: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fchsVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FermionicChainSimplify: Leaving.", FCDoControl->fchsVerbose];
		FCPrint[3, "FermionicChainSimplify: Leaving with ", res, FCDoControl->fchsVerbose];



		res

];

fermionicChainEval[rest_. FermionicChain[spinor_, i_DiracIndex] FermionicChain[chain_, i_DiracIndex, j_]]:=
	fermionicChainEval[rest FermionicChain[chain,spinor,j]];

fermionicChainEval[rest_. FermionicChain[chain_, i_, j_DiracIndex] FermionicChain[j_DiracIndex, spinor_]]:=
	fermionicChainEval[rest FermionicChain[chain,i,spinor]];

fermionicChainEval[rest_. FermionicChain[1, spinor1_Spinor, spinor2_Spinor]]:=
	holdDOT[spinor1,spinor2] fermionicChainEval[rest];

fermionicChainEval[rest_. FermionicChain[chain_/;chain=!=1, spinor1_Spinor, spinor2_Spinor]]:=
	holdDOT[spinor1,chain,spinor2] fermionicChainEval[rest];

fermionicChainEval[rest_. FermionicChain[spinor1_, i_DiracIndex] FermionicChain[i_DiracIndex, spinor2_]]:=
	holdDOT[spinor1,spinor2] fermionicChainEval[rest];

fermionicChainEval[rest_. FermionicChain[chain_,i_DiracIndex,i_DiracIndex]]:=
	DiracTrace[chain] fermionicChainEval[rest];

fermionicChainEval[rest_. FermionicChain[a__,i_DiracIndex,b___] DiracIndexDelta[i_DiracIndex,j_DiracIndex]]:=
	fermionicChainEval[rest FermionicChain[a,j,b]];

fermionicChainEval[rest_. FermionicChain[chain1_,a_,i_DiracIndex] FermionicChain[chain2_,i_DiracIndex,b_]]:=
	fermionicChainEval[rest FermionicChain[holdDOT[chain1,chain2],a,b]]/; a=!=i && b=!=i;

fermionicChainEval[rest_. DiracIndexDelta[i_DiracIndex,i_DiracIndex]]:=
	optTraceOfOne fermionicChainEval[rest];

fermionicChainEval[rest_. DiracIndexDelta[i_DiracIndex,j_DiracIndex] DiracIndexDelta[j_DiracIndex,k_DiracIndex]]:=
	fermionicChainEval[rest DiracIndexDelta[i,k]];

fermionicChainEval[rest_. DiracIndexDelta[i_DiracIndex, j_DiracIndex]^2]:=
	optTraceOfOne fermionicChainEval[rest]/; i=!=j;


FCPrint[1,"FermionicChainSimplify.m loaded"];
End[]
