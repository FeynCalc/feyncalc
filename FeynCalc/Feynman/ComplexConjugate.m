(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ComplexConjugate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  Construct the complex conjugate amplitude						*)

(* ------------------------------------------------------------------------ *)

ComplexConjugate::usage =
"ComplexConjugate[exp] returns the complex conjugate of exp, where the input \
expression must be a proper matrix element. In particular, all Dirac \
matrices are assumed to be inside closed Dirac spinor chains. If this is not \
the case, the result will be inconsistent. Futhermore, denominators may not contain \
explicit I's!";

ComplexConjugate::failmsg =
"Error! ComplexConjugate has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`ComplexConjugate`Private`"]

ccjVerbose::usage="";
holdDOT::usage="";
holdDOTReversed::usage="";


(* for large expressions it is better to not use DotSimplify *)
Options[ComplexConjugate] = {
	Conjugate				-> {},
	DotSimplify				-> True,
	FCE						-> False,
	FCI						-> False,
	FCRenameDummyIndices	-> True,
	FCVerbose				-> False
};

(*TODO Check denominators using FCExtractDenominatorFactors*)

ComplexConjugate[expr_List, opts:OptionsPattern[]]:=
	ComplexConjugate[#,opts]&/@expr;

ComplexConjugate[expr_/;Head[expr]=!=List, OptionsPattern[]]:=
	Block[{	ex,res,optConjugate, ruleConjugate, ru, time,
			prefList, diracList, sunList, pauliList,
			prefHead, pauliHead, sunHead, diracHead,
			prefListEval, diracListEval, sunListEval, pauliListEval,
			repRule, ccRules},

		optConjugate	= OptionValue[Conjugate];

		If [OptionValue[FCVerbose]===False,
			ccjVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				ccjVerbose=OptionValue[FCVerbose]
			];
		];


		FCPrint[1,"ComplexConjugate: Entering.", FCDoControl->ccjVerbose];
		FCPrint[3,"ComplexConjugate: Entering with: ", expr, FCDoControl->ccjVerbose];

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		time = AbsoluteTime[];
		FCPrint[1,"ComplexConjugate: Applying FCMatrixIsolate.", FCDoControl->ccjVerbose];
		ex = FCMatrixIsolate[ex,FCI->True, FCColorIsolate->{sunHead}, FCDiracIsolate->{diracHead,
			{FCI->True, DiracChain->True, Expanding->False, FCJoinDOTs->False, DiracSigmaExplicit->True, DiracGammaCombine->False}},
			FCPauliIsolate->{pauliHead}, Head->prefHead];
		FCPrint[1,"ComplexConjugate: Done applying FCMatrixIsolate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->ccjVerbose];
		FCPrint[3,"ComplexConjugate: After FCMatrixIsolate: ", ex, FCDoControl->ccjVerbose];

		(*
			If the matrix structure of the expression is too convoluted, we need to redo the isolation after applying
			DotSimplify (this time with Expanding set to True)
		*)
		time = AbsoluteTime[];
		FCPrint[1,"ComplexConjugate: Applying additional isolations.", FCDoControl->ccjVerbose];
		ex = ex /. {
			diracHead[z_]/; !FreeQ2[z,Join[FeynCalc`Package`PauliHeadsList,FeynCalc`Package`SUNHeadsList]] :>
			FCMatrixIsolate[DotSimplify[z,FCI->True],FCI->True, FCColorIsolate->{sunHead}, FCDiracIsolate->{diracHead,
			{FCI->True, DiracChain->True, Expanding->False, FCJoinDOTs->False, DiracSigmaExplicit->True, DiracGammaCombine->False}},
			FCPauliIsolate->{pauliHead}, Head->prefHead]
		} /. {
			pauliHead[z_]/; !FreeQ2[z,Join[FeynCalc`Package`DiracHeadsList,FeynCalc`Package`SUNHeadsList,{FCChargeConjugateTransposed}]] :>
			FCMatrixIsolate[DotSimplify[z,FCI->True],FCI->True, FCColorIsolate->{sunHead}, FCDiracIsolate->{diracHead,
			{FCI->True, DiracChain->True, Expanding->False, FCJoinDOTs->False, DiracSigmaExplicit->True, DiracGammaCombine->False}},
			FCPauliIsolate->{pauliHead}, Head->prefHead]
		} /. {
			sunHead[z_]/; !FreeQ2[z,Join[FeynCalc`Package`DiracHeadsList,FeynCalc`Package`PauliHeadsList]] :>
			FCMatrixIsolate[DotSimplify[z,FCI->True],FCI->True, FCColorIsolate->{sunHead}, FCDiracIsolate->{diracHead,
			{FCI->True, DiracChain->True, Expanding->False, FCJoinDOTs->False, DiracSigmaExplicit->True, DiracGammaCombine->False}},
			FCPauliIsolate->{pauliHead}, Head->prefHead]
		} /. {
			prefHead[z_]/; !FreeQ2[z,Join[FeynCalc`Package`DiracHeadsList,FeynCalc`Package`PauliHeadsList,FeynCalc`Package`SUNHeadsList,{DOT}]] :>
			FCMatrixIsolate[DotSimplify[z,FCI->True],FCI->True, FCColorIsolate->{sunHead}, FCDiracIsolate->{diracHead,
			{FCI->True, DiracChain->True, Expanding->False, FCJoinDOTs->False, DiracSigmaExplicit->True, DiracGammaCombine->False}},
			FCPauliIsolate->{pauliHead}, Head->prefHead]
		};
		FCPrint[1,"ComplexConjugate: Done applying additional isolations, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->ccjVerbose];
		FCPrint[3,"ComplexConjugate: After additional isolations: ", ex, FCDoControl->ccjVerbose];

		prefList	= Cases2[ex,prefHead];
		diracList	= Cases2[ex,diracHead];
		pauliList	= Cases2[ex,pauliHead];
		sunList		= Cases2[ex,sunHead];

		(*Some checks*)
		If[	!FreeQ2[prefList,Join[FeynCalc`Package`DiracHeadsList,FeynCalc`Package`PauliHeadsList,FeynCalc`Package`SUNHeadsList,{DOT}]],
			Message[ComplexConjugate::failmsg, "The list of prefactors contains forbidden objects"];
			Abort[]
		];

		If[	!FreeQ2[diracList,Join[FeynCalc`Package`PauliHeadsList,FeynCalc`Package`SUNHeadsList]],
			Message[ComplexConjugate::failmsg, "The list of Dirac chains contains forbidden objects"];
			Abort[]
		];

		If[	!FreeQ2[pauliList,Join[FeynCalc`Package`DiracHeadsList,FeynCalc`Package`SUNHeadsList,{FCChargeConjugateTransposed}]],
			Message[ComplexConjugate::failmsg, "The list of Dirac chains contains forbidden objects"];
			Abort[]
		];

		If[	!FreeQ2[sunList,Join[FeynCalc`Package`DiracHeadsList,FeynCalc`Package`PauliHeadsList]],
			Message[ComplexConjugate::failmsg, "The list of SU(N) chains contains forbidden objects"];
			Abort[]
		];

		diracListEval	= (diracChainCC	/@	(diracList	/. DOT->holdDOT /. diracHead	-> Identity)) /. {holdDOTReversed-> DOT, diracGammaHold->DiracGamma};

		FCPrint[3,"ComplexConjugate: After diracChainCC: ", diracListEval, FCDoControl->ccjVerbose];

		pauliListEval	= (pauliChainCC	/@	(pauliList	/. DOT->holdDOT /. pauliHead	-> Identity)) /. holdDOTReversed-> DOT;

		FCPrint[3,"ComplexConjugate: After pauliChainCC: ", pauliListEval, FCDoControl->ccjVerbose];

		sunListEval		= (sunChainCC	/@	(sunList	/. DOT->holdDOT /. sunHead		-> Identity)) /. holdDOTReversed-> DOT;

		FCPrint[3,"ComplexConjugate: After sunChainCC: ", sunListEval, FCDoControl->ccjVerbose];

		prefListEval	= prefCC	/@	(prefList /. prefHead -> Identity);

		FCPrint[3,"ComplexConjugate: After prefCC: ", prefListEval, FCDoControl->ccjVerbose];

		If[ OptionValue[DotSimplify],
			time = AbsoluteTime[];
			FCPrint[1,"ComplexConjugate: Applying DotSimplify.", FCDoControl->ccjVerbose];
			diracListEval = DotSimplify[#,FCI->True, Expanding->False]&/@diracListEval;
			pauliListEval = DotSimplify[#,FCI->True, Expanding->False]&/@pauliListEval;
			sunListEval = DotSimplify[#,FCI->True, Expanding->False]&/@sunListEval;
			FCPrint[1,"ComplexConjugate: Done applying DotSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->ccjVerbose];
			FCPrint[3,"ComplexConjugate: Dirac list after DotSimplify: ", diracListEval, FCDoControl->ccjVerbose];
			FCPrint[3,"ComplexConjugate: Pauli list after DotSimplify: ", pauliListEval, FCDoControl->ccjVerbose];
			FCPrint[3,"ComplexConjugate: SU(N) list after DotSimplify: ", sunListEval, FCDoControl->ccjVerbose];
		];



		repRule = Join[
			Thread[Rule[diracList,diracListEval]],
			Thread[Rule[pauliList,pauliListEval]],
			Thread[Rule[sunList,sunListEval]],
			Thread[Rule[prefList,prefListEval]]
		];

		res = ex /. Dispatch[repRule];

		If[	OptionValue[FCRenameDummyIndices],
			time = AbsoluteTime[];
			FCPrint[1,"ComplexConjugate: Renaming dummy indices in the final result.", FCDoControl->ccjVerbose];
			res = FCRenameDummyIndices[res, FCI->True];
			FCPrint[1,"ComplexConjugate: Done renaming dummy indices in the final result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->ccjVerbose];
			FCPrint[3,"ComplexConjugate: After FCRenameDummyIndices: ", ex, FCDoControl->ccjVerbose]
		];

		If[	optConjugate=!={} && Head[optConjugate]===List,

			ccRules = Union[Cases[optConjugate, (Rule | RuleDelayed)[_, _]]];
			optConjugate = Complement[optConjugate, ccRules];
			ruleConjugate= Thread[ru[optConjugate,Conjugate/@optConjugate]]/. ru->Rule;
			res = res /. Join[ruleConjugate,ccRules]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];


(*	safe for memoization, as we merely reverse the ordering of the elements in a list *)
reverseDOT[x__]:=
	MemSet[reverseDOT[x],
		holdDOTReversed@@Reverse[{x}]
	];

(*	safe for memoization, as we merely reverse the ordering of the elements in a list *)
reverseSUNTF[{a__},b_,c_]:=
	MemSet[reverseSUNTF[{a},b,c],
		SUNTF[Reverse[{a}], c, b]
	];

rev[yz__] :=
	(DOT @@ (Reverse[FRH[{ yz }]])) /; Length[Position[{yz}, Spinor]] < 3;


genericEval[x_]:=
	x;

diracChainCC[ex1_DiracChain ex2_]:=
	diracChainCC[ex1] diracChainCC[ex2];

(*
	Even a standalone Dirac matrix is assumed to be part
	of a closed chain
*)
diracChainCC[DiracGamma[arg_,dim___]]:=
	DiracGamma[arg,dim]/;!MemberQ[{5,6,7},arg];

diracChainCC[DiracGamma[5]]:=
	-DiracGamma[5];

diracChainCC[DiracGamma[6]]:=
	DiracGamma[7];

diracChainCC[DiracGamma[7]]:=
	DiracGamma[6];

diracChainCC[Spinor[arg__]]:=
	Spinor[arg];

diracChainCC[ex_/;MemberQ[{holdDOT,FCCCT},Head[ex]]]:=
	Block[{	res=ex, diracGammaHold},

		If[ !FreeQ[res, Eps],
			res = res /. a_Eps :> Conjugate[$LeviCivitaSign]/$LeviCivitaSign a
		];

		If[ !FreeQ[res, FCChargeConjugateTransposed],
			res = res /. FCChargeConjugateTransposed[z_, opts:OptionsPattern[]] :>
				(z /. DiracGamma[a__] :> - diracGammaHold[a] /. holdDOT -> holdDOTReversed)
		];

		res = res /. {DiracGamma[5]-> -diracGammaHold[5], DiracGamma[6]-> diracGammaHold[7],
			DiracGamma[7]-> diracGammaHold[6]} /. DiracGamma[a__] :> diracGammaHold[a] /. holdDOT -> reverseDOT;

		(* 	CAREFUL: Complex[a_, b_] -> Complex[a, -b] is only true if no complex
			variables are in denominators!!!!, (which is the case in HEP, unless you
			have width in the propagators ...) *)
		res = res /. Complex[a_, b_] :> Complex[a, -b];

		res

	]/; FreeQ[ex,DiracChain];

diracChainCC[DiracChain[a_, i_, j_]]:=
	DiracChain[(diracChainCC[a]/.diracGammaHold->DiracGamma/. holdDOTReversed->DOT),j,i];

diracChainCC[DiracChain[a_Spinor, i_]]:=
	DiracChain[a,i];

diracChainCC[DiracChain[i_, a_Spinor]]:=
	DiracChain[i,a];




(*
pauliChainCC[ex1_PauliChain ex2_]:=
	pauliChainCC[ex1] pauliChainCC[ex2];
*)

(*
	Even a standalone Pauli matrix is assumed to be part
	of a closed chain
*)
pauliChainCC[PauliSigma[arg__]]:=
	PauliSigma[arg];

pauliChainCC[PauliXi[Complex[0,arg_]]]:=
	PauliXi[Complex[0,-arg]];

pauliChainCC[PauliEta[Complex[0,arg_]]]:=
	PauliEta[Complex[0,-arg]];

pauliChainCC[ex_holdDOT]:=
	Block[{	res=ex, pauliSigmaHold},

		If[ !FreeQ[res, Eps],
			res= res /. a_Eps :> Conjugate[$LeviCivitaSign]/$LeviCivitaSign a
		];

		res = res  /. holdDOT -> reverseDOT;

		(* 	CAREFUL: Complex[a_, b_] -> Complex[a, -b] is only true if no complex
			variables are in denominators!!!!, (which is the case in HEP, unless you
			have width in the propagators ...) *)
		res = res /. Complex[a_, b_] :> Complex[a, -b];

		res

	];

sunChainCC[ex:Except[_Plus | _Times]]:=
	Block[{	res=ex},

		If[ !FreeQ[res, SUNTF],
			res = res /. SUNTF -> reverseSUNTF
		];

		If[ !FreeQ[res, holdDOT],
			res = res /. holdDOT -> reverseDOT
		];

		res = res /. Complex[a_, b_] :> Complex[a, -b];

		res

	];

sunChainCC[ex_Plus]:=
	sunChainCC/@ex;

sunChainCC[ex_Times]:=
	sunChainCC/@ex;


prefCC[ex:Except[_Plus | _Times]]:=
	Block[{	res=ex, pauliSigmaHold},

		If[ !FreeQ[res, Eps],
			res= res /. a_Eps :> Conjugate[$LeviCivitaSign]/$LeviCivitaSign a
		];

		(* 	CAREFUL: Complex[a_, b_] -> Complex[a, -b] is only true if no complex
			variables are in denominators!!!!, (which is the case in HEP, unless you
			have width in the propagators ...) *)
		res = res /. Complex[a_, b_] :> Complex[a, -b];

		res

	];

prefCC[ex_Plus]:=
	prefCC/@ex;

prefCC[ex_Times]:=
	prefCC/@ex;


FCPrint[1,"ComplexConjugate.m loaded."];
End[]
