(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinorChainEvaluate													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Simplification rules for products of spinor chains			*)

(* ------------------------------------------------------------------------ *)

SpinorChainEvaluate::usage =
"SpinorChainEvaluate[exp] explicitly evaluates suitable spinor chains, i.e. it
replaces a DOT[Spinor[...],...,Spinor[...]] with a scalar quantity without a
DOT.";

SpinorChainEvaluate::failmsg =
"Error! SpinorChainEvaluate has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SpinorChainEvaluate`Private`"]

holdDOT::usage="";
spcheVerbose::usage="";
normFun::usage="";

Options[SpinorChainEvaluate] = {
	Collecting 					-> True,
	DiracSpinorNormalization	-> "Relativistic",
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	Factoring					-> {Factor2, 5000},
	TimeConstrained				-> 3
};


SpinorChainEvaluate[a_ == b_, opts:OptionsPattern[]] :=
	SpinorChainEvaluate[a,opts] == SpinorChainEvaluate[b,opts];

SpinorChainEvaluate[expr_List, opts:OptionsPattern[]]:=
	SpinorChainEvaluate[#, opts]&/@expr;

SpinorChainEvaluate[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{	ex, tmp, dsHead, null1, null2, time, diracObjects, diracObjectsEval, repRule, res},

		If[	OptionValue[FCVerbose]===False,
			spcheVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				spcheVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "SpinorChainEvaluate. Entering.", FCDoControl->spcheVerbose];
		FCPrint[3, "SpinorChainEvaluate: Entering with ", ex, FCDoControl->spcheVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,Spinor],
			Return[ex]
		];

		Switch[
			OptionValue[DiracSpinorNormalization],
			"Relativistic",
				FCPrint[1, "SpinorChainEvaluate: Relativistic normalization selected.", FCDoControl->spcheVerbose];
				normFun = relNormalization,
			"Rest",
				FCPrint[1, "SpinorChainEvaluate: Rest normalization selected.", FCDoControl->spcheVerbose];
				normFun = restNormalization,
			"Nonrelativistic",
				FCPrint[1, "SpinorChainEvaluate: Nonrelativistic normalization selected.", FCDoControl->spcheVerbose];
				normFun = nrNormalization,
			_,
				Message[SpinorChainEvaluate::failmsg,"Unknown kinematic configuration"];
				Abort[]
		];

		If[	MatchQ[ex, DOT[a_Spinor,b___,c_Spinor]/;FreeQ[{b},Spinor]],

			FCPrint[1, "SpinorChainEvaluate: Recognized a standalone expression.", FCDoControl->spcheVerbose];
			tmp = dsHead[ex];
			diracObjects  = {dsHead[ex]},

			FCPrint[1, "SpinorChainEvaluate: Isolating spinor chains.", FCDoControl->spcheVerbose];
			time=AbsoluteTime[];
			tmp = FCDiracIsolate[ex,FCI->True, Head->dsHead, Spinor->True, Collecting-> OptionValue[Collecting], DiracGamma->False, Factoring -> OptionValue[Factoring]];
			FCPrint[1, "SpinorChainEvaluate: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spcheVerbose];
			FCPrint[3, "SpinorChainEvaluate: After FCDiracIsolate ", tmp, FCDoControl->spcheVerbose];

			diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;

		];

		FCPrint[3,"SpinorChainEvaluate: diracObjects: ", diracObjects , FCDoControl->spcheVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "SpinorChainEvaluate: Checking the spinor syntax.", FCDoControl->spcheVerbose];
		If[	FeynCalc`Package`spinorSyntaxCorrectQ[diracObjects]=!=True,
			Message[SpinorChainEvaluate::failmsg, "The input contains Spinor objects with incorrect syntax."];
			Abort[]
		];
		FCPrint[1,"SpinorChainEvaluate: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->spcheVerbose];


		FCPrint[1, "SpinorChainEvaluate: Evaluating suitable spinor chains.", FCDoControl->spcheVerbose];
		time=AbsoluteTime[];
		diracObjectsEval = Map[spinorChainEval, (diracObjects/.dsHead->Identity/. DOT->holdDOT)] /. spinorChainEval->Identity;
		FCPrint[1, "SpinorChainEvaluate: Done evaluating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spcheVerbose];
		FCPrint[3, "SpinorChainEvaluate: diracObjectsEval: ", diracObjectsEval, FCDoControl->spcheVerbose];


		If[	!FreeQ2[diracObjectsEval,{relNormalization,restNormalization,nrNormalization,normFun}],
			Message[SpinorChainEvaluate::failmsg, "Failed to evaluate suitable spinor chains."];
			Abort[]
		];


		FCPrint[1, "SpinorChainEvaluate: Inserting Dirac objects back.", FCDoControl->spcheVerbose];
		time=AbsoluteTime[];
		repRule = Thread[Rule[diracObjects,(diracObjectsEval/.holdDOT->DOT)]];

		FCPrint[3,"SpinorChainEvaluate: repRule: ",repRule , FCDoControl->spcheVerbose];

		res =  (tmp /. Dispatch[repRule]);

		FCPrint[1, "SpinorChainEvaluate: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->spcheVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "SpinorChainEvaluate: Leaving.", FCDoControl->spcheVerbose];
		FCPrint[3, "SpinorChainEvaluate: Leaving with ", res, FCDoControl->spcheVerbose];

		res

	];

relNormalization[Momentum[___], m_]:=
	2 m;

restNormalization[Momentum[___], _]:=
	1;

nrNormalization[Momentum[p_,___], m_]:=
	m / TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[p]];

(* massive/massless, ubar(p).u(p) and vbar(p).v(p) *)
spinorChainEval[holdDOT[Spinor[s_. Momentum[p__],m_, 1],Spinor[s_. Momentum[p__],m_, 1]]]:=
	s*normFun[Momentum[p],m];

(* massive/massless, ubar(p).v(p) and vbar(p).u(p) *)
spinorChainEval[holdDOT[Spinor[Momentum[p__],m_, 1],Spinor[-Momentum[p__],m_, 1]]]:=
	0;

spinorChainEval[holdDOT[Spinor[-Momentum[p__],m_, 1],Spinor[Momentum[p__],m_, 1]]]:=
	0;

(*
	Here we should be careful: In 4 dimensions these relations obviously hold
	In D-dimensions with D-dim spinors the derivation relies on the fact that g^5 is anticommuting.
	In a BMHV-like scheme where the spinors are D-dimensional, the product does not
	vanish, cf. e.g.

	FCSetDiracGammaScheme["BMHV"];
	SpinorUBarD[p1, m].DiracSigma[GAD[mu], GSD[p1 - p2]].GAD[5].SpinorUD[p2, m] // DiracSimplify[#, DiracOrder -> True] &
	% /. p1 | p2 -> p
*)

(* massive/massless, ubar(p).g^5.u(p) and vbar(p).g^5.v(p) *)
spinorChainEval[holdDOT[Spinor[s_. Momentum[p_],m_, 1],DiracGamma[5],Spinor[s_. Momentum[p_],m_, 1]]]:=
	0;


spinorChainEval[holdDOT[Spinor[s_. Momentum[p_,dim_Symbol],m_, 1],DiracGamma[5],Spinor[s_. Momentum[p_,dim_Symbol],m_, 1]]]:=
	0 /; MemberQ[{"NDR", "NDR-Discard"},FeynCalc`Package`DiracGammaScheme];

(* massless particles *)

(* massless, ubar(p).g^{6|7}.u(p) and vbar(p).g^{6|7}.v(p) *)
spinorChainEval[holdDOT[Spinor[s_. Momentum[p_], 0, 1],DiracGamma[6|7],Spinor[s_. Momentum[p_], 0, 1]]]:=
	0;


spinorChainEval[holdDOT[Spinor[s_. Momentum[p_,dim_Symbol], 0, 1],DiracGamma[6|7],Spinor[s_. Momentum[p_,dim_Symbol], 0, 1]]]:=
	0 /; MemberQ[{"NDR", "NDR-Discard"},FeynCalc`Package`DiracGammaScheme];

(* ubar(p).g^{5|6|7}.v(p) and vbar(p).g^{5|6|7}.u(p) *)
spinorChainEval[holdDOT[Spinor[Momentum[p_], 0, 1],DiracGamma[5|6|7],Spinor[-Momentum[p_], 0, 1]]]:=
	0;

spinorChainEval[holdDOT[Spinor[-Momentum[p_], 0, 1],DiracGamma[5|6|7],Spinor[Momentum[p_], 0, 1]]]:=
	0;

spinorChainEval[holdDOT[Spinor[Momentum[p_,dim_Symbol], 0, 1],DiracGamma[5|6|7],Spinor[-Momentum[p_,dim_Symbol], 0, 1]]]:=
	0 /; MemberQ[{"NDR", "NDR-Discard"},FeynCalc`Package`DiracGammaScheme];

spinorChainEval[holdDOT[Spinor[-Momentum[p_,dim_Symbol], 0, 1],DiracGamma[5|6|7],Spinor[Momentum[p_,dim_Symbol], 0, 1]]]:=
	0 /; MemberQ[{"NDR", "NDR-Discard"},FeynCalc`Package`DiracGammaScheme];



FCPrint[1,"SpinorChainEvaluate.m loaded"];
End[]
