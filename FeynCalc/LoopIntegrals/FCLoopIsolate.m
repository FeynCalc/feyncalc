(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopIsolate																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates loop integrals										*)

(* ------------------------------------------------------------------------ *)

FCLoopIsolate::usage =
"FCLoopIsolate[expr, {q1, q2, ...}] wraps loop integrals into heads specified
by the user. This is useful when you want to know which loop integrals appear
in the given expression.";

MultiLoop::usage =
"MultiLoop is an option for FCLoopIsolate. When set to True, FCLoopIsolate will
isolate only such loop integrals, that depend on all of the given loop
momenta. Integrals that depend only on some of the loop momenta will be
treated as non-loop terms and remain non-isolated.";

FCLoopIsolate::fail =
"FCLoopIsolate failed to isolate loop integrals in `1`!";

Begin["`Package`"]
End[]

Begin["`FCLoopIsolate`Private`"]

fcliVerbose::usage="";

Options[FCLoopIsolate] = {
	CFAD 					-> True,
	ClearHeads 				-> {FCGV["LoopInt"]},
	Collecting				-> True,
	DiracGammaExpand 		-> True,
	DotSimplify 			-> True,
	DropScaleless 			-> False,
	ExceptHeads 			-> {},
	ExpandScalarProduct 	-> False,
	Expanding 				-> True,
	FAD 					-> True,
	FCE 					-> False,
	FCI 					-> False,
	FCLoopIBPReducableQ 	-> False,
	FCVerbose 				-> False,
	Factoring 				-> {Factor2, 5000},
	FeynAmpDenominatorSplit -> True,
	Full					-> True,
	GFAD 					-> True,
	Head 					-> FCGV["LoopInt"],
	Isolate 				-> False,
	IsolateNames 			-> KK,
	MultiLoop 				-> False,
	Numerator 				-> True,
	PaVe					-> True,
	PaVeIntegralHeads 		-> FeynCalc`Package`PaVeHeadsList,
	SFAD 					-> True,
	TimeConstrained			-> 3
};

fullDep[z_,lmoms_]:=
	(Union[Cases[ExpandScalarProduct[z,FCI->True], (CartesianMomentum|Momentum)[x_, ___]/;!FreeQ2[x, lmoms] :> x, Infinity]] === Sort[lmoms]);


FCLoopIsolate[a_ == b_, y__] :=
	FCLoopIsolate[a,y] == FCLoopIsolate[b,y];

FCLoopIsolate[(h:Rule|RuleDelayed)[a_,b_], y__] :=
	With[{zz1=FCLoopIsolate[a,y],zz2=FCLoopIsolate[b,y]}, h[zz1,zz2]];

FCLoopIsolate[x_List, y__] :=
	FCLoopIsolate[#, y]& /@ x;


FCLoopIsolate[expr_, lmoms0_List /; FreeQ[lmoms0, OptionQ], OptionsPattern[]] :=
	Block[{	res, null1, null2, ex,lmoms,tmp, loopIntHeads, time, optExceptHeads, optHead,
			dummy1, dummy2},

		loopIntHeads = OptionValue[PaVeIntegralHeads];
		optExceptHeads = OptionValue[ExceptHeads];
		optHead = OptionValue[Head];

		If [OptionValue[FCVerbose]===False,
				fcliVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fcliVerbose=OptionValue[FCVerbose]
				];
		];

		FCPrint[1,"FCLoopIsolate: Entering.", FCDoControl->fcliVerbose];
		FCPrint[3,"FCLoopIsolate: Entering with: ", expr, FCDoControl->fcliVerbose];

		If[	MatchQ[lmoms0,{{___}}],
			Message[FCLoopIsolate::fail, ex];
			Abort[]
		];

		If[OptionValue[PaVe],
			lmoms = Join[lmoms0,loopIntHeads],
			lmoms = lmoms0
		];

		time = AbsoluteTime[];
		FCPrint[1,"FCLoopIsolate: Applying FCI and ClearHeads.", FCDoControl->fcliVerbose];
		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];
		FCPrint[1, "FCLoopIsolate: Done applying FCI and ClearHeads, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose];

		If[	lmoms==={},
			(*Nothing to do!*)
			Return[expr];
		];

		If[	OptionValue[Expanding],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying Expand2.", FCDoControl->fcliVerbose];
			ex = Expand2[ex, lmoms];
			FCPrint[1, "FCLoopIsolate: Done applying Expand2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		(* Here we pull loop momenta out of Dirac slashes  *)
		If[	OptionValue[ExpandScalarProduct],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying ExpandScalarProduct.", FCDoControl->fcliVerbose];
			tmp = FCSplit[ex, lmoms, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ ExpandScalarProduct[tmp[[2]],Momentum->lmoms,Full->OptionValue[Full]];
			FCPrint[1, "FCLoopIsolate: Done applying ExpandScalarProduct, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		(* Here we pull loop momenta out of Dirac slashes  *)
		If[	OptionValue[DiracGammaExpand] && !FreeQ[ex,DiracGamma],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying DiracGammaExpand.", FCDoControl->fcliVerbose];
			tmp = FCSplit[ex, lmoms, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ tmp[[2]]/. DiracGamma[x_,dim_:4]/;!FreeQ2[x,lmoms] :> DiracGammaExpand[DiracGamma[x,dim]];
			FCPrint[1, "FCLoopIsolate: Done applying DiracGammaExpand, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		(*	and out of the DOTs	*)
		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying DotSimplify.", FCDoControl->fcliVerbose];
			tmp = FCSplit[ex, lmoms, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ DotSimplify[tmp[[2]]];
			FCPrint[1, "FCLoopIsolate: Done applying DotSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		If[	OptionValue[Collecting],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying Collect2.", FCDoControl->fcliVerbose];

			If[	TrueQ[OptionValue[Numerator]],
				ex = Collect2[ex,lmoms,Factoring->OptionValue[Factoring], TimeConstrained->OptionValue[TimeConstrained]],
				(*If we care only about the denominators, we can speed up this step*)
				ex = Collect2[ex,FeynAmpDenominator,Factoring->OptionValue[Factoring], TimeConstrained->OptionValue[TimeConstrained]]
			];


			FCPrint[1, "FCLoopIsolate: Done applying Collect2, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		If[	OptionValue[FeynAmpDenominatorSplit] && !FreeQ[ex,FeynAmpDenominator],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying FeynAmpDenominatorSplit.", FCDoControl->fcliVerbose];
			ex = FeynAmpDenominatorSplit[ex,Momentum->lmoms];
			FCPrint[1, "FCLoopIsolate: Done applying FeynAmpDenominatorSplit, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];


		time = AbsoluteTime[];
		FCPrint[1,"FCLoopIsolate: Splitting products of loop and non-loop terms.", FCDoControl->fcliVerbose];
		res = (Map[(SelectFree[#, lmoms] optHead[SelectNotFree[#, lmoms]]) &, ex + null1 + null2] /. {null1 | null2 -> 0} /. optHead[1] -> 1);
		FCPrint[1, "FCLoopIsolate: Done splitting products of loop and non-loop terms, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose];


		If[	optExceptHeads=!={},
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying the option ExceptHeads.", FCDoControl->fcliVerbose];
			res = res /. {optHead[x_] /; !FreeQ2[x, optExceptHeads] :> x};
			FCPrint[1, "FCLoopIsolate: Done applying the option ExceptHeads, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];


		time = AbsoluteTime[];
		FCPrint[1,"FCLoopIsolate: Checking the intermediate result.", FCDoControl->fcliVerbose];
		If[ Together[(res /. optHead -> Identity)-ex] =!= 0,
			Message[FCLoopIsolate::fail, ex];
			Abort[]
		];
		FCPrint[1, "FCLoopIsolate: Done checking the intermediate result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose];

		If[ OptionValue[DropScaleless],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Removing scaleless integrals.", FCDoControl->fcliVerbose];
			res  = res /. optHead[z__]/; FreeQ2[z,Join[{FeynAmpDenominator},loopIntHeads]] :> 0;
			FCPrint[1, "FCLoopIsolate: Done removing scaleless integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose];
		];

		If[	OptionValue[Isolate],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying Isolate.", FCDoControl->fcliVerbose];
			res = Isolate[res,optHead,IsolateNames->OptionValue[IsolateNames]]/. optHead[x_] :> optHead[FRH[x]];
			FCPrint[1, "FCLoopIsolate: Done applying isolate, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose];
		];

		time = AbsoluteTime[];
		FCPrint[1,"FCLoopIsolate: Checking the intermediate result.", FCDoControl->fcliVerbose];
		If [ !FreeQ[res/. optHead[__] :> 1, lmoms] & ,
			Message[FCLoopIsolate::fail, ex];
			Abort[]
		];
		FCPrint[1, "FCLoopIsolate: Done checking the intermediate result, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose];

		If [ !OptionValue[Numerator],

			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying the option Numerator.", FCDoControl->fcliVerbose];
			res = res /. optHead[z_] :> optHead[SelectNotFree[z dummy1 dummy2,FeynAmpDenominator]]*SelectFree[z  dummy1 dummy2,FeynAmpDenominator] /. dummy1|dummy2->1;
			FCPrint[1, "FCLoopIsolate: Done applying the option Numerator, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		If [ OptionValue[MultiLoop],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying the option MultiLoop.", FCDoControl->fcliVerbose];
			res = res /. optHead[z__]/; !fullDep[z,lmoms0] :> z;
			FCPrint[1, "FCLoopIsolate: Done applying the option MultiLoop, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		If [ !OptionValue[SFAD] && !FreeQ[res,StandardPropagatorDenominator],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying the option SFAD.", FCDoControl->fcliVerbose];
			res = res /. optHead[z__]/; !FreeQ[{z}, StandardPropagatorDenominator] :> z;
			FCPrint[1, "FCLoopIsolate: Done applying the option SFAD, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		If [ !OptionValue[CFAD] && !FreeQ[res,CartesianPropagatorDenominator],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying the option CFAD.", FCDoControl->fcliVerbose];
			res = res /. optHead[z__]/; !FreeQ[{z}, CartesianPropagatorDenominator] :> z;
			FCPrint[1, "FCLoopIsolate: Done applying the option CFAD, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		If [ !OptionValue[GFAD] && !FreeQ[res,GenericPropagatorDenominator],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying the option GFAD.", FCDoControl->fcliVerbose];
			res = res /. optHead[z__]/; !FreeQ[{z}, GenericPropagatorDenominator] :> z;
			FCPrint[1, "FCLoopIsolate: Done applying the option GFAD, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		If [ !OptionValue[FAD] && !FreeQ[res,PropagatorDenominator],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying the option FAD.", FCDoControl->fcliVerbose];
			res = res /. optHead[z__]/; !FreeQ[{z}, PropagatorDenominator] :> z;
			FCPrint[1, "FCLoopIsolate: Done applying the option FAD, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		(* Keep only integrals that are IBP-reducible *)
		If [ OptionValue[FCLoopIBPReducableQ],
			time = AbsoluteTime[];
			FCPrint[1,"FCLoopIsolate: Applying the option FCLoopIBPReducableQ.", FCDoControl->fcliVerbose];
			res = res /. optHead[z__]/; !FCLoopIBPReducableQ[z] :> z;
			FCPrint[1, "FCLoopIsolate: Done applying the option FCLoopIBPReducableQ, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcliVerbose]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopIsolate: Leaving.", FCDoControl->fcliVerbose];
		FCPrint[3,"FCLoopIsolate: Leaving with: ", res, FCDoControl->fcliVerbose];


		res
	];

FCPrint[1,"FCLoopIsolate.m loaded."];
End[]
