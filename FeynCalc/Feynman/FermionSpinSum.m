(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FermionSpinSum													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Do the trace-formation (i.e. fermionic spin-sums) 

	Supports parallel evaluation [X]
*)

(* ------------------------------------------------------------------------ *)


FermionSpinSum::usage =
"FermionSpinSum[exp] converts products of closed spinor chains in exp into
Dirac traces. Both Dirac and Majorana particles are supported. It is
understood, that exp represents a squared amplitude.";

FermionSpinSum::spinorsleft =
"Error! After applying FermionSpinSum to all spinor chains the output \
still contains spinors.";

FermionSpinSum::fcctleft =
"Warning! The output contains structures wrapped into FCChargeConjugateTransposed. \
Those must necessarily be simplified before the computation of Dirac traces.";

FermionSpinSum::failmsg =
"Error! FermionSpinSum encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FermionSpinSum`Private`"]

optVerbose::usage="";

Options[FermionSpinSum] = {
	Collecting				-> True,
	DotSimplify				-> True,
	ExtraFactor				-> 1,
	FCE						-> False,
	FCI						-> False,
	FCParallelize			-> False,
	FCTraceFactor			-> True,
	FCVerbose				-> False,
	Factoring				-> Factor,
	Head 					-> Identity,
	Momentum 				-> All,
	SpinorChainTranspose	-> True
};

FermionSpinSum[expr_List, opts:OptionsPattern[]] :=
	Block[{optVerbose, res, time},

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		time=AbsoluteTime[];

		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize],
			FCPrint[1,"FermionSpinSum: Applying FermionSpinSum in parallel.", FCDoControl->optVerbose];
			res = ParallelMap[FermionSpinSum[#, FilterRules[{opts}, Except[FCParallelize | FCVerbose]]]&,expr,
			DistributedContexts -> None, Method->"ItemsPerEvaluation" -> Ceiling[N[Length[expr]/$KernelCount]/10]];
			FCPrint[1, "FermionSpinSum: Done applying FermionSpinSum in parallel, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],

			FCPrint[1,"FermionSpinSum: Applying FermionSpinSum.", FCDoControl->optVerbose];
			res = Map[FermionSpinSum[#, FilterRules[{opts}, Except[FCParallelize | FCVerbose]]]&,expr];
			FCPrint[1, "FermionSpinSum: Done applying FermionSpinSum, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose]
		];

		res
	];

FermionSpinSum[expr_/; Head[expr]=!=List, OptionsPattern[]] :=
	Block[ {spinPolarizationSum,extraFactor,moms, ex, spChain, ssIso, time,
			optSpinorChainTranspose, optSpinPolarizationSum},

		extraFactor 			= OptionValue[ExtraFactor];
		moms 					= OptionValue[Momentum];
		optSpinorChainTranspose = OptionValue[SpinorChainTranspose];
		optSpinPolarizationSum 	= OptionValue[Head];

		If [OptionValue[FCVerbose]===False,
			optVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				optVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,Spinor],
			Return[extraFactor ex]
		];

		FCPrint[3, "FermionSpinSum: Entering with ",ex, FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FermionSpinSum: Collecting terms w.r.t spinors.", FCDoControl->optVerbose];
		ex = FCDiracIsolate[ex, DiracTrace->False,DiracGamma->False,FCI->True,Isolate->True, IsolateFast->True, IsolateNames->ssIso,Head->spChain];
		FCPrint[1,"FermionSpinSum: collecting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
		FCPrint[3, "FermionSpinSum: After collecting terms w.r.t spinors: ",ex, FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FermionSpinSum: Applying the spin sum formula.", FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FermionSpinSum: Checking the spinor syntax.", FCDoControl->optVerbose];
		If[	FeynCalc`Package`spinorSyntaxCorrectQ[ex]=!=True,
			Message[FermionSpinSum::failmsg, "The input contains Spinor objects with incorrect syntax."];
			Abort[]
		];
		FCPrint[1,"FermionSpinSum: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];

		ex = ex //. {
		(* Product of two spinor chains, Dirac spinors *)
		spChain[DOT[Spinor[s_. * Momentum[p_,d_:4], Y_, ___], a___, Spinor[t_. * Momentum[q_,d_:4], Z_, ___]]] *
		spChain[DOT[Spinor[t_. * Momentum[q_,d_:4], Z_, ___], b___, Spinor[s_. * Momentum[p_,d_:4], Y_, ___]]]/;
		FreeQ[{a,b},Spinor] && (moms===All || (MemberQ[moms,p] && MemberQ[moms,q] )) :>
			DiracTrace[DOT[spinPolarizationSum[(DiracGamma[Momentum[p,d],d] + s Y)], a,
				spinPolarizationSum[(DiracGamma[Momentum[q,d],d] + t Z)], b]],

		spChain[DOT[Spinor[s_. * Momentum[p_,d_:4], m_, ___], a___, Spinor[t_. * Momentum[q_,d_:4], Y_, W___]]] *
		spChain[DOT[Spinor[u_. * Momentum[r_,d_:4], Z_, X___], b___, Spinor[s_. * Momentum[p_,d_:4], m_, ___]]]/;
		FreeQ[{a,b},Spinor] && (moms===All || MemberQ[moms,p]) :>
			spChain[DOT[Spinor[u Momentum[r,d], Z, X], b,
				spinPolarizationSum[(DiracGamma[Momentum[p,d],d] + s m)], a,
				Spinor[t Momentum[q,d], Y, W]]],

		(*	Product of two spinor chains, Majorana spinors, ubar(p).X vbar(p).Y.
			The -1 in front of spChain comes from switching the spinors after having transposed the second chain 	*)
		spChain[DOT[Spinor[s_. * Momentum[p_,d_:4], m_,___], a___, Spinor[t_. * Momentum[q_,d_:4], Y___]]] *
		spChain[DOT[Spinor[u_. * Momentum[p_,d_:4], m_,___], b___, Spinor[v_. * Momentum[r_,d_:4], Z___]]]/;
		FreeQ[{a,b},Spinor] && (optSpinorChainTranspose && moms===All || (MemberQ[moms,p] && (-u===s) )) :>
			-1*spChain[DOT[Spinor[- v Momentum[r,d], Z],
				If[	TrueQ[{b}==={}],
					Unevaluated[Sequence[]],
					FCChargeConjugateTransposed[DOT[b],FCI->True,DotSimplify->False,Explicit->True]
				],
				spinPolarizationSum[(DiracGamma[Momentum[p,d],d] + s m)], a, Spinor[t Momentum[q,d], Y]]],

		(* Product of two spinor chains, Majorana spinors, X.u(p) Y.v(p).
			The -1 in front of spChain comes from switching the spinors after having transposed the second chain	*)
		spChain[DOT[Spinor[s_. * Momentum[p_,d_:4], Y___], a___, Spinor[t_. * Momentum[q_,d_:4], m_,___]]] *
		spChain[DOT[Spinor[u_. * Momentum[r_,d_:4], Z___], b___, Spinor[v_. * Momentum[q_,d_:4], m_, ___]]]/;
		FreeQ[{a,b},Spinor] && (optSpinorChainTranspose && moms===All || (MemberQ[moms,q] && (-v===t) )) :>
			-1*spChain[DOT[Spinor[s Momentum[p,d], Y], a ,
				spinPolarizationSum[(DiracGamma[Momentum[q,d],d] + t m)],
				If[	TrueQ[{b}==={}],
					Unevaluated[Sequence[]],
					FCChargeConjugateTransposed[DOT[b],FCI->True,DotSimplify->False,Explicit->True]
				], Spinor[-u Momentum[r,d], Z]]],

		(* A spinor chain with one spin sum inside *)
		spChain[DOT[Spinor[s_. * Momentum[p_,d_:4], mass_, ___], x___, Spinor[s_. * Momentum[p_,d_:4], m_, ___]]] /;
		FreeQ[{x},Spinor] && (moms===All || MemberQ[moms,p])  :>
			DiracTrace[DOT[spinPolarizationSum[(DiracGamma[Momentum[p,d],d] + s m)], x]]
		};

		FCPrint[1,"FermionSpinSum: Applying the spin sum formula done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
		FCPrint[3,"FermionSpinSum: After applying the spin sum formula: ", ex, FCDoControl->optVerbose];

		If[	!FreeQ[ex,FCChargeConjugateTransposed],
			ex = ex /. FCChargeConjugateTransposed[spinPolarizationSum[x_],r___]:>
				spinPolarizationSum[FCChargeConjugateTransposed[x,Explicit->True,r]];
			If[ OptionValue[DotSimplify],
				FCPrint[1, "FermionSpinSum: Applying DotSimplify to FCCCTs.", FCDoControl->optVerbose];
				ex = ex /. FCChargeConjugateTransposed[x_,r__] :> FCChargeConjugateTransposed[x,DotSimplify->True,Explicit->True,r];
				FCPrint[1,"FermionSpinSum: Applying DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
				FCPrint[3,"FermionSpinSum: After DotSimplify: ", ex, FCDoControl->optVerbose];
				If[ !FreeQ[ex,FCChargeConjugateTransposed],
					Message[FermionSpinSum::fcctleft]
				]
			]
		];

		If[ OptionValue[DotSimplify],
			time=AbsoluteTime[];
			FCPrint[1, "FermionSpinSum: Applying DotSimplify.", FCDoControl->optVerbose];
			ex = ex /. spinPolarizationSum[x_]:> spinPolarizationSum[DotSimplify[x,Expanding->False,FCI->True]];
			FCPrint[1,"FermionSpinSum: Applying DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3,"FermionSpinSum: After DotSimplify: ", ex, FCDoControl->optVerbose]

		];

		If[ optSpinPolarizationSum===Identity && OptionValue[FCTraceFactor],
			time=AbsoluteTime[];
			FCPrint[1, "FermionSpinSum: Applying FCTraceFactor.", FCDoControl->optVerbose];
			ex = ex /. DiracTrace[x_]/;!FreeQ[x,spinPolarizationSum] :> FCTraceFactor[DiracTrace[(x/.spinPolarizationSum->Identity)],FCI->True];
			FCPrint[1,"FermionSpinSum: Applying FCTraceFactor done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3,"FermionSpinSum: After FCTraceFactor: ", ex, FCDoControl->optVerbose]
		];

		If[	!FreeQ[ex,spinPolarizationSum],
			ex = ex /. spinPolarizationSum -> optSpinPolarizationSum;
		];

		If[ moms===All && !FreeQ[ex,spChain],
			Message[FermionSpinSum::spinorsleft];
		];

		ex = ex /. spChain -> Identity;
		ex = FRH[ex,IsolateNames->ssIso];

		If [OptionValue[Collecting],
			time=AbsoluteTime[];
			FCPrint[1, "FermionSpinSum: Collecting w.r.t DiracTrace", FCDoControl->optVerbose];
			ex = Collect2[ex, DiracTrace, Factoring->OptionValue[Factoring]];
			FCPrint[1,"FermionSpinSum: Collecting w.r.t DiracTrace done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->optVerbose];
			FCPrint[3,"FermionSpinSum: After collecting w.r.t DiracTrace: ", ex, FCDoControl->optVerbose];
		];


		ex = ex extraFactor;

		If[ OptionValue[FCE],
			ex = FCE[ex]
		];

		ex
	];

FCPrint[1,"FermionSpinSum.m loaded."];
End[]
