(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FermionSpinSum						*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Do the trace-formation (i.e. fermionic spin-sums) *)

(* ------------------------------------------------------------------------ *)


FermionSpinSum::usage =
"FermionSpinSum[exp] converts products of closed spinor chains in exp into
Dirac traces. Both Dirac and Majorana particles are supported. It is understood,
that exp represents a squared amplitude.";

FermionSpinSum::spinorsleft =
"Error! After applying FermionSpinSum to all spinor chains the output \
still contains spinors.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FermionSpinSum`Private`"]

fssVerbose::usage="";

Options[FermionSpinSum] = {
	Collecting -> True,
	DotSimplify -> True,
	ExtraFactor -> 1,
	FCE -> False,
	FCI -> False,
	FCTraceFactor -> True,
	FCVerbose -> False,
	Factoring -> Factor,
	Head -> Identity,
	Momentum -> All,
	SpinorChainTranspose -> True
};

FermionSpinSum[expr_List, opts:OptionsPattern[]]:=
	Map[FermionSpinSum[#, opts]&, expr];

FermionSpinSum[expr_, OptionsPattern[]] :=
	Block[ {spinPolarizationSum,extraFactor,moms, ex, spChain, ssIso, time,
			optSpinorChainTranspose, optSpinPolarizationSum},

		extraFactor 			= OptionValue[ExtraFactor];
		moms 					= OptionValue[Momentum];
		optSpinorChainTranspose = OptionValue[SpinorChainTranspose];
		optSpinPolarizationSum 	= OptionValue[Head];

		If [OptionValue[FCVerbose]===False,
			fssVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fssVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,Spinor],
			Return[extraFactor ex]
		];

		FCPrint[3, "FermionSpinSum: Entering with ",ex, FCDoControl->fssVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FermionSpinSum: Collecting terms w.r.t spinors.", FCDoControl->fssVerbose];
		ex = FCDiracIsolate[ex, DiracTrace->False,DiracGamma->False,FCI->True,Isolate->True, IsolateFast->True, IsolateNames->ssIso,Head->spChain];
		FCPrint[1,"FermionSpinSum: collecting done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fssVerbose];
		FCPrint[3, "FermionSpinSum: After collecting terms w.r.t spinors: ",ex, FCDoControl->fssVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FermionSpinSum: Applying the spin sum formula.", FCDoControl->fssVerbose];

		ex = ex //. {
		(* Product of two spinor chains, Dirac spinors *)
		spChain[DOT[Spinor[s1_. Momentum[pe1_,dim_:4], mass1_, ___], dots1___, Spinor[s2_. Momentum[pe2_,dim_:4], mass2_, ___]]] *
		spChain[DOT[Spinor[s2_. Momentum[pe2_,dim_:4], mass2_, ___], dots2___, Spinor[s1_. Momentum[pe1_,dim_:4], mass1_, ___]]]/;
		FreeQ[{dots1,dots2},Spinor] && (moms===All || (MemberQ[moms,pe1] && MemberQ[moms,pe2] )) :>
			DiracTrace[DOT[spinPolarizationSum[(DiracGamma[Momentum[pe1,dim],dim] + s1 mass1)], dots1,
				spinPolarizationSum[(DiracGamma[Momentum[pe2,dim],dim] + s2 mass2)], dots2]],

		spChain[DOT[Spinor[s1_. Momentum[pe1_,dim_:4], mass1_, ___], dots1___, Spinor[s2_. Momentum[pe2_,dim_:4], mass2_, arg2___]]] *
		spChain[DOT[Spinor[s3_. Momentum[pe3_,dim_:4], mass3_, arg3___], dots2___, Spinor[s1_. Momentum[pe1_,dim_:4], mass1_, ___]]]/;
		FreeQ[{dots1,dots2},Spinor] && (moms===All || MemberQ[moms,pe1]) :>
			spChain[DOT[Spinor[s3 Momentum[pe3,dim], mass3, arg3], dots2,
				spinPolarizationSum[(DiracGamma[Momentum[pe1,dim],dim] + s1 mass1)], dots1,
				Spinor[s2 Momentum[pe2,dim], mass2, arg2]]],

		(*	Product of two spinor chains, Majorana spinors, ubar(p).X vbar(p).Y.
			The -1 in front of spChain comes from switching the spinors after having transposed the second chain 	*)
		spChain[DOT[Spinor[s1_. Momentum[pe1_,dim_:4], mass1_,___], dots1___, Spinor[s2_. Momentum[pe2_,dim_:4], re2___]]] *
		spChain[DOT[Spinor[t1_. Momentum[pe1_,dim_:4], mass1_,___], dots2___, Spinor[s3_. Momentum[pe3_,dim_:4], re3___]]]/;
		FreeQ[{dots1,dots2},Spinor] && (optSpinorChainTranspose && moms===All || (MemberQ[moms,pe1] && (-t1===s1) )) :>
			-1*spChain[DOT[Spinor[- s3 Momentum[pe3,dim], re3],
				If[	TrueQ[{dots2}==={}],
					Unevaluated[Sequence[]],
					FCChargeConjugateTransposed[DOT[dots2],FCI->True,DotSimplify->False,Explicit->True]
				],
				spinPolarizationSum[(DiracGamma[Momentum[pe1,dim],dim] + s1 mass1)], dots1, Spinor[s2 Momentum[pe2,dim], re2]]],

		(* Product of two spinor chains, Majorana spinors, X.u(p) Y.v(p).
			The -1 in front of spChain comes from switching the spinors after having transposed the second chain	*)
		spChain[DOT[Spinor[s1_. Momentum[pe1_,dim_:4], re1___], dots1___, Spinor[s2_. Momentum[pe2_,dim_:4], mass2_,___]]] *
		spChain[DOT[Spinor[s3_. Momentum[pe3_,dim_:4], re3___], dots2___, Spinor[t2_. Momentum[pe2_,dim_:4], mass2_, ___]]]/;
		FreeQ[{dots1,dots2},Spinor] && (optSpinorChainTranspose && moms===All || (MemberQ[moms,pe2] && (-t2===s2) )) :>
			-1*spChain[DOT[Spinor[s1 Momentum[pe1,dim], re1], dots1 ,
				spinPolarizationSum[(DiracGamma[Momentum[pe2,dim],dim] + s2 mass2)],
				If[	TrueQ[{dots2}==={}],
					Unevaluated[Sequence[]],
					FCChargeConjugateTransposed[DOT[dots2],FCI->True,DotSimplify->False,Explicit->True]
				], Spinor[-s3 Momentum[pe3,dim], re3]]],

		(* A spinor chain with one spin sum inside *)
		spChain[DOT[Spinor[s_. Momentum[pe_,dim_:4], mass_, ___], dots___, Spinor[s_. Momentum[pe_,dim_:4], mass_, ___]]] /;
		FreeQ[{dots},Spinor] && (moms===All || MemberQ[moms,pe])  :>
			DiracTrace[DOT[spinPolarizationSum[(DiracGamma[Momentum[pe,dim],dim] + s mass)], dots]]
		};

		FCPrint[1,"FermionSpinSum: Applying the spin sum formula done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fssVerbose];
		FCPrint[3,"FermionSpinSum: After applying the spin sum formula: ", ex, FCDoControl->fssVerbose];

		If[	!FreeQ[ex,FCChargeConjugateTransposed],
			ex = ex /. FCChargeConjugateTransposed[spinPolarizationSum[x_],rest___]:>
				spinPolarizationSum[FCChargeConjugateTransposed[x,Explicit->True,rest]]
		];

		If[ OptionValue[DotSimplify],
			time=AbsoluteTime[];
			FCPrint[1, "FermionSpinSum: Applying DotSimplify.", FCDoControl->fssVerbose];
			ex = ex /. spinPolarizationSum[x_]:> spinPolarizationSum[DotSimplify[x,Expanding->False,FCI->True]];
			FCPrint[1,"FermionSpinSum: Applying DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fssVerbose];
			FCPrint[3,"FermionSpinSum: After DotSimplify: ", ex, FCDoControl->fssVerbose];
		];

		If[ optSpinPolarizationSum===Identity && OptionValue[FCTraceFactor],
			time=AbsoluteTime[];
			FCPrint[1, "FermionSpinSum: Applying FCTraceFactor.", FCDoControl->fssVerbose];
			ex = ex /. DiracTrace[x_]/;!FreeQ[x,spinPolarizationSum] :> FCTraceFactor[DiracTrace[(x/.spinPolarizationSum->Identity)],FCI->True];
			FCPrint[1,"FermionSpinSum: Applying FCTraceFactor done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fssVerbose];
			FCPrint[3,"FermionSpinSum: After FCTraceFactor: ", ex, FCDoControl->fssVerbose];
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
			FCPrint[1, "FermionSpinSum: Collecting w.r.t DiracTrace", FCDoControl->fssVerbose];
			ex = Collect2[ex, DiracTrace, Factoring->OptionValue[Factoring]];
			FCPrint[1,"FermionSpinSum: Collecting w.r.t DiracTrace done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fssVerbose];
			FCPrint[3,"FermionSpinSum: After collecting w.r.t DiracTrace: ", ex, FCDoControl->fssVerbose];
		];


		ex = ex extraFactor;

		If[ OptionValue[FCE],
			ex = FCE[ex]
		];

		ex
	]/; Head[expr]=!=List;

FCPrint[1,"FermionSpinSum.m loaded."];
End[]
