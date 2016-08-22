(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FermionSpinSum						*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Do the trace-formation (i.e. fermionic spin-sums) *)

(* ------------------------------------------------------------------------ *)


FermionSpinSum::usage =
"FermionSpinSum[x] constructs Traces out of squared ampliudes in x.";

DiracSimplify::spinorsleft =
"Error! After applying FermionSpinSum to all spinor chains the output
still contains spinors. Evaluation aborted.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FermionSpinSum`Private`"]

fssVerbose::usage="";

Options[FermionSpinSum] = {
	Collecting -> True,
	Factoring -> Factor,
	ExtraFactor -> 1,
	FCI -> False,
	FCVerbose -> False,
	DotSimplify -> True,
	Momentum -> All,
	SpinPolarizationSum -> Identity
};

FermionSpinSum[expr_List, opts:OptionsPattern[]]:=
	Map[FermionSpinSum[#, opts]&, expr];

FermionSpinSum[expr_, OptionsPattern[]] :=
	Block[ {spinPolarizationSum,extraFactor,moms, ex, spChain, ssIso, time},

		extraFactor = OptionValue[ExtraFactor];
		moms = OptionValue[Momentum];

		If [OptionValue[FCVerbose]===False,
			fssVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
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


		(* Applying spin sum formula *)

		time=AbsoluteTime[];
		FCPrint[1, "FermionSpinSum: Applying spin sum formula.", FCDoControl->fssVerbose];

		ex = ex //. {
		(* Product of two spinor chains, all momenta *)
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

		(* A spinor chain with one spin sum inside *)
		spChain[DOT[Spinor[s_. Momentum[pe_,dim_:4], mass_, ___], dots___, Spinor[s_. Momentum[pe_,dim_:4], mass_, ___]]] /;
		FreeQ[{dots},Spinor] && (moms===All || MemberQ[moms,pe])  :>
			DiracTrace[DOT[spinPolarizationSum[(DiracGamma[Momentum[pe,dim],dim] + s mass)], dots]]
		};

		FCPrint[1,"FermionSpinSum: applying spin sum formula done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fssVerbose];
		FCPrint[3,"FermionSpinSum: After applying spin sum formula: ", ex, FCDoControl->fssVerbose];

		If[ OptionValue[DotSimplify],
			time=AbsoluteTime[];
			FCPrint[1, "FermionSpinSum: Applying DotSimplify.", FCDoControl->fssVerbose];
			ex = ex /. spinPolarizationSum[x_]:> spinPolarizationSum[DotSimplify[x,Expanding->False]];
			FCPrint[1,"FermionSpinSum: applying spin sum formula done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->fssVerbose];
			FCPrint[3,"FermionSpinSum: After applying spin sum formula: ", ex, FCDoControl->fssVerbose];
		];

		ex = ex /. spinPolarizationSum -> OptionValue[SpinPolarizationSum];

		If[ moms===All && !FreeQ[ex,spChain],
			Message[DiracSimplify::spinorsleft];
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

		ex
	]/; Head[expr]=!=List;

FCPrint[1,"FermionSpinSum.m loaded."];
End[]
