(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopIsolate																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates loop integrals										*)

(* ------------------------------------------------------------------------ *)

FCLoopIsolate::usage =
"FCLoopIsolate[expr,{q1,q2,...}] wraps loop integrals into heads specified \
by the user " <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/FCLoopIsolate"],
StandardForm];

MultiLoop::usage =
"MultiLoop is an option for FCLoopIsolate. When set to True, \
FCLoopIsolate will isolate only such loop integrals, that \
depend on all of the given loop momenta. Integrals \
that depend only on some of the loop momenta will be treated \
as non-loop terms and remain non-isolated."

DropScaleless::usage =
"DropScaleless is an option for FCLoopIsolate. When set to True, \
all the loop integrals that do not contain FeynAmpDenominator, \
i.e. contain only scalar products but no denominators, are set \
to zero.";

FCLoopIsolate::fail =
"FCLoopIsolate failed to isolate loop integrals in `1`!";

Begin["`Package`"]
End[]

Begin["`FCLoopIsolate`Private`"]

Options[FCLoopIsolate] = {
	CFAD -> True,
	ClearHeads -> {FCGV["LoopInt"]},
	Collecting -> True,
	DiracGammaExpand -> True,
	DotSimplify -> True,
	DropScaleless -> False,
	ExceptHeads -> {},
	ExpandScalarProduct -> False,
	Expanding -> True,
	FCI -> False,
	FCE -> False,
	FCLoopIBPReducableQ -> False,
	Factoring -> Factor,
	FeynAmpDenominatorSplit -> True,
	Full -> True,
	GFAD -> True,
	Head -> FCGV["LoopInt"],
	Isolate -> False,
	IsolateNames -> KK,
	MultiLoop -> False,
	PaVe->True,
	PaVeIntegralHeads -> FeynCalc`Package`PaVeHeadsList
};

fullDep[z_,lmoms_]:=
	(Union[Cases[ExpandScalarProduct[z,FCI->True], (CartesianMomentum|Momentum)[x_, ___]/;!FreeQ2[x, lmoms] :> x, Infinity]] === Sort[lmoms]);

FCLoopIsolate[expr_, lmoms0_List /; FreeQ[lmoms0, OptionQ], OptionsPattern[]] :=
	Block[ {res, null1, null2, ex,lmoms,tmp, loopIntHeads},

		loopIntHeads = OptionValue[PaVeIntegralHeads];

		If[	MatchQ[lmoms0,{{___}}],
			Message[FCLoopIsolate::fail, ex];
			Abort[]
		];

		If[OptionValue[PaVe],
			lmoms = Join[lmoms0,loopIntHeads],
			lmoms = lmoms0
		];

		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];

		If[	lmoms==={},
			(*Nothing to do!*)
			Return[expr];
		];

		If[	OptionValue[Expanding],
			ex = Expand2[ex, lmoms];
		];

		(* Here we pull loop momenta out of Dirac slashes  *)
		If[	OptionValue[ExpandScalarProduct],
			tmp = FCSplit[ex, lmoms, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ ExpandScalarProduct[tmp[[2]],Momentum->lmoms,Full->OptionValue[Full]]
		];

		(* Here we pull loop momenta out of Dirac slashes  *)
		If[	OptionValue[DiracGammaExpand] && !FreeQ[ex,DiracGamma],
			tmp = FCSplit[ex, lmoms, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ tmp[[2]]/. DiracGamma[x_,dim_:4]/;!FreeQ2[x,lmoms] :> DiracGammaExpand[DiracGamma[x,dim]]
		];

		(*	and out of the DOTs	*)
		If[	OptionValue[DotSimplify] && !FreeQ[ex,DOT],
			tmp = FCSplit[ex, lmoms, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ DotSimplify[tmp[[2]]]
		];

		If[	OptionValue[Collecting],
			ex = Collect2[ex,lmoms,Factoring->OptionValue[Factoring]];
		];

		If[	OptionValue[FeynAmpDenominatorSplit] && !FreeQ[ex,FeynAmpDenominator],
			ex = FeynAmpDenominatorSplit[ex,Momentum->lmoms]
		];

		res = (Map[(SelectFree[#, lmoms]*
				OptionValue[Head][SelectNotFree[#, lmoms]]) &,
				ex + null1 + null2] /. {null1 | null2 -> 0} /.
			OptionValue[Head][1] -> 1);
		res = res /. {OptionValue[Head][x_] /; !FreeQ2[x, OptionValue[ExceptHeads]] :> x};

		If[ Together[(res /. OptionValue[Head] -> Identity)-ex] =!= 0,
			Message[FCLoopIsolate::fail, ex];
			Abort[]
		];

		If[ OptionValue[DropScaleless],
			res  = res /. OptionValue[Head][z__]/; FreeQ2[z,Join[{FeynAmpDenominator},loopIntHeads]] :> 0;
		];

		If[	OptionValue[Isolate],
			res = Isolate[res,OptionValue[Head],IsolateNames->OptionValue[IsolateNames]]/.
			OptionValue[Head][x_] :> OptionValue[Head][FRH[x]]
		];

		If [ !FreeQ[res/. OptionValue[Head][__] :> 1, lmoms] & ,
			Message[FCLoopIsolate::fail, ex];
			Abort[]
		];

		If [ OptionValue[MultiLoop],
			res = res /. OptionValue[Head][z__]/; !fullDep[z,lmoms0] :> z;
		];

		If [ !OptionValue[CFAD],
			res = res /. OptionValue[Head][z__]/; !FreeQ[{z}, CartesianPropagatorDenominator] :> z;
		];

		If [ !OptionValue[GFAD],
			res = res /. OptionValue[Head][z__]/; !FreeQ[{z}, GenericPropagatorDenominator] :> z;
		];

		(* Keep only integrals that are IBP-reducable *)
		If [ OptionValue[FCLoopIBPReducableQ],
			res = res /. OptionValue[Head][z__]/; !FCLoopIBPReducableQ[z] :> z;
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint[1,"FCLoopIsolate.m loaded."];
End[]
