(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopIsolate																*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Isolates loop integrals										*)

(* ------------------------------------------------------------------------ *)

FCLoopIsolate::usage = "FCLoopIsolate[expr,{q1,q2,...}] wraps
loop integrals into heads specified by the user. This is useful
when you want to know which loop integrals appear appear in the
given expression.";

MultiLoop::usage =
"MultiLoop is an option for FCLoopIsolate. When set to True,
FCLoopIsolate will isolate only such loop integrals, that
depend on all of the given loop momenta. Integrals
that depend only on some of the loop momenta will be treated
as non-loop terms and remain non-isolated"

DropScaleless::usage =
"DropScaleless is an option for FCLoopIsolate. When set to True,
all the loop integrals that do not contain FeynAmpDenominator,
i.e. contain only scalar products but no denominators, are set
to zero.";

FCLoopIsolate::fail =
"FCLoopIsolate failed to isolate loop integrals in `1`!";

Begin["`Package`"]
End[]

Begin["`FCLoopIsolate`Private`"]

Options[FCLoopIsolate] = {
	ExceptHeads -> {},
	ExpandScalarProduct -> False,
	Head -> FCGV["LoopInt"],
	ClearHeads -> {FCGV["LoopInt"]},
	Collecting -> True,
	DotSimplify -> True,
	DiracGammaExpand -> True,
	DropScaleless -> False,
	Expanding -> True,
	FeynAmpDenominatorSplit -> True,
	Isolate -> False,
	IsolateNames -> KK,
	FCI -> False,
	MultiLoop -> False,
	PaVe->True
};

fullDep[z_,lmoms_]:=
	(Union[Cases[ExpandScalarProduct[z], Momentum[x_, _ : 4]/;!FreeQ2[x, lmoms] :> x, Infinity]] === Sort[lmoms]);

FCLoopIsolate[expr_, lmoms0_List /; FreeQ[lmoms0, OptionQ], OptionsPattern[]] :=
	Block[ {res, null1, null2, ex,lmoms,tmp},

		If[	MatchQ[lmoms0,{{___}}],
			Message[FCLoopIsolate::fail, ex];
			Abort[]
		];

		If[OptionValue[PaVe],
			lmoms = Join[lmoms0,PaVeHeadsList],
			lmoms = lmoms0
		];

		If[OptionValue[FCI],
			ex = expr/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]]),
			ex = FCI[expr]/. (Map[Rule[#, Identity] &, OptionValue[ClearHeads]])
		];


		If[	OptionValue[Expanding],
			ex = Expand2[ex, lmoms];
		];

		(* Here we pull loop momenta out of Dirac slashes  *)
		If[	OptionValue[ExpandScalarProduct],
			tmp = FCSplit[ex, lmoms, Expanding->OptionValue[Expanding]];
			ex = tmp[[1]]+ ExpandScalarProduct[tmp[[2]],Momentum->lmoms]
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
			ex = Collect2[ex,lmoms];
		];

		If[	OptionValue[FeynAmpDenominatorSplit],
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
			res  = res /. OptionValue[Head][z__]/; FreeQ2[z,Join[{FeynAmpDenominator},PaVeHeadsList]] :> 0;
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

		res
	];

FCPrint[1,"FCLoopIsolate.m loaded."];
End[]
