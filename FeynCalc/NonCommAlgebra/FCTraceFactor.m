(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCTraceFactor														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Factors traces using linearity							    *)

(* ------------------------------------------------------------------------ *)

FCTraceFactor::usage =
"FCTraceFactor[expr] factors out all expressions inside a trace to which the
trace doesn't apply. For example, all objects that are not Dirac matrices can
be safely factored out from every Dirac trace.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCTraceFactor`Private`"]

Options[FCTraceFactor] = {
	FCI -> False,
	FCE -> False
};

FCTraceFactor[expr_, OptionsPattern[]] :=
	Block[ {ex, moms,res, diracTraces, pauliTraces,
			ruleDirac, rulePauli},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[ FreeQ2[ex,{DiracTrace,PauliTrace}],
			Return[ex]
		];

		diracTraces = Cases2[ex, DiracTrace];
		pauliTraces = Cases2[ex, PauliTrace];

		ruleDirac = Thread[diracTraces -> diracTracefactor[diracTraces]];
		rulePauli = Thread[pauliTraces -> pauliTracefactor[pauliTraces]];

		If[ diracTraces =!= {} || pauliTraces=!={},
			res = ex /. Dispatch[ruleDirac] /. Dispatch[rulePauli]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

diracTracefactor[x_] :=
	x /. DOT -> holdDOT /. DiracTrace->factorDirac /. factorDirac[] -> DiracTrace[1] /.
	holdDOT[] -> Sequence[] /.
	factorDirac -> DiracTrace /. holdDOT->DOT;

pauliTracefactor[x_] :=
	x /. DOT -> holdDOT /. PauliTrace->factorPauli /. factorPauli[] -> PauliTrace[1] /.
	holdDOT[] -> Sequence[] /.
	factorPauli -> PauliTrace /. holdDOT->DOT;

holdDOT[a___,b_,c___]:=
	b holdDOT[a,c]/; NonCommFreeQ[b];

holdDOT[a___,b1_ b2_,c___]:=
	b1 holdDOT[a,b2,c]/; NonCommFreeQ[b1] && !NonCommFreeQ[b2];

factorDirac[a_] :=
	a factorDirac[]/; NonCommFreeQ[a];

factorDirac[a_factorDirac b_.] :=
	a factorDirac[b];

factorDirac[a_SUNTrace b_.] :=
	a factorDirac[b];

factorDirac[a_PauliTrace b_.] :=
	a factorDirac[b];

factorDirac[a_ b_] :=
	a factorDirac[b]/; NonCommFreeQ[a] && !NonCommFreeQ[b];


factorPauli[a_] :=
	a factorPauli[]/; NonCommFreeQ[a];

factorPauli[a_factorPauli b_.] :=
	a factorPauli[b];

factorPauli[a_SUNTrace b_.] :=
	a factorPauli[b];

factorPauli[a_DiracTrace b_.] :=
	a factorPauli[b];

factorPauli[a_ b_] :=
	a factorPauli[b]/; NonCommFreeQ[a] && !NonCommFreeQ[b];



FCPrint[1,"FCTraceFactor.m loaded."];
End[]

