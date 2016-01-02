(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorCombine *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: combines products of FeynAmpDenominatos *)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorCombine::usage =
"FeynAmpDenominatorCombine[expr] expands expr w.r.t. to \
FeynAmpDenominator and  combines products of FeynAmpDenominator \
in expr into one FeynAmpDenominator.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FeynAmpDenominatorCombine`Private`"]

feyncomb[] =
	1;
feyncomb /:
	feyncomb[a__] feyncomb[b__] :=
		feyncomb[a, b];

feyncomb /:
	feyncomb[a__]^n_Integer?Positive :=
		feyncomb @@ Flatten[Table[{a}, {ii, n}]];

lev[PropagatorDenominator[a_, 0], PropagatorDenominator[b_, 0]] :=
	If[ Length[Variables[a]] < Length[Variables[b]],
		True,
		False
	];

fdsor[a__] :=
	Apply[FeynAmpDenominator, Sort[MomentumExpand[{a}], lev]];

mfci[y_] :=
	If[ FreeQ[y, FeynAmpDenominator],
		FeynCalcInternal[y],
		y
	];

FeynAmpDenominatorCombine[x_] :=
	Expand2[x//mfci, FeynAmpDenominator]  /.
	FeynAmpDenominator -> feyncomb /. feyncomb -> fdsor;

FCPrint[1,"FeynAmpDenominatorCombine.m loaded."];
End[]
