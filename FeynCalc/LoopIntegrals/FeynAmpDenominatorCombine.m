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
		feyncomb @@ Flatten[Table[{a}, {n}]];

lev[PropagatorDenominator[a_, 0], PropagatorDenominator[b_, 0]] :=
	If[ Length[Variables[a]] < Length[Variables[b]],
		True,
		False
	];

lev[CartesianPropagatorDenominator[a1_, a2_, _, _], CartesianPropagatorDenominator[b1_, b2_, _, _]] :=
	If[ Length[Variables[{a1,a2}]] < Length[Variables[{b1,b2}]],
		True,
		If[	Length[Variables[{a1,a2}]] === Length[Variables[{b1,b2}]],
			OrderedQ[{{a1,a2},{b1,b2}}],
			False
		]
	];

lev[StandardPropagatorDenominator[a1_, a2_, _, _], StandardPropagatorDenominator[b1_, b2_, _, _]] :=
	If[ Length[Variables[{a1,a2}]] < Length[Variables[{b1,b2}]],
		True,
		If[	Length[Variables[{a1,a2}]] === Length[Variables[{b1,b2}]],
			OrderedQ[{{a1,a2},{b1,b2}}],
			False
		]
	];

fdsor[a__] :=
	Apply[FeynAmpDenominator, Sort[MomentumExpand[{a}], lev]];

Options[FeynAmpDenominatorCombine] = {
	FCI -> False,
	FCE -> False
};

FeynAmpDenominatorCombine[expr_, OptionsPattern[]] :=
	Block[{ex,res},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[FreeQ[ex,FeynAmpDenominator],
			Return[ex]
		];

		res = Expand2[ex, FeynAmpDenominator] /. FeynAmpDenominator -> feyncomb /. feyncomb -> fdsor;

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res


	];






FCPrint[1,"FeynAmpDenominatorCombine.m loaded."];
End[]
