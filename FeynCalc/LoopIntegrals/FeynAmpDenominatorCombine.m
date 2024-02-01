(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynAmpDenominatorCombine *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: combines products of FeynAmpDenominatos *)

(* ------------------------------------------------------------------------ *)

FeynAmpDenominatorCombine::usage =
"FeynAmpDenominatorCombine[expr] expands expr with respect to
FeynAmpDenominator and combines products of FeynAmpDenominator in expr into
one FeynAmpDenominator.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

fdsor;

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

fdsor[a__] :=
	Apply[FeynAmpDenominator, Sort[MomentumExpand[{a}], FeynCalc`Package`lenso]];

Options[FeynAmpDenominatorCombine] = {
	FCE 		-> False,
	FCI 		-> False,
	Momentum	-> All
};

FeynAmpDenominatorCombine[expr_, OptionsPattern[]] :=
	Block[{ex,res, optMomentum},

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		optMomentum = OptionValue[Momentum];

		If[FreeQ[ex,FeynAmpDenominator],
			Return[ex]
		];

		res = Expand2[ex, FeynAmpDenominator];


		If[ optMomentum=!=All && Head[optMomentum]===List,
			res = res /. FeynAmpDenominator[x__]/;!FreeQ2[{x},optMomentum] :> feyncomb[x] /. feyncomb -> fdsor,
			res = res /. FeynAmpDenominator -> feyncomb /. feyncomb -> fdsor
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res


	];

FCPrint[1,"FeynAmpDenominatorCombine.m loaded."];
End[]
