(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ExpandScalarProduct expands scalar products *)

(* ------------------------------------------------------------------------ *)

ExpandScalarProduct::usage =
"ExpandScalarProduct[expr]  expands scalar products of sums of \
momenta in expr. ExpandScalarProduct[x, y] expands ScalarProduct[x, y], \
where x and y may contain sums. ExpandScalarProduct does not use Expand on \
expr.";

ScalarProductExpand::usage =
"ScalarProductExpand is equivalent to ExpandScalarProduct.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ExpandScalarProduct`Private`"]

ScalarProductExpand = ExpandScalarProduct;

Options[ExpandScalarProduct] = {
	FCI -> True,
	Momentum -> {}
};

ExpandScalarProduct[x_, OptionsPattern[]] :=
	Block[ {nx = x, pali,moms},

		moms = OptionValue[Momentum];

		If[ OptionValue[FCI],
			nx = FCI[nx]
		];

		If[ FreeQ[nx,Pair],
			Return[nx]
		];

		If [moms==={},
			pali = Select[Cases2[nx, Pair], !FreeQ[#, LorentzIndex|Momentum]&],
			pali = Select[Cases2[nx, Pair], (!FreeQ[#, LorentzIndex|Momentum] && !FreeQ2[#, moms])&]
		];

		If[ pali =!= {},
			nx = nx /. Dispatch[Thread[pali -> pairexpand[pali]]]
		];

		nx
	];

ExpandScalarProduct[x_, y:Except[_?OptionQ]] :=
	scevdoit[x, y];

pairexpand[x_] :=
	x /. Pair->scevdoit ;

scevdoit[x_,y_] :=
	Distribute[Pair@@(Expand[MomentumExpand/@{x,y}])];

FCPrint[1,"ExpandScalarProduct.m loaded."];
End[]
