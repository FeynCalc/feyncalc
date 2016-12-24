(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumCombine													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  MomentumCombine												*)

(* ------------------------------------------------------------------------ *)

MomentumCombine::usage =
"MomentumCombine[expr] combines momenta and Pairs with the same LorentzIndexentz \
indices and momenta.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`MomentumCombine`Private`"];

rulesMain = {
	(n3_. Momentum[x_, dim_:4] + n4_. Momentum[y_, dim_:4]):>
		(Momentum[ Expand[n3 x + n4 y],dim]/; (NumberQ[n3] && NumberQ[n4]))
};

rulesSP = {
	(n3_. Pair[a_Momentum, Momentum[x_, dim_:4]] + n4_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		Pair[a, Momentum[Expand[n3 x + n4 y],dim]]/;(NumberQ[n3] && NumberQ[n4])
}

rulesFV = {
(n3_. Pair[a_LorentzIndex, Momentum[x_, dim_:4]] + n4_. Pair[a_LorentzIndex, Momentum[y_, dim_:4]]):>
		Pair[a, Momentum[ Expand[n3 x + n4 y],dim]]/; (NumberQ[n3] && NumberQ[n4]),

	(n3_. Pair[a_LorentzIndex, Momentum[x_, dim_:4]] + n3_. Pair[a_LorentzIndex, Momentum[y_, dim_:4]]):>
		n3 Pair[a, Momentum[Expand[x+y], dim]]/; (!NumberQ[n3])
(*
	(n3_. Pair[a_LorentzIndex, Momentum[x_,dim_:4]] + n4_. Pair[a_LorentzIndex, Momentum[y_,dim_:4]]):>
		Pair[a, Expand[MomentumExpand[n3 Momentum[x, dim] + n4 Momentum[y,dim]]]]/;
			(!NumberQ[n3] || !NumberQ[n4]) && FreeQ2[{n3,n4},{LorentzIndex,Momentum,Pair}]*)
};

Options[MomentumCombine] = {
	FCI -> False,
	FCE -> False,
	FV -> True,
	LeafCount -> 1,
	SP -> True
};

MomentumCombine[expr_, OptionsPattern[]] :=
	Block[{ex,res,rules=rulesMain},

		If[	!OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];

		If[	OptionValue[FV],
			rules = Join[rules,rulesFV]
		];

		If[	OptionValue[SP],
			rules = Join[rules,rulesSP]
		];

		If[ LeafCount[ex] < OptionValue[LeafCount],
			Return[ex]
		];

		res = ex //. Dispatch[rules];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

FCPrint["MomentumCombine.m loaded"];
End[]
