(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumCombine													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  MomentumCombine												*)

(* ------------------------------------------------------------------------ *)

MomentumCombine::usage =
"MomentumCombine[expr] is the inverse operation to MomentumExpand and
ExpandScalarProduct. MomentumCombine combines also Pairs.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`MomentumCombine`Private`"];
im::usage="";

rulesMain = {
	(n3_. Momentum[x_, dim_:4] + n4_. Momentum[y_, dim_:4]):>
		(Momentum[ Expand[n3 x + n4 y],dim]/; (NumberQ[n3] && NumberQ[n4])),

	(n3_. TemporalMomentum[x_] + n4_. TemporalMomentum[y_]):>
		(TemporalMomentum[ Expand[n3 x + n4 y]]/; (NumberQ[n3] && NumberQ[n4])),

	(n3_. CartesianMomentum[x_, dim_:3] + n4_. CartesianMomentum[y_, dim_:3]):>
		(CartesianMomentum[ Expand[n3 x + n4 y],dim]/; (NumberQ[n3] && NumberQ[n4]))
};

rulesSP = {
	(n3_. Pair[a_Momentum, Momentum[x_, dim_:4]] + n4_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		Pair[a, Momentum[Expand[n3 x + n4 y],dim]]/;(NumberQ[n3] && NumberQ[n4] && n3=!=n4),

	(n3_. Pair[a_Momentum, Momentum[x_, dim_:4]] + n3_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		n3 Pair[a, Momentum[Expand[x + y],dim]],

	(n3_. Pair[a_Momentum, Momentum[x_, dim_:4]] - n3_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		n3 Pair[a, Momentum[Expand[x - y],dim]],

	(n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[x_, dim_:3]] + n4_. CartesianPair[a_CartesianMomentum, CartesianMomentum[y_, dim_:3]]):>
		CartesianPair[a, CartesianMomentum[Expand[n3 x + n4 y],dim]]/;(NumberQ[n3] && NumberQ[n4]  && n3=!=n4),

	(n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[x_, dim_:3]] + n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[y_, dim_:3]]):>
		n3 CartesianPair[a, CartesianMomentum[Expand[x + y],dim]],

	(n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[x_, dim_:3]] - n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[y_, dim_:3]]):>
		n3 CartesianPair[a, CartesianMomentum[Expand[x - y],dim]]
}

rulesFV = {
	(n3_. Pair[a_LorentzIndex, Momentum[x_, dim_:4]] + n4_. Pair[a_LorentzIndex, Momentum[y_, dim_:4]]):>
		Pair[a, Momentum[ Expand[n3 x + n4 y],dim]]/; (NumberQ[n3] && NumberQ[n4] && n3=!=n4),

	(n3_. Pair[a_LorentzIndex, Momentum[x_, dim_:4]] + n3_. Pair[a_LorentzIndex, Momentum[y_, dim_:4]]):>
		n3 Pair[a, Momentum[Expand[x+y], dim]],

	(n3_. Pair[a_LorentzIndex, Momentum[x_, dim_:4]] - n3_. Pair[a_LorentzIndex, Momentum[y_, dim_:4]]):>
		n3 Pair[a, Momentum[Expand[x-y], dim]],

	(n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[x_, dim_:3]] + n4_. CartesianPair[a_CartesianIndex, CartesianMomentum[y_, dim_:3]]):>
		CartesianPair[a, CartesianMomentum[ Expand[n3 x + n4 y],dim]]/; (NumberQ[n3] && NumberQ[n4]  && n3=!=n4),

	(n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[x_, dim_:3]] + n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[y_, dim_:3]]):>
		n3 CartesianPair[a, CartesianMomentum[Expand[x+y], dim]],

	(n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[x_, dim_:3]] - n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[y_, dim_:3]]):>
		n3 CartesianPair[a, CartesianMomentum[Expand[x-y], dim]],

	(n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[x_]] + n4_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[y_]]):>
		TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[ Expand[n3 x + n4 y]]]/; (NumberQ[n3] && NumberQ[n4]  && n3=!=n4),

	(n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[x_]] + n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[y_]]):>
		n3 TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[Expand[x+y]]],

	(n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[x_]] - n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[y_]]):>
		n3 TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[Expand[x-y]]]

(*
	(n3_. Pair[a_LorentzIndex, Momentum[x_,dim_:4]] + n4_. Pair[a_LorentzIndex, Momentum[y_,dim_:4]]):>
		Pair[a, Expand[MomentumExpand[n3 Momentum[x, dim] + n4 Momentum[y,dim]]]]/;
			(!NumberQ[n3] || !NumberQ[n4]) && FreeQ2[{n3,n4},{LorentzIndex,Momentum,Pair}]*)
};

rulesLC = {
	(n3_. Eps[a___, (h:CartesianMomentum|Momentum)[x_, dim___], b___] + n4_. Eps[a___, (h:CartesianMomentum|Momentum)[y_, dim___], b___]):>
		Eps[a, h[ Expand[n3 x + n4 y],dim], b]/; (NumberQ[n3] && NumberQ[n4] && n3=!=n4),

	(n3_. Eps[a___, (h:CartesianMomentum|Momentum)[x_, dim___], b___] + n3_. Eps[a___, (h:CartesianMomentum|Momentum)[y_, dim___], b___]):>
		n3 Eps[a, h[ Expand[x + y],dim], b],

	(n3_. Eps[a___, (h:CartesianMomentum|Momentum)[x_, dim___], b___] - n3_. Eps[a___, (h:CartesianMomentum|Momentum)[y_, dim___], b___]):>
		n3 Eps[a, h[ Expand[x - y],dim], b]

};

Options[MomentumCombine] = {
	Complex		-> True,
	FCE 		-> False,
	FCI 		-> False,
	FV 			-> True,
	LC 			-> True,
	LeafCount	-> 1,
	SP 			-> True
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

		If[	OptionValue[LC],
			rules = Join[rules,rulesLC]
		];

		If[ LeafCount[ex] < OptionValue[LeafCount],
			Return[ex]
		];

		res = ex //. Dispatch[rules];

		If[	OptionValue[Complex] && !FreeQ[res,Complex],
			res = res /. {
				(h: Momentum|CartesianMomentum|TemporalMomentum)[a_, dim___]/; !FreeQ[a, Complex] :>
					factorIm[h[a, dim]]
			}
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res
	];

factorIm[(h: Momentum|CartesianMomentum|TemporalMomentum)[a_, dim___]] :=
	h[Factor2[a /. HoldPattern[Complex[b__]] -> complex[b]] /. im -> I, dim];

complex[a_, b_] := a + im b;

FCPrint["MomentumCombine.m loaded"];
End[]
