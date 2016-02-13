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

MomentumCombine2::usage =
"MomentumCombine2[expr]  is the inverse operation to MomentumExpand and \
ExpandScalarProduct. MomentumCombine2 combines also FourVectors.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`MomentumCombine`Private`"];


MomentumCombine2[expr_] :=
	expr /. Plus-> plm;

plm[xX__] :=
	If[ Length[{xX}] > 10,
		Plus[xX],
		Plus[xX] //. {
		(n3_. Momentum[x_,di___] + n4_. Momentum[y_,di___]
		):>
		(Momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3]&&NumberQ[n4])),
		(n3_. Pair[a_LorentzIndex, Momentum[x_,di___]] +
			n4_. Pair[a_LorentzIndex, Momentum[y_,di___]]
		):> (Pair[a, Momentum[ Expand[n3 x + n4 y],di]
							]/;(NumberQ[n3] && NumberQ[n4])
				)
		,
		(n3_ Pair[a_LorentzIndex, Momentum[x_,di___]] +
			n3_ Pair[a_LorentzIndex, Momentum[y_,di___]]
		):> (n3 Pair[a, Momentum[Expand[x+y], di]]/;(!NumberQ[n3])
				),
		(n3_. Pair[a_LorentzIndex, Momentum[x_,di___]] +
			n4_. Pair[a_LorentzIndex, Momentum[y_,di___]]
		):> (Pair[a, Expand[MomentumExpand[
									n3 Momentum[Expand[x], di] + n4 Momentum[Expand[y],di]
												]              ]
							]/;(!NumberQ[n3] || NumberQ[n4]) &&
								FreeQ2[{n3, n4}, {Pair, Momentum, LorentzIndex}]
				)
																				}
	];

Options[MomentumCombine] = {LeafCount -> 1};

MomentumCombine[expr_, OptionsPattern[]] :=
	If[ LeafCount[expr] < OptionValue[LeafCount],
		expr,
		If[ FreeQ[expr, Momentum],
			FeynCalcInternal[expr],
			expr
		] //. {

		(n3_. Momentum[x_,di___] + n4_. Momentum[y_,di___]):>
		(Momentum[ Expand[n3 x + n4 y],di]/;(NumberQ[n3] && NumberQ[n4])),

		(n3_. Pair[a_LorentzIndex, Momentum[x_,di___]] + n4_. Pair[a_LorentzIndex, Momentum[y_,di___]]):>
		(Pair[a, Momentum[ Expand[n3 x + n4 y],di]
		]/;(NumberQ[n3] && NumberQ[n4])),

		(n3_. Pair[a_Momentum, Momentum[x_,di___]] + n4_. Pair[a_Momentum, Momentum[y_,di___]]):>
		(Pair[a, Momentum[ Expand[n3 x + n4 y],di]
		]/;(NumberQ[n3] && NumberQ[n4]))
		}
	];

FCPrint["MomentumCombine.m loaded"];
End[]
