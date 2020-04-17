(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExpandScalarProduct												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary: Expansion of scalar products and vectors						*)

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
tmpHead::usage="";
objects::usage="";

Options[ExpandScalarProduct] = {
	EpsEvaluate -> False,
	FCE 		-> False,
	FCI 		-> False,
	Full 		-> True,
	Momentum 	-> All
};

ExpandScalarProduct[expr_, OptionsPattern[]] :=
	Block[ {ex, pairList, pairListExpanded, moms, protect, momentum, relevant, null1, null2},

		moms = OptionValue[Momentum];

		If[ moms=!=All && Head[moms]=!=List,
			moms = {moms}
		];

		objects = Join[$FCTensorList,{TemporalPair}];

		If[ !OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];


		(* This is to speed up things when dealing with tirival scalar products *)
		If[	MatchQ[ex, Pair[Momentum[_, ___], Momentum[_, ___]] | CartesianPair[CartesianMomentum[_, ___], CartesianMomentum[_, ___]]] && FreeQ[ex,Plus],
			Return[ex]
		];


		If[ FreeQ2[ex,objects],
			Return[ex]
		];


		relevant = Cases[ex + null1 + null2, (Alternatives @@ objects)[a_, b___] /; !FreeQ[{a, b}, Plus], Infinity];

		If[relevant==={},
			Return[ex],
			relevant = relevant//Sort//DeleteDuplicates
		];

		If [moms===All,
			pairList = Select[Cases2[relevant, objects], !FreeQ2[#, TensorArgsList]&];
			pairListExpanded = pairList,
			pairList = Select[Cases2[relevant, objects], (!FreeQ2[#, TensorArgsList] && !FreeQ2[#, moms])&];
			If[ TrueQ[!OptionValue[Full]],
				pairListExpanded = pairList //. {
					Momentum[c_. mom_ + rest_: 0, dim___] /; MemberQ[moms, mom] :> momentum[c mom + protect[rest], dim],
					Momentum[rest_, dim___] /; FreeQ[rest, moms] :> momentum[protect[rest], dim]
				} /. momentum -> Momentum,
				pairListExpanded = pairList
			]
		];

		If[ pairList =!= {},
			pairListExpanded =  (pairexpand/@pairListExpanded) /. protect -> Identity;
			ex = ex /. Dispatch[Thread[Rule[pairList,pairListExpanded]]]
		];

		If[	OptionValue[EpsEvaluate] && !FreeQ[ex,Eps],
			ex = EpsEvaluate[ex,FCI->True,Momentum->OptionValue[Momentum]]
		];

		If[	OptionValue[FCE],
			ex = FCE[ex]
		];

		ex
	];

pairexpand[x_] :=
	x /. (head : (Alternatives @@ objects))[arg__]/; head=!=Eps :>scevdoit[head,arg] //. {

		(head : (Alternatives @@ objects))[arg1___, z_ n_?NumberQ, arg2___]/; head=!=Eps :>
			n head[arg1,z,arg2],

		(head : (Alternatives @@ objects))[arg1___, z_ n_/;DataType[n,FCVariable], arg2___]/; head=!=Eps :>
			n head[arg1,z,arg2]
	};


scevdoit[head_,arg__] :=
	Distribute[tmpHead@@(Expand[MomentumExpand/@{arg}])]/.tmpHead->head;

FCPrint[1,"ExpandScalarProduct.m loaded."];
End[]
