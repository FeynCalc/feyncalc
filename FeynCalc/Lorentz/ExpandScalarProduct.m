(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExpandScalarProduct												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
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
	FCI -> True,
	FCE -> False,
	Full -> True,
	Momentum -> All
};

ExpandScalarProduct[expr_, OptionsPattern[]] :=
	Block[ {ex, pairList, pairListExpanded, moms, protect, momentum},

		moms = OptionValue[Momentum];

		If[ moms=!=All && Head[moms]=!=List,
			moms = {moms}
		];

		objects = Join[$FCTensorList,{TemporalPair}];

		If[ OptionValue[FCI],
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

		If [moms===All,
			pairList = Select[Cases2[ex, objects], !FreeQ2[#, TensorArgsList]&];
			pairListExpanded = pairList,
			pairList = Select[Cases2[ex, objects], (!FreeQ2[#, TensorArgsList] && !FreeQ2[#, moms])&];
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

(* TODO this is a legacy syntax that one should get rid of! *)
ExpandScalarProduct[x_, y:Except[_?OptionQ], OptionsPattern[]] :=
	scevdoit[Pair,x, y];

pairexpand[x_] :=
	x /. (head : (Alternatives @@ objects))[arg__]/; head=!=Eps :>scevdoit[head,arg] ;

scevdoit[head_,arg__] :=
	Distribute[tmpHead@@(Expand[MomentumExpand/@{arg}])]/.tmpHead->head;

FCPrint[1,"ExpandScalarProduct.m loaded."];
End[]
