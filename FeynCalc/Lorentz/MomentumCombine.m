(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: MomentumCombine													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  MomentumCombine												*)

(* ------------------------------------------------------------------------ *)

MomentumCombine::usage =
"MomentumCombine[expr] is the inverse operation to MomentumExpand and
ExpandScalarProduct. MomentumCombine combines also Pairs.
Notice, that MomentumCombine cannot complete squares. It can, however, bring
expressions containing scalar products to a suitable form that allows for a
square completion using other means.

This function offers multiple options.

The option NumberQ (default is True) specifies whether one should only merge
quantities with numerical prefactors or not. Setting it to False allows for
symbolic prefactors.

Setting the option \"Quadratic\" to False (default is True) effectively means
that momenta squared will not be combined with anything else.

With the option \"ExcludeScalarProducts\" we can ensure that scalar products
containing any of the momenta listed are not merged with anything else. So a.x
+ a.y can be merged either if a contains no such momenta, or if both x and y
are free of them.

The option Except forbids merging the listed momenta with anything else. It is
much more restrictive than \"ExcludeScalarProducts\" that allows for merging
terms linear in the listed momenta.

The option Select allows for gathering all terms linear in the given momenta
before applying any other combining rules.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`MomentumCombine`Private`"];

im::usage="";

Options[MomentumCombine] = {
	"ExcludeScalarProducts"	-> {},
	"Quadratic"				-> True,
	Complex					-> True,
	Except					-> {},
	FCE 					-> False,
	FCI 					-> False,
	FCVerbose				-> False,
	FV 						-> True,
	Factoring				-> False,
	FCParallelize			-> False,
	LC 						-> True,
	LeafCount				-> 1,
	NumberQ 				-> True,
	Select					-> {},
	SP 						-> True
};

MomentumCombine[expr_, OptionsPattern[]] :=
	Block[{	ex, res, rules, optFactoring, moms, momsFac, optSelect, mcVerbose, time,
			optExcept, optExcludeScalarProducts, optQuadratic, optNumberQ},

		optNumberQ 					= OptionValue[NumberQ];
		optQuadratic				= OptionValue["Quadratic"];
		optExcludeScalarProducts	= OptionValue["ExcludeScalarProducts"];
		optExcept					= OptionValue[Except];
		optFactoring				= OptionValue[Factoring];
		optSelect					= OptionValue[Select];

		If [OptionValue[FCVerbose]===False,
			mcVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				mcVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"MomentumCombine: Entering.", FCDoControl->mcVerbose];
		FCPrint[3,"MomentumCombine: Entering with ", expr, FCDoControl->mcVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"MomentumCombine: Applying FCI." , FCDoControl->mcVerbose];
		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];
		FCPrint[1,"MomentumCombine: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mcVerbose];

		If[ Head[expr]=!=List,
			ex = {ex}
		];

		If[ LeafCount[ex] < OptionValue[LeafCount],
			FCPrint[1,"MomentumCombine: Leaving due to the LeafCount being too small.", FCDoControl->mcVerbose];
			If[ Head[expr]=!=List,
				Return[First[ex]],
				Return[ex]
			];
		];

		time=AbsoluteTime[];
		FCPrint[1,"MomentumCombine: Building up the ruleset." , FCDoControl->mcVerbose];
		rules=rulesMain[optNumberQ];

		If[	OptionValue[FV],
			rules = Join[rules,rulesFV[optExcept]]
		];

		If[	OptionValue[SP],
			rules = Join[rules,rulesSP[optQuadratic, optExcept, optExcludeScalarProducts,optNumberQ]]
		];

		If[	OptionValue[LC],
			rules = Join[rules,rulesLC]
		];

		FCPrint[1,"MomentumCombine: Done building up the ruleset, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mcVerbose];


		If[	optSelect=!={},
			time=AbsoluteTime[];
			FCPrint[1,"MomentumCombine: Evaluating the Select option." , FCDoControl->mcVerbose];
			ex = Fold[(#1 //. rulesSpecificSP[#2,optQuadratic, optExcept, optExcludeScalarProducts,optNumberQ])&, ex,optSelect];

			If[	optFactoring=!=False,

				moms = Cases2[ex,Momentum];
				momsFac = moms /. Momentum[a_, dim___] :> Momentum[optFactoring[a], dim];
				ex = ex /. Dispatch[Thread[Rule[moms,momsFac]]]
			];
			FCPrint[1,"MomentumCombine: Done evaluating the select option, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mcVerbose];
		];

		time=AbsoluteTime[];
		If[	$ParallelizeFeynCalc && OptionValue[FCParallelize] && Length[ex]>1,

			FCPrint[1,"MomentumCombine: Applying the ruleset to the input expression in parallel." , FCDoControl->mcVerbose];
				With[{xxx = rules},
					ParallelEvaluate[FCParallelContext`MomentumCombine`rules = Dispatch[xxx]; , DistributedContexts -> None]
				];

			res = ParallelMap[(#//. FCParallelContext`MomentumCombine`rules )&,ex, DistributedContexts -> None,
				Method->"ItemsPerEvaluation" -> Ceiling[N[Length[ex]/$KernelCount]/10]],

			FCPrint[1,"MomentumCombine: Applying the ruleset to the input expression." , FCDoControl->mcVerbose];
			res = ex //. Dispatch[rules]
		];
		FCPrint[1,"MomentumCombine: Done applying the ruleset to the input expression, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mcVerbose];


		If[	OptionValue[Complex] && !FreeQ[res,Complex],
			time=AbsoluteTime[];
			FCPrint[1,"MomentumCombine: Handling complex momenta." , FCDoControl->mcVerbose];
			res = res /. {
				(h: Momentum|CartesianMomentum|TemporalMomentum)[a_, dim___]/; !FreeQ[a, Complex] :>
					factorIm[h[a, dim]]
			};
			FCPrint[1,"MomentumCombine: Done handling complex momenta, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mcVerbose];
		];

		If[	optFactoring=!=False,
			time=AbsoluteTime[];
			FCPrint[1,"MomentumCombine: Factoring the arguments." , FCDoControl->mcVerbose];
			moms = Cases2[res,Momentum];
			momsFac = moms /. Momentum[a_, dim___] :> Momentum[optFactoring[a], dim];
			res = res /. Dispatch[Thread[Rule[moms,momsFac]]];
			FCPrint[1,"MomentumCombine: Done factoring the arguments, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->mcVerbose];
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		If[ Head[expr]=!=List,
			res = First[res]
		];

		FCPrint[1,"MomentumCombine: Leaving.", FCDoControl->mcVerbose];
		FCPrint[3,"MomentumCombine: Leaving with ", expr, FCDoControl->mcVerbose];
		res
	];

factorIm[(h: Momentum|CartesianMomentum|TemporalMomentum)[a_, dim___]] :=
	h[Factor2[a /. HoldPattern[Complex[b__]] -> complex[b]] /. im -> I, dim];

complex[a_, b_] := a + im b;

(* Joining single momenta inside pairs or Cartesian pairs *)
rulesMain[optNumberQ_] := {
	(n3_. Momentum[x_, dim_:4] + n4_. Momentum[y_, dim_:4]):>
		(Momentum[ Expand[n3 x + n4 y],dim]/; (NumberQ[n3] && NumberQ[n4])),

	(n3_. Momentum[x_, dim_:4] + n4_. Momentum[y_, dim_:4]):>
		(Momentum[ Expand[n3 x + n4 y],dim]/; !optNumberQ && FreeQ2[{n3,n4},{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}]),

	(n3_. LightConePerpendicularComponent[Momentum[x_, dim_:4],n_,nb_] + n4_. LightConePerpendicularComponent[Momentum[y_, dim_:4],n_,nb_]):>
		(LightConePerpendicularComponent[Momentum[ Expand[n3 x + n4 y],dim],n,nb]/; optNumberQ && (NumberQ[n3] && NumberQ[n4])),

	(n3_. LightConePerpendicularComponent[Momentum[x_, dim_:4],n_,nb_] + n4_. LightConePerpendicularComponent[Momentum[y_, dim_:4],n_,nb_]):>
		(LightConePerpendicularComponent[Momentum[ Expand[n3 x + n4 y],dim],n,nb]/; !optNumberQ && FreeQ2[{n3,n4},{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}]),

	(n3_. TemporalMomentum[x_] + n4_. TemporalMomentum[y_]):>
		(TemporalMomentum[ Expand[n3 x + n4 y]]/; (NumberQ[n3] && NumberQ[n4])),

	(n3_. TemporalMomentum[x_] + n4_. TemporalMomentum[y_]):>
		(TemporalMomentum[ Expand[n3 x + n4 y]]/; !optNumberQ && FreeQ2[{n3,n4},{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}]),

	(n3_. CartesianMomentum[x_, dim_:3] + n4_. CartesianMomentum[y_, dim_:3]):>
		(CartesianMomentum[ Expand[n3 x + n4 y],dim]/; (NumberQ[n3] && NumberQ[n4])),

	(n3_. CartesianMomentum[x_, dim_:3] + n4_. CartesianMomentum[y_, dim_:3]):>
		(CartesianMomentum[ Expand[n3 x + n4 y],dim]/; !optNumberQ && FreeQ2[{n3,n4},{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}])
};

rulesSpecificSP[chosenMom_, optQuadratic_, optExcept_, optExcludeScalarProducts_, optNumberQ_] := {

	(* Quadratic on, numeric on: 2 k1.n + 3 k2.n = (2*k1+3*k2).n or 2 k1.k1 + 3 k1.n = (2*k1+3*n).k1   *)
	(n3_. Pair[Momentum[chosenMom,dim_:4], Momentum[x_, dim_:4]] + n4_. Pair[Momentum[chosenMom,dim_:4], Momentum[y_, dim_:4]]):>
		Pair[Momentum[chosenMom,dim], Momentum[Expand[n3 x + n4 y],dim]]/; optQuadratic &&
		(NumberQ[n3] && NumberQ[n4]) &&
		(FreeQ2[{x,y},optExcludeScalarProducts]) &&
		!IntersectingQ[{x,y},optExcept],

	(* Quadratic on, numeric off: x k1.n + y k2.n = (x*k1+y*k2).n or x k1.k1 + y k1.n = (x*k1+y*n).k1   *)
	(n3_. Pair[Momentum[chosenMom,dim_:4], Momentum[x_, dim_:4]] + n4_. Pair[Momentum[chosenMom,dim_:4], Momentum[y_, dim_:4]]):>
		Pair[Momentum[chosenMom,dim], Momentum[Expand[n3 x + n4 y],dim]]/; optQuadratic &&
		!optNumberQ && FreeQ2[{n3,n4},{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts]) &&
		!IntersectingQ[{x,y},optExcept],

	(* Quadratic off, numeric on: 2 k1.n + 3 k2.n = (2*k1+3*k2).n only   *)
	(n3_. Pair[Momentum[chosenMom,dim_:4], Momentum[x_, dim_:4]] + n4_. Pair[Momentum[chosenMom,dim_:4], Momentum[y_, dim_:4]]):>
		Pair[Momentum[chosenMom,dim], Momentum[Expand[n3 x + n4 y],dim]]/; !optQuadratic && (chosenMom=!=x && chosenMom=!=y) &&
		(NumberQ[n3] && NumberQ[n4]) &&
		(FreeQ2[{x,y},optExcludeScalarProducts]) &&
		!IntersectingQ[{x,y},optExcept],

	(* Quadratic off, numeric off: x k1.n + y k2.n = (x*k1+y*k2).n only   *)
	(n3_. Pair[Momentum[chosenMom,dim_:4], Momentum[x_, dim_:4]] + n4_. Pair[Momentum[chosenMom,dim_:4], Momentum[y_, dim_:4]]):>
		Pair[Momentum[chosenMom,dim], Momentum[Expand[n3 x + n4 y],dim]]/; !optQuadratic && chosenMom=!=x && chosenMom=!=y &&
		!optNumberQ && FreeQ2[{n3,n4},{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts]) &&
		!IntersectingQ[{x,y},optExcept]

};

(* Joining scalar products *)
rulesSP[optQuadratic_, optExcept_, optExcludeScalarProducts_,optNumberQ_] := {

(*
	"Quadratic"->False effectively means that momenta squared will not be combined with anything else

	With the "ExcludeScalarProducts" option we can ensure that scalar products containing any of the
	momenta listed are not merged with anything else. So "a.x + a.y" can be merged either if "a" contains
	no such momenta, or if both "x" and "y" are free of them.

	The "Except" option completely prohibits joining scalar products containing listed momenta with anything
	else.
*)

	(* Quadratic on, numeric on: 2 k1.n + 3 k2.n = (2*k1+3*k2).n or 2 k1.k1 + 3 k1.n = (2*k1+3*n).k1   *)
	(n3_. Pair[a_Momentum, Momentum[x_, dim_:4]] + n4_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		Pair[a, Momentum[Expand[n3 x + n4 y],dim]]/; optQuadratic &&
		(NumberQ[n3] && NumberQ[n4] && n3=!=n4) &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[{a},optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],

	(* Quadratic on, numeric off: x k1.n + y k2.n = (x*k1+y*k2).n or x k1.k1 + y k1.n = (x*k1+y*n).k1   *)
	(n3_. Pair[a_Momentum, Momentum[x_, dim_:4]] + n4_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		Pair[a, Momentum[Expand[n3 x + n4 y],dim]]/; optQuadratic &&
		!optNumberQ && FreeQ2[{n3,n4},{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],

	(* Quadratic off, numeric on: 2 k1.n + 3 k2.n = (2*k1+3*k2).n only   *)
	(n3_. Pair[a_Momentum, Momentum[x_, dim_:4]] + n4_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		Pair[a, Momentum[Expand[n3 x + n4 y],dim]]/; !optQuadratic && (a[[1]]=!=x && a[[1]]=!=y) &&
		(NumberQ[n3] && NumberQ[n4] && n3=!=n4) &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],

	(* Quadratic off, numeric off: x k1.n + y k2.n = (x*k1+y*k2).n only   *)
	(n3_. Pair[a_Momentum, Momentum[x_, dim_:4]] + n4_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		Pair[a, Momentum[Expand[n3 x + n4 y],dim]]/; !optQuadratic && a[[1]]=!=x && a[[1]]=!=y &&
		!optNumberQ && FreeQ2[{n3,n4},{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],

	(* These rules apply both for numerical and symbolical prefactors *)

	(* Quadratic on, + *)
	(n_. Pair[a_Momentum, Momentum[x_, dim_:4]] + n_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		n Pair[a, Momentum[Expand[x + y],dim]]/; optQuadratic &&
		FreeQ2[n,{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],

	(* Quadratic off, + *)
	(n_. Pair[a_Momentum, Momentum[x_, dim_:4]] + n_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		n Pair[a, Momentum[Expand[x + y],dim]]/; !optQuadratic && !optNumberQ && a[[1]]=!=x && a[[1]]=!=y &&
		FreeQ2[n,{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],

	(* Quadratic on, - *)
	(n_. Pair[a_Momentum, Momentum[x_, dim_:4]] - n_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		n Pair[a, Momentum[Expand[x - y],dim]]/; optQuadratic &&
		FreeQ2[n,{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],

	(* Quadratic off, - *)
	(n_. Pair[a_Momentum, Momentum[x_, dim_:4]] - n_. Pair[a_Momentum, Momentum[y_, dim_:4]]):>
		n Pair[a, Momentum[Expand[x - y],dim]]/; !optQuadratic && !optNumberQ && a[[1]]=!=x && a[[1]]=!=y &&
		FreeQ2[n,{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],

	(*TODO add all the above options here *)
	(n3_. Pair[LightConePerpendicularComponent[a_Momentum,n_,nb_],
		LightConePerpendicularComponent[Momentum[x_, dim_:4],n_,nb_]] + n4_. Pair[LightConePerpendicularComponent[a_Momentum,n_,nb_], LightConePerpendicularComponent[Momentum[y_, dim_:4],n_,nb_]]):>
		Pair[LightConePerpendicularComponent[a,n,nb], LightConePerpendicularComponent[Momentum[Expand[n3 x + n4 y],dim],n,nb]]/;
		(NumberQ[n3] && NumberQ[n4] && n3=!=n4) &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],


	(* These rules apply both for numerical and symbolical prefactors *)

	(n1_. Pair[LightConePerpendicularComponent[a_Momentum,n_,nb_], LightConePerpendicularComponent[Momentum[x_, dim_:4],n_,nb_]] +
	n1_. Pair[LightConePerpendicularComponent[a_Momentum,n_,nb_], LightConePerpendicularComponent[Momentum[y_, dim_:4],n_,nb_]]):>
		n1 Pair[LightConePerpendicularComponent[a,n,nb], LightConePerpendicularComponent[Momentum[Expand[x + y],dim],n,nb]]/;
		FreeQ2[n,{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],

	(n1_. Pair[LightConePerpendicularComponent[a_Momentum,n_,nb_], LightConePerpendicularComponent[Momentum[x_, dim_:4],n_,nb_]] -
	n1_. Pair[LightConePerpendicularComponent[a_Momentum,n_,nb_], LightConePerpendicularComponent[Momentum[y_, dim_:4],n_,nb_]]):>
		n1 Pair[LightConePerpendicularComponent[a,n,nb], LightConePerpendicularComponent[Momentum[Expand[x - y],dim],n,nb]]/;
		FreeQ2[n,{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] &&
		(FreeQ2[{x,y},optExcludeScalarProducts] || FreeQ2[a[[1]],optExcludeScalarProducts]) &&
		!IntersectingQ[{a,x,y},optExcept],


	(n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[x_, dim_:3]] + n4_. CartesianPair[a_CartesianMomentum, CartesianMomentum[y_, dim_:3]]):>
		CartesianPair[a, CartesianMomentum[Expand[n3 x + n4 y],dim]]/;(NumberQ[n3] && NumberQ[n4]  && n3=!=n4) && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[x_, dim_:3]] + n4_. CartesianPair[a_CartesianMomentum, CartesianMomentum[y_, dim_:3]]):>
		CartesianPair[a, CartesianMomentum[Expand[n3 x + n4 y],dim]]/; !optNumberQ && a[[1]]=!=x && a[[1]]=!=y && FreeQ2[{n3,n4},{Pair,CartesianPair,TemporalPair,FeynAmpDenominator,DOT}] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[x_, dim_:3]] + n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[y_, dim_:3]]):>
		n3 CartesianPair[a, CartesianMomentum[Expand[x + y],dim]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[x_, dim_:3]] - n3_. CartesianPair[a_CartesianMomentum, CartesianMomentum[y_, dim_:3]]):>
		n3 CartesianPair[a, CartesianMomentum[Expand[x - y],dim]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y]
}

rulesFV[optExcept_] := {
	(n3_. Pair[a_LorentzIndex, Momentum[x_, dim_:4]] + n4_. Pair[a_LorentzIndex, Momentum[y_, dim_:4]]):>
		Pair[a, Momentum[ Expand[n3 x + n4 y],dim]]/; (NumberQ[n3] && NumberQ[n4] && n3=!=n4) && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. Pair[a_LorentzIndex, Momentum[x_, dim_:4]] + n3_. Pair[a_LorentzIndex, Momentum[y_, dim_:4]]):>
		n3 Pair[a, Momentum[Expand[x+y], dim]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. Pair[a_LorentzIndex, Momentum[x_, dim_:4]] - n3_. Pair[a_LorentzIndex, Momentum[y_, dim_:4]]):>
		n3 Pair[a, Momentum[Expand[x-y], dim]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],


	(n3_. Pair[LightConePerpendicularComponent[a_LorentzIndex,n_,nb_], LightConePerpendicularComponent[Momentum[x_, dim_:4],n_,nb_]]
	+ n4_. Pair[LightConePerpendicularComponent[a_LorentzIndex,n_,nb_], LightConePerpendicularComponent[Momentum[y_, dim_:4],n_,nb_]]):>
		Pair[LightConePerpendicularComponent[a,n,nb], LightConePerpendicularComponent[Momentum[ Expand[n3 x + n4 y],dim],n,nb]]/; (NumberQ[n3] && NumberQ[n4] && n3=!=n4) && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. Pair[LightConePerpendicularComponent[a_LorentzIndex,n_,nb_], LightConePerpendicularComponent[Momentum[x_, dim_:4],n_,nb_]] +
	n3_. Pair[LightConePerpendicularComponent[a_LorentzIndex,n_,nb_], LightConePerpendicularComponent[Momentum[y_, dim_:4],n_,nb_]]):>
		n3 Pair[LightConePerpendicularComponent[a,n,nb], LightConePerpendicularComponent[Momentum[Expand[x+y], dim],n,nb]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. Pair[LightConePerpendicularComponent[a_LorentzIndex,n_,nb_], LightConePerpendicularComponent[Momentum[x_, dim_:4],n_,nb_]] -
	n3_. Pair[LightConePerpendicularComponent[a_LorentzIndex,n_,nb_], LightConePerpendicularComponent[Momentum[y_, dim_:4],n_,nb_]]):>
		n3 Pair[LightConePerpendicularComponent[a,n,nb], LightConePerpendicularComponent[Momentum[Expand[x-y], dim],n,nb]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[x_, dim_:3]] + n4_. CartesianPair[a_CartesianIndex, CartesianMomentum[y_, dim_:3]]):>
		CartesianPair[a, CartesianMomentum[ Expand[n3 x + n4 y],dim]]/; (NumberQ[n3] && NumberQ[n4]  && n3=!=n4) && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[x_, dim_:3]] + n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[y_, dim_:3]]):>
		n3 CartesianPair[a, CartesianMomentum[Expand[x+y], dim]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[x_, dim_:3]] - n3_. CartesianPair[a_CartesianIndex, CartesianMomentum[y_, dim_:3]]):>
		n3 CartesianPair[a, CartesianMomentum[Expand[x-y], dim]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[x_]] + n4_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[y_]]):>
		TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[ Expand[n3 x + n4 y]]]/; (NumberQ[n3] && NumberQ[n4]  && n3=!=n4) && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[x_]] + n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[y_]]):>
		n3 TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[Expand[x+y]]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y],

	(n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[x_]] - n3_. TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[y_]]):>
		n3 TemporalPair[ExplicitLorentzIndex[0], TemporalMomentum[Expand[x-y]]] && !MemberQ[optExcept,x] && !MemberQ[optExcept,y]

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

FCPrint["MomentumCombine.m loaded"];
End[]
