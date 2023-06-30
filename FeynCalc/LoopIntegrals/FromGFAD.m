(* ::Package:: *)



(* :Title: FromGFAD                                                       	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:	Converts GFADs to SFADs and CFADs							*)

(* ------------------------------------------------------------------------ *)


FromGFAD::usage =
"FromGFAD[exp] converts all suitable generic propagator denominators into
standard and Cartesian propagator denominators.

The options InitialSubstitutions and IntermediateSubstitutions can be used to
help the function handle nontrivial propagators.

For propagators containing symbolic variables it might be necessary to tell
the function that those are larger than zero (if applicable), so that
expressions such as $\\sqrt{\\lambda^2}$ can be simplified accordingly.";

FromGFAD::failmsg =
"Error! FromGFAD has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FromGFAD`Private`"]

fgfVerbose::usage="";
dummy::usage="";
optPowerExpand::usage="";
optFeynAmpDenominatorExplicit="";
noFAD::usage="";
sfadHold::usage="";

Options[FromGFAD] = {
		Check						-> 	True,
		FCE							->	False,
		FCI							->	False,
		FCVerbose					-> 	False,
		FeynAmpDenominatorExplicit	-> 	True,
		InitialSubstitutions		->	{},
		IntermediateSubstitutions	->	{},
		LoopMomenta					-> 	{},
		MomentumCombine				->  True,
		PowerExpand					-> 	{}
};

FromGFAD[expr_, OptionsPattern[]] :=
	Block[{	res, ex, pds, pdsConverted,rulePds, check, optLoopMomenta,
			optInitialSubstitutions, optIntermediateSubstitutions},

		optInitialSubstitutions 		= OptionValue[InitialSubstitutions];
		optIntermediateSubstitutions	= OptionValue[IntermediateSubstitutions];
		optPowerExpand					= OptionValue[PowerExpand];
		optFeynAmpDenominatorExplicit	= OptionValue[FeynAmpDenominatorExplicit];
		optLoopMomenta					= OptionValue[LoopMomenta];

		If[	Head[optPowerExpand]=!=List,
			Message[FromGFAD::failmsg,"The value of the option PowerExpand must be a list."];
			Abort[]
		];

		If [OptionValue[FCVerbose]===False,
			fgfVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fgfVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FromGFAD: Entering.", FCDoControl->fgfVerbose];
		FCPrint[3, "FromGFAD: Entering with: ", expr, FCDoControl->fgfVerbose];

		If[	!OptionValue[FCI],
			{ex, optInitialSubstitutions, optIntermediateSubstitutions} =
				FCI[{expr,optInitialSubstitutions, optIntermediateSubstitutions}],
			ex = expr
		];

		If[	FreeQ2[ex, {GenericPropagatorDenominator}],
			(*	Nothing to do.	*)
			FCPrint[1, "FromGFAD: Leaving (nothing to do).", FCDoControl->fgfVerbose];
			Return[ex]
		];

		pds = Cases2[ex, GenericPropagatorDenominator];

		FCPrint[3, "FromGFAD: Unique GenericPropagatorDenominators: ", pds, FCDoControl->fgfVerbose];

		pdsConverted = pds /. optInitialSubstitutions /. optIntermediateSubstitutions;

		FCPrint[3, "FromGFAD: After initial and intermediate substitutions: ", pdsConverted, FCDoControl->fgfVerbose];

		If[OptionValue[MomentumCombine],
			pdsConverted = MomentumCombine[pdsConverted,FCI->True, NumberQ->False, "Quadratic" -> False, Except -> optLoopMomenta]
		];

		FCPrint[3, "FromGFAD: After applying substitution rules: ", pdsConverted, FCDoControl->fgfVerbose];

		pdsConverted = (fromGFAD/@pdsConverted) /. fromGFAD->fromGFAD2 /. fromGFAD2->fromGFAD;

		FCPrint[3, "FromGFAD: After fromGFAD: ", pdsConverted, FCDoControl->fgfVerbose];

		pdsConverted = pdsConverted /. fromGFAD->Identity;

		If[	!FreeQ[pdsConverted,GenericPropagatorDenominator],
			FCPrint[0, "FromGFAD: Following GFADs could not be eliminated: ", FeynAmpDenominator/@SelectNotFree[pdsConverted,GenericPropagatorDenominator,sfadHold], FCDoControl->fgfVerbose];
		];

		If[	OptionValue[Check],
			check=ExpandAll[(powExp[(1/FeynAmpDenominatorExplicit[FeynAmpDenominator/@pdsConverted] -
					1/FeynAmpDenominatorExplicit[FeynAmpDenominator/@pds])/. optInitialSubstitutions /. optIntermediateSubstitutions])/. noFAD->Identity];
			If[	!MatchQ[check,{0..}],
				FCPrint[3, "FromGFAD: Check: ", check, FCDoControl->fgfVerbose];
				Message[FromGFAD::failmsg,"Something went wrong when elmiinating GenericPropagatorDenominators."];
				Abort[]
			];
		];

		rulePds = Thread[Rule[pds,pdsConverted]];

		FCPrint[3, "FromGFAD: Final replacement rule: ", rulePds, FCDoControl->fgfVerbose];


		res = ex /. Dispatch[rulePds];

		If[!FreeQ[res,noFAD],
			res = res //. FeynAmpDenominator[noFAD[r_]] :> r //. FeynAmpDenominator[x___,noFAD[r_],y___] :> r FeynAmpDenominator[x,y]
		];


		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "FromGFAD: Leaving.", FCDoControl->fgfVerbose];
		FCPrint[3, "FromGFAD: Leaving with: ", res, FCDoControl->fgfVerbose];

		res

	];

powExp[x_]:=
	PowerExpand[x,Join[{dummy},optPowerExpand]];

(*SFADs*)

(*	Here we have purely quadratic SFADs	*)

(* # a.a + # a.b + # b.b -> (#a+#b)^2 *)
fromGFAD[GenericPropagatorDenominator[pref1_. Pair[Momentum[a_,dim___],Momentum[a_,dim___]] + pref2_. Pair[Momentum[a_,dim___],Momentum[b_,dim___]]
+ pref3_. Pair[Momentum[b_,dim___],Momentum[b_,dim___]]	 + c_:0,{n_,s_}]] :=
	StandardPropagatorDenominator[powExp[Sqrt[pref1]  Momentum[a, dim] + Sqrt[pref3]  Momentum[b, dim]],0,c,{n, s}]/;
	(ExpandAll[powExp[ExpandScalarProduct[Pair[Sqrt[pref1]  Momentum[a, dim] + Sqrt[pref3]  Momentum[b, dim], Sqrt[pref1]  Momentum[a, dim] + Sqrt[pref3]  Momentum[b, dim]] -
	pref1 Pair[Momentum[a,dim],Momentum[a,dim]] - pref2 Pair[Momentum[a,dim],Momentum[b,dim]] - pref3 Pair[Momentum[b,dim],Momentum[b,dim]]]]] ===0) &&
	FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}] && !Internal`SyntacticNegativeQ[pref2] && !Internal`SyntacticNegativeQ[pref1] &&
	!Internal`SyntacticNegativeQ[pref3];


(* # -a.a + # a.b - # b.b -> (I #a- I #b)^2 *)
fromGFAD[GenericPropagatorDenominator[pref1_. Pair[Momentum[a_,dim___],Momentum[a_,dim___]] + pref2_. Pair[Momentum[a_,dim___],Momentum[b_,dim___]]
+ pref3_. Pair[Momentum[b_,dim___],Momentum[b_,dim___]]	 + c_:0,{n_,s_}]] :=
	StandardPropagatorDenominator[ powExp[I Sqrt[-pref1]  Momentum[a, dim] + I Sqrt[-pref3]  Momentum[b, dim]],0,c,{n, s}]/;
	(ExpandAll[powExp[ExpandScalarProduct[Pair[I Sqrt[-pref1]  Momentum[a, dim] + I Sqrt[-pref3]  Momentum[b, dim], I Sqrt[-pref1]  Momentum[a, dim] + I Sqrt[-pref3]  Momentum[b, dim]] -
	pref1 Pair[Momentum[a,dim],Momentum[a,dim]] - pref2 Pair[Momentum[a,dim],Momentum[b,dim]] - pref3 Pair[Momentum[b,dim],Momentum[b,dim]]]]] ===0) &&
	FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}] &&  Internal`SyntacticNegativeQ[pref1] &&
	Internal`SyntacticNegativeQ[pref3];

(* # a.a - # a.b + # b.b -> (#a-#b)^2 *)
fromGFAD[GenericPropagatorDenominator[pref1_. Pair[Momentum[a_,dim___],Momentum[a_,dim___]] + pref2_. Pair[Momentum[a_,dim___],Momentum[b_,dim___]]
+ pref3_. Pair[Momentum[b_,dim___],Momentum[b_,dim___]]	 + c_:0,{n_,s_}]] :=
	StandardPropagatorDenominator[powExp[Sqrt[pref1]  Momentum[a, dim] - Sqrt[pref3]  Momentum[b, dim]],0,c,{n, s}]/;
	(ExpandAll[powExp[ExpandScalarProduct[Pair[Sqrt[pref1]  Momentum[a, dim] - Sqrt[pref3]  Momentum[b, dim], Sqrt[pref1]  Momentum[a, dim] - Sqrt[pref3]  Momentum[b, dim]] -
	pref1 Pair[Momentum[a,dim],Momentum[a,dim]] - pref2 Pair[Momentum[a,dim],Momentum[b,dim]] - pref3 Pair[Momentum[b,dim],Momentum[b,dim]]]]] ===0) &&
	FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}] && Internal`SyntacticNegativeQ[pref2] && !Internal`SyntacticNegativeQ[pref1]  &&
	!Internal`SyntacticNegativeQ[pref3];

(* # -a.a - # a.b - # b.b -> (I #a+ I #b)^2 *)
fromGFAD[GenericPropagatorDenominator[pref1_. Pair[Momentum[a_,dim___],Momentum[a_,dim___]] + pref2_. Pair[Momentum[a_,dim___],Momentum[b_,dim___]]
+ pref3_. Pair[Momentum[b_,dim___],Momentum[b_,dim___]]	 + c_:0,{n_,s_}]] :=
	StandardPropagatorDenominator[ powExp[I Sqrt[-pref1]  Momentum[a, dim] - I Sqrt[-pref3]  Momentum[b, dim]],0,c,{n, s}]/;
	(ExpandAll[powExp[ExpandScalarProduct[Pair[I Sqrt[-pref1]  Momentum[a, dim] - I Sqrt[-pref3]  Momentum[b, dim], I Sqrt[-pref1]  Momentum[a, dim] - I Sqrt[-pref3]  Momentum[b, dim]] -
	pref1 Pair[Momentum[a,dim],Momentum[a,dim]] - pref2 Pair[Momentum[a,dim],Momentum[b,dim]] - pref3 Pair[Momentum[b,dim],Momentum[b,dim]]]]] ===0) &&
	FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}]  && Internal`SyntacticNegativeQ[pref1] &&
	Internal`SyntacticNegativeQ[pref3];


(*	Mixed quadratic/eikonal SFADs require special care, especially when there are multiple linear scalar products	*)

(* # a.a + # b.c -> (#a)^2 + # b.c *)
fromGFAD[GenericPropagatorDenominator[pref1_. Pair[Momentum[a1_,dim___],Momentum[a1_,dim___]] + pref2_. Pair[Momentum[a2_,dim___],Momentum[b2_,dim___]] + c_:0,{n_,s_}]] :=
	StandardPropagatorDenominator[powExp[Sqrt[pref1]] Momentum[a1,dim], pref2 Pair[Momentum[a2,dim],Momentum[b2,dim]],c,{n, s}]/;
		FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];


fromGFAD[GenericPropagatorDenominator[pref1_. Pair[Momentum[a1_,dim___],Momentum[a1_,dim___]] + pref2_. Pair[Momentum[a2_,dim___],Momentum[b2_,dim___]] + c_:0,{n_,s_}]] :=
	fromGFAD[sfadHold[powExp[Sqrt[pref1]] Momentum[a1,dim], pref2 Pair[Momentum[a2,dim],Momentum[b2,dim]],c,{n, s}]]/;
		!FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];

fromGFAD[sfadHold[slot1_,slot2_, pref2_. Pair[Momentum[a2_,dim___],Momentum[b2_,dim___]] + c_:0 ,slot3_]] :=
	fromGFAD[sfadHold[slot1, slot2 +  pref2 Pair[Momentum[a2,dim],Momentum[b2,dim]], c, slot3]];

fromGFAD[sfadHold[slot1_,slot2_, c_ ,slot3_]] :=
	StandardPropagatorDenominator[slot1, slot2, c, slot3]/;
		FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];

(* # a.a -> (#a)^2 *)
fromGFAD[GenericPropagatorDenominator[pref_. Pair[Momentum[a_,dim___],Momentum[a_,dim___]] + c_:0,{n_,s_}]] :=
	StandardPropagatorDenominator[powExp[Sqrt[pref]] Momentum[a,dim],0,c,{n, s}] /; FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];

(* # a.b -> #a.b *)
fromGFAD[GenericPropagatorDenominator[pref_. Pair[Momentum[a_,dim___],Momentum[b_,dim___]] + c_:0,{n_,s_}]] :=
	StandardPropagatorDenominator[0, pref Pair[Momentum[a,dim],Momentum[b,dim]],c,{n, s}] /; FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];

(* # a.b -> #a.b *)
fromGFAD2[GenericPropagatorDenominator[pref_. Pair[Momentum[a_,dim___],Momentum[b_,dim___]] + c_:0,{n_,s_}]] :=
	fromGFAD[sfadHold[0, pref Pair[Momentum[a,dim],Momentum[b,dim]],c,{n, s}]] /; !FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];

fromGFAD[GenericPropagatorDenominator[c_:0,{n_,s_}]] :=
	noFAD[FeynAmpDenominatorExplicit[FeynAmpDenominator[GenericPropagatorDenominator[c,{n,s}]]]] /; FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}] &&
	optFeynAmpDenominatorExplicit;

(*CFADs*)

(* # a.a + # a.b + # b.b -> (#a+#b)^2 *)
fromGFAD[GenericPropagatorDenominator[pref1_. CartesianPair[CartesianMomentum[a_,dim___],CartesianMomentum[a_,dim___]] + pref2_. CartesianPair[CartesianMomentum[a_,dim___],CartesianMomentum[b_,dim___]]
+ pref3_. CartesianPair[CartesianMomentum[b_,dim___],CartesianMomentum[b_,dim___]]	 + c_:0,{n_,s_}]] :=
	CartesianPropagatorDenominator[powExp[Sqrt[pref1]  CartesianMomentum[a, dim] + Sqrt[pref3]  CartesianMomentum[b, dim]],0,c,{n, s}]/;
	(ExpandAll[powExp[ExpandScalarProduct[CartesianPair[Sqrt[pref1]  CartesianMomentum[a, dim] + Sqrt[pref3]  CartesianMomentum[b, dim], Sqrt[pref1]  CartesianMomentum[a, dim] +
	Sqrt[pref3]  CartesianMomentum[b, dim]] - pref1 CartesianPair[CartesianMomentum[a,dim],CartesianMomentum[a,dim]] - pref2 CartesianPair[CartesianMomentum[a,dim],CartesianMomentum[b,dim]] -
	pref3 CartesianPair[CartesianMomentum[b,dim],CartesianMomentum[b,dim]]]]]===0) && FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];

(* # a.a + # a.b -> (#a)^2 + # a.b *)
fromGFAD[GenericPropagatorDenominator[pref1_. CartesianPair[CartesianMomentum[a1_,dim___],CartesianMomentum[a1_,dim___]] + pref2_. CartesianPair[CartesianMomentum[a2_,dim___],CartesianMomentum[b2_,dim___]] + c_:0,{n_,s_}]] :=
	CartesianPropagatorDenominator[powExp[Sqrt[pref1]] CartesianMomentum[a1,dim], pref2 CartesianPair[CartesianMomentum[a2,dim],CartesianMomentum[b2,dim]],c,{n, s}]/;
	FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];

(* # a.a -> (#a)^2 *)
fromGFAD[GenericPropagatorDenominator[pref_. CartesianPair[CartesianMomentum[a_,dim___],CartesianMomentum[a_,dim___]] + c_:0,{n_,s_}]] :=
	CartesianPropagatorDenominator[powExp[Sqrt[pref]] CartesianMomentum[a,dim],0,c,{n, s}] /;
	FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];

(* # a.b -> #a.b *)
fromGFAD[GenericPropagatorDenominator[pref_. CartesianPair[CartesianMomentum[a_,dim___],CartesianMomentum[b_,dim___]] + c_:0,{n_,s_}]] :=
	CartesianPropagatorDenominator[0, pref CartesianPair[CartesianMomentum[a,dim],CartesianMomentum[b,dim]],c,{n, s}] /;
	FreeQ2[c,{Pair,CartesianPair,TemporalPair,Momentum,CartesianMomentum,TemporalMomentum}];




FCPrint[1,"FromGFAD.m loaded."];
End[]
