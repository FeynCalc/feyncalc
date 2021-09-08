(* ::Package:: *)



(* :Title: ToGFAD                                                       	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Converts propagators to GFADs								*)

(* ------------------------------------------------------------------------ *)


ToGFAD::usage =
"ToGFAD[exp] converts all occurring propagator types (FAD, SFAD, CFAD) to
GFADs. This is mainly useful when doing expansions in kinematic invariants,
where e.g. scalar products may not be appear explicitly when using FAD- or
SFAD-notation.

ToGFAD is the inverse operation to FromGFAD.";

ToGFAD::failmsg =
"Error! ToGFAD has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`ToGFAD`Private`"]

tgfVerbose::usage="";
dummy::usage="";
optPowerExpand::usage="";

Options[ToGFAD] = {
		FCE							->	False,
		FCI							->	False,
		FCVerbose					-> 	False,
		FinalSubstitutions			->	{}
};

ToGFAD[expr_, OptionsPattern[]] :=
	Block[{	res, ex, pds,pdsConverted,rulePds, check,
			optFinalSubstitutions},

		optFinalSubstitutions = OptionValue[FinalSubstitutions];


		If [OptionValue[FCVerbose]===False,
			tgfVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				tgfVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "ToGFAD: Entering.", FCDoControl->tgfVerbose];
		FCPrint[3, "ToGFAD: Entering with: ", expr, FCDoControl->tgfVerbose];

		If[	!OptionValue[FCI],
			{ex, optFinalSubstitutions} = FCI[{expr, optFinalSubstitutions}],
			ex = expr
		];

		If[	FreeQ2[ex, {PropagatorDenominator,StandardPropagatorDenominator,CartesianPropagatorDenominator}],
			(*	Nothing to do.	*)
			FCPrint[1, "ToGFAD: Leaving (nothing to do).", FCDoControl->tgfVerbose];
			Return[ex]
		];

		pds = Cases2[ex, {PropagatorDenominator,StandardPropagatorDenominator,CartesianPropagatorDenominator}];

		FCPrint[3, "ToGFAD: Unique PropagatorDenominators: ", pds, FCDoControl->tgfVerbose];

		pdsConverted = (toGFAD/@pds);

		FCPrint[3, "ToGFAD: After ToGFAD: ", pdsConverted, FCDoControl->tgfVerbose];

		If[	!FreeQ2[pdsConverted,{toGFAD,PropagatorDenominator,StandardPropagatorDenominator,CartesianPropagatorDenominator}],
			Message[ToGFAD::failmsg, "Failed to convert all propagators to GFADs"];
			Abort[]
		];

		pdsConverted = pdsConverted /. optFinalSubstitutions;

		FCPrint[3, "ToGFAD: After applying substitution rules: ", pdsConverted, FCDoControl->tgfVerbose];


		rulePds = Thread[Rule[pds,pdsConverted]];

		FCPrint[3, "ToGFAD: Final replacement rule: ", rulePds, FCDoControl->tgfVerbose];


		res = ex /. Dispatch[rulePds];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "ToGFAD: Leaving.", FCDoControl->tgfVerbose];
		FCPrint[3, "ToGFAD: Leaving with: ", res, FCDoControl->tgfVerbose];

		res
	];


toGFAD[(h:StandardPropagatorDenominator|CartesianPropagatorDenominator)[args__,{n_,s_}]] :=
	GenericPropagatorDenominator[1/FeynAmpDenominatorExplicit[FeynAmpDenominator[h[args,{1,s}]],FCI->True,ExpandScalarProduct->True],{n,s}];

toGFAD[PropagatorDenominator[args__]] :=
	GenericPropagatorDenominator[1/FeynAmpDenominatorExplicit[FeynAmpDenominator[PropagatorDenominator[args]],FCI->True,ExpandScalarProduct->True],{1,1}];


FCPrint[1,"ToGFAD.m loaded."];
End[]
