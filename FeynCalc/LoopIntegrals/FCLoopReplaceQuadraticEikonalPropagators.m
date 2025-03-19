(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopReplaceQuadraticEikonalPropagators							*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Tries to eliminate quadratic-eikonal propagators by
				completing the square										*)

(* ------------------------------------------------------------------------ *)

FCLoopReplaceQuadraticEikonalPropagators::usage =
"FCLoopReplaceQuadraticEikonalPropagators[topologies] identifies SFADs and
CFADs in topologies that represent mixed quadratic-eikonal propagators, e.g.
$[p^2 - 2 p \\cdot q]$. Using the information on loop momenta provided by the
user the routine will try to rewrite those denominators by completing the
square, e.g. as in $[(p-q)^2 - q^2]$.

This procedure is useful because one cannot easily determine the momentum flow
from looking at quadratic-eikonal propagators as it is possible in the case of
purely quadratic ones.

For this to work it is crucial to specify the loop momenta via the LoopMomenta
option as well as the kinematics (IntermediateSubstitutions) and the rules for
completing the square (InitialSubstitutions) on the purely loop-momentum
dependent piece of the propagator (e.g. $p_1^2 - 2 p_1 \\cdot p_2 + p_2^2$ goes
to $(p_1+p_2)^2$.

Internally this routine uses ToGFAD and FromGFAD.";

FCLoopReplaceQuadraticEikonalPropagators::failmsg =
"FCLoopReplaceQuadraticEikonalPropagators has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopReplaceQuadraticEikonalPropagators`Private`"]

rqepVerbose::usage="";

Options[FCLoopReplaceQuadraticEikonalPropagators] = {
		Check						-> 	True,
		ExpandScalarProduct			-> 	True,
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

FCLoopReplaceQuadraticEikonalPropagators[topo_FCTopology, opts:OptionsPattern[]]:=
	FCLoopReplaceQuadraticEikonalPropagators[{topo}, opts];

FCLoopReplaceQuadraticEikonalPropagators[toposRaw_List, OptionsPattern[]]:=
	Block[{	topos, optInitialSubstitutions, optIntermediateSubstitutions,
			optPowerExpand, optFeynAmpDenominatorExplicit, optLoopMomenta,
			props, propsConverted, repRule},

		optInitialSubstitutions 		= OptionValue[InitialSubstitutions];
		optIntermediateSubstitutions	= OptionValue[IntermediateSubstitutions];

		If [OptionValue[FCVerbose]===False,
			rqepVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				rqepVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FCLoopReplaceQuadraticEikonalPropagators: Entering.", FCDoControl->rqepVerbose];
		FCPrint[3, "FCLoopReplaceQuadraticEikonalPropagators: Entering with: ", {toposRaw}, FCDoControl->rqepVerbose];

		{optInitialSubstitutions,optIntermediateSubstitutions} = FRH[{optInitialSubstitutions,optIntermediateSubstitutions}];

		If[	!OptionValue[FCI],
			{topos, optInitialSubstitutions, optIntermediateSubstitutions} =
				FCI[{toposRaw,optInitialSubstitutions, optIntermediateSubstitutions}],
			topos = toposRaw
		];

		props = Union[Cases[topos, FeynAmpDenominator[(StandardPropagatorDenominator|CartesianPropagatorDenominator)[a_ /; a =!= 0, b_ /; b =!= 0, ___]], Infinity]];

		If[props==={},
			FCPrint[1, "FCLoopReplaceQuadraticEikonalPropagators: Nothing to do.", FCDoControl->rqepVerbose];
			Return[topos]
		];

		propsConverted = ToGFAD[props,FCI->True,FinalSubstitutions->optIntermediateSubstitutions];

		FCPrint[3, "FCLoopReplaceQuadraticEikonalPropagators: After ToGFAD: ", propsConverted, FCDoControl->rqepVerbose];

		propsConverted = FromGFAD[propsConverted,FCI->True,
			InitialSubstitutions->optInitialSubstitutions,
			IntermediateSubstitutions->optIntermediateSubstitutions,
			PowerExpand->OptionValue[PowerExpand],
			FeynAmpDenominatorExplicit->OptionValue[FeynAmpDenominatorExplicit],
			ExpandScalarProduct->OptionValue[ExpandScalarProduct],
			LoopMomenta->OptionValue[LoopMomenta],
			MomentumCombine->OptionValue[MomentumCombine],
			Check->OptionValue[Check]];

		repRule = Thread[Rule[props,propsConverted]];

		FCPrint[3, "FCLoopReplaceQuadraticEikonalPropagators: Replacement rule: ", repRule, FCDoControl->rqepVerbose];

		topos = topos /. Dispatch[repRule];

		FCPrint[1, "FCLoopReplaceQuadraticEikonalPropagators: Leaving.", FCDoControl->rqepVerbose];
		FCPrint[3, "FCLoopReplaceQuadraticEikonalPropagators: Leaving with: ", topos, FCDoControl->rqepVerbose];

		topos


		]

FCPrint[1,"FCLoopReplaceQuadraticEikonalPropagators.m loaded."];
End[]
