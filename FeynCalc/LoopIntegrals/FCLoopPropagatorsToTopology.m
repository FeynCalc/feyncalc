(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopPropagatorsToTopology										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Creates list of propagators 								*)

(* ------------------------------------------------------------------------ *)

FCLoopPropagatorsToTopology::usage =
"FCLoopPropagatorsToTopology[{prop1, prop2, ...}] takes a list of Pairs and
FeynAmpDenominators and converts it into a list of propagators that can be
used to describe a topology.

The input can also consist of an FCTopology object or a list thereof.";

FCLoopPropagatorsToTopology::failmsg =
"Error! FCLoopPropagatorsToTopology encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"]
End[]

Begin["`FCLoopPropagatorsToTopology`Private`"]

fclpttVerbose::usage="";

Options[FCLoopPropagatorsToTopology] = {
	DeleteDuplicates	-> True,
	ExpandScalarProduct -> False,
	FCVerbose 			-> False,
	FCE 				-> False,
	FCI 				-> False,
	MomentumCombine 	-> False
};

FCLoopPropagatorsToTopology[topos : {__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopPropagatorsToTopology[#, opts]&/@topos;

FCLoopPropagatorsToTopology[topoRaw_FCTopology, opts:OptionsPattern[]] :=
	Block[{topo,optFinalSubstitutions},


		If[	OptionValue[FCI],
			topo = topoRaw,
			topo = FCI[topoRaw]
		];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCLoopPropagatorsToTopology::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		FCLoopPropagatorsToTopology[topo[[2]], Join[{FCI->True},FilterRules[{opts}, Except[FCI]]]]
	];

FCLoopPropagatorsToTopology[props_List /; props =!= {} && FreeQ[props,FCTopology], OptionsPattern[]] :=
	Block[{expr, tmp, res},

		If [ OptionValue[FCVerbose]===False,
			fclpttVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fclpttVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1,"FCLoopPropagatorsToTopology: Entering.", FCDoControl->fclpttVerbose];
		FCPrint[3,"FCLoopPropagatorsToTopology: Entering with: ", props, FCDoControl->fclpttVerbose];

		If[! OptionValue[FCI],
			expr = FCI[props],
			expr = props
		];



		If[	OptionValue[MomentumCombine],
			expr = MomentumCombine[expr, FCI -> True]
		];

		tmp = expr /. {
			FeynAmpDenominator[a_PropagatorDenominator] :>
				1/FeynAmpDenominator[a],
			FeynAmpDenominator[(h : StandardPropagatorDenominator | CartesianPropagatorDenominator | GenericPropagatorDenominator)[a__, {n_, s_}]] /;
				n=!=0 :> FeynAmpDenominator[h[a, {-1, s}]]
		};

		FCPrint[3,"FCLoopPropagatorsToTopology: tmp: ", tmp, FCDoControl->fclpttVerbose];

		res = FeynAmpDenominatorExplicit[#, FCI -> True, ExpandScalarProduct -> False] & /@ tmp;

		FCPrint[3,"FCLoopPropagatorsToTopology: Raw result: ", res, FCDoControl->fclpttVerbose];

		If[	OptionValue[ExpandScalarProduct],
			res = ExpandScalarProduct[res, FCI -> True]
		];


		If[	OptionValue[DeleteDuplicates],
			If[	DeleteDuplicates[res]=!=res,
				Print[res];
				Message[FCLoopPropagatorsToTopology::failmsg, "The list of propagators contains dupicates."];
				Abort[]
			]
		];

		If[	!FreeQ2[Denominator/@res,{Pair,CartesianPair,TemporalPair}],
			Message[FCLoopPropagatorsToTopology::failmsg, "Inverse scalar products should be entered via FeynAmpDenominators."];
			Abort[]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopPropagatorsToTopology: Leaving.", FCDoControl->fclpttVerbose];
		FCPrint[3,"FCLoopPropagatorsToTopology: Leaving with: ", res, FCDoControl->fclpttVerbose];


		res
];


FCPrint[1,"FCLoopPropagatorsToTopology.m loaded."];
End[]
