(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopAddScalingParameter										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  	Add scalings of masses and momenta							*)

(* ------------------------------------------------------------------------ *)

FCLoopAddScalingParameter::usage =
"FCLoopAddScalingParameter[topo, la, rules] multiplies masses and momenta in
the propagators of the topology topo by the scaling parameter la according to
the scaling rules in rules. The id of the topology remains unchanged. This is
useful e.g. for asymptotic expansions of the corresponding loop integrals
given as GLIs.

The scaling variable should be declared as FCVariable via the DataType
mechanism.

Notice that if all terms in a propagator have the same scaling, the scaling
variable in the respective propagator will be set to unity.";

FCLoopAddScalingParameter::failmsg =
"Error! FCLoopAddScalingParameter has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

FCLoopAddScalingParameter::notfcvar =
"Warning! The scaling variable `1` has not been declared to be of FCVariable datatype. This can lead \
to issues when working with the resulting topologies. Please evaluate \"DataType[`1`,FCVariable]=True\" to \
fix this problem.";

Begin["`Package`"]
End[]

Begin["`FCLoopAddScalingParameter`Private`"]

fclaspVerbose::usage = "";
lhs::usage ="";

Options[FCLoopAddScalingParameter] = {
	FCE 		-> False,
	FCI 		-> False,
	FCVerbose 	-> False,
	Factoring 	-> Factor2,
	Names		-> Function[{x}, x]
};

FCLoopAddScalingParameter[topos: {__FCTopology}, scalingVar_, scalingRules_List, opts:OptionsPattern[]] :=
	FCLoopAddScalingParameter[#, scalingVar, scalingRules, opts]&/@topos;

FCLoopAddScalingParameter[topoRaw_FCTopology, scalingVar_, scalingRulesRaw_List, OptionsPattern[]] :=
Block[{	topo, props, tmp, propsFinal, simp, etaSigns, protectScaling, dummy, aux,
		scalings, res, vars, varsRescaled, optFactoring, optNames, id, scalingRules},

	optFactoring 	= OptionValue[Factoring];
	optNames 		= OptionValue[Names];

	If [OptionValue[FCVerbose]===False,
				fclaspVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fclaspVerbose=OptionValue[FCVerbose]
				];
	];

	FCPrint[1,"FCLoopAddScalingParameter: Entering.", FCDoControl->fclaspVerbose];
	FCPrint[3,"FCLoopAddScalingParameter: Entering with: ", topo, FCDoControl->fclaspVerbose];

	If[	!OptionValue[FCI],
		{topo,scalingRules} = FCI[{topoRaw,scalingRulesRaw}],
		topo = topoRaw
	];

	If[	!FCLoopValidTopologyQ[topo],
		Message[FCLoopAddScalingParameter::failmsg, "The supplied topology is incorrect."];
		Abort[]
	];

	If[	!AtomQ[scalingVar],
		Message[FCLoopAddScalingParameter::failmsg, "The scaling variable must be atomic."];
		Abort[]
	];

	If[	DataType[scalingVar, FCVariable]=!=True,
		Message[FCLoopAddScalingParameter::notfcvar, scalingVar]
	];

	id 			= optNames[topo[[1]]];
	etaSigns	= First[FCLoopGetEtaSigns[#]] & /@ topo[[2]];
	props 		= (1/FeynAmpDenominatorExplicit[topo[[2]], FCI -> True]) /. topo[[5]];

	If[	scalingRules==={},
		(*Nothing to do*)
		FCTopology[id, Sequence @@ topo[[2 ;;]]];
	];


	vars = Variables2[Variables2[props] /. Pair | CartesianPair | TemporalPair -> List /.
		(TemporalMomentum | CartesianMomentum | Momentum)[x_, ___] :> x];

	FCPrint[2,"FCLoopAddScalingParameter: New topology id: ", id, FCDoControl->fclaspVerbose];

	FCPrint[3,"FCLoopAddScalingParameter: I*eta signs in the topology: ", etaSigns, FCDoControl->fclaspVerbose];
	FCPrint[3,"FCLoopAddScalingParameter: Original propagators: ", props, FCDoControl->fclaspVerbose];

	FCPrint[2,"FCLoopAddScalingParameter: Momenta and masses in the propagators : ", vars, FCDoControl->fclaspVerbose];

	varsRescaled = vars /. scalingRules;
	scalings = Thread[Rule[vars,varsRescaled]]/.Rule[a_,a_]:>Rule[a, HoldForm[HoldForm[scalingVar^0] a]];

	FCPrint[0,"Scalings of momenta and masses in the propagators of ", ToString[topo[[1]]], " : " ,scalings, FCDoControl->fclaspVerbose];

	If[	optFactoring=!=False,
		propsFinal = optFactoring[props /. scalingRules],
		propsFinal = props /. scalingRules;
	];

	FCPrint[3,"FCLoopAddScalingParameter: Propagators with scaling rules: ", propsFinal, FCDoControl->fclaspVerbose];

	propsFinal =
		Map[(simp[#] //. simp[scalingVar^nn_. c_] :> protectScaling[scalingVar^nn] simp[c] /. simp -> Identity) &, propsFinal];

	FCPrint[3,"FCLoopAddScalingParameter: Simplified propagators: ", propsFinal, FCDoControl->fclaspVerbose];

	aux = Transpose[FCProductSplit[#, {protectScaling}] & /@ (dummy propsFinal)] /. dummy -> 1;

	{propsFinal, scalings} = aux;

	FCPrint[3,"FCLoopAddScalingParameter: scalings of each propagator: ", scalings, FCDoControl->fclaspVerbose];

	tmp = MapThread[
			If[	ExpandAll[#1 - #2] === 0,
				(1/#5) #3,
			(1/#5) FeynAmpDenominator[GenericPropagatorDenominator[#1, {1, #4}]]] &, {propsFinal, props, topo[[2]], etaSigns, scalings}
	](*/.protectScaling->FCGV["ProtectedScaling"]*)/. protectScaling[_] -> 1;

	FCPrint[3,"FCLoopAddScalingParameter: Final list of propagators including scalings: ", tmp, FCDoControl->fclaspVerbose];

	res = FCTopology[id, tmp, Sequence @@ topo[[3 ;;]]];

	If[	OptionValue[FCE],
		res = FCE[res]
	];

	res

];


FCPrint[1,"FCLoopAddScalingParameter.m loaded."];
End[]
