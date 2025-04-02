(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopAugmentTopology											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Add propagators to topologies								*)

(* ------------------------------------------------------------------------ *)


FCLoopAugmentTopology::usage =
"FCLoopAugmentTopology[topo, {extraProps}] augments the topology topo by adding
new propagators extraProps to the basis. This is usually needed when a tensor
reduction requires us to introduce an auxiliary vector that will appear in
scalar products involving loop momenta.

The input topologies do not have to be complete.

The output of this routine contains augmented topologies and a list of
replacement rules for converting GLIs depending on the old topologies into new
ones.";



FCLoopAugmentTopology::failmsg = "Error! FCLoopAugmentTopology has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopAugmentTopology`Private`"];

atVerbose::usage="";


Options[FCLoopAugmentTopology] = {
	AugmentedTopologyMarker		-> FCGV["AddPropagators"],
	FCLoopBasisFindCompletion	-> False,
	FinalSubstitutions			-> {},
	FCE							-> False,
	FCI							-> False,
	FCVerbose					-> False,
	ToSFAD						-> True,
	Names						-> "A",
	Hold						-> True,
	Method						-> ScalarProduct
};


FCLoopAugmentTopology[topos:{__FCTopology}, extraProps_List, opts:OptionsPattern[]] :=
	Map[FCLoopAugmentTopology[#,extraProps,opts]&,topos];


FCLoopAugmentTopology[topoRaw_FCTopology, extraPropsRaw_List, OptionsPattern[]] :=
	Block[{	id, props, lmoms, extmoms, kinRules, topo, extraProps, newmoms, rest, res,
			optNames, newName, optFinalSubstitutions, newTopo, gliRules, optAugmentedTopologyMarker
			},

		optNames = OptionValue[Names];
		optFinalSubstitutions =	OptionValue[FinalSubstitutions];
		optAugmentedTopologyMarker = OptionValue[AugmentedTopologyMarker];

		If [OptionValue[FCVerbose]===False,
				atVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					atVerbose=OptionValue[FCVerbose]
				];
		];

		If[ !OptionValue[FCI],
			{topo, extraProps,optFinalSubstitutions} = FCI[{topoRaw, extraPropsRaw,optFinalSubstitutions}],
			{topo, extraProps} = {topoRaw, extraPropsRaw}
		];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCLoopFromGLI::failmsg, "The supplied topology " <> ToString[topo[[1]],InputForm] <>  " is incorrect."];
			Abort[]
		];

		props		= topo[[2]];
		lmoms		= topo[[3]];
		extmoms		= topo[[4]];
		kinRules	= topo[[5]];
		rest		= topo[[6]];

		FCPrint[1,"FCLoopAugmentTopology: Entering.", FCDoControl->atVerbose];
		FCPrint[3,"FCLoopAugmentTopology: Entering with: ", topo, FCDoControl->atVerbose];

		If[ OptionValue[ToSFAD],
			{props,extraProps} = ToSFAD[{props,extraProps}]
		];

		extraProps = SelectFree[extraProps,props];
		newmoms = Union[Cases[extraProps,Momentum[p_,___] -> p, Infinity]];
		newmoms = Sort[SelectFree[newmoms,Join[extmoms,lmoms]]];

		FCPrint[2,"FCLoopAugmentTopology: New external momenta to be added: ", newmoms, FCDoControl->atVerbose];

		props = Join[props,extraProps];
		extmoms = Join[extmoms,newmoms];

		If[	OptionValue[Hold],
			optFinalSubstitutions = optFinalSubstitutions/. {Pair->Hold[Pair]};
		];

		optFinalSubstitutions  = SelectNotFree[optFinalSubstitutions,extmoms];

		kinRules = Join[kinRules,optFinalSubstitutions];

		Switch[
				optNames,
				_String,
					newName=ToString[topo[[1]]]<>optNames,
				_Symbol,
					newName=ToExpression[ToString[topo[[1]]]<>ToString[optNames]],
				_Function,
					newName=optNames[topo[[1]]],
				_,
				Message[FCLoopBasisFindCompletion::failmsg,"Unknown value of the Names option."];
				Abort[]
		];

		newTopo = FCTopology[newName,props,lmoms,extmoms,kinRules,rest];

		FCPrint[2,"FCLoopAugmentTopology: Augmented topology: ", newTopo, FCDoControl->atVerbose];

		If[ OptionValue[FCLoopBasisFindCompletion],
			newTopo = FCLoopBasisFindCompletion[newTopo, Method->OptionValue[Method]]
		];

		gliRules = FCLoopCreateRuleGLIToGLI[newTopo,topo];

		If[	TrueQ[optAugmentedTopologyMarker=!=False],
			gliRules = gliRules /. RuleDelayed[a_,b_] :> RuleDelayed[a optAugmentedTopologyMarker[newmoms], b]
		];

		FCPrint[1,"FCLoopAugmentTopology: Leaving.", FCDoControl->atVerbose];

		If[	OptionValue[FCE],
			newTopo = FCE[newTopo]
		];

		{newTopo,gliRules}
	];


End[]
