(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopCreateFactorizingRules												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  	Detects factorizing loop integrals


				Supports parallel evaluation [X]

*)

(* ------------------------------------------------------------------------ *)

FCLoopCreateFactorizingRules::usage =
"FCLoopCreateFactorizingRules[ints, topos] processes the given list of GLIs and
corresponding topologies and returns a list of rules for replacing all
factorizing integrals by simpler integrals with less loops.

Notice that we automatically generate suitable FCTopology objects for the
simpler integrals. Using the options PreferredTopologies or PreferredIntegrals
those can be mapped to a desired set.";

FCLoopCreateFactorizingRules::failmsg =
"Error! FCLoopCreateFactorizingRules has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopCreateFactorizingRules`Private`"]

nonFactorizing::usage="";

Options[FCLoopCreateFactorizingRules] = {
	Collecting					-> True,
	FCI							-> False,
	FCLoopFindIntegralMappings	-> True,
	FCLoopFindTopologyMappings	-> True,
	FCParallelize				-> False,
	FCVerbose 					-> False,
	Factoring 					-> {Factor2, 5000},
	FeynCalcExternal			-> False,
	FinalSubstitutions			-> {},
	Names						-> "loopint",
	PreferredIntegrals			-> {},
	PreferredTopologies			-> {},
	TimeConstrained 			-> 3,
	Unique						-> False
};

FCLoopCreateFactorizingRules[expr_, toposRaw:{__FCTopology}, OptionsPattern[]] :=
	Block[{	ex, res, time, posFactorizing, topos, originalGLIs, optFactoring,
			optTimeConstrained,  optVerbose, loopInt, optFCParallelize,
			glis, factorizedInts, tmp, aux,	optNames, repRule, uniqueInts,
			mappings, finalGLIs, topoIDs},

		If[	OptionValue[FCVerbose] === False,
			optVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			optVerbose = OptionValue[FCVerbose]];
		];

		optNames						= OptionValue[Names];
		optFactoring 					= OptionValue[Factoring];
		optTimeConstrained 				= OptionValue[TimeConstrained];
		optFCParallelize				= OptionValue[FCParallelize];


		If[	!OptionValue[FCI],
			(*	For large expressions FCI might require a considerable amount of time! *)
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopCreateFactorizingRules: Applying FCI.", FCDoControl->optVerbose];
			{ex, topos} = FCI[{expr, toposRaw}];
			FCPrint[1, "FCLoopCreateFactorizingRules: Done applying FCI, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose],
			{ex, topos} = {expr, toposRaw}
		];

		If[	!FCLoopValidTopologyQ[topos],
			Message[FCLoopCreateFactorizingRules::failmsg, "The supplied topologies are incorrect."];
			Abort[]
		];

		(*We need a list of all GLIs in the expression*)
		glis = Cases2[ex,GLI];

		If[glis==={},
			FCPrint[1,"FCLoopCreateFactorizingRules: The input expression contains no GLIs.", FCDoControl->optVerbose];
			(* Nothing to do *)
			Return[{{},{}}]
		];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopCreateFactorizingRules: Applying FCLoopFactorizingSplit." , FCDoControl->optVerbose];
		tmp = FCLoopFactorizingSplit[glis,topos,FCParallelize->optFCParallelize,Head->loopInt];
		FCPrint[1,"FCLoopCreateFactorizingRules: FCLoopFactorizingSplit done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopCreateFactorizingRules: Extracting factorizing integrals." , FCDoControl->optVerbose];
		posFactorizing	= Position[tmp, zz_ /; (Length[zz]>1), 1];
		originalGLIs 	= Extract[glis,posFactorizing];
		factorizedInts	= Extract[tmp,posFactorizing];
		FCPrint[1,"FCLoopCreateFactorizingRules: Done extracting factorizing integrals, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		time=AbsoluteTime[];
		FCPrint[1,"FCLoopCreateFactorizingRules: Applying FCLoopToGLI." , FCDoControl->optVerbose];
		uniqueInts = Cases2[factorizedInts,loopInt];

		FCPrint[3,"FCLoopCreateFactorizingRules: Unique integrals: ", uniqueInts , FCDoControl->optVerbose];

		If[	uniqueInts==={},
			(* Nothing to do *)
			FCPrint[1,"FCLoopCreateFactorizingRules: No factorizing integrals found.", FCDoControl->optVerbose];
			Return[{{},{}}]
		];

		Switch[
			optNames,
			_String,
				topoIDs=Table[optNames<>ToString[i],{i,1,Length[uniqueInts]}],
			_Symbol,
				topoIDs=Table[ToExpression[ToString[optNames]<>ToString[i]],{i,1,Length[uniqueInts]}],
			_Function,
				topoIDs=Table[optNames[i],{i,1,Length[uniqueInts]}],
			_,
			Message[FCLoopFindTopologies::failmsg,"Unknown value of the Names option."];
			Abort[]
		];

		tmp=Transpose[uniqueInts/.loopInt->List];

		{aux,topos} = FCLoopToGLI[tmp[[1]],tmp[[2]],FinalSubstitutions->tmp[[3]],Names->OptionValue[Names],Unique->OptionValue[Unique],FCParallelize->optFCParallelize];
		FCPrint[1, "FCLoopCreateFactorizingRules: FCLoopToGLI done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];

		repRule = Thread[Rule[uniqueInts,aux]];
		aux = Map[Times @@ Flatten[#] &, (factorizedInts /. Dispatch[repRule])];
		res = Thread[Rule[originalGLIs, aux]];

		If[	OptionValue[FCLoopFindTopologyMappings],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopCreateFactorizingRules: Applying FCLoopFindTopologyMappings." , FCDoControl->optVerbose];
			mappings = FCLoopFindTopologyMappings[topos,FCParallelize->optFCParallelize,FCVerbose->-1,PreferredTopologies->OptionValue[PreferredTopologies]];
			topos = mappings[[2]];
			If[	mappings[[1]]=!={},
				res = res /. Dispatch[Transpose[mappings[[1]]][[3]]]
			];
			FCPrint[1,"FCLoopCreateFactorizingRules: Done applying FCLoopFindTopologyMappings, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		];

		finalGLIs = Cases2[Last/@res,GLI];

		If[	OptionValue[FCLoopFindIntegralMappings],
			time=AbsoluteTime[];
			FCPrint[1,"FCLoopCreateFactorizingRules: Applying FCLoopFindIntegralMappings." , FCDoControl->optVerbose];
			mappings = FCLoopFindIntegralMappings[finalGLIs,topos,FCParallelize->optFCParallelize,FCVerbose->-1,PreferredIntegrals->OptionValue[PreferredIntegrals]];
			topos = FCLoopSelectTopology[mappings[[2]],topos];
			res = res /. Dispatch[mappings[[1]]];
			finalGLIs = mappings[[2]];
			FCPrint[1,"FCLoopCreateFactorizingRules: Done applying FCLoopFindIntegralMappings, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->optVerbose];
		];

		FCPrint[0, "FCLoopCreateFactorizingRules: ", FeynCalc`Package`FCStyle["Number of factorizing integrals: ", {Darker[Green,0.55], Bold}], Length[res], FCDoControl->optVerbose];
		FCPrint[0, "FCLoopCreateFactorizingRules: ", FeynCalc`Package`FCStyle["Number of simpler integrals: ", {Darker[Green,0.55], Bold}], Length[finalGLIs], FCDoControl->optVerbose];

		If[	OptionValue[FCE],
			topos = FCE[topos]
		];

		FCPrint[1, "FCLoopCreateFactorizingRules: Leaving.", FCDoControl->optVerbose];

		{res,finalGLIs,topos}
	];

FCPrint[1,"FCLoopCreateFactorizingRules.m loaded."];
End[]
