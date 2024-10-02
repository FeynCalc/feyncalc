(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopGLIRaiseDimension											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Raising dimension shift									*)

(* ------------------------------------------------------------------------ *)

FCLoopGLIRaiseDimension::usage =
"FCLoopGLIRaiseDimension[gli, topo] raises the dimension of the given GLI from
N to N+2 and expresses it in terms of N-dimensional loop integrals returned in
the output.

The algorithm is based on the code of  the function RaisingDRR from R. Lee's
LiteRed";

FCLoopGLIRaiseDimension::failmsg =
"Error! FCLoopGLIRaiseDimension has encountered a fatal problem and must abort the computation. The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopGLIRaiseDimension`Private`"];

rgdVerbose::usage="";
holdDerivative::usage="";
CC::usage="";
aux::usage="";

Options[FCLoopGLIRaiseDimension] = {
	Collecting		-> 	True,
	FCE				-> 	False,
	FCI				-> 	False,
	FCVerbose		->	False,
	Factoring		->	{Factor2,5000},
	FinalSubstitutions -> {},
	TimeConstrained	->	3
};

FCLoopGLIRaiseDimension[gli_GLI, topos_List, opts:OptionsPattern[]]:=
	FCLoopGLIRaiseDimension[gli,FCLoopSelectTopology[gli,topos],opts];

FCLoopGLIRaiseDimension[gli_GLI, {topoRaw_FCTopology}, opts:OptionsPattern[]] :=
	FCLoopGLIRaiseDimension[gli, topoRaw, opts];

FCLoopGLIRaiseDimension[gli_GLI, topoRaw_FCTopology, OptionsPattern[]] :=
	Block[{	res, ex,  topo, nProps, lMoms, nLoops, extMoms,
			invProps, dim,  coeffs, dot, null1, null2, tmp,
			optCollecting, optFactoring, optTimeConstrained,
			gramDet, nExtMoms, numRules, optFinalSubstitutions},

		optCollecting		= OptionValue[Collecting];
		optFactoring		= OptionValue[Factoring];
		optTimeConstrained	= OptionValue[TimeConstrained];
		optFinalSubstitutions = FRH[Join[OptionValue[FinalSubstitutions],topoRaw[[5]]]];

		If [OptionValue[FCVerbose]===False,
				rgdVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					rgdVerbose=OptionValue[FCVerbose]
				];
		];

		If[ !OptionValue[FCI],
			{topo, optFinalSubstitutions} = FCI[{topoRaw, optFinalSubstitutions}],
			topo = topoRaw
		];

		FCPrint[1,"FCLoopGLIRaiseDimension: Entering.", FCDoControl->rgdVerbose];
		FCPrint[3,"FCLoopGLIRaiseDimension: Entering with: ", gli, FCDoControl->rgdVerbose];
		FCPrint[3,"FCLoopGLIRaiseDimension: Topology: ", topo, FCDoControl->rgdVerbose];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCLoopGLIRaiseDimension::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		nProps		= Length[topo[[2]]];
		nLoops		= Length[topo[[3]]];
		lMoms		= topo[[3]];
		extMoms 	= topo[[4]];
		nExtMoms 	= Length[topo[[4]]];
		invProps	= 1/FeynAmpDenominatorExplicit[topo[[2]],ExpandScalarProduct->True,FCI->True];

		dim = FCGetDimensions[invProps];

		If[	Length[dim]=!=1,
			Message[FCLoopGLIRaiseDimension::failmsg, "The topology contains multiple dimensions."];
		];
		dim = First[dim];

		gramDet = FCGramDeterminant[extMoms, Prefactor -> 1] /.Dispatch[optFinalSubstitutions];

		tmp = 2^nLoops/gramDet/Pochhammer[((dim + 1) - nLoops) - nExtMoms, nLoops];


		gramDet = FCGramDeterminant[Join[lMoms,extMoms], Prefactor -> 1]/.Dispatch[optFinalSubstitutions];

		numRules = FCLoopCreateRulesToGLI[topo]//Flatten;

		gramDet = gramDet/.numRules;


		res = tmp*Expand2[gli gramDet,GLI]/.GLI->GLIMultiply /. GLIMultiply->GLI;


		If[	optCollecting=!=False,
			Which[
				optCollecting===True,
					res = Collect2[res,GLI,Factoring->optFactoring, TimeConstrained->optTimeConstrained],
				Head[optCollecting]===List,
					res = Collect2[res,optCollecting,Factoring->optFactoring, TimeConstrained->optTimeConstrained],
				True,
					Message[FCLoopGLIRaiseDimension::failmsg, "Unsupported value of the Collecting option."];
					Abort[]
			]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopGLIRaiseDimension: Leaving.", FCDoControl->rgdVerbose];

		res
	];

End[]
