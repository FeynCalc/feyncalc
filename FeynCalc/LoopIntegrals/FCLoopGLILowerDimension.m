(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopGLILowerDimension											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:	Lowering dimension shift									*)

(* ------------------------------------------------------------------------ *)

FCLoopGLILowerDimension::usage =
"FCLoopGLILowerDimension[gli, topo] lowers the dimension of the given GLI from
D to D-2 and expresses it in terms of D-dimensional loop integrals returned in
the output.

The algorithm is based on the code of  the function RaisingDRR from R. Lee's
LiteRed";

FCLoopGLILowerDimension::failmsg =
"Error! FCLoopGLILowerDimension has encountered a fatal problem and must abort the computation. The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopGLILowerDimension`Private`"];

rgdVerbose::usage="";
holdDerivative::usage="";
CC::usage="";
aux::usage="";
tempSimp::usage="";

Options[FCLoopGLILowerDimension] = {
	Collecting		-> 	True,
	FCE				-> 	False,
	FCI				-> 	False,
	FCVerbose		->	False,
	Factoring		->	{Factor2,5000},
	TimeConstrained	->	3
};

FCLoopGLILowerDimension[gli_GLI, topos_List, opts:OptionsPattern[]]:=
	FCLoopGLILowerDimension[gli,FCLoopSelectTopology[gli,topos],opts];

FCLoopGLILowerDimension[gli_GLI, {topoRaw_FCTopology}, opts:OptionsPattern[]] :=
	FCLoopGLILowerDimension[gli, topoRaw, opts];

FCLoopGLILowerDimension[gli_GLI, topoRaw_FCTopology, OptionsPattern[]] :=
	Block[{	res, ex,  topo, nProps, lMoms, nLoops, extMoms,
			invProps, dim,  coeffs, dot, null1, null2, tmp,
			optCollecting, optFactoring, optTimeConstrained},

		optCollecting		= OptionValue[Collecting];
		optFactoring		= OptionValue[Factoring];
		optTimeConstrained	= OptionValue[TimeConstrained];

		If [OptionValue[FCVerbose]===False,
				rgdVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					rgdVerbose=OptionValue[FCVerbose]
				];
		];

		If[ !OptionValue[FCI],
			topo = FCI[topoRaw],
			topo= topoRaw
		];

		FCPrint[1,"FCLoopGLILowerDimension: Entering.", FCDoControl->rgdVerbose];
		FCPrint[3,"FCLoopGLILowerDimension: Entering with: ", gli, FCDoControl->rgdVerbose];
		FCPrint[3,"FCLoopGLILowerDimension: Topology: ", topo, FCDoControl->rgdVerbose];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCLoopGLILowerDimension::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];

		nProps		= Length[topo[[2]]];
		nLoops		= Length[topo[[3]]];
		lMoms		= topo[[3]];
		extMoms 	= Length[topo[[4]]];
		invProps	= 1/FeynAmpDenominatorExplicit[topo[[2]],ExpandScalarProduct->True,FCI->True];

		dim = FCGetDimensions[invProps];

		If[	Length[dim]=!=1,
			Message[FCLoopGLILowerDimension::failmsg, "The topology contains multiple dimensions."];
		];
		(*TODO check symbolic and stufff...*)

		If[ !FreeQ2[topo,{CartesianMomentum,CartesianIndex}],
			Message[FCLoopGLILowerDimension::failmsg, "Cartesian integrals are not supported."];
			Abort[]
		];

		dim = First[dim];
		coeffs = Table[CC[i],{i,1,nProps}];

		tmp = Table[
			dot[
				Map[(If[i===k,1,1/2]* Coefficient[#,Pair[Momentum[lMoms[[i]],dim],Momentum[lMoms[[k]],dim]]])&, invProps],
			coeffs], {i,1,nLoops},{k,1,nLoops}
		];

		FCPrint[3,"FCLoopGLILowerDimension: Preliminary result: ", tmp, FCDoControl->rgdVerbose];

		tmp = Det[tmp /. dot->Dot];

		FCPrint[3,"FCLoopGLILowerDimension: Preliminary result: ", tmp, FCDoControl->rgdVerbose];

		tmp =  gli (List@@(tmp+null1+null2)/. null1|null2 -> Unevaluated[Sequence[]]);

		tmp = Collect2[tmp,{gli,CC},Factoring->optFactoring, TimeConstrained->optTimeConstrained];

		FCPrint[3,"FCLoopGLILowerDimension: Preliminary result: ", tmp, FCDoControl->rgdVerbose];

		res = termSimp/@tmp;

		FCPrint[3,"FCLoopGLILowerDimension: Preliminary result: ", res, FCDoControl->rgdVerbose];

		If[!FreeQ[res,termSimp],
			Message[FCLoopGLILowerDimension::failmsg,"Something went wrong when simplyfing the prefactors"];
			Abort[]
		];

		res = Total[res];

		If[	optCollecting=!=False,
			Which[
				optCollecting===True,
					res = Collect2[res,GLI,Factoring->optFactoring, TimeConstrained->optTimeConstrained],
				Head[optCollecting]===List,
					res = Collect2[res,optCollecting,Factoring->optFactoring, TimeConstrained->optTimeConstrained],
				True,
					Message[FCLoopGLILowerDimension::failmsg, "Unsupported value of the Collecting option."];
					Abort[]
			]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCLoopGLILowerDimension: Leaving.", FCDoControl->rgdVerbose];

		res
	];

termSimp[a_Plus]:=
	termSimp/@a;

termSimp[rest_. CC[i_]^j_. GLI[id_, inds_List] ]:=
	Pochhammer[inds[[i]],j] termSimp[rest GLI[id, ReplacePart[inds, i -> inds[[i]] + j]]];

termSimp[rest_. GLI[id_,{inds__}] ]:=
	rest GLI[id,{inds}]/; FreeQ[rest,CC];


End[]
