(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopCreateRulesToGLI											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Creates replacement rules for switching to the
				GLI-notation												*)

(* ------------------------------------------------------------------------ *)

FCLoopCreateRulesToGLI::usage =
"FCLoopCreateRulesToGLI[topo] creates replacement rules for converting
numerators from the given topology to GLI objects with inverse propagators.

It is also possible to use FCLoopCreateRulesToGLI[{topo1, topo2, ...}].";

FCLoopCreateRulesToGLI::failmsg = "Error! FCLoopCreateRulesToGLI has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopCreateRulesToGLI`Private`"];

crtgVerbose::usage="";


Options[FCLoopCreateRulesToGLI] = {
	Check		-> True,
	FCE			-> False,
	FCI			-> False,
	FCPrint		-> True,
	FCVerbose	-> False
};


FCLoopCreateRulesToGLI[toposRaw:{__FCTopology}, rest___]:=
	FCLoopCreateRulesToGLI[#, rest]&/@toposRaw;

FCLoopCreateRulesToGLI[topoRaw_FCTopology, OptionsPattern[]] :=
	Block[{	props, topo, allMoms,extMoms,dims,spList,nProps,
			array, id, eqSystem, inverseProps, res, topoCheck,
			gfad, lmoms},

		If [OptionValue[FCVerbose]===False,
			crtgVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				crtgVerbose=OptionValue[FCVerbose]
			];
		];

		If[ !OptionValue[FCI],
			topo = FCI[topoRaw],
			topo = topoRaw
		];


		FCPrint[1,"FCLoopCreateRulesToGLI: Entering.", FCDoControl->crtgVerbose];
		FCPrint[3,"FCLoopCreateRulesToGLI: Entering with: ", topo, FCDoControl->crtgVerbose];


		If[	!FCLoopValidTopologyQ[topo],
			Message[FCFeynmanPrepare::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];


		If[	OptionValue[Check],

			If[ FCLoopBasisIncompleteQ[topo],
				Message[FCLoopCreateRulesToGLI::failmsg, "The given topology does not correspond to a complete propagator basis."];
				Abort[]
			];

			If[ FCLoopBasisOverdeterminedQ[topo],
				Message[FCLoopCreateRulesToGLI::failmsg, "The given topology contains linearly dependent propagators."];
				Abort[]
			];

		];


		id = topo[[1]];
		props = topo[[2]];
		lmoms = topo[[3]];

		FCPrint[3,"FCLoopCreateRulesToGLI: List of the propagators: ", topo, FCDoControl->crtgVerbose];

		nProps = Length[props];
		If[	Sort[props]=!=Union[props],
			Message[FCLoopCreateRulesToGLI::failmsg,"The list of the propagators contains duplicates!"];
			Abort[]
		];

		allMoms=Cases[MomentumExpand[props], Momentum[x_,___]:>x,Infinity]//Sort//DeleteDuplicates;
		extMoms=Complement[allMoms,lmoms];
		dims=FCGetDimensions[props];

		If[	Length[dims]>1 || dims==={},
			Message[FCLoopCreateRulesToGLI::failmsg,"The topology may contain only one dimension."];
			Abort[]
		];

		spList = FCLoopBasisCreateScalarProducts[lmoms, extMoms, dims, Pair];

		FCPrint[3,"FCLoopCreateRulesToGLI: List of the scalar possible prodcuts: ", spList, FCDoControl->crtgVerbose];

		array = ConstantArray[0, nProps - 1];

		(*props = props /. FeynAmpDenominator[g_GenericPropagatorDenominator] :> gfad[FeynAmpDenominator[g]];*)

		inverseProps = FCLoopBasisPropagatorsToTopology[props,FCI->True,ExpandScalarProduct->True];

		FCPrint[3,"FCLoopCreateRulesToGLI: List of the inverse propagators: ", inverseProps, FCDoControl->crtgVerbose];


		eqSystem = Table[Equal[inverseProps[[i]], GLI[id, Insert[array, -1, i]]], {i, 1, nProps}];

		res = Solve[eqSystem,Join[spList,Cases2[inverseProps,gfad]]];

		If[	res==={},
			Message[FCLoopCreateRulesToGLI::failmsg,"Failed to solve the linear system."];
			Abort[]
		];

		If[	Length[res]>1,
			Message[FCLoopCreateRulesToGLI::failmsg,"The solution of the linear system is not unique."];
			Abort[]
		];

		res = First[res];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3,"FCLoopCreateRulesToGLI: Final set of rules: ", res, FCDoControl->crtgVerbose];
		FCPrint[1,"FCLoopCreateRulesToGLI: Leaving.", FCDoControl->crtgVerbose];

		res

]


End[]
