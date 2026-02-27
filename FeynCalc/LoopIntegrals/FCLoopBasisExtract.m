(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopBasisExtract												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:	Extracts propagator basis									*)

(* ------------------------------------------------------------------------ *)

FCLoopBasisExtract::usage=
"FCLoopBasisExtract[int, {q1, q2, ...}] is an auxiliary function that extracts
the scalar products that form the basis of the loop integral in int. It needs
to know the loop momenta on which the integral depends and the dimensions of
the momenta that may occur in the integral.";

FCLoopBasisExtract::failmsg =
"Error! FCLoopBasisExtract has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";


Begin["`Package`"]
End[]

Begin["`FCLoopBasisExtract`Private`"]

Options[FCLoopBasisExtract] = {
	CartesianPair		-> True,
	FCI 				-> False,
	FCE 				-> False,
	FCVerbose			-> False,
	FCTopology			-> False,
	FinalSubstitutions	-> {},
	Pair				-> True,
	Rest				-> None,
	SetDimensions		-> {3, 4, D, D-1},
	SortBy				-> Identity
};


FCLoopBasisExtract[exp_, loopmoms_List, OptionsPattern[]]:=
	Block[{	expr, coeffs, lmoms,allmoms, extmoms, basisElements,
			availableDims, dims, res, useToSFAD, integralBasis, integralBasisT,
			coeffsPair, coeffsCartesianPair, coeffsTemporalPair,
			lorentzianDims, cartesianDims, null1, null2, aux, optSortBy,
			optFinalSubstitutions, etaSigns, etaSignsLorentzian,
			etaSignsCartesian, etaSignsGeneric, optVerbose},

		If [OptionValue[FCVerbose]===False,
				optVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					optVerbose=OptionValue[FCVerbose]
				];
		];

		dims					= OptionValue[SetDimensions];
		optSortBy				= OptionValue[SortBy];
		optFinalSubstitutions	= OptionValue[FinalSubstitutions];

		If[	dims==={},
			Message[FCLoopBasisExtract::failmsg,"The list of dimensions cannot be empty."];
			Abort[]
		];

		If[	loopmoms==={},
			Message[FCLoopBasisExtract::failmsg,"The list of loop momenta cannot be empty."];
			Abort[]
		];

		If[	!OptionValue[FCI],
			{expr,optFinalSubstitutions} = FCI[{exp,FRH[optFinalSubstitutions]}],
			{expr,optFinalSubstitutions} = {exp,FRH[optFinalSubstitutions]}
		];


		FCPrint[1,"FCLoopBasisExtract: Entering.", FCDoControl->optVerbose];
		FCPrint[3,"FCLoopBasisExtract: Entering with: ", expr, FCDoControl->optVerbose];
		FCPrint[3,"FCLoopBasisExtract: Loop momenta: ", loopmoms, FCDoControl->optVerbose];

		useToSFAD = !FreeQ[expr, StandardPropagatorDenominator];


		etaSignsLorentzian	= FCLoopGetEtaSigns[expr, SFAD->True,  GFAD->False, CFAD->False];
		etaSignsCartesian	= FCLoopGetEtaSigns[expr, SFAD->False, GFAD->False, CFAD->True];
		etaSignsGeneric		= FCLoopGetEtaSigns[expr, SFAD->False, GFAD->True,  CFAD->False];


		(*{} accounts for input like {SPD[q1] - m1^2} *)
		If[	etaSignsLorentzian==={},
			etaSignsLorentzian={1}
		];

		If[	etaSignsCartesian==={},
			etaSignsCartesian={-1}
		];

		If[	etaSignsGeneric==={},
			etaSignsGeneric={1}
		];


		If[	!MatchQ[etaSignsLorentzian,{1}|{-1}],
				Message[FCLoopBasisExtract::failmsg, "The integral contains Lorentzian propagators with different EtaSign prescriptions. " <>
				"Please use FCLoopSwitchEtaSign to have the same prescription in all propagators."];
				Abort[]
		];

		If[	!MatchQ[etaSignsCartesian,{1}|{-1}],
				Message[FCLoopBasisExtract::failmsg, "The integral contains Cartesian propagators with different EtaSign prescriptions. " <>
				"Please use FCLoopSwitchEtaSign to have the same prescription in all propagators."];
				Abort[]
		];

		If[	!MatchQ[etaSignsGeneric,{1}|{-1}],
				Message[FCLoopBasisExtract::failmsg, "The integral contains generic propagators with different EtaSign prescriptions. " <>
				"Please use FCLoopSwitchEtaSign to have the same prescription in all propagators."];
				Abort[]
		];

		integralBasis = FCLoopIntegralToPropagators[expr, loopmoms, FCI->True, EtaSign->{etaSignsLorentzian[[1]],etaSignsCartesian[[1]],etaSignsGeneric[[1]]},
			Rest->OptionValue[Rest], Negative->True, Tally->True,
			Pair->OptionValue[Pair],CartesianPair->OptionValue[CartesianPair], ToSFAD->useToSFAD, MomentumCombine -> True, ExpandScalarProduct->True,
			Sort->False
		];

		FCPrint[3,"FCLoopBasisExtract: Integral basis from FCLoopIntegralToPropagators: ", integralBasis, FCDoControl->optVerbose];

		If[ FreeQ2[expr,{FCTopology,GLI}] && Head[expr]=!=List,
			(*
				We can reshuffle the propagators to have quadratic ones coming first and the eikonals last
				only if their ordering in the original integral did not matter!
			*)
			integralBasis = Join[SelectNotFree[integralBasis,Pair,CartesianPair],SelectFree[integralBasis,Pair,CartesianPair]];
			FCPrint[3,"FCLoopBasisExtract: Reordered integral basis: ", integralBasis, FCDoControl->optVerbose];
		];

		If[	optSortBy=!=Identity,
			integralBasis = SortBy[integralBasis, optSortBy];
			FCPrint[3,"FCLoopBasisExtract: Sorted integral basis: ", integralBasis, FCDoControl->optVerbose]
		];


		integralBasisT = Transpose[integralBasis];

		(*	List of all momenta that appear inside the integral	*)
		allmoms=Cases[MomentumExpand[integralBasis], (Momentum|CartesianMomentum|TemporalMomentum)[x_,___]:>x,Infinity]//Sort//DeleteDuplicates;

		(*	All momenta that are not listed as loop momenta will be treated as external momenta.	*)
		extmoms = Complement[allmoms,loopmoms];

		(*
			Normally, if the integral does not depend on some of the loop momenta specified by the user,
			we will not include these momenta to the basis. However, if we are dealing with a subtopology
			of a given topology, the full dependence must be taken into account. This is achieved by setting
			the option FCTopology to True.
		*)
		If[	OptionValue[FCTopology],
			lmoms = loopmoms,
			lmoms = Intersection[loopmoms,Complement[allmoms,extmoms]]
		];

		basisElements = FCLoopPropagatorsToTopology[integralBasisT[[1]],FCI->True,ExpandScalarProduct->True, DeleteDuplicates->False];

		If[	optFinalSubstitutions=!={},
			basisElements = basisElements //. optFinalSubstitutions
		];

		FCPrint[3,"FCLoopBasisExtract: Basis elements from FCLoopPropagatorsToTopology: ", basisElements, FCDoControl->optVerbose];

		availableDims = Intersection[FCGetDimensions[basisElements],dims];

		If[	availableDims==={},
			Message[FCLoopBasisExtract::failmsg,"The supplied dimensions are not present in the given expression."];
			Abort[]
		];

		lorentzianDims=Cases[availableDims, 4 | _Symbol];
		If[	lorentzianDims=!={},
			coeffsPair = Sort[FCLoopBasisCreateScalarProducts[lmoms,extmoms,lorentzianDims,Pair]],
			coeffsPair = {}
		];

		cartesianDims=Cases[availableDims, 3 | _Symbol - 1];
		If[	cartesianDims=!={},
			coeffsCartesianPair = Sort[FCLoopBasisCreateScalarProducts[lmoms,extmoms,cartesianDims,CartesianPair]],
			coeffsCartesianPair = {}
		];

		coeffsTemporalPair 	= Sort[TemporalPair[ExplicitLorentzIndex[0],TemporalMomentum[#]]&/@lmoms];
		coeffs				= {};

		If[	!FreeQ[expr,Momentum],
			coeffs = Join[coeffs,coeffsPair]
		];

		If[	!FreeQ[expr,CartesianMomentum],
			coeffs = Join[coeffs,coeffsCartesianPair]
		];

		If[	!FreeQ[expr,TemporalMomentum],
			coeffs = Join[coeffs,coeffsTemporalPair]
		];

		(* 	Experimental: Sort of support propagators that are not linear in the loop momentum dependent
			scalar poducts. We can count every f(sp(l1,x)) as a separate coefficient. This is not optimal,
			but should mostly work. *)
		aux = Flatten[List @@ (ExpandAll[#] + null1 + null2) & /@ basisElements /. null1 | null2 :> Unevaluated[Sequence[]]];
		aux = Union[Map[SelectNotFree[#, lmoms] &, aux] //. {r___, 1, s___} :> {r, s}];
		coeffs = Join[coeffs, Complement[aux,coeffs]];

		FCPrint[3,"FCLoopBasisExtract: Loop momentum dependent coefficients: ", coeffs, FCDoControl->optVerbose];

		FCPrint[1,"FCLoopBasisExtract: Leaving.", FCDoControl->optVerbose];

		res =  {basisElements, coeffs, integralBasisT[[2]], integralBasisT[[1]]};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		res

];

FCPrint[1,"FCLoopBasisExtract.m loaded."];
End[]
