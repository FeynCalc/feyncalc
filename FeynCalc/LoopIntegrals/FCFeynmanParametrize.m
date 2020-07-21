(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFeynmanParametrize											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  	Returns Feynman parameter integrand of the given loop
				integral													*)

(* ------------------------------------------------------------------------ *)

FCFeynmanParametrize::usage =
"FCFeynmanParametrize[int,{q1,q2,...}] introduces Feynman parameters for the \
scalar multi-loop integral int. Tensor integrals are not supported. The function \
returns {fpInt,pref,vars}, where fpInt is the integrand without the prefactor, \
pref is the prefactor free of Feynman parameters and vars is the list of integration \
variables. The overall Dirac delta in the integrand is omitted unless the option \
DiracDelta is set to True.";

FCFeynmanParametrize::failmsg =
"Error! FCFeynmanParametrize has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCFeynmanParametrize`Private`"]

dim::usage="";
fcfpVerbose::usage="";
null1::usage="";
null2::usage="";
isCartesian::usage="";

Options[FCFeynmanParametrize] = {
	Assumptions				-> {},
	Dimension				-> D,
	DiracDelta				-> False,
	FCE						-> False,
	FCI						-> False,
	FCReplaceD				-> {},
	FCVerbose				-> False,
	FinalSubstitutions		-> {},
	Indexed					-> True,
	Names					-> FCGV["x"],
	Numerator				-> False,
	Simplify				-> False,
	Variables				-> {}
};


FCFeynmanParametrize[expr_, lmoms_List /; ! OptionQ[lmoms], opts:OptionsPattern[]]:=
	FCFeynmanParametrize[expr, 1, lmoms, opts];


FCFeynmanParametrize[expr_, extra_/; Head[extra]=!=List, lmoms_List /; ! OptionQ[lmoms], OptionsPattern[]] :=
	Block[{	res, optFinalSubstitutions, dim, uPoly, fPoly, pows, mat, powsT,
			nM,nLoops,fPow,pref,fpInt, fpPref, optFCReplaceD, vars, optVariavbles, aux, ex},

		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		dim						= OptionValue[Dimension];
		optFCReplaceD			= OptionValue[FCReplaceD];
		optVariavbles			= OptionValue[Variables];

		If [OptionValue[FCVerbose]===False,
			fcfpVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcfpVerbose=OptionValue[FCVerbose]
			];
		];

		If[OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		Which[
			!FreeQ[ex,Momentum] && FreeQ[ex,CartesianMomentum],
			isCartesian=False,
			(*Lorentzian integral *)
			FreeQ[ex,Momentum] && !FreeQ[ex,CartesianMomentum],
			isCartesian=True,
			(*Cartesian integral *)
			!FreeQ[ex,Momentum] && !FreeQ[ex,CartesianMomentum],
			(*Mixed integral*)
			Message[FCSymanzikPolynomials::failmsg,"Integrals that simultaneously depend on Lorentz and Cartesian vectors are not supported."];
			Abort[]
		];

		{uPoly, fPoly, pows, mat} = FCSymanzikPolynomials[ex,lmoms, FCI->True,
			FinalSubstitutions->OptionValue[FinalSubstitutions], Names->OptionValue[Names], Indexed->OptionValue[Indexed]];


		nLoops	= Length[lmoms];
		powsT 	= Transpose[pows];
		nM 		= Total[Last[powsT]];
		fPow 	= nM - nLoops dim/2;

		FCPrint[1,"FCFeynmanParametrize: U: ", uPoly, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: F: ", fPoly, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: pows: ", pows, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: Number of loops: ", nLoops, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: Sum of propagator powers: ", nM, FCDoControl->fcfpVerbose];

		If[!MatchQ[Last[powsT],{(_Integer?Positive| _Symbol)..}] && !OptionValue[Numerator],
			Message[FCFeynmanParametrize::failmsg,"Numerators are currently not supported."];
			Abort[]
		];

		pref = Gamma[fPow]/(Times @@ (Gamma /@ Last[powsT]));

		If[!isCartesian,
			pref = pref*(-1)^nM
		];


		If[	pref===0 || !Internal`ExceptionFreeQ[pref],
			Message[FCFeynmanParametrize::failmsg,"Incorrect prefactor."];
			Abort[]
		];

		fpPref = (Times @@ Map[Power[#[[1]],(#[[3]]-1)] &, pows]);
		fpInt =  Power[uPoly,fPow - dim/2]/Power[fPoly,fPow];

		FCPrint[1,"FCFeynmanParametrize: fpPref: ", fpPref, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: pref: ", pref, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: raw fpInt: ", fpInt, FCDoControl->fcfpVerbose];

		fpInt = fpInt fpPref;

		If[	uPoly===0 || fPoly===0,
			fpInt = 0
		];

		vars = First[powsT];

		(*
			If there is only a single Feynman parameter, the integration over the Dirac delta
			is trivial and can be done right away!
		*)
		If[	Length[vars]===1,
			fpInt = fpInt /. vars[[1]] -> 1;
			vars = {}
		];

		If[	OptionValue[DiracDelta],
			fpInt = fpInt*DiracDelta[1-Total[vars]]
		];

		If[ Length[optVariavbles]=!=0,
			vars = Join[vars, optVariavbles]
		];

		aux		= FCProductSplit[extra,vars];

		FCPrint[1,"FCFeynmanParametrize: aux: ", aux, FCDoControl->fcfpVerbose];

		pref	= pref aux[[1]];
		fpInt	= fpInt aux[[2]];

		FCPrint[1,"FCFeynmanParametrize: fpInt: ", fpInt, FCDoControl->fcfpVerbose];

		If[MatchQ[optFCReplaceD,{_Rule}],
			fpInt  = FCReplaceD[fpInt,First[optFCReplaceD]];
			pref = FCReplaceD[pref,First[optFCReplaceD]]
		];

		If[OptionValue[Simplify],
			fpInt	= Simplify[fpInt, Assumptions->OptionValue[Assumptions]];
			pref	= Simplify[pref, Assumptions->OptionValue[Assumptions]]
		];



		res = {fpInt,pref,vars};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCFeynmanParametrize: Leaving.", FCDoControl->fcfpVerbose];
		FCPrint[3,"FCFeynmanParametrize: Leaving with: ", res, FCDoControl->fcfpVerbose];

		res
];


FCPrint[1,"FCFeynmanParametrize.m loaded."];
End[]
