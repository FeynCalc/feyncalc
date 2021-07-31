(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCFeynmanParametrize											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Returns Feynman parameter integrand of the given loop
				integral													*)

(* ------------------------------------------------------------------------ *)

FCFeynmanParametrize::usage =
"FCFeynmanParametrize[int,{q1,q2,...}] introduces Feynman parameters for the \
scalar multi-loop integral int.  The function \
returns {fpInt,pref,vars}, where fpInt is the integrand without the prefactor, \
pref is the prefactor free of Feynman parameters and vars is the list of integration \
variables. The overall Dirac delta in the integrand is omitted unless the option \
DiracDelta is set to True.\n
By default FCFeynmanParametrize uses normalization that is common in multiloop calculations. \
If you want to have the standard 1/(2*Pi)^D normalization or yet another value, please set the \
option FeynmanIntegralPrefactor accordingly.\n
To calculate D-dimensional Euclidean integrals (as opposed to D-1 dimensional Cartesian \
ones or D-dimensional Minkowski integrals) written in terms of FVD, SPD, FAD, SFAD etc., \
you need to set the option \"Euclidean\" to True.";

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
	Assumptions					-> {},
	DiracDelta					-> False,
	"Euclidean"					-> False,
	Expanding					-> True,
	FCE							-> False,
	FCI							-> False,
	FCReplaceD					-> {},
	FCVerbose					-> False,
	(*Factoring 					-> {Factor2, 5000},*)
	FeynmanIntegralPrefactor	-> "Multiloop1",
	FinalSubstitutions			-> {},
	Indexed						-> True,
	Names						-> FCGV["x"],
	Reduce						-> False,
	Method						-> "Feynman",
	Simplify					-> False,
	(*TimeConstrained 			-> 3,*)
	Variables					-> {}
};


FCFeynmanParametrize[expr_, lmoms_List /; ! OptionQ[lmoms], opts:OptionsPattern[]]:=
	FCFeynmanParametrize[expr, 1, lmoms, opts];

(**)
FCFeynmanParametrize[expr_, extra_/; Head[extra]=!=List, lmoms_List /; ! OptionQ[lmoms], OptionsPattern[]] :=
	Block[{	res, optFinalSubstitutions, dim, uPoly, fPoly, pows, mat, powsT, propPowers,
			propPowersHat, propPowersTilde, ppSymbols, ppSymbolsRule,
			denPowers, zeroPowerProps, numPowers, numVars, zeroDenVars,
			nM,nLoops,fPow,pref, fpInt, fpPref, optFCReplaceD, vars, optVariavbles,
			aux, ex, Q, J, tensorPart, tensorRank, optMethod, extraPref, optFeynmanIntegralPrefactor,
			optEuclidean, inverseMeasure},

		optFinalSubstitutions		= OptionValue[FinalSubstitutions];
		optFCReplaceD				= OptionValue[FCReplaceD];
		optVariavbles				= OptionValue[Variables];
		optMethod					= OptionValue[Method];
		optFeynmanIntegralPrefactor = OptionValue[FeynmanIntegralPrefactor];
		optEuclidean				= OptionValue["Euclidean"];

		If [OptionValue[FCVerbose]===False,
			fcfpVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcfpVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			ex = expr,
			{ex,optFinalSubstitutions} = FCI[{expr,optFinalSubstitutions}]
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
			Message[FCFeynmanParametrize::failmsg,"Integrals that simultaneously depend on Lorentz and Cartesian vectors are not supported."];
			Abort[]
		];

		If[	TrueQ[Head[ex]=!=List],
			{extraPref, ex} = FCProductSplit[ex, Join[{lmoms},{FeynAmpDenominator, Pair, CartesianPair}]],
			extraPref = 1
		];

		FCPrint[1,"FCFeynmanParametrize: Prefactor in the input: ", extraPref, FCDoControl->fcfpVerbose];

		dim = FCGetDimensions[ex/. {TemporalPair[_,ExplicitLorentzIndex[0]]:>Unique[]}] ;

		If[	Length[dim]=!=1,
			Message[FCFeynmanPrepare::failmsg,"The loop integrals contains momenta in different dimensions."];
			Abort[]
		];
		dim = First[dim];

		FCPrint[1,"FCFeynmanParametrize: Dimension: ", dim, FCDoControl->fcfpVerbose];

		FCPrint[1,"FCFeynmanParametrize: Calling FCFeynmanPrepare.", FCDoControl->fcfpVerbose];

		{uPoly, fPoly, pows, mat, Q, J, tensorPart, tensorRank} = FCFeynmanPrepare[ex,lmoms, FCI->True,
			FinalSubstitutions->optFinalSubstitutions, Names->OptionValue[Names], Indexed->OptionValue[Indexed], Reduce->OptionValue[Reduce],
			"Euclidean" -> optEuclidean];


		nLoops	= Length[lmoms];
		powsT 	= Transpose[pows];
		nM 		= Total[Last[powsT]];
		fPow 	= nM - nLoops dim/2;

		FCPrint[1,"FCFeynmanParametrize: U: ", uPoly, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: F: ", fPoly, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: pows: ", pows, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: Number of loops: ", nLoops, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: Sum of propagator powers: ", nM, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: Tensor part: ", tensorPart, FCDoControl->fcfpVerbose];

		propPowers 	= Last[powsT];


		If[	MatchQ[propPowers,{___,0,___}],
			Message[FCFeynmanParametrize::failmsg,"Propagators cannot be raised to zero powers!"];
			Abort[]
		];

		If[MatchQ[optFCReplaceD,{_Rule}],
			propPowers = FCReplaceD[propPowers,First[optFCReplaceD]]
		];

		ppSymbols = Cases[propPowers, _Symbol, Infinity];
		If[ppSymbols=!={},
			ppSymbolsRule = Alternatives@@ppSymbols -> 0,
			ppSymbolsRule = {}
		];

		propPowersHat = Max[0, -Floor[# /. ppSymbolsRule ]]&/@propPowers;

		(*The tilde propagators are either symbolic or manifestly positive *)
		propPowersTilde = propPowers + propPowersHat;

		FCPrint[1,"FCFeynmanParametrize: propPowersHat: ", propPowersHat, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: propPowersTilde: ", propPowersTilde, FCDoControl->fcfpVerbose];

		If[	!MatchQ[propPowersTilde /. ppSymbolsRule,  {_Integer?NonNegative ..}] ||
			!MatchQ[propPowersHat,  {_Integer?NonNegative ..}],
			Message[FCFeynmanParametrize::failmsg,"Splitting of propagator powers failed."];
			Abort[]
		];

		zeroPowerProps = Position[propPowersTilde, 0];

		If[	!MatchQ[zeroPowerProps, {{_Integer} ...}],
			Message[FCFeynmanParametrize::failmsg, "Identification of zero-power propagators failed."];
			Abort[]
		];

		(*remove contributions from tilded m_i that are zero *)
		propPowers = Delete[propPowers,zeroPowerProps];
		vars = Delete[First[powsT],zeroPowerProps];

		If[	vars=!={},
			fpPref = (Times @@ Map[Power[#[[1]],(#[[2]]-1)] &, Transpose[{vars,propPowers}]]),
			fpPref = 1
		];

		If[	TrueQ[tensorPart=!=1],
			tensorPart = tensorPart /. FCGV["F"] -> fPoly;
			pref = extraPref/(Times @@ (Gamma /@ propPowers)),

			pref = extraPref*Gamma[fPow]/(Times @@ (Gamma /@ propPowers))
		];

		If[!isCartesian && !optEuclidean,
			pref = pref*(-1)^nM;
			inverseMeasure = (I Pi^(dim/2))^nLoops,
			inverseMeasure = (Pi^(dim/2))^nLoops
		];
		If[	StringQ[optFeynmanIntegralPrefactor],

				Switch[optFeynmanIntegralPrefactor,
					"Unity",
					pref = inverseMeasure*pref,
					"Textbook",
					pref = inverseMeasure/(2*Pi)^(dim*nLoops)*pref,
					"Multiloop1",
					Null,
					"Multiloop2",
					pref = Exp[nLoops*EulerGamma*(4-dim)/2] pref,
					_,
					Message[FCFeynmanParametrize::failmsg, "Unknown convention for the Feynman integral prefactor."];
					Abort[]
				],

				pref = optFeynmanIntegralPrefactor*inverseMeasure*pref
		];


		If[	pref===0 || !Internal`ExceptionFreeQ[pref],
			Message[FCFeynmanParametrize::failmsg,"Incorrect prefactor."];
			Abort[]
		];


		fpInt =  Power[uPoly,fPow - dim/2 - tensorRank]/Power[fPoly,fPow]*tensorPart;

		FCPrint[1,"FCFeynmanParametrize: fpPref: ", fpPref, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: pref: ", pref, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: raw fpInt: ", fpInt, FCDoControl->fcfpVerbose];



		If[	!MatchQ[propPowersHat,  {0..}],
			FCPrint[1,"FCFeynmanParametrize: Handling scalar products in the numerator.", FCDoControl->fcfpVerbose];
			numPowers = Position[propPowersHat, x_ /; x =!= 0, {1}, Heads -> False];

			If[	!MatchQ[numPowers, {{_Integer} ...}],
				Message[FCFeynmanParametrize::failmsg, "Identification of numerator powers failed."];
				Abort[]
			];

			numVars 		= Extract[First[powsT], numPowers];
			zeroDenVars 	= Extract[First[powsT], zeroPowerProps];
			propPowersHat	= Extract[propPowersHat, numPowers];

			FCPrint[1,"FCFeynmanParametrize: numVars: ", numVars, FCDoControl->fcfpVerbose];
			FCPrint[1,"FCFeynmanParametrize: zeroDenVars: ", zeroDenVars, FCDoControl->fcfpVerbose];
			FCPrint[1,"FCFeynmanParametrize: propPowersHat: ", propPowersHat, FCDoControl->fcfpVerbose];

			If[	Length[numVars]=!=Length[propPowersHat],
				Message[FCFeynmanParametrize::failmsg, "The number of x-variables doesn't match the number of propagators with negative powers."];
				Abort[]
			];


			fpInt = Fold[( (-1)^#2[[2]] * D[#1, #2]) &, fpInt, Transpose[{numVars,propPowersHat}]];

			FCPrint[3,"FCFeynmanParametrize: raw fpInt after the differentiation: ", fpInt, FCDoControl->fcfpVerbose];
			fpInt =  fpInt /. Map[Rule[#,0]&,zeroDenVars];
			FCPrint[3,"FCFeynmanParametrize: raw fpInt after setting x[i] to zero: ", fpInt, FCDoControl->fcfpVerbose]

		];

		fpInt = fpInt fpPref;

		If[	uPoly===0 || fPoly===0,
			fpInt = 0
		];


		(*
			If there is only a single Feynman parameter, the integration over the Dirac delta
			is trivial and can be done right away!
		*)
		If[	Length[vars]===1 && !OptionValue[DiracDelta],
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

		If[	MatchQ[optFCReplaceD,{_Rule}],
			fpInt  = FCReplaceD[fpInt,First[optFCReplaceD]];
			pref = FCReplaceD[pref,First[optFCReplaceD]]
		];

		If[	OptionValue[Simplify],
			fpInt	= Simplify[fpInt, Assumptions->OptionValue[Assumptions]];
			pref	= Simplify[pref, Assumptions->OptionValue[Assumptions]]
		];

		If[	Head[fpInt]=!=Times,
			fpInt = Together[fpInt]
		];

		aux		= FCProductSplit[fpInt, vars];
		pref	= pref aux[[1]];
		fpInt	= aux[[2]];

		res = {fpInt,pref,vars};

		If[	OptionValue[Expanding],
			res = res /. {
				Gamma[x_]:> Gamma[ExpandAll[x]],
				Power[x_,y_]:> Power[x,ExpandAll[y]]
			}
		];


		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1,"FCFeynmanParametrize: Leaving.", FCDoControl->fcfpVerbose];
		FCPrint[3,"FCFeynmanParametrize: Leaving with: ", res, FCDoControl->fcfpVerbose];

		res
];


FCPrint[1,"FCFeynmanParametrize.m loaded."];
End[]
