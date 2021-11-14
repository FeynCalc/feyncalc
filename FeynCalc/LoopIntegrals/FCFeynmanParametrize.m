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
"FCFeynmanParametrize[int, {q1, q2, ...}] introduces Feynman parameters for the
multi-loop integral int.

The function returns {fpInt,pref,vars},  where fpInt is the integrand in
Feynman parameters, pref is the prefactor free of Feynman parameters and vars
is the list of integration variables.

If the chosen parametrization contains a Dirac delta multiplying the
integrand, it will be omitted unless the option DiracDelta is set to True.

By default FCFeynmanParametrize uses normalization that is common in
multi-loop calculations, i.e. $\\frac{1}{i \\pi^{D/2}}$ or $\\frac{1}{\\pi^{D/2}}$
per loop for Minkowski or Euclidean/Cartesian integrals respectively.

If you want to have the standard $\\frac{1}{(2 \\pi)^D}$ normalization or yet
another value, please set the option FeynmanIntegralPrefactor accordingly.
Following values are available

- \"MultiLoop1\" - default value explained above
- \"MultiLoop2\" - like the default value but with an extra $e^{\\gamma_E
\\frac{4-D}{2}}$ per loop
- \"Textbook\" - $\\frac{1}{(2 \\pi)^D}$ per loop
- \"Unity\" - no extra prefactor multiplying the integral measure

The calculation of $D$-dimensional Minkowski integrals and $D-1$-dimensional
Cartesian integrals is straightforward.

To calculate a $D$-dimensional Euclidean integral (i.e. an integral defined
with the Euclidean
metric signature $(1,1,1,1)$ you need to write it in terms of FVD, SPD, FAD,
SFAD etc. and set the option \"Euclidean\" to True.

The function can derive different representations of a loop integral. The
choice of the representation is controlled by the option Method. Following
representations are available

- \"Feynman\" - the standard Feynman representation (default value). Both
tensor integrals and integrals with scalar products in the numerator are
supported.
- \"Lee-Pomeransky\" - this representation was first introduced in
[1308.6676](https://arxiv.org/abs/1308.6676) by Roman Lee and Andrei
Pomeransky. Currently, only scalar integrals without numerators are supported.

FCFeynmanParametrize can also be employed in conjunction with
FCFeynmanParameterJoin, where one first joins suitable propagators using
auxiliary Feynman
parameters and then finally integrates out loop momenta.

For a proper analysis of a loop integral one usually needs the U and F
polynomials separately. Since internally FCFeynmanParametrize uses
FCFeynmanPrepare, the information available from the latter is also accessible
to FCFeynmanParametrize.

By setting the option FCFeynmanPrepare to True, the output of FCFeynmanPrepare
will be added the the output of FCFeynmanParametrize as the 4th list element.";

SplitSymbolicPowers::usage=
"SplitSymbolicPowers is an option for FCFeynmanParametrize and other functions.
When set to True, propagator powers containing symbols will be split into a
nonnegative integer piece and the remaining piece.
This leads to a somewhat different form of the resulting parametric integral,
although the final result remains the same. The default value is False.";

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
	Expanding					-> True,
	"Euclidean"					-> False,
	FCE							-> False,
	FCI							-> False,
	FCReplaceD					-> {},
	FCVerbose					-> False,
	FeynmanIntegralPrefactor	-> "Multiloop1",
	FinalSubstitutions			-> {},
	Indexed						-> True,
	FCFeynmanPrepare			-> False,
	Method						-> "Feynman",
	Names						-> FCGV["x"],
	Reduce						-> False,
	Simplify					-> False,
	SplitSymbolicPowers			-> False,
	Variables					-> {}
};


FCFeynmanParametrize[expr_, lmoms_List /; ! OptionQ[lmoms], opts:OptionsPattern[]]:=
	FCFeynmanParametrize[expr, 1, lmoms, opts];

FCFeynmanParametrize[expr_, extra_/; Head[extra]=!=List, lmoms_List /; ! OptionQ[lmoms], OptionsPattern[]] :=
	Block[{	res, optFinalSubstitutions, dim, uPoly, fPoly, pows, mat, powsT, propPowers,
			propPowersHat, propPowersTilde, ppSymbols, ppSymbolsRule,
			denPowers, zeroPowerProps, numPowers, numVars, zeroDenVars,
			nM,nLoops,fPow,pref, fpInt, fpPref, optFCReplaceD, vars, optVariavbles,
			aux, ex, Q, J, tensorPart, tensorRank, optMethod, extraPref, optFeynmanIntegralPrefactor,
			optEuclidean, inverseMeasure, optNames, outputFCFeynmanPrepare},

		optFinalSubstitutions		= OptionValue[FinalSubstitutions];
		optFCReplaceD				= OptionValue[FCReplaceD];
		optVariavbles				= OptionValue[Variables];
		optMethod					= OptionValue[Method];
		optFeynmanIntegralPrefactor = OptionValue[FeynmanIntegralPrefactor];
		optEuclidean				= OptionValue["Euclidean"];
		optNames					= OptionValue[Names];

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

		(*extraPref is just a loop-unrelated prefactor multiplying the integral *)
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
			FinalSubstitutions->optFinalSubstitutions, Names->optNames, Indexed->OptionValue[Indexed], Reduce->OptionValue[Reduce],
			"Euclidean" -> optEuclidean];

		outputFCFeynmanPrepare = {uPoly, fPoly, pows, mat, Q, J, tensorPart, tensorRank};


		nLoops	= Length[lmoms];
		powsT 	= Transpose[pows];
		nM 		= Total[Last[powsT]];


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

		(*The hat powers are either 0 or manifestly positive *)
		If[	TrueQ[OptionValue[SplitSymbolicPowers]],
			(*split symbolic powers into two pieces*)
			propPowersHat = Max[0, -Floor[# /. ppSymbolsRule ]]&/@propPowers,
			(*do not split symbolic powers into two pieces*)
			propPowersHat = Map[
				If[	Together[(#/. ppSymbolsRule)-#]===0,
					Max[0, -Floor[# /. ppSymbolsRule ]],
					0
				]&,propPowers]
		];

		(*The tilde powers are either symbolic or manifestly positive *)
		propPowersTilde = ExpandAll[propPowers + propPowersHat];

		FCPrint[1,"FCFeynmanParametrize: propPowersHat: ", propPowersHat, FCDoControl->fcfpVerbose];
		FCPrint[1,"FCFeynmanParametrize: propPowersTilde: ", propPowersTilde, FCDoControl->fcfpVerbose];

		FCPrint[1,"FCFeynmanParametrize: propPowersTilde-propPowersHat === propPowers: ",
			propPowersTilde-propPowersHat, FCDoControl->fcfpVerbose];

		If[	(*!MatchQ[NonNegative[propPowersTilde /. ppSymbolsRule],  {True ..}] ||*)
			!MatchQ[NonNegative[propPowersHat],  {True ..}],
			Message[FCFeynmanParametrize::failmsg,"Splitting of propagator powers failed."];
			Abort[]
		];

		zeroPowerProps = Position[propPowersTilde, 0];

		If[	!MatchQ[zeroPowerProps, {{_Integer} ...}],
			Message[FCFeynmanParametrize::failmsg, "Identification of zero-power propagators failed."];
			Abort[]
		];

		(*remove contributions from tilded m_i that are zero *)
		propPowersTilde = Delete[propPowersTilde,zeroPowerProps];
		vars = Delete[First[powsT],zeroPowerProps];

		If[	vars=!={},
			fpPref = (Times @@ Map[Power[#[[1]],(#[[2]]-1)] &, Transpose[{vars,propPowersTilde}]]),
			fpPref = 1
		];

		Switch[
			optMethod,
				"Feynman",
					FCPrint[1,"FCFeynmanParametrize: Deriving Feynman parametrization.", FCDoControl->fcfpVerbose];
					(* N_\nu - L D/2 *)
					fPow = nM - nLoops dim/2;
					If[	TrueQ[tensorPart=!=1],
						(* Tensor integral *)
						tensorPart = tensorPart /. FCGV["F"] -> fPoly;
						(* [\prod_{j-1}^N \Gamma(\nu_j)]^{-1} *)
						pref = extraPref/(Times @@ (Gamma /@ propPowersTilde)),

						(* Scalar integral *)
						(* \Gamma(N_\nu - L D/2) [\prod_{j-1}^N \Gamma(\nu_j)]^{-1} *)
						pref = extraPref*Gamma[fPow]/(Times @@ (Gamma /@ propPowersTilde))
					];
					fpInt =  Power[uPoly,fPow - dim/2 - tensorRank]/Power[fPoly,fPow]*tensorPart;
					If[	!isCartesian && !optEuclidean,
						pref = pref*(-1)^nM;
						inverseMeasure = (I Pi^(dim/2))^nLoops,
						inverseMeasure = (Pi^(dim/2))^nLoops
					];

					FCPrint[1,"FCFeynmanParametrize: fpPref: ", fpPref, FCDoControl->fcfpVerbose];
					FCPrint[1,"FCFeynmanParametrize: raw pref: ", pref, FCDoControl->fcfpVerbose];
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
						{fpInt, uPoly,fPoly} =  {fpInt, uPoly,fPoly} /. Map[Rule[#,0]&,zeroDenVars];
						FCPrint[3,"FCFeynmanParametrize: raw fpInt after setting x[i] to zero: ", fpInt, FCDoControl->fcfpVerbose]
					];

					fpInt = fpInt fpPref;
					(* Check the scalefullness *)
					If[	FCLoopPakScalelessQ[uPoly*fPoly,vars],
						FCPrint[1,"FCFeynmanParametrize: According to FCLoopPakScalelessQ this integral is scaleless.", FCDoControl->fcfpVerbose];
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

					,
				"Lee-Pomeransky",
					FCPrint[1,"FCFeynmanParametrize: Deriving Lee-Pomeransky parametrization.", FCDoControl->fcfpVerbose];
					If[	TrueQ[tensorPart=!=1],
						Message[FCFeynmanParametrize::failmsg, "Tensor integrals in the Lee-Pomeransky representation are currently not implemented."];
						Abort[]
					];
					If[	!MatchQ[propPowersHat,  {0..}],
						Message[FCFeynmanParametrize::failmsg, "Integrals with propagators raised to negative powers in the Lee-Pomeransky representation are currently not implemented."];
						Abort[]
					];

					(* Scalar integral *)
					(* \Gamma(D/2) [\Gamma((L+1) D/2 - N_\nu)\prod_{j-1}^N \Gamma(\nu_j)]^{-1} *)
					pref = extraPref*Gamma[dim/2]/Gamma[(nLoops+1)*dim/2-nM]/(Times @@ (Gamma /@ propPowers));
					fpInt =  Power[uPoly+fPoly,- dim/2];
					If[	!isCartesian && !optEuclidean,
							pref = pref*(-1)^nM;
							inverseMeasure = (I Pi^(dim/2))^nLoops,
							inverseMeasure = (Pi^(dim/2))^nLoops
						];
					FCPrint[1,"FCFeynmanParametrize: fpPref: ", fpPref, FCDoControl->fcfpVerbose];
					FCPrint[1,"FCFeynmanParametrize: raw pref: ", pref, FCDoControl->fcfpVerbose];
					FCPrint[1,"FCFeynmanParametrize: raw fpInt: ", fpInt, FCDoControl->fcfpVerbose];

					fpInt = fpInt fpPref;
					,
				_,
					Message[FCFeynmanParametrize::failmsg,"Unknown Feynman integral representation."];
					Abort[]
		];


		(*	Work out the overall prefactor (representation independent!)	*)
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

		FCPrint[1,"FCFeynmanParametrize: final pref: ", pref, FCDoControl->fcfpVerbose];


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
			pref = FCReplaceD[pref,First[optFCReplaceD]];
			FCPrint[3,"FCFeynmanParametrize: fpInt after FCReplaceD: ", fpInt, FCDoControl->fcfpVerbose];
			FCPrint[3,"FCFeynmanParametrize: pref after FCReplaceD: ", pref, FCDoControl->fcfpVerbose]
		];

		If[	OptionValue[Simplify],
			fpInt	= Simplify[fpInt, Assumptions->OptionValue[Assumptions]];
			pref	= Simplify[pref, Assumptions->OptionValue[Assumptions]];
			FCPrint[3,"FCFeynmanParametrize: fpInt after Simplify: ", fpInt, FCDoControl->fcfpVerbose];
			FCPrint[3,"FCFeynmanParametrize: pref after Simplify: ", pref, FCDoControl->fcfpVerbose]
		];

		If[	Head[fpInt]=!=Times && !MemberQ[{"Lee-Pomeransky"},optMethod],
			fpInt = Together[fpInt];
			FCPrint[3,"FCFeynmanParametrize: fpInt after Together: ", fpInt, FCDoControl->fcfpVerbose]
		];

		aux		= FCProductSplit[fpInt, vars];
		pref	= pref aux[[1]];
		fpInt	= aux[[2]];

		res = {fpInt,pref,vars};

		If[	OptionValue[Expanding],
			res = res /. Gamma[x_]:> Gamma[ExpandAll[x]] /.
				Power[x_,y_]:> Power[x,ExpandAll[y]];
			FCPrint[3,"FCFeynmanParametrize: res after ExpandAll: ", res, FCDoControl->fcfpVerbose]
		];

		If[ OptionValue[FCFeynmanPrepare],
			res = Join[res, {outputFCFeynmanPrepare}]
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
