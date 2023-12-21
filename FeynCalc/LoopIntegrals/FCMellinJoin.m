(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCMellinJoin													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  	Joins terms inside propagators for Mellin-Barnes			*)

(* ------------------------------------------------------------------------ *)

FCMellinJoin::usage =
"FCMellinJoin[int, {q1, q2, ...}, {prop1, prop2, ...}] applies the standard
formula for splitting propagators prop1, prop2, ... into summands by
introducing integrations along a contour in the complex space.

The main purpose of this routine is to convert massive propagators into
massless ones when using Mellin-Barnes integration techniques.

The output consists of a list containing two elements, the first one being the
prefactor and the second one the product of remaining propagators. The second
element (or, alternatively, the product of both elements) can be then further
processed using FCFeynmanParametrize. Setting the option List to False will
return a product instead of a list.

The option FCSplit can be used to split a propagators in more than 2 terms as
it is done by default.";

FCMellinJoin::failmsg =
"Error! FCMellinJoin has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCMellinJoin`Private`"]

fcmjVerbose::usage="";
null1::usage="";
null2::usage="";

Options[FCMellinJoin] = {
	EtaSign					-> False,
	FCE						-> False,
	FCI						-> False,
	FCSplit					-> {},
	FCLoopGetEtaSigns		-> True,
	FCLoopSwitchEtaSign		-> False,
	FCVerbose				-> False,
	FinalSubstitutions		-> {},
	Indexed					-> True,
	Names					-> FCGV["z"],
	List					-> True,
	SortBy 					-> Function[x, x[[2]] < 0]
};

(*
FCMellinJoin[gli_, topos:{__FCTopology}, opts:OptionsPattern[]] :=
	FCMellinJoin[gli,FCLoopSelectTopology[gli,topos],opts]/;
		MatchQ[gli, (_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..])];

FCMellinJoin[gli_, topo_FCTopology, opts:OptionsPattern[]] :=
	Block[{int,optFinalSubstitutions, lmomsHead, lmoms, optLoopMomenta},

		optLoopMomenta = OptionValue[LoopMomenta];
		lmomsHead = Head[optLoopMomenta[1,1]];

		int = FCLoopFromGLI[gli, topo, FCI->OptionValue[FCI], LoopMomenta->optLoopMomenta, FeynAmpDenominatorExplicit->False];

		If[	OptionValue[FCI],
			optFinalSubstitutions = topo[[5]],
			optFinalSubstitutions = FCI[topo[[5]]]
		];

		optFinalSubstitutions = FCI@FRH[optFinalSubstitutions];

		If[	!FreeQ[int,lmomsHead],
			lmoms = Join[Cases2[int,lmomsHead],topo[[3]]],
			lmoms = topo[[3]]
		];

		FCMellinJoin[int, lmoms, Join[{FCI->True,FinalSubstitutions->optFinalSubstitutions},
			FilterRules[{opts}, Except[FCI | FinalSubstitutions]]]]
	]/; MatchQ[gli, (_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..])];


FCMellinJoin[glis_, topos:{__FCTopology}, opts:OptionsPattern[]] :=
	Block[{	ints, finalSubstitutions, relTopos, lmomsList, optLoopMomenta,
			lmomsHead},

		If[	OptionValue[FCVerbose]===False,
			fcmjVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcmjVerbose=OptionValue[FCVerbose]
			];
		];


		optLoopMomenta = OptionValue[LoopMomenta];
		lmomsHead = Head[optLoopMomenta[1,1]];

		ints = FCLoopFromGLI[glis, topos, FCI->OptionValue[FCI], LoopMomenta->optLoopMomenta, FeynAmpDenominatorExplicit->False];

		(*relTopos is a list of lists*)
		relTopos= FCLoopSelectTopology[{#},topos]&/@glis;

		If[	!MatchQ[relTopos,{{__FCTopology}..}],
			Message[FCMellinJoin::failmsg, "Something went wrong when extracting topologies relevant for the given GLIs."];
			Abort[]
		];

		finalSubstitutions = Flatten /@ Map[Function[x, Map[#[[5]] &, x]], relTopos];

		(*TODO: Tricky, if different topologies define different substitutions w.r.t. the same scalar products ...*)

		lmomsList = Union[Flatten[#[[3]]&/@Flatten[relTopos]]];

		If[	!FreeQ[ints,lmomsHead],
			lmomsList = Join[lmomsList,Cases2[ints,lmomsHead]];
		];

		FCPrint[1,"FCMellinJoin: All loop momenta: ", lmomsList, FCDoControl->fcmjVerbose];


		If[	!OptionValue[FCI],
			finalSubstitutions = FCI[finalSubstitutions]
		];

		finalSubstitutions = FCI@FRH[finalSubstitutions];

		MapThread[FCMellinJoin[#1, lmomsList, Join[{FCI->True,FinalSubstitutions->#2},
			FilterRules[{opts}, Except[FCI | FinalSubstitutions]]]]&,{ints,finalSubstitutions}]


	]/; MatchQ[glis,{(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) ..}];

FCMellinJoin[toposRaw: {__FCTopology}, opts:OptionsPattern[]]:=
	FCMellinJoin[#, opts]&/@toposRaw;

FCMellinJoin[topoRaw_FCTopology, opts:OptionsPattern[]] :=
	Block[{topo,optFinalSubstitutions},

		optFinalSubstitutions	= OptionValue[FinalSubstitutions];

		If[	OptionValue[FCI],
			topo = topoRaw,
			{topo,optFinalSubstitutions} = FCI[{topoRaw,optFinalSubstitutions}]
		];

		If[	!FCLoopValidTopologyQ[topo],
			Message[FCMellinJoin::failmsg, "The supplied topology is incorrect."];
			Abort[]
		];
		optFinalSubstitutions = Join[optFinalSubstitutions,FCI@FRH[topo[[5]]]];

		FCMellinJoin[topo[[2]], topo[[3]], Join[{FCI->True,FinalSubstitutions->optFinalSubstitutions},
			FilterRules[{opts}, Except[FCI | FinalSubstitutions]]]]

	];
*)


FCMellinJoin[expr_/;FreeQ[expr,{GLI,FCTopology}], lmomsRaw_List, propsToTreatRaw__List /; !OptionQ[propsToTreatRaw], OptionsPattern[]] :=
	Block[{	tmp, ex, spd, nDenoms, res,  lmoms, optFCLoopGetEtaSigns,
			optFinalSubstitutions, optNames, dim, optFCSplit, extraPref,
			tensorPart, scalarPart, time, sortBy, pref, etaSigns, optFCLoopSwitchEtaSign,
			propsToTreat, aux, rest, mellinZ, listA1A2},

		optFCSplit				= OptionValue[FCSplit];
		optFCLoopGetEtaSigns	= OptionValue[FCLoopGetEtaSigns];
		optFCLoopSwitchEtaSign  = OptionValue[FCLoopSwitchEtaSign];
		optFinalSubstitutions	= OptionValue[FinalSubstitutions];
		optNames				= OptionValue[Names];


		If[	OptionValue[FCVerbose]===False,
			fcmjVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcmjVerbose=OptionValue[FCVerbose]
			];
		];

		If[	OptionValue[FCI],
			ex = expr,
			{ex,optFinalSubstitutions,propsToTreat} = FCI[{expr,optFinalSubstitutions,propsToTreatRaw}]
		];

		propsToTreat = MomentumCombine[propsToTreat,FCI->True];

		FCPrint[1,"FCMellinJoin: Entering.", FCDoControl->fcmjVerbose];
		FCPrint[3,"FCMellinJoin: Entering  with: ", ex, FCDoControl->fcmjVerbose];
		FCPrint[3,"FCMellinJoin: Final substitutions: ", optFinalSubstitutions, FCDoControl->fcmjVerbose];

		lmoms = Select[lmomsRaw,!FreeQ[ex,#]&];

		FCPrint[3,"FCMellinJoin: Relevant loop momenta: ", lmoms, FCDoControl->fcmjVerbose];

		If [!FreeQ2[$ScalarProducts, lmoms],
			Message[FCMellinJoin::failmsg, "Some of the loop momenta have scalar product rules attached to them."];
			Abort[]
		];

		If[	!DuplicateFreeQ[lmoms],
			Message[FCMellinJoin::failmsg, "The list of the loop momenta may not contain duplicate entries."];
			Abort[]
		];

		ex = FCLoopPropagatorPowersCombine[ex,FCI->True];
		{ex,propsToTreat} = ToSFAD[{ex,propsToTreat},FCI->True];

		If[	TrueQ[optFCLoopSwitchEtaSign=!=False],
			{ex,propsToTreat} = FCLoopSwitchEtaSign[{ex,propsToTreat},optFCLoopSwitchEtaSign];
			propsToTreat = propsToTreat/. c_ f_FeynAmpDenominator/; FreeQ[c,FeynAmpDenominator] :> f;
			FCPrint[1, "FCMellinJoin: After switching eta signs:", ex, FCDoControl -> fcmjVerbose];
		];

		FCPrint[3,"FCMellinJoin: Propagators to be treated: ", propsToTreat, FCDoControl->fcmjVerbose];

		If[	!MatchQ[ex,{__}|_. _FeynAmpDenominator],
			Message[FCMellinJoin::failmsg, "The input expression is not a proper integral or list of propagators"];
			Abort[]

		];


		dim = FCGetDimensions[ex/. {TemporalPair[_,ExplicitLorentzIndex[0]]:>Unique[]}] ;

		If[	Length[dim]=!=1,
			Message[FCMellinJoin::failmsg,"The loop integrals contains momenta in different dimensions: " <> ToString[dim,InputForm]];
			Abort[]
		];

		dim = First[dim];

		If[	Union[FreeQ[ex,#]&/@lmoms]=!={False},
			Message[FCMellinJoin::failmsg,"Some of the specified loop momenta are not contained in the input expression."];
			Abort[]
		];

		(*extraPref is just a loop-unrelated prefactor multiplying the integral *)
		If[	TrueQ[Head[ex]=!=List],
			{extraPref, ex} = FCProductSplit[ex, Join[{lmoms},{FeynAmpDenominator, Pair, CartesianPair, GLI}]],
			extraPref = 1
		];

		If[	Head[ex]===List,
			Which[
				FreeQ2[ex,{LorentzIndex,CartesianIndex}],
					scalarPart = ex;
					tensorPart = 1,
				FreeQ2[Most[ex],{LorentzIndex,CartesianIndex}] && !FreeQ2[Last[ex],{LorentzIndex,CartesianIndex}],
					tensorPart = Last[ex];
					scalarPart = Most[ex],
				True,
					Message[FCMellinJoin::failmsg,"Failed to parse the supplied list of propagators."];
					Abort[]
			],
			{scalarPart, tensorPart} =  FCProductSplit[ex, {LorentzIndex, CartesianIndex}]
		];

		time=AbsoluteTime[];

		If[	TrueQ[Head[ex]===List],
			sortBy = Identity,
			sortBy = OptionValue[SortBy]
		];

		FCPrint[1, "FCMellinJoin: Calling FCLoopBasisExtract.", FCDoControl -> fcmjVerbose];

		tmp = FCLoopBasisExtract[scalarPart, lmoms, SetDimensions->{dim}, SortBy -> sortBy];

		FCPrint[1, "FCMellinJoin: FCLoopBasisExtract done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcmjVerbose];

		FCPrint[3,"FCMellinJoin: List of denominators: ", tmp, FCDoControl->fcmjVerbose];

		tmp = Transpose[{tmp[[1]], tmp[[3]], tmp[[4]]}];
		aux = SelectNotFree[tmp, propsToTreat];

		FCPrint[3,"FCMellinJoin: Selected propagators: ", aux, FCDoControl->fcmjVerbose];


		If[	aux==={},
			Message[FCMellinJoin::failmsg, "None of the specified propagators is present in the input expression."];
			Abort[]

		];

		If[	optFCLoopGetEtaSigns,

			etaSigns = FCLoopGetEtaSigns/@aux;
			FCPrint[3,"FCMellinJoin: eta signs: ", etaSigns, FCDoControl->fcmjVerbose];
			If[	!MatchQ[etaSigns, {HoldPattern[({1} | {-1}) ..]}],
				Message[FCMellinJoin::failmsg, "The integral contains propagators with different EtaSign prescriptions. " <>
				"Please use FCLoopSwitchEtaSign to have the same prescription in all propagators or set the option FCLoopGetEtaSigns to False."];
				Abort[]
			];
			etaSigns = First/@etaSigns,

			etaSigns = ConstantArray[1,Length[aux]]
		];

		If[	TrueQ[optFCSplit==={}],
			FCPrint[3,"FCMellinJoin: Standard splitting of propagators into massive and massless ones.",  FCDoControl->fcmjVerbose];
			listA1A2 = FCSplit[#[[1]],{Momentum,CartesianMomentum,TemporalMomentum}]&/@aux,

			FCPrint[3,"FCMellinJoin: Custom splitting prescriptions.",  FCDoControl->fcmjVerbose];
			If[	Length[optFCSplit]=!=Length[propsToTreat],
				Message[FCMellinJoin::failmsg,"The value of the FCSplit option must be either an empty list or a list of lists as long as the
				list of propagators to be converted."];
				Abort[]
			];
			listA1A2 = MapThread[fcSplit2[#1[[1]],#2]&, {aux,optFCSplit}]
		];

		If[	optFinalSubstitutions=!={},
			listA1A2 = listA1A2 /. optFinalSubstitutions
		];

		FCPrint[3,"FCMellinJoin: List of splitted propagators: ", listA1A2, FCDoControl->fcmjVerbose];

		mellinZ = Table[optNames[i],{i,1, Length[aux]}];

		aux = MapThread[mellinSplit[#1,#2[[2]],#3,#4]&, {listA1A2, aux, mellinZ, etaSigns}];

		If[	!optFCLoopGetEtaSigns,
			aux = aux /. FCGV["Eta"] -> 0
		];

		If[	optFCSplit==={},
			aux = aux /. optNames[i_Integer][1] -> optNames[i];
			FCPrint[3,"FCMellinJoin: Simplified mellin z variables: ", mellinZ, FCDoControl->fcmjVerbose];
		];

		If[	!OptionValue[Indexed],
			aux = aux /. s_Symbol[i_Integer][j_Integer] :> ToExpression[ToString[s]<>ToString[i]<>"$"<>ToString[j]]
			/. s_Symbol[i_Integer] :> ToExpression[ToString[s]<>ToString[i]];
		];

		FCPrint[3,"FCMellinJoin: aux: ", aux, FCDoControl->fcmjVerbose];

		rest = SelectFree[tmp, propsToTreat];

		rest = Times @@ Map[(#[[3]]^#[[2]]) &, rest];

		aux = Transpose[aux];

		res = {extraPref Times@@aux[[1]], tensorPart FeynAmpDenominatorCombine[rest Times@@aux[[2]]]};

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		If[ !OptionValue[List],
			res = Times@@res
		];

		FCPrint[1,"FCMellinJoin: Leaving.", FCDoControl->fcmjVerbose];
		FCPrint[3,"FCMellinJoin: Leaving with: ", res, FCDoControl->fcmjVerbose];

		res
];


mellinSplit[listAn_, la_, mellinZ_, etaSign_]:=
	Block[{tmp,len},
		len = Length[listAn];
		{1/Gamma[la] 1/(2 Pi I)^(len-1)*
		Times@@Table[Gamma[-mellinZ[i] ],{i,1,len-1}]*
		Gamma[Total[Table[mellinZ[i],{i,1,len-1}]]+la],

		Times@@Table[(listAn[[i]] + I*etaSign*SMP["Eta"])^mellinZ[i],{i,1,len-1}]*
		FeynAmpDenominator[GenericPropagatorDenominator[Last[listAn],{(+Total[Table[mellinZ[i],{i,1,len-1}]]+la),etaSign}]]
		}

	];

hold[0] :=
	Unevaluated[Sequence[]]

fcSplit2[expr_, {}] :=
	expr;
fcSplit2[expr_, {v_}] :=
	Reverse[FCSplit[expr, {v}]];

fcSplit2[expr_, {v1_, v2__}] :=
	Reverse[Flatten[splitEval[expr, {v1, v2}]]] /. hold -> Identity;

splitEval[expr_, {v_}] :=
	hold /@ FCSplit[expr, {v}];

splitEval[expr_, {v1_, v2__}] :=
	Block[{tmp},
		tmp = FCSplit[expr, {v1}];
		{splitEval[tmp[[1]], {v2}], splitEval[tmp[[2]], {v2}]}
	];



FCPrint[1,"FCMellinJoin.m loaded."];
End[]
