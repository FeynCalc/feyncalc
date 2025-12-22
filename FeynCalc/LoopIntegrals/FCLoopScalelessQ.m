(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopScalelessQ													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  	Detects scaleless loop integrals 							*)

(* ------------------------------------------------------------------------ *)

FCLoopScalelessQ::usage =
"FCLoopScalelessQ[int, {p1, p2, ...}] checks whether the loop integral int
depending on the loop momenta p1, p2, ... is scaleless. Only integrals that
admit a Feynman parametrization with proper $U$ and $F$ polynomials are
supported.

The function uses the algorithm of Alexey Pak
[arXiv:1111.0868](https://arxiv.org/abs/1111.0868). Cf. also the PhD thesis of
Jens Hoff [10.5445/IR/1000047447](https://doi.org/10.5445/IR/1000047447) for
the detailed description of a possible implementation.";

FCLoopScalelessQ::failmsg =
"Error! FCLoopScalelessQ has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopScalelessQ`Private`"]

fclsVerbose::usage = "";

Options[FCLoopScalelessQ] = {
	Collecting				-> True,
	FCI						-> False,
	FCVerbose 				-> False,
	Factoring 				-> {Factor2, 5000},
	FCFeynmanParametrize	-> True,
	FinalSubstitutions		-> {},
	TimeConstrained 		-> 3
};


FCLoopScalelessQ[expr: {__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopScalelessQ[expr, {FCGV["dummy"]}, opts];

FCLoopScalelessQ[expr_FCTopology, opts:OptionsPattern[]] :=
	FCLoopScalelessQ[expr, {FCGV["dummy"]}, opts];

FCLoopScalelessQ[expr_, lmomsRaw_/; !OptionQ[lmomsRaw], OptionsPattern[]] :=
	Block[{	uPoly, fPoly, pows, mat, Q, J, tensorPart,
			tensorRank, res, time, x, lmoms, optFinalSubstitutions,
			ex, notList = False, tmp, intsWithNegativePows,
			extraRules},

		If[	OptionValue[FCVerbose] === False,
			fclsVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fclsVerbose = OptionValue[FCVerbose]];
		];

		If[	Head[lmomsRaw]=!=List,
			Message[FCLoopScalelessQ::failmsg,"The second argument must be a list of loop momenta"];
			Abort[]
		];

		optFinalSubstitutions = OptionValue[FinalSubstitutions];

		FCPrint[1, "FCLoopScalelessQ: Entering.", FCDoControl -> fclsVerbose];
		FCPrint[3, "FCLoopScalelessQ: Entering with: ", expr, FCDoControl -> fclsVerbose];

		If[	OptionValue[FCI],
			{ex, lmoms} = {expr,lmomsRaw},
			{ex, lmoms, optFinalSubstitutions} = FCI[{expr, lmomsRaw, optFinalSubstitutions}]
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopScalelessQ: Calling FCFeynmanPrepare.", FCDoControl -> fclsVerbose];

		If[	lmomsRaw==={FCGV["dummy"]},
			lmoms=Sequence[]
		];

		Which[
			(*Single integral *)
			MatchQ[ex,_. _FeynAmpDenominator] || MatchQ[ex, _GLI | _FCTopology],
				notList = True;
				tmp =	FCFeynmanPrepare[ex, lmoms, FCI -> True, Names -> x, Check->False,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained],
				Factoring -> OptionValue[Factoring], FinalSubstitutions -> optFinalSubstitutions, FCLoopGetEtaSigns -> False,
					"IgnoreNumerator" -> True, "MarkNonUnitNumerators"->True];
				tmp = {tmp};
				ex = {ex},
			(*List of integrals *)
			MatchQ[ex, {__GLI} | {__FCTopology}],
				tmp = FCFeynmanPrepare[ex, lmoms, FCI -> True, Names -> x, Check->False,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained],
				Factoring -> OptionValue[Factoring], FinalSubstitutions-> optFinalSubstitutions, FCLoopGetEtaSigns -> False,
					"IgnoreNumerator" -> True, "MarkNonUnitNumerators"->True],
			(*List of integrals *)
			MatchQ[ex, {_. _FeynAmpDenominator ..}],
				tmp =	FCFeynmanPrepare[#, lmoms, FCI -> True, Names -> x, Check->False,
				Collecting -> OptionValue[Collecting], TimeConstrained -> OptionValue[TimeConstrained],
				Factoring -> OptionValue[Factoring], FinalSubstitutions-> optFinalSubstitutions, FCLoopGetEtaSigns -> False,
				"IgnoreNumerator" -> True, "MarkNonUnitNumerators"->True]&/@ex,
			True,
				Message[FCLoopScalelessQ::failmsg,"Failed to recognize the form of the input expression."];
				Abort[]
		];

		FCPrint[1, "FCLoopScalelessQ: FCFeynmanPrepare done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclsVerbose];
		tmp = Transpose[tmp];
		{uPoly, fPoly} = tmp[[1;;2]];
		intsWithNegativePows = Position[Last[tmp],True];


		FCPrint[3, "FCLoopScalelessQ: U: ", uPoly, FCDoControl -> fclsVerbose];
		FCPrint[3, "FCLoopScalelessQ: F: ", fPoly, FCDoControl -> fclsVerbose];
		FCPrint[3, "FCLoopScalelessQ: Negative propagator powers: ", intsWithNegativePows, FCDoControl -> fclsVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopScalelessQ: Calling FCPakScalelessQ.", FCDoControl -> fclsVerbose];
		(*TODO Caching*)
		res = MapThread[FCLoopPakScalelessQ[#1*#2,x]&,{uPoly, fPoly}];
		FCPrint[1, "FCLoopScalelessQ: FCPakScalelessQ done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclsVerbose];

		(*
			If the integral also contains numerators, it might vanish despite of not being scaleless
			according to the Pak criterion.
		*)
		If[	intsWithNegativePows=!={},
			If[	OptionValue[FCFeynmanParametrize],
				time=AbsoluteTime[];
				FCPrint[1, "FCLoopScalelessQ: Calling FCFeynmanParametrize.", FCDoControl -> fclsVerbose];
				extraRules=MapThread[checkIntsWithNumerators[#1,lmoms,#2]&,{Extract[ex,intsWithNegativePows],Extract[res,intsWithNegativePows]}];
				FCPrint[1, "FCLoopScalelessQ: FCFeynmanParametrize done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fclsVerbose];
				extraRules=Thread[Rule[intsWithNegativePows,extraRules]]/. Rule[_,False]->Unevaluated[Sequence[]];
				FCPrint[3, "FCLoopScalelessQ: extraRules: ", extraRules, FCDoControl -> fclsVerbose];
				If[!MatchQ[extraRules, {Rule[_, True]...}],
					Message[FCLoopScalelessQ::failmsg,"Something went wrong when determining extra vanishing integrals."];
					Abort[]
				];
				If[	extraRules=!={},
					extraRules=First/@extraRules;
					res = ReplaceAt[res,_->True,extraRules]
				]
			]
		];

		If[	notList,
			res = First[res]
		];

		FCPrint[3, "FCLoopScalelessQ: Leaving.", FCDoControl -> fclsVerbose];
		FCPrint[3, "FCLoopScalelessQ: Leaving with: ", res, FCDoControl -> fclsVerbose];

		res
	];

checkIntsWithNumerators[_,_,True]:=
	True;

checkIntsWithNumerators[int_,lmoms_,False]:=
	TrueQ[FCFeynmanParametrize[int,lmoms][[2]]===0];

FCPrint[1,"FCLoopScalelessQ.m loaded."];
End[]
