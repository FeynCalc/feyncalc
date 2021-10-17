(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopFindIntegralMappings										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Finds equivalent loop integrals								*)

(* ------------------------------------------------------------------------ *)

FCLoopFindIntegralMappings::usage =
"FCLoopFindIntegralMappings[{int1, int2, ...}, {p1, p2, ...}] finds mappings
between scalar multiloop integrals int1, int2, ... that depend on the loop
momenta p1, p2, ... using the algorithm of Alexey Pak
[arXiv:1111.0868](https://arxiv.org/abs/1111.0868).

The current implementation is based on the FindEquivalents function from FIRE
6 [arXiv:1901.07808](https://arxiv.org/abs/1901.07808)

It is also possible to invoke the function as
FCLoopFindIntegralMappings[{GLI[...], ...}, {FCTopology[...], ...}] or
FCLoopFindIntegralMappings[{FCTopology[...], ...}].

Notice that in this case the value of the option FinalSubstitutions is
ignored, as replacement rules will be extracted directly from the definition
of the topology.";

FCLoopFindIntegralMappings::failmsg =
"Error! FCLoopFindIntegralMappings has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopFindIntegralMappings`Private`"]

fcfpmVerbose::usage = "";

Options[FCLoopFindIntegralMappings] = {
	CharacteristicPolynomial	-> Function[{U,F}, U+F],
	FCE 						-> False,
	FCI 						-> False,
	FCVerbose 					-> False,
	FinalSubstitutions			-> {},
	Function					-> Function[{U, F, charPoly, pows, head, int, sigma}, {head[int, Transpose[pows]], head[ExpandAll[U], ExpandAll[F]]}],
	List						-> False,
	PreferredIntegrals			-> {}
};

FCLoopFindIntegralMappings[expr: {__FCTopology}, opts:OptionsPattern[]] :=
	FCLoopFindIntegralMappings[expr, {FCGV["dummy"]}, opts];

FCLoopFindIntegralMappings[exprRaw_List, lmomsRaw_List, OptionsPattern[]] :=
	Block[{	expr, pakFormInts, lmoms, res, time, x, pakHead, powerMark,
			topoidMode, optPreferredIntegrals, finalMasters},

		If[	OptionValue[FCVerbose] === False,
			fcfpmVerbose = $VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
			fcfpmVerbose = OptionValue[FCVerbose]];
		];

		optPreferredIntegrals = OptionValue[PreferredIntegrals];

		FCPrint[1, "FCLoopFindIntegralMappings: Entering.", FCDoControl -> fcfpmVerbose];
		FCPrint[3, "FCLoopFindIntegralMappings: Entering with: ", exprRaw, FCDoControl -> fcfpmVerbose];
		FCPrint[3, "FCLoopFindIntegralMappings: and: ", lmomsRaw, FCDoControl -> fcfpmVerbose];

		If[	(optPreferredIntegrals=!={}) && !MatchQ[optPreferredIntegrals,{(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) ..}],
			Message[FCLoopFindIntegralMappings::failmsg,"Incorrect value of the PreferredIntegrals option."];
			Abort[]
		];

		If[	lmomsRaw==={FCGV["dummy"]},
			lmoms=Sequence[],
			lmoms=lmomsRaw
		];

		If[	MatchQ[exprRaw,{(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) ..}],
			expr = Union[Join[exprRaw,optPreferredIntegrals]],
			expr = exprRaw
		];

		time=AbsoluteTime[];
		FCPrint[1, "FCLoopFindIntegralMappings: Calling FCLoopToPakForm.", FCDoControl -> fcfpmVerbose];

		pakFormInts = FCLoopToPakForm[expr, lmoms, FCI->OptionValue[FCI], FinalSubstitutions->OptionValue[FinalSubstitutions],
			Check->False, Collecting->False, Names->x, CharacteristicPolynomial->OptionValue[CharacteristicPolynomial],
			Function->OptionValue[Function], Head->pakHead, Power->powerMark];
		FCPrint[1, "FCLoopFindIntegralMappings: FCLoopToPakForm done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcfpmVerbose];

		FCPrint[3, "FCLoopFindIntegralMappings: Output of FCLoopToPakForm: ", pakFormInts, FCDoControl->fcfpmVerbose];

		(* 2nd element are the grouped mappings *)
		res = Reap[(Sow[Sequence @@ #] & /@ pakFormInts), _][[2]];

		If[	!FreeQ[expr,FCTopology],
			res = res /. pakHead[FCTopology[id_, props_List, rest__], {_List, propsReordered_List, _List}] :>
				List[FCTopology[id, props, rest], FCTopology[id, propsReordered, rest]],

			res = res /. FeynCalc`FCLoopFindIntegralMappings`Private`pakHead[zz_, __] :> zz
		];

		If[	!FreeQ[res,pakHead],
			Message[FCLoopFindIntegralMappings::failmsg,"Something went wrong while trying to process the output of FCLoopToPakForm."];
			Abort[]
		];

		FCPrint[3, "FCLoopFindIntegralMappings: Preliminary result: ", res, FCDoControl->fcfpmVerbose];

		(*TODO Filter out preferred masters that do not occur here*)

		(*Only return replacement rules if the input is a list of GLIs and the List option is set to False*)
		If[	!OptionValue[List] && MatchQ[expr,{(_GLI | Power[_GLI, _] | HoldPattern[Times][(_GLI | Power[_GLI, _]) ..]) ..}],

				res = makeMappingRules[#,optPreferredIntegrals]&/@res;

				FCPrint[3, "FCLoopFindIntegralMappings: After makeMappingRules: ", res, FCDoControl->fcfpmVerbose];

				If[	!FreeQ[res,makeMappingRules],
					Message[FCLoopFindIntegralMappings::failmsg,"Failed to create GLI mapping rules"];
					Abort[]
				];
				res= Flatten[res];
				finalMasters = Union[Last/@ res];
				res = res/. Rule[a_,a_] :> Unevaluated[Sequence[]];
				res = {res,finalMasters}

		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[3, "FCLoopFindIntegralMappings: Leaving.", FCDoControl -> fcfpmVerbose];
		FCPrint[3, "FCLoopFindIntegralMappings: Leaving with: ", res, FCDoControl -> fcfpmVerbose];

		res
	];

(*TODO Memoization*)
makeMappingRules[ints_List, (*preferredIntegrals*)_List]:=
	{ints[[1]] -> ints[[1]]}/; Length[ints]===1;

makeMappingRules[ints_List, preferredIntegrals_List/; preferredIntegrals=!={}]:=
	Rule[#, First[ints]]&/@Rest[ints]/; Length[ints]>1 && FreeQ2[ints,preferredIntegrals];

makeMappingRules[ints_List, {}]:=
	Rule[#, First[ints]]&/@Rest[ints]/; Length[ints]>1;

makeMappingRules[ints_List, preferredIntegrals_List/; preferredIntegrals=!={}]:=
	(
	lhs = First[SelectNotFree[ints,preferredIntegrals]];
	Rule[#,lhs]&/@ (SelectFree[ints,lhs])
	)/; Length[ints]>1 && !FreeQ2[ints,preferredIntegrals];


FCPrint[1,"FCLoopFindIntegralMappings.m loaded."];
End[]
