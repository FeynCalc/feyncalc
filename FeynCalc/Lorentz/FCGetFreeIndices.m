(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCGetFreeIndices													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary: Extracts free indices											*)

(* ------------------------------------------------------------------------ *)



FCGetFreeIndices::usage =
"FCGetFreeIndices[exp, {head1, head2, ...}]  returns the list of free
(uncontracted) indices from heads head1, head2, ...

As always in FeynCalc, Einstein summation convention is implicitly assumed.
The function is optimized for large expressions, i.e. it is not so good as a
criterion in e.g. Select.

If it is understood that each term in the expression contains the same number
of free indices, setting the option First to True can considerably speed up
the evaluation.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCGetFreeIndices`Private`"]

fcgfiVerbose::usage="";

Options[FCGetFreeIndices] = {
	DiracChainExpand	-> True,
	DotSimplify			-> True,
	Expanding			-> True,
	FCTraceExpand 		-> True,
	FCVerbose			-> False,
	First 				-> False,
	Inverse				-> False,
	PauliChainExpand	-> True
};

FCGetFreeIndices[_, {}, OptionsPattern[]] :=
	{};

FCGetFreeIndices[expr_, heads_List/;(heads =!= {} && !OptionQ[heads]), OptionsPattern[]] :=
	{}/; FreeQ2[expr,heads];

FCGetFreeIndices[expr_, heads_List/;(heads =!= {} && !OptionQ[heads]), OptionsPattern[]] :=
	Block[{	fullLiList, freeIndicesList, allIndicesList, dummyIndicesList, ex, times,
			i, sel, sel2, optFirst, time, optExpanding, null1, null2, res, optInverse},

		If [OptionValue[FCVerbose]===False,
			fcgfiVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcgfiVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "FCGetFreeIndices: Entering.", FCDoControl->fcgfiVerbose];
		FCPrint[3, "FCGetFreeIndices: Entering with: ", expr, FCDoControl->fcgfiVerbose];

		optFirst = OptionValue[First];
		optExpanding = OptionValue[Expanding];
		optInverse = OptionValue[Inverse];

		If[ Head[expr]===Times,
			ex = SelectNotFree[expr, heads],
			ex = expr
		];




		If[ OptionValue[FCTraceExpand],
			time=AbsoluteTime[];
			FCPrint[1, "FCGetFreeIndices: Applying FCTraceExpand.", FCDoControl->fcgfiVerbose];
			ex = FCTraceExpand[ex,FCI->True];
			FCPrint[1, "FCGetFreeIndices: FCTraceExpand done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcgfiVerbose];
			FCPrint[3, "FCGetFreeIndices: After FCTraceExpand: ", ex, FCDoControl->fcgfiVerbose];
		];

		If[ OptionValue[DotSimplify],
			time=AbsoluteTime[];
			FCPrint[1, "FCGetFreeIndices: Applying DotSimplify.", FCDoControl->fcgfiVerbose];
			ex = DotSimplify[ex, FCI->True, Expanding->optExpanding];
			FCPrint[1, "FCGetFreeIndices: DotSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcgfiVerbose];
			FCPrint[3, "FCGetFreeIndices: After DotSimplify: ", ex, FCDoControl->fcgfiVerbose];

		];

		If[ OptionValue[DiracChainExpand] && !FreeQ[ex,DiracChain],
			time=AbsoluteTime[];
			FCPrint[1, "FCGetFreeIndices: Applying DiracChainExpand.", FCDoControl->fcgfiVerbose];
			ex = DiracChainExpand[ex,FCI->True];
			FCPrint[1, "FCGetFreeIndices: DiracChainExpand done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcgfiVerbose];
			FCPrint[3, "FCGetFreeIndices: After DiracChainExpand: ", ex, FCDoControl->fcgfiVerbose];
		];

		If[ OptionValue[PauliChainExpand] && !FreeQ[ex,PauliChain],
			time=AbsoluteTime[];
			FCPrint[1, "FCGetFreeIndices: Applying PauliChainExpand.", FCDoControl->fcgfiVerbose];
			ex = PauliChainExpand[ex,FCI->True];
			FCPrint[1, "FCGetFreeIndices: PauliChainExpand done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->fcgfiVerbose];
			FCPrint[3, "FCGetFreeIndices: After PauliChainExpand: ", ex, FCDoControl->fcgfiVerbose];
		];

		ex = ExpandAll2[ex];

		If[Head[ex]===Plus && optFirst && !optInverse,
			ex=First[ex]
		];


		If[	!FreeQ[ex, Power],
			ex = ex /. Power[a_, b_Integer?Positive] /; !FreeQ2[a, heads] :>
				Apply[times, Table[a, {i, b}]];
		];

		FCPrint[3, "FCGetFreeIndices: Final ex: ", ex, FCDoControl->fcgfiVerbose];

		sel = Blank/@heads;
		If[	Length[heads]===1,
			sel = Identity@@(Blank/@heads);
			sel2 = Identity@@heads,

			sel = Alternatives@@(Blank/@heads);
			sel2 = Alternatives@@heads
		];

		FCPrint[1, "FCGetFreeIndices: sel: ", sel, FCDoControl->fcgfiVerbose];
		FCPrint[1, "FCGetFreeIndices: sel2: ", sel2, FCDoControl->fcgfiVerbose];

		freeIndicesList = getFreeIndices[#,sel,sel2]&/@(List@@(ex+null1+null2));
		freeIndicesList = Union[Flatten[freeIndicesList]];

		If[	TrueQ[optInverse],
			allIndicesList = Cases[ex, sel, Infinity]//ReplaceAll[#, (sel2)[z_, ___] :> z] &;
			allIndicesList = Union[Flatten[allIndicesList]];
			dummyIndicesList = Complement[allIndicesList, freeIndicesList];
			res = dummyIndicesList,
			res = freeIndicesList
		];

		res

	]/; !FreeQ2[expr,heads];

getFreeIndices[x_,sel_,sel2_]:=
	Cases[x, sel, Infinity]//
			ReplaceAll[#, (sel2)[z_, ___] :> z] & // Tally // Cases[#, {z_, 1} :> z] &;

FCPrint[1,"FCGetFreeIndices.m loaded."];
End[]
