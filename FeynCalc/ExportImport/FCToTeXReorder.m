(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCToTeXReorder												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Overrides Mathematica's term ordering						*)

(* ------------------------------------------------------------------------ *)

FCToTeXReorder::usage =
"FCToTeXReorder[exp, {{v1,v2,...},{a1,a2,...},{b1,b2,...}}] is an auxiliary \
function that helps to bring the given Mathematica expression exp into a form \
suitable for being inserted into a LaTeX document. To override the built-in \
ordering of Plus and Times, the expression is converted into a nested list \
made of elements of the form {a,b,...,Plus} or {a,b,...,Times} for a sum \
or a product respectively. \n
To that aim each expression that is not this form yet is first split into \
terms are depend on {v1,v2,...} and those that are of these variables. Then, \
the function applies Collect2 w.r.t. {a1,a2,...} and {b1,b2,...} to the \
first and the second pieces of the original expression respectively. Finally, \
the option SortBy allows use to specify two sorting functions that will be \
used to reorder the terms in both groups. \n
FCToTeXReorder can be also applied to the output of a previous function \
call. This allows for arbitrarily deep nesting. You can check if the final \
result satisfies your expectations by using FCPreviewTermOrder";

FCToTeXReorder::failmsg = "Error! FCToTeXReorder has encountered a fatal problem and must \
abort the computation. The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`FCToTeXReorder`"]
End[]

Begin["`FCToTeXReorder`Private`"]


rflVerbose::usage="";

Options[FCToTeXReorder] = {
	Check		-> True,
	FCVerbose	-> False,
	Factoring	-> {Factor2,Factor2},
	Reverse		-> False,
	SortBy		-> {Identity, Identity},
	Split		-> True
};


FCToTeXReorder[{li__}, vars_List /; (!OptionQ[vars] || vars==={{},{},{}}), opts : OptionsPattern[]] :=
	FCToTeXReorder[#, vars, opts] & /@ {li};

FCToTeXReorder[ex_ /; (Head[ex] =!= List) && !MemberQ[{Plus, Times}, Head[ex]], vars_List /; (!OptionQ[vars] || vars==={{},{},{}}), OptionsPattern[]] :=
	ex;

FCToTeXReorder[ex_ /; (Head[ex] =!= List) && MemberQ[{Plus, Times}, Head[ex]], vars_List /; (!OptionQ[vars] || vars==={{},{},{}}), OptionsPattern[]] :=
	Block[{	dummy1, dummy2, splitVars, collectVars1, collectVars2,
			tmp, innerHead, outerHead, list, freeOf, notFreeOf, optFactoring},

		If [OptionValue[FCVerbose]===False,
			rflVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				rflVerbose=OptionValue[FCVerbose]
			];
		];

		optFactoring = OptionValue[Factoring];

		FCPrint[1, "FCToTeXReorder. Entering.", FCDoControl->rflVerbose];
		FCPrint[3, "FCToTeXReorder: Entering with ", ex, FCDoControl->rflVerbose];

		If[	!MatchQ[vars,{_List, _List, _List}],
			Message[FCToTeXReorder::failmsg, "Malformed second argument."];
			Abort[]
		];

		{splitVars, collectVars1, collectVars2} = vars;

		If[	splitVars =!= {},
			{freeOf, notFreeOf} = FCSplit[ex, splitVars],
			freeOf = 0;
			notFreeOf = ex;
		];


		notFreeOf = Collect2[notFreeOf, collectVars1, Head -> {innerHead, outerHead}, Factoring->optFactoring[[1]]];
		freeOf = Collect2[freeOf, collectVars2, Head -> {innerHead, outerHead}, Factoring->optFactoring[[2]]];

		FCPrint[3, "FCToTeXReorder: freeOf after Collect2: ", freeOf, FCDoControl->rflVerbose];
		FCPrint[3, "FCToTeXReorder: notFreeOf after Collect2: ", notFreeOf, FCDoControl->rflVerbose];


		If[OptionValue[Split],
			{freeOf, notFreeOf} = {freeOf, notFreeOf} /. {
				outerHead[x_, innerHead[y_]] :> list[x, y, Times],
				outerHead[x_, 1] :> list[x, Plus]
			},

			{freeOf, notFreeOf} = {freeOf, notFreeOf} /. {
				outerHead[x_, innerHead[y_]] :> x y,
				outerHead[x_, 1] :> list[x, Plus]
			}
		];

		freeOf = Join[List @@ (freeOf + dummy1 + dummy2), {Plus}] /. list -> List /. dummy1 | dummy2 :> Unevaluated[Sequence[]];
		notFreeOf = Join[List @@ (notFreeOf + dummy1 + dummy2), {Plus}] /. list -> List /. dummy1 | dummy2 :> Unevaluated[Sequence[]];

		FCPrint[3, "FCToTeXReorder: freeOf as a list: ", freeOf, FCDoControl->rflVerbose];
		FCPrint[3, "FCToTeXReorder: notFreeOf as a list: ", notFreeOf, FCDoControl->rflVerbose];

		If[	OptionValue[SortBy] =!= False,

			If[MatchQ[notFreeOf, {__, Plus | Times}],
				notFreeOf =	Join[SortBy[Most[notFreeOf], OptionValue[SortBy][[1]]], {Last[notFreeOf]}]
			];

			If[MatchQ[freeOf, {__, Plus | Times}],
				freeOf = Join[SortBy[Most[freeOf], OptionValue[SortBy][[2]]], {Last[freeOf]}]
			];

			FCPrint[3, "FCToTeXReorder: Sorted freeOf: ", freeOf, FCDoControl->rflVerbose];
			FCPrint[3, "FCToTeXReorder: Sorted notFreeOf: ", notFreeOf, FCDoControl->rflVerbose]
		];

		If[	OptionValue[Reverse],
			tmp = {freeOf, notFreeOf, Plus} /. list -> List,
			tmp = {notFreeOf, freeOf, Plus} /. list -> List
		];
		tmp = tmp //. {Plus} :> Unevaluated[Sequence[]] //. {
			{x_, Plus} :> x
		};

		FCPrint[3, "FCToTeXReorder: Leaving with: ", tmp, FCDoControl->rflVerbose];

		If[	OptionValue[Check],
			If[	Together[ex-FRH[FCToTeXPreviewTermOrder[tmp]]]=!=0,
				Message[FCToTeXReorder::failmsg, "Check using FCPreviewTermOrder failed."];
				FCPrint[3, "FCToTeXReorder: Difference: ", Together[ex-FRH[FCToTeXPreviewTermOrder[tmp]]], FCDoControl->rflVerbose];
				Abort[]
			]
		];

		tmp
	]

FCPrint[1,"FCToTeXReorder.m loaded."];
End[]
