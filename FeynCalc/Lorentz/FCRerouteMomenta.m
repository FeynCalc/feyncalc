(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCRerouteMomenta												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:	Changes the routing of the momenta by exploiting
				4-momentum conservation										*)

(* ------------------------------------------------------------------------ *)

FCRerouteMomenta::usage =
"FCRerouteMomenta[exp, {p1, p2, ...}, {k1, k2, ...}]  changes the routing of
the momenta by exploiting the 4-momentum conservation law $p_1+p_2+ \\ldots =
k_1+k_2+ \\ldots$.

The main aim of this function is to simplify the input expression by replacing
simple linear combinations of the external momenta with shorter expressions.

For example, in a process $a(p_1) + b(p_2) -> c(k_1)+ d(k_2)+ e(k_3)$, the
combination $k_1+k_2-p_2$ can be replaced with the shorter expression
$p_1-k_3$.

The replacements are applied using the FeynCalcExternal form of the
expression. Ideally, this function should be used directly on the output of a
diagram generator such as FeynArts or QGRAF.";

FCRerouteMomenta::failmsg =
"Error! FCRerouteMomenta has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCRerouteMomenta`Private`"]


pickMomenta::usage="";
list::usage="";
fcrmVerbose::usage="";


Options[FCRerouteMomenta] = {
	Check 					-> True,
	DiracGammaCombine		-> False,
	Eliminate 				-> {},
	Expand 					-> ExpandAll,
	FCE 					-> False,
	FCVerbose 				-> False,
	InitialSubstitutions	-> {},
	MaxIterations 			-> Infinity,
	MomentumCombine 		-> False,
	Replace 				-> {
								FAD, SFAD, CFAD, GFAD,
								GS, GSD, GSE,
								FV, FVD, FVE,
								CV, CVD, CVE,
								SP, SPD, SPE,
								CSP, CSPD, CSPE, TC
							}
};

FCRerouteMomenta[expr_, in_List/; in=!={}, out_List /; (out =!= {} && ! OptionQ[out]), OptionsPattern[]] :=
	Block[{	ex, lin = Length[in], lout = Length[out], rule = {}, longest, shortest, lenl, lens,
			lentot, nums, selections = {}, selections2, objects, objectsEval, ruleFinal,
			res, optEliminate, optInitialSubstitutions, lastRule},

		optEliminate 			= OptionValue[Eliminate];
		optInitialSubstitutions = OptionValue[InitialSubstitutions];

		If [OptionValue[FCVerbose]===False,
			fcrmVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				fcrmVerbose=OptionValue[FCVerbose]
			];
		];

		ex = FCI[expr];

		FCPrint[1,"FCRerouteMomenta: Entering.", FCDoControl->fcrmVerbose];
		FCPrint[3,"FCRerouteMomenta: Entering with ", expr, FCDoControl->fcrmVerbose];

		If[	OptionValue[DiracGammaCombine] && !FreeQ[ex,DiracGamma],
			FCPrint[1,"FCRerouteMomenta: Applying DiracGammaCombine.", FCDoControl->fcrmVerbose];
			ex = DiracGammaCombine[ex,FCI->True]
		];

		If[	OptionValue[MomentumCombine] && !FreeQ2[ex,{Pair,CartesianPair,TemporalPair}],
			FCPrint[1,"FCRerouteMomenta: Applying MomentumCombine.", FCDoControl->fcrmVerbose];
			ex = MomentumCombine[ex,FCI->True]
		];

		ex = FCE[ex];

		If[! MatchQ[{in, out}, {{__Symbol}, {__Symbol}}],
			Print["error. incorrect input"];
			Abort[]
		];

		(*We need to know which list contains the largest number of momenta *)
		If[TrueQ[Length[in] > Length[out]],
			{longest, shortest} = {in, out};
			{lenl, lens} = {Length[longest], Length[shortest]},

			{longest, shortest} = {out, in};
			{lenl, lens} = {Length[longest], Length[shortest]}
		];
		lentot = lenl + lens;

		(* 	In the following we assume that the momentum conservation equation is of the form
			Total[longest]=Total[shortest], i.e. the largest number of the momenta is on the lhs *)

		(*Determine the maximum number of momenta on the lhs of a rule*)
		nums = Table[lentot - i, {i, 0, Quotient[lentot, 2]}];

		(*	If the total number of the momenta is an even number, e.g.
			2->2 or 1->3, then a rule with an equal number of momenta on both sides
			is apriori not useful!
			However, it may be useful if we want to eliminate some momenta in favor
			of other momenta! *)

		If[TrueQ[EvenQ[lenl + lens]] && optEliminate==={},
			nums = Most[nums]
		];

		(* 	Generate all possible replacements by picking a given number of
			the momenta on the lhs and subtracting from it some momenta from the rhs.

			In any case the number of the momenta on the lhs of the
			replacement rule is larger than the number on the rhs.	*)
		selections = Flatten[pickMomenta[#, longest, shortest] & /@ nums] /. list[] -> Unevaluated[Sequence[]] /. list -> List;

		(* 	We can also generate replacement rules by just omiting a given number of
			the momenta on the lhs of the 4-momentum conservation equation  *)

		nums = Range[Quotient[lentot, 2] + 1, lenl - 1];
		If[	TrueQ[nums =!= {}],
			selections2 = Flatten[Map[Subsets[longest, {#}] &, nums] /. List[a___Symbol] :> list[a]],
			selections2 = {}
		];

		selections = Join[selections, selections2];

		(* 	If the numbers of the incoming and outgoing momenta are different, we can also use the
			replacement rule lhs->rhs. If the numbers are the same, we should not use this rule, unless
			we want to eliminate some momenta!
		*)
		If[	lenl =!= lens,
			selections = Join[selections, {longest}],
			If[ optEliminate=!={},
				Which[
					FreeQ2[longest,optEliminate] && !FreeQ2[shortest,optEliminate],
						selections = Join[selections, {shortest}],
					!FreeQ2[longest,optEliminate] && FreeQ2[shortest,optEliminate],
						selections = Join[selections, {longest}],
					True,
						Null
				]
			]
		];

		(*	Obtain the final set of the replacement rules	*)
		rule = Map[Rule[Total[#], Total[#] - Total[longest] + Total[shortest]] &, selections];

		(*	Check that each replacement rule is correct, i.e. it does not violate the
			4-momentum conservation *)
		If[	OptionValue[Check],
			If [DeleteDuplicates[Map[(#[[1]]-#[[2]]-Total[longest]+Total[shortest])&, rule]]=!={0},
				Message[FCRerouteMomenta::failmsg,"Some of the automatically generated rules violate the 4-momentum conservation."];
				Abort[]
			]
		];

		(*	Duplicate each replacement rule by considering a case where the
			overall sign is opposite *)

		rule = Join[rule, Expand[rule /. Rule[a_, b_] :> Rule[-a, -b]]];

		(* 	If some momenta where previously eliminated, we must ensure that they will not be
			reintroduced by some of the replacement rules! *)
		If[ optEliminate=!={},
			rule = Select[rule, FreeQ2[#[[2]], optEliminate] &]
		];

		lastRule = Select[rule, (NTerms[#[[1]]]===NTerms[#[[2]]])&];
		rule = Complement[rule,lastRule];


		FCPrint[1,"FCRerouteMomenta: Final set of replacement rules: " ,rule, FCDoControl->fcrmVerbose];

		(*	Now we extract all the FeynCalc objects, to which the replacement rules are applicable	*)
		objects = Cases2[ex, OptionValue[Replace]];

		FCPrint[3,"FCRerouteMomenta: List of suitable objects: " ,objects, FCDoControl->fcrmVerbose];

		If[	TrueQ[optInitialSubstitutions=!={}],
			objectsEval = objects/. optInitialSubstitutions,
			objectsEval = objects
		];

		objectsEval = FixedPoint[ReplaceAll[OptionValue[Expand]/@(#),rule]&, objectsEval,OptionValue[MaxIterations]];
		FCPrint[3,"FCRerouteMomenta: List of suitable objects after applying the replacements: " ,objects, FCDoControl->fcrmVerbose];

		(* 	If we want to eliminate particular momenta, we may also make use of replacement rules
			that do not change the total number of the momenta. Such rules are available only if the
			total number of the momenta is even. Obviously, such rules must be applied only at the very
			end, since otherwise we might miss more useful rules or run into an infinite recursion

		*)

		objectsEval = objectsEval /. lastRule;


		ruleFinal = Thread[Rule[objects, objectsEval]];
		FCPrint[3,"FCRerouteMomenta: The resulting replacement rule: ", ruleFinal, FCDoControl->fcrmVerbose];


		res = ex /. ruleFinal;
		If[	!OptionValue[FCE],
			res = FCI[res]
		];

		FCPrint[1,"FCRerouteMomenta: Leaving.", FCDoControl->fcrmVerbose];
		FCPrint[3,"FCRerouteMomenta: Leaving with ", expr, FCDoControl->fcrmVerbose];

		res


];

pickMomenta[num_, l_, s_] :=
	Flatten[Table[Outer[Union, Subsets[l, {num - i}], Subsets[-s, {i}], 1], {i, 1, num - 1}] /. List[a___Symbol, -1 b_Symbol, c___] :> list[a, -b, c]];


FCPrint[1,"FCRerouteMomenta.m loaded."];
End[]
