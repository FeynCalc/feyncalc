(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCPermuteMomentaRules											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Generates all possible permutations of the given momenta		*)

(* ------------------------------------------------------------------------ *)



FCPermuteMomentaRules::usage =
"FCPermuteMomentaRules[{p1, p2, ...}] returns a set of rules that contain all
possible permutations of the momenta p1, p2, ... . This can be useful when
working with amplitudes that exhibit a symmetry in some or all of the final
state momenta or when trying to find mappings between loop integrals from
different topologies.";

FCPermuteMomentaRules::failmsg =
"Error! FCPermuteMomentaRules has encountered a fatal problem and must abort the computation. \n
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCPermuteMomentaRules`Private`"]

FCPermuteMomentaRules[ex_ /; Head[ex] =!= List] :=
	(
	Message[FCPermuteMomentaRules::failmsg, "The input expression is not a list."];
	Abort[]
	);

FCPermuteMomentaRules[{}] :=
	{{}};

FCPermuteMomentaRules[{_}] :=
	{{}};

FCPermuteMomentaRules[lmoms_List] :=
	Block[{	permutations, len, rule, res},

		If[! FCDuplicateFreeQ[lmoms],
			Message[FCPermuteMomentaRules::failmsg, "The list of the momenta contains duplicates."];
			Abort[]
		];
		len = Length[lmoms];

		permutations = Permute[lmoms, PermutationGroup[Cycles[{#}] & /@ Subsets[Range[len], {2, len}]]];

		If[ !MatchQ[permutations, {{_, __} ..}],
			Message[FCPermuteMomentaRules::failmsg, "The obtained list of permutations is incorrect."];
			Abort[]
		];

		res = Thread[rule[ConstantArray[lmoms, Length[permutations]], permutations]];
		res = res /. rule[x_List, y_List] :> Thread[rule[x, y]] /. rule[a_, a_] :> Unevaluated[Sequence[]] /. rule -> Rule;
		res = SortBy[res, Length];

		If[ !MatchQ[res, {{}, {_Rule ..} ..}],
			Message[FCPermuteMomentaRules::failmsg, "The obtained list of rules is incorrect."];
			Abort[]
		];

		res
	];

FCPrint[1,"FCPermuteMomentaRules.m loaded."];
End[]
