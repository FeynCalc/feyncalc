(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopSolutionList												*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  	Creates subsitution lists of type Integral->
				simplified Integral from the output of FCLoopCanonicalize	*)

(* ------------------------------------------------------------------------ *)

FCLoopSolutionList::usage = "FCLoopSolutionList[loopList, reversedRepIndexList,
canIndexList, uniqueCanIndexList}, solsList] is an auxiliary \
internal function that uses the output of FCLoopCanonicalize and the list of \
simplified integrals solsList to create the substitution list of type \
\"Integral\" -> \"simplified Integral\"."

FCLoopSolutionList::fail=
"FCLoopSolutionList failed to create a list of solutions out of the given \
integrals";

FCLoopSolutionList::indexmissmatch=
"FCLoopSolutionList found that the Lorentz indices in the list of solutions \
are different from those in the list of unique integrals. `1` doesn't match `2` and
`3` is not zero!";


Begin["`Package`"]
End[]

Begin["`FCLoopSolutionList`Private`"]

Options[FCLoopSolutionList] = {
	Dispatch -> True
};

FCLoopSolutionList[{loopList_List, reversedRepIndexList_List,canIndexList_List,
	uniqueCanIndexList_List}, solsList_List, OptionsPattern[]] :=
	Block[{repSolList, reducedLoopList, finalRepList,z1,z2,z3},

	(*	Small cross-check	*)
	If[	Length[uniqueCanIndexList]=!=Length[solsList] ||
		Length[canIndexList]=!=Length[reversedRepIndexList],
		Message[FCLoopSolutionList::fail];
		Print["1"];
		Abort[]
	];
	(* 	Also check that the solutions have the same Lorentz indices
		as the integrals for which they will be substituted *)
	Inner[If[	((z1=Union@Sort@Cases[#1, LorentzIndex[_, _ : 4], Infinity])=!=
				(z2=Union@Sort@Cases[#2, LorentzIndex[_, _ : 4], Infinity])) && !MatchQ[z3=#2,0 | _[0] ],
				Message[FCLoopSolutionList::indexmissmatch,z1,z2,z3];
				Abort[]
		]&,uniqueCanIndexList,solsList];

	(*	This is the replacement list Integral -> Solution for the integrals
		with canonicalized indices	*)
	repSolList =
		MapIndexed[(Rule[#1, First[solsList[[#2]]]]) &,
		uniqueCanIndexList];
	(*	This is the more general replacement list for integrals with
		indices that are not canonicalized*)
	reducedLoopList = MapIndexed[((#1 /.First[(reversedRepIndexList[[#2]])])) &, (canIndexList /.repSolList)];
	(*This is the final replacement list Original integral -> Reduced integral*)
	finalRepList =
	MapIndexed[(Rule[#1, First[reducedLoopList[[#2]]]]) &, loopList];

	(*	Check also that the final replacement list contains no
		canonicalized indices	*)
	If[	!FreeQ2[finalRepList,
		Union[Cases[canIndexList, LorentzIndex[_, _ : 4], Infinity]]],
		Message[FCLoopSolutionList::fail];
		Print["3"];
		Abort[]
	];

	If[ OptionValue[Dispatch],
		finalRepList = Dispatch[finalRepList]
	];

	(*	Return the final result	*)
	finalRepList
]
FCPrint[1,"FCLoopSolutionList.m loaded."];
End[]
