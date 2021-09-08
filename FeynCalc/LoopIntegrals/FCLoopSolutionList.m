(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopSolutionList												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  	Creates subsitution lists of type Integral->
				simplified Integral from the output of FCLoopCanonicalize	*)

(* ------------------------------------------------------------------------ *)

FCLoopSolutionList::usage =
"FCLoopSolutionList[loopList, reversedRepIndexList, canIndexList,
uniqueCanIndexList}, solsList] is an auxiliary internal function that uses the
output of FCLoopCanonicalize and the list of simplified integrals solsList to
create the substitution list of type \"Integral\" -> \"simplified Integral\".";

FCLoopSolutionList::failmsg =
"Error! FCLoopSolutionList has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"


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
		Message[FCLoopSolutionList::failmsg, "The input is inconsistent."];
		Abort[]
	];


	Inner[If[((z1=Union@Sort@Cases[#1, (CartesianIndex|LorentzIndex)[_, ___], Infinity])=!=
			(z2=Union@Sort@Cases[#2, (CartesianIndex|LorentzIndex)[_, ___], Infinity])) && !MatchQ[z3=#2,0 | _[0] ],
			Message[FCLoopSolutionList::failmsg,"The indices in the solutions list are different from those in the unique integrals list."];
			Abort[]]&,uniqueCanIndexList,solsList];

	(*	This is the replacement list Integral -> Solution for the integrals	with canonicalized indices.	*)
	repSolList =
		MapIndexed[(Rule[#1, First[solsList[[#2]]]]) &,
		uniqueCanIndexList];

	(*	This is the more general replacement list for integrals with indices that are not canonicalized	*)
	reducedLoopList = MapIndexed[((#1 /.First[(reversedRepIndexList[[#2]])])) &, (canIndexList /.repSolList)];

	(*	This is the final replacement list Original integral -> Reduced integral	*)
	finalRepList =
	MapIndexed[(Rule[#1, First[reducedLoopList[[#2]]]]) &, loopList];

	(*	Check also that the final replacement list contains no canonicalized indices	*)
	If[	!FreeQ2[finalRepList, Union[Cases[canIndexList, LorentzIndex[_, _ : 4], Infinity]]] ||
		!FreeQ2[finalRepList, Union[Cases[canIndexList, CartesianIndex[_, _ : 3], Infinity]]],
		Message[FCLoopSolutionList::failmsg,"Failed to create a list of solutions out of the given integrals."];
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
