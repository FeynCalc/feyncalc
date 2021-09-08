(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCLoopCanonicalize												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Canonicalizes indices of 1-loop integrals						*)

(* ------------------------------------------------------------------------ *)

FCLoopCanonicalize::usage =
"FCLoopCanonicalize[exp, q, loopHead] is an auxiliary internal function that
canonicalizes indices of 1-loop integrals with loop momentum q that are
wrapped with loopHead. The output is given as a list of 4 entries, of which
the last one contains a list of all the unique 1-loop integrals in the given
expression. After those are simplified, the original output of
FCLoopCanonicalize together with the list of the simplified unique integrals
should be inserted into FCLoopSolutionList to obtain the final replacement
list that will be applied to the original expression.";

FCLoopCanonicalize::failmsg =
"Error! FCLoopCanonicalize has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

Begin["`Package`"]
End[]

Begin["`FCLoopCanonicalize`Private`"]

Options[FCLoopCanonicalize] = {
	FCI -> False,
	PaVeIntegralHeads -> FeynCalc`Package`PaVeHeadsList
};

FCLoopCanonicalize[expr_, q_, head_, OptionsPattern[]] :=
	Block[{	ex, loopList, repIndexList, reversedRepIndexList,
			canIndexList, uniqueCanIndexList, null1, null2, seed,
			res, loopIntHeads},

		loopIntHeads = OptionValue[PaVeIntegralHeads];
		seed = ToString[Unique["cli"]];

		(*This is the list of all the loop integrals in the expression.*)
		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		loopList = Union[Cases[ex + null1 + null2, head[_] , Infinity]];

		If[ !FreeQ[ex,seed],
			Message[FCLoopCanonicalize::failmsg,"The generated dummy indices are not unique."];
			Abort[]
		];

		If[ Cases[loopList, head[x_]/;FreeQ2[x,Join[{q},loopIntHeads]] , Infinity]=!={},
			Message[FCLoopCanonicalize::failmsg, "The input expression incorrect contains nonloop terms."];
			Abort[]
		];


		(*	Here we collect the tensor indices of each integral from the previous list	*)
		repIndexList =
					Which[
						!FreeQ[#,LorentzIndex] && FreeQ[#,CartesianIndex],

						((MapIndexed[Rule[#1,LorentzIndex[FCGV[(seed <> ToString[Identity @@ #2])], (#1/.LorentzIndex[_,dim_:4]:>dim)]] &,
							Cases[#, Pair[x_, LorentzIndex[y__]] /; ! FreeQ[x, q] :> LorentzIndex[y], Infinity] // Union] // Flatten)),

						FreeQ[#,LorentzIndex] && !FreeQ[#,CartesianIndex],
						(MapIndexed[Rule[#1,CartesianIndex[FCGV[(seed <> ToString[Identity @@ #2])], (#1/.CartesianIndex[_,dim_:3]:>dim)]] &, Cases[#,
							CartesianPair[x_, CartesianIndex[y__]] /; ! FreeQ[x, q] :>
								CartesianIndex[y], Infinity] // Union] // Flatten),

						FreeQ[#,LorentzIndex] && FreeQ[#,CartesianIndex],
						{},
						_,
						Message[FCLoopCanonicalize::failmsg, "Unknown integral type."];
						Abort[]
					] & /@loopList;

		(*	This is the list of all the loop tensor integrals with
			canonicalized indices.	*)
		canIndexList = (MapIndexed[(#1 /. First[repIndexList[[#2]]]) &,
			loopList]);
		reversedRepIndexList = Map[(Reverse /@ #) &, repIndexList];

		(*	Finally we obtain the (usually much smaller) list of all the
			unique tensor integrals.Only those need to be reduced.*)
		uniqueCanIndexList = canIndexList // DeleteDuplicates;

		(* Final cross-check to ensure that we didn't mess anything up *)
		res = {loopList, reversedRepIndexList, canIndexList, uniqueCanIndexList};
		If [(ex/. head->Identity) =!= (ex/.FCLoopSolutionList[res, (res[[4]]/. head->Identity)]),
			Message[FCLoopCanonicalize::failexp,"Failed to canonicalize loop integrals in the input expression."];
			Abort[]
		];

		res
	]



FCPrint[1,"FCLoopCanonicalize.m loaded."];
End[]
