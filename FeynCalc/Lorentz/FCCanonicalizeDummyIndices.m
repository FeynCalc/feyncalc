(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCCanonicalizeDummyIndices												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Canonicalizes dummy Lorentz indices *)

(* ------------------------------------------------------------------------ *)

FCCanonicalizeDummyIndices::usage = "
FCCanonicalizeDummyIndices[expr] canonicalizes all dummy Lorentz indices \
in the expression " <> ToString[
Hyperlink[Style["\[RightSkeleton]", "SR"], "paclet:FeynCalc/ref/FCCanonicalizeDummyIndices"],
StandardForm];

FCCanonicalizeDummyIndices::failmsg =
"Error! FCCanonicalizeDummyIndices has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

NotMomentum::usage = "
NotMomentum is an option of FCCanonicalizeDummyIndices. It specifies a list of momenta for which
no canonicalization should be done."


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCCanonicalizeDummyIndices`Private`"]

canodummyVerbose::usage="";

Options[FCCanonicalizeDummyIndices] = {
	FCI -> False,
	LorentzIndexNames -> {},
	Momentum -> All,
	NotMomentum -> {},
	FCVerbose-> False,
	DotSimplify -> True,
	FCTraceExpand -> True
};

FCCanonicalizeDummyIndices[expr_, OptionsPattern[]] :=
	Block[ {indexTypes, indexList = {}, replacementList,
		exprFCI,ex,tmp,null1,null2,dummyHeads, lorNames, lorRenamingRule,
		rest0=0,rest1=0,lihead,seed,moms,notmoms,finalList,isoHead,uniqueExpressions,repIndexList,
		canIndexList,finalRepList,res},

		indexTypes = {LorentzIndex,SUNIndex,SUNFIndex};
		seed = ToString[Unique["li"]];

		If [OptionValue[FCVerbose]===False,
			canodummyVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				canodummyVerbose=OptionValue[FCVerbose]
			];
		];

		lorNames = OptionValue[LorentzIndexNames];
		moms = OptionValue[Momentum];
		notmoms = OptionValue[NotMomentum];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex,indexTypes],
			Return[ex]
		];

		If[ !FreeQ[ex,seed],
			Message[FCCanonicalizeDummyIndices::failmsg,
				"The random index already appears in the expression!"];
			Abort[]
		];

		tmp = Expand2[ex, indexTypes];

		{rest0,tmp} = FCSplit[tmp,indexTypes];

		If[	notmoms=!={},
			{rest1,tmp} = FCSplit[tmp,notmoms]
		];

		If[ OptionValue[FCTraceExpand],
			tmp = FCTraceExpand[tmp,FCI->True]
		];

		indexList =
				Map[Tally, Map[Cases[#, (Alternatives@@indexTypes)[ind_, ___]/;(Head[ind]=!=Upper &&
						Head[ind]=!=Lower) :> ind,
						Infinity]&,Apply[List, tmp+null1+null2]]]// Flatten[#, 1] & // Union;

		FCPrint[2,"FCCanonicalizeDummyIndices: List of indices and their multiplicities: ", indexList,
			FCDoControl->canodummyVerbose];

		If[ Select[indexList, ((#[[2]]) > 2) &]=!={},
			Message[FCCanonicalizeDummyIndices::failmsg,"The input expression contains dummy indices that
			appear more than twice, which violates the Einstein convention."];
			Abort[]
		];

		finalList = Cases[indexList, {ind_, 2} -> ind ];

		tmp  = FCLoopIsolate[tmp,finalList,Head->isoHead,Factoring->False,DotSimplify->OptionValue[DotSimplify]];

		If[	Head[moms]=!=All && Head[moms]===List,
			tmp = tmp /. isoHead[x_]/; FreeQ2[x,moms]:> x
		];

		uniqueExpressions = Cases2[tmp, isoHead];

		(* TODO at the moment only Lorentz*)
		If[	Head[moms]=!=All && Head[moms]===List,
		(* only for particular momenta *)
		repIndexList = ((MapIndexed[Rule[#1, LorentzIndex[lihead@FCGV[(seed <> ToString[Identity @@ #2])], (#1 /.
			LorentzIndex[_, dim_: 4] :> dim)]] &,
			Cases[#, _[a___, LorentzIndex[y__],b___]/;!FreeQ2[{a,b},moms] && FreeQ2[{a,b},notmoms]:> LorentzIndex[y], Infinity] // Union] // Flatten) & /@ uniqueExpressions),

		(* for all particular momenta *)
		repIndexList = ((MapIndexed[Rule[#1, LorentzIndex[lihead@FCGV[(seed <> ToString[Identity @@ #2])], (#1 /.
			LorentzIndex[_, dim_: 4] :> dim)]] &,
			Cases[#, _[a___, LorentzIndex[y__],b___]/; FreeQ2[{a,b},notmoms]:> LorentzIndex[y], Infinity] // Union] // Flatten) & /@ uniqueExpressions)
		];

		If[ lorNames=!={},

			If[	!FreeQ2[ex,lorNames],
				Message[FCCanonicalizeDummyIndices::failmsg,
				"Dummy index names supplied for renaming are already present in the expression!"];
				Abort[]
			];

			dummyHeads = Cases2[repIndexList,lihead];
			If[	Length[lorNames]<Length[dummyHeads],
				lorRenamingRule = MapThread[Rule[#1, #2] &, {dummyHeads[[1 ;; Length[lorNames]]], lorNames}],
				lorRenamingRule = MapThread[Rule[#1, #2] &, {dummyHeads, lorNames[[1 ;; Length[dummyHeads]]]}]
			];
			repIndexList = repIndexList/.lorRenamingRule
		];

		(* Here the renaming *)

		canIndexList = (MapIndexed[(#1 /. First[repIndexList[[#2]]]) &, uniqueExpressions]);

		finalRepList = MapThread[Rule[#1, #2] &, {uniqueExpressions, canIndexList}];

		res = (rest0+rest1+tmp) /.finalRepList /. isoHead|lihead->Identity;

		res
	]

FCPrint[1,"FCCanonicalizeDummyIndices.m loaded."];
End[]
