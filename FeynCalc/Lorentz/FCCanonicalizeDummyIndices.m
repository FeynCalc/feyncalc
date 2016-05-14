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
	SUNIndexNames -> {},
	SUNFIndexNames -> {},
	Momentum -> All,
	NotMomentum -> {},
	Head -> {},
	FCVerbose-> False,
	DotSimplify -> True,
	FCTraceExpand -> True,
	FCVerbose -> False
};

FCCanonicalizeDummyIndices[expr_, OptionsPattern[]] :=
	Block[ {indexTypes, indexList = {}, replacementList,
		exprFCI,ex,tmp,null1,null2,dummyHeads, lorNames, lorRenamingRule,
		rest0=0,rest1=0,lihead,seedLor,moms,notmoms,finalList,isoHead,uniqueExpressions,repIndexListLor,
		canIndexList,seedSUN,seedSUNF,
		finalRepList,
		sunfRenamingRule,sunRenamingRule,
		res, repIndexListSUN,repIndexListSUNF,sunNames,sunfNames,
		sunhead,sunfhead,indhead,seedCustom},

		If [OptionValue[FCVerbose]===False,
			canodummyVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				canodummyVerbose=OptionValue[FCVerbose]
			];
		];

		indhead = OptionValue[Head];

		indexTypes = {LorentzIndex,SUNIndex,SUNFIndex};

		If[	indhead=!={},
			indexTypes = Join[indexTypes,{indhead}]
		];



		seedLor = ToString[Unique["li"]];
		seedSUN = ToString[Unique["sun"]];
		seedSUNF = ToString[Unique["sunf"]];
		seedCustom = ToString[Unique["cs"]];

		If [OptionValue[FCVerbose]===False,
			canodummyVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				canodummyVerbose=OptionValue[FCVerbose]
			];
		];

		lorNames = OptionValue[LorentzIndexNames];
		sunNames = OptionValue[SUNIndexNames];
		sunfNames = OptionValue[SUNFIndexNames];

		moms = OptionValue[Momentum];
		notmoms = OptionValue[NotMomentum];

		FCPrint[2,"FCCanonicalizeDummyIndices: Entering with: ", expr, FCDoControl->canodummyVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex,indexTypes],
			Return[ex]
		];

		If[ !FreeQ[ex,seedLor],
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
						Head[ind]=!=Lower),
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

		FCPrint[2,"FCCanonicalizeDummyIndices: Unique expressions: ", uniqueExpressions,
			FCDoControl->canodummyVerbose];


		If[	Head[moms]=!=All && Head[moms]===List,
		(* only for particular momenta *)
		repIndexListLor = ((MapIndexed[Rule[#1, LorentzIndex[lihead@FCGV[(seedLor <> ToString[Identity @@ #2])], (#1 /.
			LorentzIndex[_, dim_: 4] :> dim)]] &,
			Cases[#, _[a___, LorentzIndex[y__],b___]/;!FreeQ2[{a,b},moms] && FreeQ2[{a,b},notmoms] && MemberQ[finalList,LorentzIndex[y]]:> LorentzIndex[y], Infinity] // Union] // Flatten) & /@ uniqueExpressions),

		(* for all particular momenta *)
		repIndexListLor = ((MapIndexed[Rule[#1, LorentzIndex[lihead@FCGV[(seedLor <> ToString[Identity @@ #2])], (#1 /.
			LorentzIndex[_, dim_: 4] :> dim)]] &,
			Union@Cases[#, _[a___, LorentzIndex[y__],b___]/; FreeQ2[{a,b},notmoms] && MemberQ[finalList,LorentzIndex[y]]:> LorentzIndex[y], Infinity] // Union] // Flatten) & /@ uniqueExpressions)
		];

		repIndexListCustom = ((MapIndexed[Rule[#1, indhead[cushead@FCGV[(seedCustom <> ToString[Identity @@ #2])]]] &,
			Union@Cases[#, indhead[y_]/; MemberQ[finalList,indhead[y]]:> indhead[y],Infinity]] //Union // Flatten) & /@ uniqueExpressions);

		repIndexListSUN = ((MapIndexed[Rule[#1, SUNIndex[sunhead@FCGV[(seedSUN <> ToString[Identity @@ #2])]]] &,
			Union@Cases[#, SUNIndex[y_]/; MemberQ[finalList,SUNIndex[y]]:> SUNIndex[y],Infinity]] //Union // Flatten) & /@ uniqueExpressions);

		repIndexListSUNF = ((MapIndexed[Rule[#1, SUNFIndex[sunfhead@FCGV[(seedSUNF <> ToString[Identity @@ #2])]]] &,
			Union@Cases[#, SUNFIndex[y_]/; MemberQ[finalList,SUNFIndex[y]]:> SUNFIndex[y],Infinity]] //Union // Flatten) & /@ uniqueExpressions);

		FCPrint[2,"FCCanonicalizeDummyIndices: List of replacements for LorentzIndex type: ", repIndexListLor,
			FCDoControl->canodummyVerbose];

		FCPrint[2,"FCCanonicalizeDummyIndices: List of replacements for SUNIndex type: ", repIndexListSUN,
			FCDoControl->canodummyVerbose];

		FCPrint[2,"FCCanonicalizeDummyIndices: List of replacements for SUNFIndex type: ", repIndexListSUNF,
			FCDoControl->canodummyVerbose];

		If[ lorNames=!={},

			If[	!FreeQ2[ex,lorNames],
				Message[FCCanonicalizeDummyIndices::failmsg,
				"Dummy index names supplied for renaming are already present in the expression!"];
				Abort[]
			];

			dummyHeads = Cases2[repIndexListLor,lihead];
			If[	Length[lorNames]<Length[dummyHeads],
				lorRenamingRule = MapThread[Rule[#1, #2] &, {dummyHeads[[1 ;; Length[lorNames]]], lorNames}],
				lorRenamingRule = MapThread[Rule[#1, #2] &, {dummyHeads, lorNames[[1 ;; Length[dummyHeads]]]}]
			];
			repIndexListLor = repIndexListLor/.lorRenamingRule;

			FCPrint[2,"FCCanonicalizeDummyIndices: List of replacements for LorentzIndex type using given names: ", repIndexListLor,
			FCDoControl->canodummyVerbose];
		];

		If[ sunNames=!={},

			If[	!FreeQ2[ex,sunNames],
				Message[FCCanonicalizeDummyIndices::failmsg,
				"Dummy index names supplied for renaming are already present in the expression!"];
				Abort[]
			];

			dummyHeads = Cases2[repIndexListSUN,sunhead];
			If[	Length[sunNames]<Length[dummyHeads],
				sunRenamingRule = MapThread[Rule[#1, #2] &, {dummyHeads[[1 ;; Length[sunNames]]], sunNames}],
				sunRenamingRule = MapThread[Rule[#1, #2] &, {dummyHeads, sunNames[[1 ;; Length[dummyHeads]]]}]
			];
			FCPrint[2,"FCCanonicalizeDummyIndices: renaming rule for SUNIndex: ", sunRenamingRule,
			FCDoControl->canodummyVerbose];
			repIndexListSUN = repIndexListSUN/.sunRenamingRule;

			FCPrint[2,"FCCanonicalizeDummyIndices: List of replacements for SUNIndex type using given names: ", repIndexListSUN,
			FCDoControl->canodummyVerbose];
		];

		If[ sunfNames=!={},

			If[	!FreeQ2[ex,sunfNames],
				Message[FCCanonicalizeDummyIndices::failmsg,
				"Dummy index names supplied for renaming are already present in the expression!"];
				Abort[]
			];

			dummyHeads = Cases2[repIndexListSUNF,sunfhead];
			If[	Length[sunfNames]<Length[dummyHeads],
				sunfRenamingRule = MapThread[Rule[#1, #2] &, {dummyHeads[[1 ;; Length[sunfNames]]], sunfNames}],
				sunfRenamingRule = MapThread[Rule[#1, #2] &, {dummyHeads, sunfNames[[1 ;; Length[dummyHeads]]]}]
			];
			FCPrint[2,"FCCanonicalizeDummyIndices: renaming rule for SUNFIndex: ", sunfRenamingRule,
			FCDoControl->canodummyVerbose];
			repIndexListSUNF = repIndexListSUNF/.sunfRenamingRule;

			FCPrint[2,"FCCanonicalizeDummyIndices: List of replacements for SUNFIndex type using given names: ", repIndexListSUNF,
			FCDoControl->canodummyVerbose];
		];

		(* Here the renaming *)

		canIndexList = (MapIndexed[(#1 /. First[repIndexListLor[[#2]]] /. First[repIndexListSUN[[#2]]] /.
			First[repIndexListSUNF[[#2]]] /. First[repIndexListCustom[[#2]]]) &, uniqueExpressions]);
		finalRepList = MapThread[Rule[#1, #2] &, {uniqueExpressions, canIndexList}];

		res = (rest0+rest1+tmp) /.finalRepList /. isoHead|lihead|sunhead|sunfhead|cushead->Identity;

		res
	]

FCPrint[1,"FCCanonicalizeDummyIndices.m loaded."];
End[]
