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

CustomIndexNames::usage = "
CustomIndexNames is an option of FCCanonicalizeDummyIndices. It allow to specify custom names \
for canonicalized dummy indices of custom index heads, e.g.
FCCanonicalizeDummyIndices[ T1[MyIndex2[a], MyIndex1[b]] v1[MyIndex1[b]] v2[MyIndex2[a]] + \
T1[MyIndex2[c], MyIndex1[f]] v1[MyIndex1[f]] v2[MyIndex2[c]], Head -> {MyIndex1, MyIndex2}, \
CustomIndexNames -> {{MyIndex1, {i1}}, {MyIndex2, {i2}}}]";



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
	CustomIndexNames -> {},
	Momentum -> All,
	NotMomentum -> {},
	Head -> {LorentzIndex,SUNIndex,SUNFIndex},
	FCVerbose-> False,
	DotSimplify -> True,
	FCTraceExpand -> True,
	FCVerbose -> False,
	Function -> Function[{x, seed}, FCGV[(ToString[seed] <> ToString[Identity @@ x])]]
};

makeRepIndexList[mIndexHead_,mWrappinHead_,mSeed_,mFunc_,mFinalList_,mUniqueExp_]:=
			((MapIndexed[Rule[#1, mIndexHead[mWrappinHead@mFunc[#2,mSeed]]] &,
			Union@Cases[#, mIndexHead[y_]/; MemberQ[mFinalList,mIndexHead[y]]:> mIndexHead[y],Infinity]] //Union // Flatten) & /@ mUniqueExp);



renameDummies[dummyNames_,wrapHead_, totalRepLis_]:=
	Block[{dummyHeads,renamingRule = {}},
		If[ dummyNames=!={},

			dummyHeads = Cases2[totalRepLis,wrapHead];
			If[	Length[dummyNames]<Length[dummyHeads],
				renamingRule = MapThread[Rule[#1, #2] &, {dummyHeads[[1 ;; Length[dummyNames]]], dummyNames}],
				renamingRule = MapThread[Rule[#1, #2] &, {dummyHeads, dummyNames[[1 ;; Length[dummyHeads]]]}]
			]
		];
		renamingRule
	];



FCCanonicalizeDummyIndices[expr_, OptionsPattern[]] :=
	Block[ {indexList = {}, ex,tmp,null1,null2, renamingRule,
			rest0=0,lihead,seedLor,moms,notmoms,finalList,isoHead, uniqueExpressions,
			repIndexListLor, canIndexList, finalRepList,repIndexListTotal,
			res, sunhead,sunfhead,indhead,repIndexListsCustom={},fu,otherHeads,
			renamingList,cList},

		If [OptionValue[FCVerbose]===False,
			canodummyVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				canodummyVerbose=OptionValue[FCVerbose]
			];
		];

		indhead = OptionValue[Head];

		If[ Head[indhead] =!=List,
			indhead = {indhead};
		];

		fu = OptionValue[Function];


		seedLor = Unique["li"];

		If [OptionValue[FCVerbose]===False,
			canodummyVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer?Positive | 0],
				canodummyVerbose=OptionValue[FCVerbose]
			];
		];

		moms = OptionValue[Momentum];
		notmoms = OptionValue[NotMomentum];

		FCPrint[2,"FCCanonicalizeDummyIndices: Entering with: ", expr, FCDoControl->canodummyVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ2[ex,indhead],
			Return[ex]
		];

		tmp = Expand2[ex, indhead];

		{rest0,tmp} = FCSplit[tmp,indhead];

		If[ OptionValue[FCTraceExpand],
			tmp = FCTraceExpand[tmp,FCI->True]
		];

		indexList =
				Map[Tally, Map[Cases[#, (Alternatives@@indhead)[ind_, ___]/;(Head[ind]=!=Upper &&
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
		repIndexListLor = ((MapIndexed[Rule[#1, LorentzIndex[lihead@fu[#2,seedLor], (#1 /.
			LorentzIndex[_, dim_: 4] :> dim)]] &,
			Cases[#, _[a___, LorentzIndex[y__],b___]/;!FreeQ2[{a,b},moms] && FreeQ2[{a,b},notmoms] && MemberQ[finalList,LorentzIndex[y]]:> LorentzIndex[y], Infinity] // Union] // Flatten) & /@ uniqueExpressions),

		(* for all particular momenta *)
		repIndexListLor = ((MapIndexed[Rule[#1, LorentzIndex[lihead@fu[#2,seedLor], (#1 /.
			LorentzIndex[_, dim_: 4] :> dim)]] &,
			Union@Cases[#, _[a___, LorentzIndex[y__],b___]/; FreeQ2[{a,b},notmoms] && MemberQ[finalList,LorentzIndex[y]]:> LorentzIndex[y], Infinity] // Union] // Flatten) & /@ uniqueExpressions)
		];

		otherHeads = Complement[Union[indhead],{LorentzIndex,SUNIndex,SUNFIndex}];
		FCPrint[1,"FCCanonicalizeDummyIndices: Custom index heads present: ", otherHeads,
			FCDoControl->canodummyVerbose];

		If[otherHeads =!={},
			repIndexListsCustom = Map[makeRepIndexList[#,ToExpression[ToString[#]<>"head"],
				Unique[ToLowerCase[ToString[#]]],fu,finalList,uniqueExpressions]&, otherHeads]
		];

		repIndexListTotal = {repIndexListLor,
			makeRepIndexList[SUNIndex,sunhead,Unique["sun"],fu,finalList,uniqueExpressions],
			makeRepIndexList[SUNFIndex,sunfhead,Unique["sunf"],fu,finalList,uniqueExpressions]
		};
		If [ repIndexListsCustom =!={},
			repIndexListTotal = Join[repIndexListTotal,repIndexListsCustom];
		];

		repIndexListTotal = Flatten/@ Transpose[repIndexListTotal];

		FCPrint[2,"FCCanonicalizeDummyIndices: List of replacements: ", repIndexListTotal,
			FCDoControl->canodummyVerbose];


		(* Renaming of dummy indices according to the supplied list *)
		cList = {{OptionValue[LorentzIndexNames],lihead},{OptionValue[SUNIndexNames],sunhead},{OptionValue[SUNFIndexNames],sunfhead}};
		If[ OptionValue[CustomIndexNames]=!={},
			cList = Join[cList,Map[{Last[#], ToExpression[ToString[First[#]]<>"head"]}&,OptionValue[CustomIndexNames]]];
		];
		cList = cList /. {{},_} :> Unevaluated[Sequence[]];


		If[	!FreeQ2[ex,Flatten[cList /. {l_List,_} :> l]],
				Message[FCCanonicalizeDummyIndices::failmsg,
				"Dummy index names supplied for renaming are already present in the expression!"];
				Abort[]
		];

		FCPrint[2,"FCCanonicalizeDummyIndices: cList: ", cList, FCDoControl->canodummyVerbose];

		If [cList=!={},
			renamingRule = Flatten[Map[renameDummies[#[[1]],#[[2]], repIndexListTotal]&,cList]];
			FCPrint[2,"FCCanonicalizeDummyIndices: User-supplied renaming of dummy indices: ", renamingRule,
			FCDoControl->canodummyVerbose];
			repIndexListTotal = repIndexListTotal/. renamingRule;
		];

		(* The final renaming *)
		canIndexList = (MapIndexed[(#1 /. First[repIndexListTotal[[#2]]]) &, uniqueExpressions]);
		FCPrint[3,"FCCanonicalizeDummyIndices: canIndexList: ", canIndexList, FCDoControl->canodummyVerbose];

		finalRepList = MapThread[Rule[#1, #2] &, {uniqueExpressions, canIndexList}];

		res = (rest0+tmp) /.finalRepList /. isoHead|lihead|sunhead|sunfhead->Identity;

		res
	]

FCPrint[1,"FCCanonicalizeDummyIndices.m loaded."];
End[]
