(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCCanonicalizeDummyIndices												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Canonicalizes dummy Lorentz indices *)

(* ------------------------------------------------------------------------ *)

FCCanonicalizeDummyIndices::usage =
"FCCanonicalizeDummyIndices[expr]  canonicalizes all dummy Lorentz indices in
the expression. The option Momentum provides a possibility to limit the
canonicalization only to particular Momenta.

With the option LorentzIndexNames one can provide a list of names to be used
for the canonicalized indices, to have say $\\mu$, $\\nu$, $\\rho$ etc. instead
of some random names.";

FCCanonicalizeDummyIndices::failmsg =
"Error! FCCanonicalizeDummyIndices has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

NotMomentum::usage =
"NotMomentum is an option of FCCanonicalizeDummyIndices. It specifies a list of
momenta for which
no canonicalization should be done.";

CustomIndexNames::usage =
"CustomIndexNames is an option of FCCanonicalizeDummyIndices. It allows to
specify custom names for canonicalized dummy indices of custom index heads.";



(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FCCanonicalizeDummyIndices`Private`"]

canodummyVerbose::usage="";

Options[FCCanonicalizeDummyIndices] = {
	CartesianIndexNames -> {},
	CustomIndexNames 	-> {},
	DiracChainExpand	-> True,
	DiracIndexNames 	-> {},
	DotSimplify 		-> True,
	FCE 				-> False,
	FCI 				-> False,
	FCTraceExpand 		-> True,
	FCVerbose 			-> False,
	FCVerbose			-> False,
	Function			-> Function[{x, seed}, FCGV[(ToString[seed] <> ToString[Identity @@ x])]],
	Head				-> {LorentzIndex,CartesianIndex,SUNIndex,SUNFIndex, DiracIndex},
	LorentzIndexNames 	-> {},
	Momentum			-> All,
	NotMomentum			-> {},
	PauliChainExpand	-> True,
	SUNFIndexNames		-> {},
	SUNIndexNames		-> {}
};

makeRepIndexList[mIndexHead_,mWrappinHead_,mSeed_,mFunc_,mFinalList_,mUniqueExp_]:=
		Block[{indList},
			indList = DeleteDuplicates/@(Map[Cases[#, mIndexHead[y__]/; MemberQ[mFinalList,mIndexHead[y]], Infinity]&, mUniqueExp]);
			Map[Function[x, MapIndexed[Rule[#1, mIndexHead[mWrappinHead@mFunc[#2, mSeed]]] &,x]][#] &, indList]
		];

renameDummies[dummyNames_,wrapHead_, totalRepLis_]:=
	Block[{dummyHeads,renamingRule = {}},
		If[ dummyNames=!={},

			dummyHeads = Cases2[totalRepLis,wrapHead];
			If[	Length[dummyNames]<Length[dummyHeads],
				renamingRule = Thread[Rule[dummyHeads[[1 ;; Length[dummyNames]]], dummyNames]],
				renamingRule = Thread[Rule[dummyHeads, dummyNames[[1 ;; Length[dummyHeads]]] ]]
			]
		];
		renamingRule
	];

FCCanonicalizeDummyIndices[expr_, OptionsPattern[]] :=
	Block[ {indexList = {}, ex,exUnexpanded,tmp,null1,null2, renamingRule,
			rest0=0,lihead,cihead,seedLor,moms,notmoms,finalList,isoHead, uniqueExpressions,
			repIndexListLor, canIndexList, finalRepList,repIndexListTotal,
			res, sunhead,sunfhead,indhead,dihead,repIndexListsCustom={},fu,otherHeads,
			renamingList,cList,indexExtract, seedCar, repIndexListCar, times, dimensions, rule},

		If [OptionValue[FCVerbose]===False,
			canodummyVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				canodummyVerbose=OptionValue[FCVerbose]
			];
		];

		indhead = OptionValue[Head];

		If[ Head[indhead] =!=List,
			indhead = {indhead};
		];

		fu = OptionValue[Function];


		seedLor = Unique["li"];
		seedCar = Unique["ci"];

		If [OptionValue[FCVerbose]===False,
			canodummyVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
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
		exUnexpanded = ex;

		If[	FreeQ2[ex,indhead],
			Return[ex]
		];

		If[ OptionValue[FCTraceExpand],
			ex = FCTraceExpand[ex,FCI->True]
		];

		If[ OptionValue[DotSimplify],
			ex = DotSimplify[ex,FCI->True]
		];

		If[ OptionValue[DiracChainExpand],
			ex = DiracChainExpand[ex,FCI->True]
		];

		If[ OptionValue[PauliChainExpand],
			ex = PauliChainExpand[ex,FCI->True]
		];

		tmp = Expand2[ex, indhead];

		{rest0,tmp} = FCSplit[tmp,indhead];



		If[	!FreeQ[tmp,Power],
			tmp = tmp /. Power[z_, n_Integer?Positive]/;!FreeQ2[z, indhead] :> Apply[times, Table[z, {Abs[n]}]]^Sign[n]
		];

		dimensions = FCGetDimensions[tmp,FCI->True];
		FCPrint[3,"FCCanonicalizeDummyIndices: dimensions: ", dimensions, FCDoControl->canodummyVerbose];

		indexList = Map[
			ReplaceAll[Cases[#, (Alternatives@@indhead)[_, ___], Infinity],(h:(Alternatives@@indhead))[ind_, ___]:>h[ind]]&,
		Apply[List, tmp+null1+null2]];

		FCPrint[3,"FCCanonicalizeDummyIndices: Preliminary indexList: ", indexList, FCDoControl->canodummyVerbose];

		indexList = Tally/@(indexList);

		FCPrint[3,"FCCanonicalizeDummyIndices: Preliminary indexList after Tally: ", indexList, FCDoControl->canodummyVerbose];

		indexList = indexList// Flatten[#, 1] & // Union;

		FCPrint[2,"FCCanonicalizeDummyIndices: List of indices and their multiplicities: ", indexList, FCDoControl->canodummyVerbose];

		If[ Select[indexList, ((#[[2]]) > 2) &]=!={},
			Message[FCCanonicalizeDummyIndices::failmsg,"The input expression contains dummy indices that
			appear more than twice, which violates the Einstein convention."];
			Abort[]
		];

		finalList = Cases[indexList, {ind_, 2} -> ind ];
		finalList = finalList /. {(h:LorentzIndex|CartesianIndex)[x_] :> Map[h[x,#]&,dimensions]};
		finalList = Flatten[finalList];

		FCPrint[3,"FCCanonicalizeDummyIndices: finalList: ", finalList, FCDoControl->canodummyVerbose];

		If[	finalList==={},
			FCPrint[1,"FCCanonicalizeDummyIndices: No dummy indices to canonicalize, returning the original expression.", FCDoControl->canodummyVerbose];
			Return[exUnexpanded];
		];

		tmp  = FCLoopIsolate[tmp,finalList,Head->isoHead,Factoring->False,DotSimplify->OptionValue[DotSimplify]];

		If[	Head[moms]=!=All && Head[moms]===List,
			tmp = tmp /. isoHead[x_]/; FreeQ2[x,moms]:> x
		];

		uniqueExpressions = Cases2[tmp, isoHead];

		FCPrint[2,"FCCanonicalizeDummyIndices: Unique expressions: ", uniqueExpressions, FCDoControl->canodummyVerbose];


		(* Lorentz indices *)
		If[	!FreeQ[indhead,LorentzIndex],
			If[	(Head[moms]=!=All && Head[moms]===List) || notmoms=!={},
				(* only for particular momenta *)
				indexExtract = Map[Cases[#, _[a___, LorentzIndex[y__],b___]/;!FreeQ2[{a,b},moms] && FreeQ2[{a,b},notmoms] && MemberQ[finalList,LorentzIndex[y]]:> LorentzIndex[y], Infinity]&, uniqueExpressions],
				(* for all momenta *)
				indexExtract = Map[Cases[#, LorentzIndex[y__]/; MemberQ[finalList,LorentzIndex[y]], Infinity]&, uniqueExpressions]
			];

			indexExtract = DeleteDuplicates/@indexExtract;

			FCPrint[2,"FCCanonicalizeDummyIndices: Set of dummy Lorentz indices: ", indexExtract, FCDoControl->canodummyVerbose];

			If[	!MatchQ[indexExtract, {{___LorentzIndex} ...}],
					Message[FCCanonicalizeDummyIndices::failmsg,
					"Failed to  properly extract dummy Lorentz indices."];
					FCPrint[1,"FCCanonicalizDummyIndices: Entering with: ", indexExtract, FCDoControl->canodummyVerbose];
					Abort[]
			];

			repIndexListLor = Map[Function[x, MapIndexed[Rule[#1, LorentzIndex[lihead@fu[#2, seedLor]]] &,x]][#] &,
				DeleteDuplicates/@(indexExtract/. (LorentzIndex[aa_,_]:> LorentzIndex[aa]))
			];

			FCPrint[2,"FCCanonicalizeDummyIndices: repIndexListLor: ", repIndexListLor, FCDoControl->canodummyVerbose];

			repIndexListLor = repIndexListLor/. Rule -> rule /.
				{rule[LorentzIndex[x_],LorentzIndex[y_]]:> Map[rule[LorentzIndex[x,#],LorentzIndex[y,#]]&,dimensions]} /. rule -> Rule;

			repIndexListLor = DeleteDuplicates/@(Flatten/@repIndexListLor);

			FCPrint[2,"FCCanonicalizeDummyIndices: repIndexListLor: ", repIndexListLor, FCDoControl->canodummyVerbose],

			repIndexListLor=Sequence[];
		];

		(* Cartesian indices *)
		If[	!FreeQ[indhead,CartesianIndex],
			If[	(Head[moms]=!=All && Head[moms]===List) || notmoms=!={},
				(* only for particular momenta *)
				indexExtract = Map[Cases[#, _[a___, CartesianIndex[y__],b___]/;!FreeQ2[{a,b},moms] && FreeQ2[{a,b},notmoms] && MemberQ[finalList,CartesianIndex[y]]:> CartesianIndex[y], Infinity]&, uniqueExpressions],
				(* for all momenta *)
				indexExtract = Map[Cases[#, CartesianIndex[y__]/; MemberQ[finalList,CartesianIndex[y]], Infinity]&, uniqueExpressions]
			];

			indexExtract = DeleteDuplicates/@indexExtract;

			FCPrint[2,"FCCanonicalizeDummyIndices: Set of dummy Cartesian indices: ", indexExtract, FCDoControl->canodummyVerbose];

			If[	!MatchQ[indexExtract, {{___CartesianIndex} ...}],
					Message[FCCanonicalizeDummyIndices::failmsg,
					"Failed to  properly extract dummy Cartesian indices."];
					FCPrint[1,"FCCanonicalizDummyIndices: Entering with: ", indexExtract, FCDoControl->canodummyVerbose];
					Abort[]
			];

			repIndexListCar = Map[Function[x, MapIndexed[Rule[#1, CartesianIndex[cihead@fu[#2, seedCar]]] &,x]][#] &,
				DeleteDuplicates/@(indexExtract/. (CartesianIndex[aa_,_]:> CartesianIndex[aa]))];

			repIndexListCar = repIndexListCar/. Rule -> rule /.
				{rule[CartesianIndex[x_],CartesianIndex[y_]]:> Map[rule[CartesianIndex[x,#],CartesianIndex[y,#]]&,dimensions]} /. rule -> Rule;

			repIndexListCar = DeleteDuplicates/@(Flatten/@repIndexListCar);



			FCPrint[2,"FCCanonicalizeDummyIndices: repIndexListCar: ", repIndexListCar, FCDoControl->canodummyVerbose],
			repIndexListCar = Sequence[]

		];

		(* Rest *)

		otherHeads = Complement[Union[indhead],{LorentzIndex,CartesianIndex,SUNIndex,SUNFIndex,DiracIndex}];
		FCPrint[1,"FCCanonicalizeDummyIndices: Custom index heads present: ", otherHeads, FCDoControl->canodummyVerbose];

		If[otherHeads =!={},
			repIndexListsCustom = Map[makeRepIndexList[#,ToExpression[ToString[#]<>"head"],
				Unique[ToLowerCase[ToString[#]]],fu,finalList,uniqueExpressions]&, otherHeads];
			FCPrint[3,"FCCanonicalizeDummyIndices: repIndexListsCustom: ", repIndexListsCustom,
			FCDoControl->canodummyVerbose];
		];

		repIndexListTotal = {repIndexListLor,repIndexListCar,
			makeRepIndexList[SUNIndex,sunhead,Unique["sun"],fu,finalList,uniqueExpressions],
			makeRepIndexList[SUNFIndex,sunfhead,Unique["sunf"],fu,finalList,uniqueExpressions],
			makeRepIndexList[DiracIndex,dihead,Unique["di"],fu,finalList,uniqueExpressions]
		};


		If [ repIndexListsCustom =!={},
			repIndexListTotal = Join[repIndexListTotal,repIndexListsCustom];
		];

		repIndexListTotal = Flatten/@ Transpose[repIndexListTotal];

		FCPrint[2,"FCCanonicalizeDummyIndices: List of replacements: ", repIndexListTotal, FCDoControl->canodummyVerbose];


		(* Renaming of dummy indices according to the supplied list *)
		cList = {
			{OptionValue[LorentzIndexNames],lihead},
			{OptionValue[CartesianIndexNames],cihead},
			{OptionValue[SUNIndexNames],sunhead},
			{OptionValue[SUNFIndexNames],sunfhead},
			{OptionValue[DiracIndexNames],dihead}
		};
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

		finalRepList = Thread[Rule[uniqueExpressions, canIndexList]];

		FCPrint[3,"FCCanonicalizeDummyIndices: canIndexList: ", finalRepList, FCDoControl->canodummyVerbose];

		res = (rest0+tmp) /.finalRepList /. isoHead|lihead|cihead|sunhead|sunfhead|dihead->Identity /. times -> Times;

		If[OptionValue[FCE],
			res = FCE[res]
		];

		res
	]

FCPrint[1,"FCCanonicalizeDummyIndices.m loaded."];
End[]
