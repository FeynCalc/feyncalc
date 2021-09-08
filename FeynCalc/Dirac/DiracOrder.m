(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracOrder														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Canonical ordering of Dirac matrices							*)

(* ------------------------------------------------------------------------ *)

DiracOrder::usage =
"DiracOrder[exp] orders the Dirac matrices in exp lexicographically.
DiracOrder[exp, orderlist] orders the Dirac matrices in exp according to
orderlist. DiracOrder is also an option of DiracSimplify and some other
functions dealing with Dirac algebra. If set to True, the function DiracOrder
will be applied to the intermediate result to reorder the Dirac matrices
lexicographically.";

DiracOrder::failmsg =
"Error! DiracOrder has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracOrder`Private`"];

doVerbose::usage="";
holdDOT::usage="";
tmp::usage="";

Options[DiracOrder] = {
	DiracGammaCombine	-> False,
	DiracTrick 			-> True,
	DotSimplify			-> True,
	FCDiracIsolate		-> True,
	FCE					-> False,
	FCI					-> False,
	FCJoinDOTs			-> True,
	FCVerbose			-> False,
	MaxIterations		-> Infinity
};

DiracOrder[a_ == b_, rest___] :=
	DiracOrder[a,rest] == DiracOrder[b,rest];

DiracOrder[expr_List, rest___]:=
	DiracOrder[#, rest]&/@expr;

DiracOrder[expr_, (opts:OptionsPattern[])/;opts=!={}] :=
	DiracOrder[expr, {}, opts];

DiracOrder[expr_/; !MemberQ[{List,Equal},expr], orderList_List/; (!OptionQ[orderList] || orderList==={}), OptionsPattern[]]:=
	Block[{	ex,res,dsHead,dsPart,freePart,null1,null2,diracObjects,
			tmp, maxIterations, diracObjectsEval, repRule,time},

		maxIterations = OptionValue[MaxIterations];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[ FreeQ2[ex,DiracHeadsList],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			doVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				doVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "DiracOrder. Entering.", FCDoControl->doVerbose];
		FCPrint[3, "DiracOrder: Entering with ", ex, FCDoControl->doVerbose];

		If[	OptionValue[FCDiracIsolate],

			(* This is the normal mode which works well both for large and small expressions *)
			FCPrint[1, "DiracOrder: Normal mode.", FCDoControl->doVerbose];
			time=AbsoluteTime[];
			FCPrint[1, "DiracOrder: Extracting Dirac objects.", FCDoControl->doVerbose];
			ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->True, DiracChain->True,
				DiracGammaCombine->OptionValue[DiracGammaCombine], FCJoinDOTs->OptionValue[FCJoinDOTs], Split->False,
				LorentzIndex->True, CartesianIndex->True];


			{freePart,dsPart} = FCSplit[ex,{dsHead}];
			FCPrint[3,"DiracOrder: dsPart: ",dsPart , FCDoControl->doVerbose];
			FCPrint[3,"DiracOrder: freePart: ",freePart , FCDoControl->doVerbose];

			diracObjects = Cases[dsPart+null1+null2, dsHead[_], Infinity]//DeleteDuplicates//Sort;
			FCPrint[1, "DiracOrder: Done extracting Dirac objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose];

			diracObjectsEval = diracObjects;

			If[	OptionValue[DiracTrick]===True,
				time=AbsoluteTime[];
				FCPrint[1, "DiracOrder: Applying DiracTrick.", FCDoControl->doVerbose];
				diracObjectsEval = DiracTrick[#,FCI->True]&/@diracObjectsEval;
				diracObjectsEval =FeynCalc`Package`diracChainContract /@ diracObjectsEval;
				FCPrint[1, "DiracOrder: Done applying DiracTrick,timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose];
				FCPrint[3, "DiracOrder: After DiracTrick: ", diracObjectsEval, FCDoControl->doVerbose];
			];

			time=AbsoluteTime[];
			If[orderList=!={},
				FCPrint[1, "DiracOrder: Ordering according to: ", orderList, FCDoControl->doVerbose];
				diracObjectsEval = Map[diracOrderCustom[#,orderList]&, (diracObjectsEval/. DOT -> holdDOT/.dsHead->Identity)];
				FCPrint[1, "DiracOrder: Ordering done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose],

				FCPrint[1, "DiracOrder. Using canonical ordering.", FCDoControl->doVerbose];
				diracObjectsEval = Map[diracOrderLex[#,maxIterations]&, (diracObjectsEval/. DOT -> holdDOT/.dsHead->Identity)];
				FCPrint[1, "DiracOrder: Ordering done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose]
			];

			FCPrint[3, "DiracOrder: Intermediate result: ", diracObjectsEval, FCDoControl->doVerbose];


			time=AbsoluteTime[];
			FCPrint[1, "DiracOrder: Inserting Dirac objects back.", FCDoControl->doVerbose];
			diracObjectsEval = diracObjectsEval /. holdDOT[]->1 /.holdDOT->DOT /. PairContract->Pair;

			If[	OptionValue[DotSimplify],
				time=AbsoluteTime[];
				FCPrint[1, "DiracOrder: Applying DotSimplify.", FCDoControl->doVerbose];
				diracObjectsEval = DotSimplify[#,FCI->True, Expanding->True]&/@diracObjectsEval;
				FCPrint[1, "DiracOrder: Done applying DotSimplify,timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose];
				FCPrint[3, "DiracOrder: After DotSimplify: ", diracObjectsEval, FCDoControl->doVerbose];
			];


			If[	(OptionValue[DiracTrick]===True || OptionValue[DiracTrick]===Last)  && FreeQ[orderList,ExplicitLorentzIndex[0]],
				time=AbsoluteTime[];
				FCPrint[1, "DiracOrder: Applying DiracTrick.", FCDoControl->doVerbose];
				diracObjectsEval = DiracTrick[#,FCI->True, FCDiracIsolate -> {DotSimplify -> False, Expanding -> False,
					FCJoinDOTs -> False, DiracGammaCombine -> False, Factoring -> False}]&/@diracObjectsEval;
				diracObjectsEval =FeynCalc`Package`diracChainContract /@ diracObjectsEval;
				FCPrint[1, "DiracOrder: Done applying DiracTrick,timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose];
				FCPrint[3, "DiracOrder: After DiracTrick: ", diracObjectsEval, FCDoControl->doVerbose];
			];

			repRule = Thread[Rule[diracObjects, diracObjectsEval]];

			FCPrint[3,"DiracOrder: repRule: ",repRule , FCDoControl->doVerbose];
			tmp = freePart + (dsPart/. Dispatch[repRule]);
			FCPrint[1, "DiracOrder: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose];
			FCPrint[3,"DiracOrder: Intermediate result: ", tmp, FCDoControl->doVerbose],

			(* This is the fast mode for standalone Dirac chains *)
			FCPrint[1, "DiracOrder: Fast mode.", FCDoControl->doVerbose];
			time=AbsoluteTime[];
			tmp = ex /. DOT -> holdDOT;

			If[orderList=!={},
				FCPrint[1, "DiracOrder: Ordering according to: ", orderList, FCDoControl->doVerbose];
				tmp = diracOrderCustom[tmp, orderList];
				FCPrint[1, "DiracOrder: Ordering done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose],

				FCPrint[1, "DiracOrder. Using lexicographic ordering.", FCDoControl->doVerbose];
				tmp = diracOrderLex[tmp, maxIterations];
				FCPrint[1, "DiracOrder: Ordering done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose]

			];
			tmp = tmp/. holdDOT[]->1 /.holdDOT->DOT /. PairContract->Pair;
			tmp = FeynCalc`Package`diracChainContract[tmp];

			If[	OptionValue[DiracTrick] && FreeQ[orderList,ExplicitLorentzIndex[0]],
				time=AbsoluteTime[];
				FCPrint[1, "DiracOrder: Applying DiracTrick.", FCDoControl->doVerbose];
				tmp = DiracTrick[tmp,FCI->True];
				tmp = FeynCalc`Package`diracChainContract[tmp];
				FCPrint[1, "DiracOrder: Done applying DiracTrick,timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->doVerbose]
			]

		];


		res = tmp;

		FCPrint[1, "DiracOrder: Leaving.", FCDoControl->doVerbose];
		FCPrint[3, "DiracOrder: Leaving with ", res, FCDoControl->doVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

diracOrderLex[x_, maxIterations_]:=
	FixedPoint[(FeynCalc`Package`diracChainContract[#] /. {
		holdDOT[a___,DiracGamma[(h1:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[ar1__], dim1_:4],DiracGamma[(h2:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[ar2__], dim2_:4],b___]/;
			!OrderedQ[{h1[First[{ar1}],dim1],h2[First[{ar2}],dim2]}] && h1[First[{ar1}],dim1]=!=h2[First[{ar2}],dim2] :>
			-holdDOT[a, DiracGamma[h2[ar2],dim2], DiracGamma[h1[ar1],dim1] ,b] +
			2 PairContract[h1[ar1],h2[ar2]] holdDOT[a,b],

		(* The g^0 matrices are moved to the very right *)
		holdDOT[a___,DiracGamma[ExplicitLorentzIndex[0]],DiracGamma[(h2:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[ar2__], dim2_:4],b___] :>
			-holdDOT[a, DiracGamma[h2[ar2],dim2], DiracGamma[ExplicitLorentzIndex[0]] ,b] +
			2 PairContract[ExplicitLorentzIndex[0],h2[ar2]] holdDOT[a,b],

		holdDOT[a___,DiracGamma[(h:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum|ExplicitLorentzIndex)[ar___], dim_:4],
			DiracGamma[(h:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum|ExplicitLorentzIndex)[ar___], dim_:4],b___] :>
			holdDOT[a,b] DiracTrick[DOT[DiracGamma[h[ar],dim].DiracGamma[h[ar],dim]],FCI->True,FCDiracIsolate->False]


	})&, x, maxIterations]

customOrdering[x_, currentElement_]:=
	FeynCalc`Package`diracChainContract[x] //. {
		holdDOT[a___,DiracGamma[(h1:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[ar1__], dim1_:4],
			DiracGamma[(h2:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[ar2__], dim2_:4],b___]/;
			!FreeQ[h2[First[{ar2}],dim2],currentElement] && h1[First[{ar1}],dim1]=!=h2[First[{ar2}],dim2] :>
			-holdDOT[a, DiracGamma[h2[ar2],dim2], DiracGamma[h1[ar1],dim1] ,b] +
			2 PairContract[h1[ar1],h2[ar2]] holdDOT[a,b],

		holdDOT[a___,DiracGamma[(h1:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[ar1__], dim1_:4],DiracGamma[ExplicitLorentzIndex[0]],b___]/;
			!FreeQ[ExplicitLorentzIndex[0],currentElement] && h1[First[{ar1}],dim1]=!=ExplicitLorentzIndex[0] :>
			-holdDOT[a, DiracGamma[ExplicitLorentzIndex[0]], DiracGamma[h1[ar1],dim1] ,b] +
			2 PairContract[h1[ar1],ExplicitLorentzIndex[0]] holdDOT[a,b],

		holdDOT[a___,DiracGamma[ExplicitLorentzIndex[0]],DiracGamma[(h2:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum)[ar2__], dim2_:4],b___]/;
			!FreeQ[h2[First[{ar2}],dim2],currentElement] && ExplicitLorentzIndex[0]=!=h2[First[{ar2}],dim2] :>
			-holdDOT[a, DiracGamma[h2[ar2],dim2], DiracGamma[ExplicitLorentzIndex[0]] ,b] +
			2 PairContract[ExplicitLorentzIndex[0],h2[ar2]] holdDOT[a,b],


		holdDOT[a___,DiracGamma[(h:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum|ExplicitLorentzIndex)[ar___], dim_:4],
			DiracGamma[(h:LorentzIndex|Momentum|CartesianIndex|CartesianMomentum|ExplicitLorentzIndex)[ar___], dim_:4],b___] :>
			holdDOT[a,b] DiracTrick[DOT[DiracGamma[h[ar],dim].DiracGamma[h[ar],dim]],FCI->True,FCDiracIsolate->False]
};



diracOrderCustom[x_, orderList_List]:=
	(
	tmp=x;
	Scan[(tmp = customOrdering[tmp,#])&, Reverse[orderList]];
	tmp
	);




FCPrint[1,"DiracOrder.m loaded."];
End[]
