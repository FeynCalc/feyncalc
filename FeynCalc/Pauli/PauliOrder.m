(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliOrder														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Lexicographic ordering of Pauli matrices						*)

(* ------------------------------------------------------------------------ *)

PauliOrder::usage =
"PauliOrder[exp] orders the Pauli matrices in expr alphabetically.

PauliOrder[exp, orderlist] orders the Pauli matrices in expr according to
orderlist.";

PauliOrder::failmsg =
"Error! PauliOrder has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliOrder`Private`"];

pauliReduce::usage="";
poVerbose::usage="";
holdDOT::usage="";
tmp::usage="";

Options[PauliOrder] = {
	FCE 				-> False,
	FCI 				-> False,
	FCJoinDOTs 			-> True,
	FCPauliIsolate		-> True,
	FCVerbose			-> False,
	MaxIterations		-> Infinity,
	PauliReduce			-> False,
	PauliSigmaCombine	-> False,
	PauliTrick 			-> True
};

PauliOrder[expr_, (opts:OptionsPattern[])/;opts=!={}] :=
	PauliOrder[expr, {}, opts];

PauliOrder[expr_, orderList_List/; (!OptionQ[orderList] || orderList==={}), OptionsPattern[]]:=
	Block[{ex,res,dsHead,dsPart,freePart,null1,null2,pauliObjects,tmp, maxIterations, pauliObjectsEval, repRule,time},

		maxIterations = OptionValue[MaxIterations];

		pauliReduce = OptionValue[PauliReduce];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[ FreeQ2[ex,PauliHeadsList],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			poVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				poVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "PauliOrder. Entering.", FCDoControl->poVerbose];
		FCPrint[3, "PauliOrder: Entering with ", ex, FCDoControl->poVerbose];

		If[	OptionValue[FCPauliIsolate],
			(* This is the normal mode which works well both for large and small expressions *)
			FCPrint[1, "PauliOrder: Normal mode.", FCDoControl->poVerbose];
			time=AbsoluteTime[];
			FCPrint[1, "PauliOrder: Extracting Pauli objects.", FCDoControl->poVerbose];
			ex = FCPauliIsolate[ex,FCI->True,Head->dsHead, DotSimplify->True, PauliSigmaCombine->OptionValue[PauliSigmaCombine],
				FCJoinDOTs->OptionValue[FCJoinDOTs]];


			{freePart,dsPart} = FCSplit[ex,{dsHead}];
			FCPrint[3,"PauliOrder: dsPart: ",dsPart , FCDoControl->poVerbose];
			FCPrint[3,"PauliOrder: freePart: ",freePart , FCDoControl->poVerbose];

			pauliObjects = Cases[dsPart+null1+null2, dsHead[_], Infinity]//DeleteDuplicates//Sort;
			FCPrint[1, "PauliOrder: Done extracting Pauli objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->poVerbose];

			time=AbsoluteTime[];
			If[orderList=!={},
				FCPrint[1, "PauliOrder: Ordering according to: ", orderList, FCDoControl->poVerbose];
				pauliObjectsEval = Map[pauliOrderCustom[#,orderList]&, (pauliObjects/. DOT -> holdDOT/.dsHead->Identity)];
				FCPrint[1, "PauliOrder: Ordering done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->poVerbose],

				FCPrint[1, "PauliOrder. Using lexicographic ordering.", FCDoControl->poVerbose];
				pauliObjectsEval = Map[pauliOrderLex[#,maxIterations]&, (pauliObjects/. DOT -> holdDOT/.dsHead->Identity)];
				FCPrint[1, "PauliOrder: Ordering done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->poVerbose]
			];

			time=AbsoluteTime[];
			FCPrint[1, "PauliOrder: Inserting Pauli objects back.", FCDoControl->poVerbose];

			pauliObjectsEval = pauliObjectsEval /. holdDOT[]->1 /.holdDOT->DOT /. CartesianPairContract -> CartesianPair;
			If[	!FreeQ[pauliObjectsEval,Pair],
				pauliObjectsEval = pauliObjectsEval/. Pair->PairContract /. PairContract->Pair
			];

			repRule = Thread[Rule[pauliObjects,pauliObjectsEval]];
			FCPrint[3,"PauliOrder: repRule: ",repRule , FCDoControl->poVerbose];
			tmp = freePart + (dsPart /. Dispatch[repRule]);
			FCPrint[1, "PauliOrder: Done inserting Pauli objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->poVerbose];
			FCPrint[3,"PauliOrder: Intermediate result: ", tmp, FCDoControl->poVerbose],

			(* This is the fast mode for standalone Pauli chains *)
			FCPrint[1, "PauliOrder: Fast mode.", FCDoControl->poVerbose];
			time=AbsoluteTime[];
			tmp = ex /. DOT -> holdDOT;

			If[orderList=!={},
				FCPrint[1, "PauliOrder: Ordering according to: ", orderList, FCDoControl->poVerbose];
				tmp = pauliOrderCustom[tmp, orderList];
				FCPrint[1, "PauliOrder: Ordering done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->poVerbose],

				FCPrint[1, "PauliOrder. Using lexicographic ordering.", FCDoControl->poVerbose];
				tmp = pauliOrderLex[tmp, maxIterations];
				FCPrint[1, "PauliOrder: Ordering done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->poVerbose]

			];
			tmp = tmp/. holdDOT[]->1 /.holdDOT->DOT /. CartesianPairContract->CartesianPair;
			If[	!FreeQ[pauliObjectsEval,Pair],
				pauliObjectsEval = pauliObjectsEval/. Pair->PairContract /. PairContract->Pair
			];

		];


		res = tmp;

		If[	OptionValue[PauliTrick],
				time=AbsoluteTime[];
				FCPrint[1, "PauliOrder: Applying PauliTrick.", FCDoControl->poVerbose];
				res = PauliTrick[res, FCI->True, PauliReduce->pauliReduce];
				FCPrint[1, "PauliOrder: Done applying PauliTrick,timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->poVerbose]
		];

		FCPrint[1, "PauliOrder: Leaving.", FCDoControl->poVerbose];
		FCPrint[3, "PauliOrder: Leaving with ", res, FCDoControl->poVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		res

	];

pauliOrderLex[x_, maxIterations_]:=
	FixedPoint[(# /. {
		holdDOT[a___,PauliSigma[(h1:CartesianIndex|CartesianMomentum)[ar1__], dim1_:3],PauliSigma[(h2:CartesianIndex|CartesianMomentum)[ar2__], dim2_:3],b___]/;
			!OrderedQ[{h1[First[{ar1}],dim1],h2[First[{ar2}],dim2]}] && h1[First[{ar1}],dim1]=!=h2[First[{ar2}],dim2] :>
			-holdDOT[a, PauliSigma[h2[ar2],dim2], PauliSigma[h1[ar1],dim1] ,b] +
			2 CartesianPairContract[h1[ar1],h2[ar2]] holdDOT[a,b],

		holdDOT[a___,PauliSigma[(h:CartesianIndex|CartesianMomentum|ExplicitLorentzIndex)[ar___], dim_:3],
			PauliSigma[(h:CartesianIndex|CartesianMomentum|ExplicitLorentzIndex)[ar___], dim_:3],b___] :>
			holdDOT[a,b] PauliTrick[DOT[PauliSigma[h[ar],dim].PauliSigma[h[ar],dim]],FCI->True,FCPauliIsolate->False, PauliReduce->pauliReduce]


	})&, x, maxIterations]

customOrdering[x_, currentElement_]:=
	x //. {
		holdDOT[a___,PauliSigma[(h1:CartesianIndex|CartesianMomentum)[ar1__], dim1_:3],PauliSigma[(h2:CartesianIndex|CartesianMomentum)[ar2__], dim2_:3],b___]/;
			!FreeQ[h2[First[{ar2}],dim2],currentElement] && h1[First[{ar1}],dim1]=!=h2[First[{ar2}],dim2] :>
			-holdDOT[a, PauliSigma[h2[ar2],dim2], PauliSigma[h1[ar1],dim1] ,b] +
			2 CartesianPairContract[h1[ar1],h2[ar2]] holdDOT[a,b],

		holdDOT[a___,PauliSigma[(h:CartesianIndex|CartesianMomentum|ExplicitLorentzIndex)[ar___], dim_:3],
			PauliSigma[(h:CartesianIndex|CartesianMomentum|ExplicitLorentzIndex)[ar___], dim_:3],b___] :>
			holdDOT[a,b] PauliTrick[DOT[PauliSigma[h[ar],dim].PauliSigma[h[ar],dim]],FCI->True,FCPauliIsolate->False, PauliReduce->pauliReduce]
};



pauliOrderCustom[x_, orderList_List]:=
	(
	tmp=x;
	Scan[(tmp = customOrdering[tmp,#])&, Reverse[orderList]];
	tmp
	);




FCPrint[1,"PauliOrder.m loaded."];
End[]
