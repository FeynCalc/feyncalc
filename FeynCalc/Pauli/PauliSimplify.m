(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PauliSimplify													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: 	Like PauliTrick, but including non-commutative expansions
				and simplifications of spinor chains						*)

(* ------------------------------------------------------------------------ *)

PauliSimplify::usage =
"PauliSimplify[exp] simplifies products of Pauli matrices and expands
non-commutative products. Double indices and vectors are contracted. The order
of the Pauli matrices is not changed.";

PauliSimplify::failmsg =
"Error! PauliSimplify encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`PauliSimplify`Private`"]

psVerbose::usage="";
optInsidePauliTrace::usage="";
optExpanding::usage="";
optExpandScalarProduct::usage="";
optPauliSigmaCombine::usage="";
optPauliOrder::usage="";
optFactoring::usage="";
optEpsContract::usage="";
optContract::usage="";
optPauliReduce::usage="";

Options[PauliSimplify] = {
	Contract			-> True,
	EpsContract			-> True,
	Expand2				-> True,
	ExpandScalarProduct	-> True,
	Expanding			-> True,
	FCCheckSyntax		-> False,
	FCE					-> False,
	FCI    				-> False,
	FCPauliIsolate		-> True,
	FCVerbose			-> False,
	Factoring			-> False,
	InsidePauliTrace    -> False,
	PauliChain			-> True,
	PauliChainJoin		-> True,
	PauliOrder			-> False,
	PauliReduce 		-> False,
	PauliSigmaCombine	-> False,
	PauliTrace			-> True,
	PauliTraceEvaluate	-> True
};

PauliSimplify[a_ == b_, opts:OptionsPattern[]] :=
	PauliSimplify[a,opts] == PauliSimplify[b,opts];

PauliSimplify[expr_List, opts:OptionsPattern[]] :=
	PauliSimplify[#, opts]&/@expr;

PauliSimplify[expr_, OptionsPattern[]] :=
	Block[{ex,res,time, null1, null2, holdDOT, freePart=0, psPart, pauliObjects,
			pauliObjectsEval, repRule, tmp, tmpHead},

		If [OptionValue[FCVerbose]===False,
			psVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				psVerbose=OptionValue[FCVerbose]
			];
		];

		optContract				= OptionValue[Contract];
		optPauliSigmaCombine	= OptionValue[PauliSigmaCombine];
		optPauliOrder			= OptionValue[PauliOrder];
		optEpsContract			= OptionValue[EpsContract];
		optExpandScalarProduct	= OptionValue[ExpandScalarProduct];
		optExpanding  			= OptionValue[Expanding];
		optPauliReduce			= OptionValue[PauliReduce];
		optInsidePauliTrace		= OptionValue[InsidePauliTrace];


		If[ OptionValue[Factoring] === Automatic,
			optFactoring =
				Function[x, If[ LeafCount[x] <  5000,
								Factor[x],
								x
							]
				],
			optFactoring = OptionValue[Factoring]
		];

		FCPrint[1, "PauliSimplify: Entering.", FCDoControl->psVerbose];
		FCPrint[3, "PauliSimplify: Entering with ", expr, FCDoControl->psVerbose];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	OptionValue[FCCheckSyntax],
			time=AbsoluteTime[];
			FCPrint[1, "PauliSimplify: Checking the syntax", FCDoControl->psVerbose];
			FCCheckSyntax[ex,FCI->True];
			FCPrint[1, "PauliSimplify: Checking the syntax done", FCDoControl->psVerbose];
			FCPrint[1,"PauliSimplify: Done checking the syntax, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->psVerbose]
		];

		If[ FreeQ2[ex,PauliHeadsList],
			Return[ex]
		];

		(* Here we separately simplify each chain of Pauli matrices	*)
		If[	OptionValue[FCPauliIsolate],
			(*	This is the standard mode for calling PauliSimplify	*)
			FCPrint[1,"PauliSimplify: Normal mode.", FCDoControl->psVerbose];
			time=AbsoluteTime[];
			FCPrint[1, "PauliSimplify: Extracting Pauli objects.", FCDoControl->psVerbose];
			(* 	First of all we need to extract all the Pauli structures in the input. *)
			ex = FCPauliIsolate[ex,FCI->True,Head->psHead, DotSimplify->True, PauliSigmaCombine->OptionValue[PauliSigmaCombine],
				LorentzIndex->True, CartesianIndex->True, PauliChain->OptionValue[PauliChain]];


			If[	!FreeQ[ex,PauliTrace] && !OptionValue[PauliTrace],
				ex = ex /. psHead[zz_]/; !FreeQ[zz,PauliTrace] :> zz
			];


			{freePart,psPart} = FCSplit[ex,{psHead}];
			FCPrint[3,"PauliSimplify: psPart: ",psPart , FCDoControl->psVerbose];
			FCPrint[3,"PauliSimplify: freePart: ",freePart , FCDoControl->psVerbose];
			FCPrint[1, "PauliSimplify: Done extracting Pauli objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->psVerbose];

			pauliObjects = Cases[psPart+null1+null2, psHead[_], Infinity]//Sort//DeleteDuplicates;
			pauliObjectsEval = pauliObjects;
			FCPrint[3,"PauliSimplify: pauliObjects: ", pauliObjects , FCDoControl->psVerbose];


			If[ OptionValue[PauliTraceEvaluate],
				time=AbsoluteTime[];
				FCPrint[1, "PauliSimplify: Calculating Pauli traces.", FCDoControl->psVerbose];
				pauliObjectsEval = pauliObjectsEval /. PauliTrace[zz_, opts:OptionsPattern[]] :> PauliTrace[zz,
					PauliTraceEvaluate->True, Expand-> optExpandScalarProduct, opts];
				FCPrint[1, "PauliSimplify: Done calculating Pauli traces, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->psVerbose];
				FCPrint[3,"PauliSimplify: pauliObjects after calcuating Pauli traces: ", pauliObjects , FCDoControl->psVerbose]
			];

			If[ OptionValue[PauliChainJoin] && !FreeQ[pauliObjectsEval,PauliChain],
				time=AbsoluteTime[];
				FCPrint[1, "PauliSimplify: Contracting Pauli indices.", FCDoControl->psVerbose];
				pauliObjectsEval = pauliObjectsEval /. psHead[x_]/;!FreeQ[x,PauliChain] :>
					PauliChainJoin[x, FCI->True, FCPauliIsolate->False];
				FCPrint[1, "PauliSimplify: Done contracting Pauli indices, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->psVerbose];
				FCPrint[3, "PauliSimplify: pauliObjectsEval after contracting Pauli indices: ", pauliObjects , FCDoControl->psVerbose]
			];


			time=AbsoluteTime[];
			FCPrint[1, "PauliSimplify: Applying pauliSimplifyEval", FCDoControl->psVerbose];

			pauliObjectsEval = FeynCalc`Package`pauliTrickEvalFastFromPauliSimplifyList[(pauliObjectsEval/.psHead->Identity),
				{optInsidePauliTrace,optPauliOrder}];

			pauliObjectsEval = pauliSimplifyEval/@pauliObjectsEval;

			FCPrint[3,"PauliSimplify: After pauliSimplifyEval: ", pauliObjectsEval, FCDoControl->psVerbose];
			FCPrint[1,"PauliSimplify: pauliSimplifyEval done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->psVerbose];

			If[ !FreeQ2[pauliObjectsEval,{FeynCalc`Package`pauliTrickEvalFastFromPauliSimplifyList,pauliSimplifyEval,holdDOT}],
				Message[PauliSimplify::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];

			FCPrint[1, "PauliSimplify: Inserting Pauli objects back.", FCDoControl->psVerbose];
			time=AbsoluteTime[];
			repRule = Thread[Rule[pauliObjects,pauliObjectsEval]];
			FCPrint[3,"PauliSimplify: repRule: ",repRule , FCDoControl->psVerbose];
			tmp =  (psPart /. Dispatch[repRule]);
			FCPrint[1, "PauliSimplify: Done inserting Pauli objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->psVerbose],

			(* 	This is a fast mode for input that is already isolated, e.g. for calling PauliSimplify/@exprList
				from internal functions	*)
			FCPrint[1,"PauliSimplify: Fast mode.", FCDoControl->psVerbose];

			tmp = FeynCalc`Package`pauliTrickEvalFastFromPauliSimplifySingle[ex, {tmpHead,False, optInsidePauliTrace,optPauliOrder}];

			(* It might happen that after pauliTrickEvalFast there are no Pauli matrices left.*)

			FCPrint[3,"PauliSimplify: After pauliTrickEvalFast: ", tmp , FCDoControl->psVerbose];

			If[ !FreeQ2[tmp,{PauliHeadsList,tmpHead}],
				tmp = tmp /. tmpHead -> pauliSimplifyEval
			];

			If[ !FreeQ2[tmp,{FeynCalc`Package`pauliTrickEvalFastFromPauliSimplifySingle,pauliSimplifyEval,holdDOT}],
				Message[PauliSimplify::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			]
		];

		FCPrint[3, "PauliSimplify: Intermediate result: ", tmp, FCDoControl->psVerbose];

		res = freePart + tmp;

		If[	optExpanding && OptionValue[Expand2],
			time=AbsoluteTime[];
			FCPrint[1, "PauliSimplify: Expanding the result.", FCDoControl->psVerbose];
			res = Expand2[res];
			FCPrint[1,"PauliSimplify: Expanding done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->psVerbose];
			FCPrint[3, "PauliSimplify: After expanding: ", res, FCDoControl->psVerbose]
		];


		If[ OptionValue[FCE],
			res = FCE[res]
		];

		(*res = ex;*)
		FCPrint[1,"PauliSimplify: Leaving.", FCDoControl->psVerbose];
		FCPrint[3,"PauliSimplify: Leaving with ", res, FCDoControl->psVerbose];
		res
	];


(*TODO Figure out some way to treat spinors as well! *)
pauliSimplifyEval[PauliChain[expr_,i_,j_]]:=
	PauliChain[pauliSimplifyEval[expr],i,j]/; !optExpanding

pauliSimplifyEval[PauliChain[expr_,i_,j_]]:=
	PauliChainExpand[PauliChain[pauliSimplifyEval[expr],i,j], FCI->True]/; optExpanding


pauliSimplifyEval[expr_]:=
	Block[{tmp=expr, time, time2, res},

		(*	General algorithm of pauliSimplifyEval:

			1)	Apply PauliTrick to the unexpanded expression
			2)	Expand the expression using DotSimplify and apply PauliTrick again
			3)	If there are uncontracted indices, contract them
			4)	If needed, expand scalar products

			5)	If needed, order the remaining Pauli matrices canonically
			6)	If needed, factor the result

		*)



		FCPrint[1, "PauliSimplify: pauliSimplifyEval: Entering", FCDoControl->psVerbose];
		FCPrint[3, "PauliSimplify: pauliSimplifyEval: Entering with: ", tmp, FCDoControl->psVerbose];



		(* First application of PauliTrick, no expansions so far *)
		If[ !FreeQ[tmp, PauliSigma],
			time=AbsoluteTime[];
			FCPrint[1,"PauliSimplify: pauliSimplifyEval: Applying PauliTrick.", FCDoControl->psVerbose];
			tmp = PauliTrick[tmp, FCI -> True, InsidePauliTrace-> optInsidePauliTrace, FCPauliIsolate->False, PauliReduce->optPauliReduce];
			FCPrint[1,"PauliSimplify: pauliSimplifyEval: PauliTrick done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->psVerbose];
			FCPrint[3,"PauliSimplify: pauliSimplifyEval: After PauliTrick: ", tmp, FCDoControl->psVerbose]
		];

		(*	Expansion of Pauli slashes	*)
		If[	!optPauliSigmaCombine && !FreeQ[tmp, PauliSigma],
			tmp = PauliSigmaExpand[tmp,FCI->True];
		];


		If[	optExpanding && !FreeQ[tmp, PauliSigma],
				time2=AbsoluteTime[];
				FCPrint[1,"PauliSimplify: pauliSimplifyEval: Applying Dotsimplify.", FCDoControl->psVerbose];
				tmp = DotSimplify[tmp, FCI->True, Expanding -> True, FCJoinDOTs->False];
				FCPrint[1,"PauliSimplify: pauliSimplifyEval: Dotsimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->psVerbose];
				FCPrint[3,"PauliSimplify: pauliSimplifyEval: After Dotsimplify: ", tmp, FCDoControl->psVerbose];
		];

		If[ !FreeQ[tmp, PauliSigma],

			If[	optContract=!=False && !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],
				time2=AbsoluteTime[];
				FCPrint[1, "PauliSimplify: pauliSimplifyEval: Applying Contract.", FCDoControl->psVerbose];
				tmp = Contract[tmp, Expanding->True, EpsContract-> False, Factoring->False];
				FCPrint[1, "PauliSimplify: pauliSimplifyEval: Contract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->psVerbose];
				FCPrint[3, "PauliSimplify: pauliSimplifyEval: After Contract: ", tmp, FCDoControl->psVerbose];

				time2=AbsoluteTime[];
				FCPrint[1, "PauliSimplify: pauliSimplifyEval: Applying EpsContract.", FCDoControl->psVerbose];
				If[	optEpsContract,
					tmp = EpsContract[tmp,FCI->True]
				];
				FCPrint[1, "PauliSimplify: pauliSimplifyEval: EpsContract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->psVerbose];
				FCPrint[3, "PauliSimplify: pauliSimplifyEval: After EpsContract: ", tmp, FCDoControl->psVerbose];


			];

			time2=AbsoluteTime[];
			FCPrint[1,"PauliSimplify: pauliSimplifyEval: Applying PauliTrick.", FCDoControl->psVerbose];
			tmp = PauliTrick[tmp, FCI -> True, InsidePauliTrace-> optInsidePauliTrace, FCJoinDOTs->False, PauliReduce->optPauliReduce];
			FCPrint[1,"PauliSimplify: pauliSimplifyEval: PauliTrick done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->psVerbose];
			FCPrint[3,"PauliSimplify: pauliSimplifyEval: After PauliTrick: ", tmp, FCDoControl->psVerbose];
		];

		(* Doing index contractions *)
		If[	optContract=!=False && !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],
			time2=AbsoluteTime[];
			FCPrint[1, "PauliSimplify: pauliSimplifyEval: Applying Contract.", FCDoControl->psVerbose];
			tmp = Contract[tmp, EpsContract-> False];
			FCPrint[1, "PauliSimplify: pauliSimplifyEval: Contract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->psVerbose];

			time2=AbsoluteTime[];
			FCPrint[1, "PauliSimplify: pauliSimplifyEval: Applying EpsContract.", FCDoControl->psVerbose];
				If[	optEpsContract,
					tmp = EpsContract[tmp,FCI->True]
				];
			FCPrint[1, "PauliSimplify: pauliSimplifyEval: EpsContract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->psVerbose];
			FCPrint[3, "PauliSimplify: pauliSimplifyEval: After EpsContract: ", tmp, FCDoControl->psVerbose];
		];

		(* 	Expansion of the scalar products.	*)
		If[ optExpandScalarProduct && !FreeQ[tmp,Momentum],
			time2=AbsoluteTime[];
			FCPrint[1,"PauliSimplify: pauliSimplifyEval: Expanding scalar products", FCDoControl->psVerbose];
			tmp = ExpandScalarProduct[tmp,FCI->False];
			FCPrint[1,"PauliSimplify:pauliSimplifyEvale: Done expanding the result, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->psVerbose]
		];

		(* 	Canonical ordering of the Pauli matrices.	*)
		If[ optPauliOrder,
				time2=AbsoluteTime[];
				FCPrint[1,"PauliSimplify: pauliSimplifyEval: Applying PauliOrder.", FCDoControl->psVerbose];
				tmp = PauliOrder[tmp, FCI->True, FCJoinDOTs->OptionValue[Expanding]];
				FCPrint[1,"PauliSimplify:pauliSimplifyEvale: Done applying PauliOrder, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->psVerbose]
		];

		If[	optExpanding && (!optPauliSigmaCombine),
			time2=AbsoluteTime[];
			FCPrint[1,"PauliSimplify: pauliSimplifyEval: Applying Dotsimplify.", FCDoControl->psVerbose];
			tmp = DotSimplify[tmp, FCI->True, Expanding -> True];
			FCPrint[1,"PauliSimplify: pauliSimplifyEval: Dotsimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->psVerbose];
			FCPrint[3,"PauliSimplify: pauliSimplifyEval: After Dotsimplify: ", tmp, FCDoControl->psVerbose];
		];

		(* Factoring	*)
		If[ optFactoring=!=False,
				time2=AbsoluteTime[];
				FCPrint[1,"PauliSimplify: pauliSimplifyEval: Factoring the result.", FCDoControl->psVerbose];
				tmp = optFactoring[tmp];
				FCPrint[1,"PauliSimplify:pauliSimplifyEvale: Done factoring, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->psVerbose]
		];

		res = tmp;

		FCPrint[3,"PauliSimplify: pauliSimplifyEval: Leaving with: ", res, FCDoControl->psVerbose];

		res


	]/;Head[expr]=!=PauliChain;

FCPrint[1,"PauliSimplify.m loaded."];
End[]
