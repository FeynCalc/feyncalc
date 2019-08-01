(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimplify													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary: 	Like DiracTrick, but including non-commutative expansions
				and simplifications of spinor chains						*)

(* ------------------------------------------------------------------------ *)

DiracSimplify::usage =
"DiracSimplify[exp] simplifies products of Dirac matrices \
and expands non-commutative products. \
Double Lorentz indices and four vectors are contracted. \
The Dirac equation is applied. \
All DiracGamma[5], DiracGamma[6] and DiracGamma[7] are moved to \
the right. The order of the Dirac matrices is not changed.";

DiracSimplify::failmsg =
"Error! DiracSimplify encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`DiracSimplify`Private`"]

dsVerbose::usage="";
optInsideDiracTrace::usage="";
optExpanding::usage="";
optExpandScalarProduct::usage="";
optDiracGammaCombine::usage="";
optDiracSubstitute67::usage="";
optDiracSubstitute5::usage="";
optDiracSigmaExplicit::usage="";
optDiracEquation::usage="";
optDiracOrder::usage="";
optFactoring::usage="";
optEpsContract::usage="";
optContract::usage="";
optToDiracGamma67::usage="";

Options[DiracSimplify] = {
	Contract			-> True,
	DiracChain			-> True,
	DiracChainJoin		-> True,
	DiracEquation		-> True,
	DiracGammaCombine	-> False,
	DiracOrder			-> False,
	DiracSigmaExplicit	-> True,
	DiracSubstitute5	-> False,
	DiracSubstitute67	-> False,
	DiracTrace			-> True,
	DiracTraceEvaluate	-> True,
	EpsContract			-> True,
	Expand2				-> True,
	ExpandScalarProduct	-> True,
	Expanding			-> True,
	FCCheckSyntax		-> False,
	FCDiracIsolate		-> True,
	FCE					-> False,
	FCI    				-> False,
	FCVerbose			-> False,
	Factoring			-> False,
	InsideDiracTrace    -> False,
	SirlinSimplify		-> True,
	SpinorChainTrick	-> True,
	ToDiracGamma67		-> True
};

DiracSimplify[expr_, OptionsPattern[]] :=
	Block[{ex,res,time, null1, null2, holdDOT, freePart=0, dsPart, diracObjects,
			diracObjectsEval, repRule, tmp, tmpHead},

		If [OptionValue[FCVerbose]===False,
			dsVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				dsVerbose=OptionValue[FCVerbose]
			];
		];

		optContract				= OptionValue[Contract];
		optDiracEquation		= OptionValue[DiracEquation];
		optDiracGammaCombine	= OptionValue[DiracGammaCombine];
		optDiracOrder			= OptionValue[DiracOrder];
		optDiracSigmaExplicit	= OptionValue[DiracSigmaExplicit];
		optDiracSubstitute5		= OptionValue[DiracSubstitute5];
		optDiracSubstitute67	= OptionValue[DiracSubstitute67];
		optEpsContract			= OptionValue[EpsContract];
		optExpandScalarProduct	= OptionValue[ExpandScalarProduct];
		optExpanding  			= OptionValue[Expanding];
		optInsideDiracTrace		= OptionValue[InsideDiracTrace];
		optToDiracGamma67		= OptionValue[ToDiracGamma67];

		If[ OptionValue[Factoring] === Automatic,
			optFactoring =
				Function[x, If[ LeafCount[x] <  5000,
								Factor[x],
								x
							]
				],
			optFactoring = OptionValue[Factoring]
		];

		FCPrint[1, "DiracSimplify: Entering.", FCDoControl->dsVerbose];
		FCPrint[3, "DiracSimplify: Entering with ", expr, FCDoControl->dsVerbose];

		If[	OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	OptionValue[FCCheckSyntax],
			time=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: Checking the syntax", FCDoControl->dsVerbose];
			FCCheckSyntax[ex,FCI->True];
			FCPrint[1, "DiracSimplify: Checking the syntax done", FCDoControl->dsVerbose];
			FCPrint[1,"DiracSimplify: Done checking the syntax, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose]
		];

		If[ FreeQ2[ex,DiracHeadsList],
			Return[ex]
		];

		(* Here we separately simplify each chain of Dirac matrices	*)
		If[	OptionValue[FCDiracIsolate],
			(*	This is the standard mode for calling DiracSimplify	*)
			FCPrint[1,"DiracSimplify: Normal mode.", FCDoControl->dsVerbose];
			time=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: Extracting Dirac objects.", FCDoControl->dsVerbose];
			(* 	First of all we need to extract all the Dirac structures in the input. *)
			ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->True, DiracGammaCombine->OptionValue[DiracGammaCombine],
				DiracSigmaExplicit->OptionValue[DiracSigmaExplicit], LorentzIndex->True, CartesianIndex->True,
				ToDiracGamma67->optToDiracGamma67, DiracChain->OptionValue[DiracChain]];

			If[	!FreeQ[ex,DiracTrace] && !OptionValue[DiracTrace],
				ex = ex /. dsHead[zz_]/; !FreeQ[zz,DiracTrace] :> zz
			];

			{freePart,dsPart} = FCSplit[ex,{dsHead}];
			FCPrint[3,"DiracSimplify: dsPart: ",dsPart , FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: freePart: ",freePart , FCDoControl->dsVerbose];
			FCPrint[1, "DiracSimplify: Done extracting Dirac objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];

			diracObjects = Cases[dsPart+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;
			diracObjectsEval = diracObjects;
			FCPrint[3,"DiracSimplify: diracObjects: ", diracObjects , FCDoControl->dsVerbose];

			If[ OptionValue[DiracTraceEvaluate],
				time=AbsoluteTime[];
				FCPrint[1, "DiracSimplify: Calculating Dirac traces.", FCDoControl->dsVerbose];
				diracObjectsEval = diracObjectsEval /. DiracTrace[zz_, opts:OptionsPattern[]] :> DiracTrace[zz,
					DiracTraceEvaluate->True, Expand-> optExpandScalarProduct, opts];
				FCPrint[1, "DiracSimplify: Done calculating Dirac traces, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3,"DiracSimplify: diracObjects after calcuating Dirac traces: ", diracObjects , FCDoControl->dsVerbose]

			];

			If[ OptionValue[DiracChainJoin],
				time=AbsoluteTime[];
				FCPrint[1, "DiracSimplify: Contracting Dirac indices.", FCDoControl->dsVerbose];
				diracObjectsEval = diracObjectsEval /. dsHead[x_]/;!FreeQ[x,DiracChain] :>
					DiracChainJoin[x, FCI->True, FCDiracIsolate->False];
				FCPrint[1, "DiracSimplify: Done calculating Dirac traces, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3,"DiracSimplify: diracObjects after calcuating Dirac traces: ", diracObjects , FCDoControl->dsVerbose]
			];

			time=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: Applying diracSimplifyEval", FCDoControl->dsVerbose];

			diracObjectsEval = FeynCalc`Package`diracTrickEvalFastFromDiracSimplifyList[(diracObjectsEval/.dsHead->Identity), {optInsideDiracTrace,optDiracOrder}];

			diracObjectsEval = diracSimplifyEval/@diracObjectsEval;

			FCPrint[3,"DiracSimplify: After diracSimplifyEval: ", diracObjectsEval, FCDoControl->dsVerbose];
			FCPrint[1,"DiracSimplify: diracSimplifyEval done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];

			If[ !FreeQ2[diracObjectsEval,{FeynCalc`Package`diracTrickEvalFastFromDiracSimplifyList,diracSimplifyEval,holdDOT}],
				Message[DiracSimplify::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];

			FCPrint[1, "DiracSimplify: Inserting Dirac objects back.", FCDoControl->dsVerbose];
			time=AbsoluteTime[];
			repRule = Thread[Rule[diracObjects,diracObjectsEval]];
			FCPrint[3,"DiracSimplify: repRule: ", repRule, FCDoControl->dsVerbose];
			tmp =  (dsPart /. Dispatch[repRule]);
			FCPrint[1, "DiracSimplify: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose],

			(*
				This is a fast mode for input that is already isolated, e.g. for calling DiracSimplify/@exprList
				from internal functions
			*)
			FCPrint[1,"DiracSimplify: Fast mode.", FCDoControl->dsVerbose];

			tmp = FeynCalc`Package`diracTrickEvalFastFromDiracSimplifySingle[ex, {tmpHead,optInsideDiracTrace,optDiracOrder}];

			(* It might happen that after diracTrickEvalFast there are no Dirac matrices left.*)

			FCPrint[3,"DiracSimplify: After diracTrickEvalFast: ", tmp , FCDoControl->dsVerbose];

			If[ OptionValue[ToDiracGamma67],
				res = res /. tmpHead[zzz_]/;!FreeQ[{zzz}, DiracGamma[5]] :> tmpHead[ToDiracGamma67[zzz,FCI->True]]
			];

			If[ !FreeQ2[tmp,{DiracHeadsList,tmpHead}],
				tmp = tmp /. tmpHead -> diracSimplifyEval
			];

			If[ !FreeQ2[tmp,{FeynCalc`Package`diracTrickEvalFastFromDiracSimplifySingle,diracSimplifyEval,holdDOT}],
				Message[DiracSimplify::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			]
		];

		FCPrint[3, "DiracSimplify: Intermediate result: ", tmp, FCDoControl->dsVerbose];

		(* To simplify products of spinor chains we need to work with the full expression	*)
		If[	!FreeQ[tmp,Spinor],


			If[	OptionValue[SpinorChainTrick],

				FCPrint[1, "DiracSimplify: Applying SpinorChainTrick.", FCDoControl->dsVerbose];
				time=AbsoluteTime[];
				tmp = SpinorChainTrick[tmp, FCI->True,DiracGammaCombine->optDiracGammaCombine, DiracSigmaExplicit->False];

				FCPrint[1,"DiracSimplify: Done applying SpinorChainTrick, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3, "DiracSimplify: After SpinorChainTrick: ", tmp, FCDoControl->dsVerbose];
			];


			If [ OptionValue[SirlinSimplify] && FCGetDimensions[tmp]==={4},

				FCPrint[1, "DiracSimplify: Applying SirlinSimplify.", FCDoControl->dsVerbose];
				time=AbsoluteTime[];
				tmp = SirlinSimplify[tmp, FCI->True,DiracGammaCombine->optDiracGammaCombine, DiracSigmaExplicit->False];

				FCPrint[1,"DiracSimplify: Done applying SirlinSimplify, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
				FCPrint[3, "DiracSimplify: AfteSirlinSimplify: ", tmp, FCDoControl->dsVerbose]
			]

		];



		res = freePart + tmp;

		If[	optExpanding && OptionValue[Expand2],
			time=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: Expanding the result.", FCDoControl->dsVerbose];
			res = Expand2[res];
			FCPrint[1,"DiracSimplify: Expanding done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3, "DiracSimplify: After expanding: ", res, FCDoControl->dsVerbose]
		];


		If[ OptionValue[FCE],
			res = FCE[res]
		];

		(*res = ex;*)
		FCPrint[1,"DiracSimplify: Leaving.", FCDoControl->dsVerbose];
		FCPrint[3,"DiracSimplify: Leaving with ", res, FCDoControl->dsVerbose];
		res
	];

(*TODO Figure out some way to treat spinors as well! *)
diracSimplifyEval[DiracChain[expr_,i_,j_]]:=
	DiracChain[diracSimplifyEval[expr],i,j]/; !optExpanding

diracSimplifyEval[DiracChain[expr_,i_,j_]]:=
	DiracChainExpand[DiracChain[diracSimplifyEval[expr],i,j], FCI->True]/; optExpanding

diracSimplifyEval[expr_]:=
	Block[{tmp=expr, time, time2, res},

		(*	General algorithm of diracSimplifyEval:

			1)	Apply DiracTrick to the unexpanded expression
			2)	Expand the expression using DotSimplify and apply DiracTrick again
			3)	If there are uncontracted indices, contract them
			4)	If needed, expand scalar products
			5) 	If needed, replace chiral projectors by their explicit values and apply DotSimplify
				afterwards

			6) 	If the expression contains spinors
				6.1) If needed, apply the Dirac equation
				6.2) If needed, apply Sirlin's relations

			7)	If needed, order the remaining Dirac matrices canonically
			8)	If needed, factor the result

		*)



		FCPrint[1, "DiracSimplify: diracSimplifyEval: Entering", FCDoControl->dsVerbose];
		FCPrint[3, "DiracSimplify: diracSimplifyEval: Entering with: ", tmp, FCDoControl->dsVerbose];



		(* First application of DiracTrick, no expansions so far *)
		If[ !FreeQ[tmp, DiracGamma],
			time=AbsoluteTime[];
			FCPrint[1,"DiracSimplify: diracSimplifyEval: Applying DiracTrick.", FCDoControl->dsVerbose];
			tmp = DiracTrick[tmp, FCI -> True, InsideDiracTrace-> optInsideDiracTrace, FCDiracIsolate->False, ToDiracGamma67->optToDiracGamma67];
			FCPrint[1,"DiracSimplify: diracSimplifyEval: DiracTrick done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: diracSimplifyEval: After DiracTrick: ", tmp, FCDoControl->dsVerbose]
		];

		(*	Expansion of Dirac slashes	*)
		If[	!optDiracGammaCombine && !FreeQ[tmp, DiracGamma],
			tmp = DiracGammaExpand[tmp,FCI->True];
		];


		If[	optExpanding && !FreeQ[tmp, DiracGamma],
				time2=AbsoluteTime[];
				FCPrint[1,"DiracSimplify: diracSimplifyEval: Applying Dotsimplify.", FCDoControl->dsVerbose];
				tmp = DotSimplify[tmp, FCI->True, Expanding -> True, FCJoinDOTs->False];
				FCPrint[1,"DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose];
				FCPrint[3,"DiracSimplify: diracSimplifyEval: After Dotsimplify: ", tmp, FCDoControl->dsVerbose];
		];

		If[ !FreeQ[tmp, DiracGamma],

			If[	optContract=!=False && !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],
				time2=AbsoluteTime[];
				FCPrint[1, "DiracSimplify: diracSimplifyEval: Applying Contract.", FCDoControl->dsVerbose];
				tmp = Contract[tmp, Expanding->True, EpsContract-> False, Factoring->False];
				FCPrint[1, "DiracSimplify: diracSimplifyEval: Contract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->dsVerbose];
				FCPrint[3, "DiracSimplify: diracSimplifyEval: After Contract: ", tmp, FCDoControl->dsVerbose];

				time2=AbsoluteTime[];
				FCPrint[1, "DiracSimplify: diracSimplifyEval: Applying EpsContract.", FCDoControl->dsVerbose];
				If[	optEpsContract,
					tmp = EpsContract[tmp,FCI->True]
				];
				FCPrint[1, "DiracSimplify: diracSimplifyEval: EpsContract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->dsVerbose];
				FCPrint[3, "DiracSimplify: diracSimplifyEval: After EpsContract: ", tmp, FCDoControl->dsVerbose];


			];

			time2=AbsoluteTime[];
			FCPrint[1,"DiracSimplify: diracSimplifyEval: Applying DiracTrick.", FCDoControl->dsVerbose];
			tmp = DiracTrick[tmp, FCI -> True, InsideDiracTrace-> optInsideDiracTrace, FCJoinDOTs->False, ToDiracGamma67->optToDiracGamma67];
			FCPrint[1,"DiracSimplify: diracSimplifyEval: DiracTrick done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: diracSimplifyEval: After DiracTrick: ", tmp, FCDoControl->dsVerbose];
		];

		(* Doing index contractions *)
		If[	optContract=!=False && !DummyIndexFreeQ[tmp,{LorentzIndex,CartesianIndex}],
			time2=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: diracSimplifyEval: Applying Contract.", FCDoControl->dsVerbose];
			tmp = Contract[tmp, EpsContract-> False];
			FCPrint[1, "DiracSimplify: diracSimplifyEval: Contract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->dsVerbose];

			time2=AbsoluteTime[];
			FCPrint[1, "DiracSimplify: diracSimplifyEval: Applying EpsContract.", FCDoControl->dsVerbose];
				If[	optEpsContract,
					tmp = EpsContract[tmp,FCI->True]
				];
			FCPrint[1, "DiracSimplify: diracSimplifyEval: EpsContract done, timing: ", N[AbsoluteTime[] - time2, 4] , FCDoControl->dsVerbose];
			FCPrint[3, "DiracSimplify: diracSimplifyEval: After EpsContract: ", tmp, FCDoControl->dsVerbose];
		];

		(* 	Expansion of the scalar products.	*)
		If[ optExpandScalarProduct && !FreeQ[tmp,Momentum],
			time2=AbsoluteTime[];
			FCPrint[1,"DiracSimplify: diracSimplifyEval: Expanding scalar products", FCDoControl->dsVerbose];
			tmp = ExpandScalarProduct[tmp,FCI->False];
			FCPrint[1,"DiracSimplify:diracSimplifyEvale: Done expanding the result, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose]
		];

		(*	Substituting the explicit values of Dirac sigma	*)
		If [ optDiracSigmaExplicit && !FreeQ[tmp,DiracSigma],
			tmp = DiracSigmaExplicit[tmp,FCI->True]
		];

		(* 	Canonical ordering of the Dirac matrices.	*)
		If[ optDiracOrder,
				time2=AbsoluteTime[];
				FCPrint[1,"DiracSimplify: diracSimplifyEval: Applying DiracOrder.", FCDoControl->dsVerbose];
				tmp = DiracOrder[tmp, FCI->True, FCJoinDOTs->OptionValue[Expanding]];
				FCPrint[1,"DiracSimplify:diracSimplifyEvale: Done applying DiracOrder, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose]
		];


		(*	Expansion of Dirac slashes	*)
		(*
		If[	optDiracGammaExpand,
			FCPrint[1,"DiracSimplify: diracSimplifyEval: Expanding Dirac slashes.", FCDoControl->dsVerbose];
			tmp = DiracGammaExpand[tmp,FCI->True];

		];*)


		(*	Substituting the explicit values of the chiral projectors	*)
		If[ optDiracSubstitute67 && !FreeQ2[tmp,{DiracGamma[6],DiracGamma[7]}],
			FCPrint[1,"DiracSimplify: diracSimplifyEval: Substituting the explicit values of the chiral projectors.", FCDoControl->dsVerbose];
			tmp = DiracSubstitute67[tmp, FCI->True]
		];

		(*	Rewriting DiracGamma[5] in terms of the chiral projectors 	*)
		If[ optDiracSubstitute5 && !FreeQ[tmp,DiracGamma[5]],
			FCPrint[1,"DiracSimplify: diracSimplifyEval: Rewriting DiracGamma[5] in terms of the chiral projectors.", FCDoControl->dsVerbose];
			tmp = DiracSubstitute5[tmp, FCI->True]
		];


		If[	optExpanding && (!optDiracGammaCombine || optDiracSubstitute67),
			time2=AbsoluteTime[];
			FCPrint[1,"DiracSimplify: diracSimplifyEval: Applying Dotsimplify.", FCDoControl->dsVerbose];
			tmp = DotSimplify[tmp, FCI->True, Expanding -> True];
			FCPrint[1,"DiracSimplify: diracSimplifyEval: Dotsimplify done, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose];
			FCPrint[3,"DiracSimplify: diracSimplifyEval: After Dotsimplify: ", tmp, FCDoControl->dsVerbose];
		];


		(*	Dirac equation	*)
		If[	!FreeQ[tmp,Spinor] && optDiracEquation,
			tmp = DiracEquation[tmp, FCI->True]
		];

		(* 	Covariant normalization of ubar.u or vbar.v (as in Peskin and Schroeder).
			The combinations ubar.v and vbar.u are orthogonal and hence vanish *)
		If[	!FreeQ[tmp,Spinor],
			FCPrint[2,"DiracSimplify: Applying spinor normalization on ", tmp, FCDoControl->dsVerbose];

			tmp = tmp/. DOT->holdDOT //.
			{	holdDOT[Spinor[s_. Momentum[p__],m_, 1],Spinor[s_. Momentum[p__],m_, 1]] -> s 2 m,
				holdDOT[Spinor[- Momentum[p__],m_, 1],Spinor[Momentum[p__],m_, 1]] -> 0,
				holdDOT[Spinor[Momentum[p__],m_, 1],Spinor[- Momentum[p__],m_, 1]] -> 0} /.
			holdDOT -> DOT
		];

		(* Factoring	*)
		If[ optFactoring=!=False,
				time2=AbsoluteTime[];
				FCPrint[1,"DiracSimplify: diracSimplifyEval: Factoring the result.", FCDoControl->dsVerbose];
				tmp = optFactoring[tmp];
				FCPrint[1,"DiracSimplify:diracSimplifyEvale: Done factoring, timing: ", N[AbsoluteTime[] - time2, 4], FCDoControl->dsVerbose]
		];

		res = tmp;

		FCPrint[3,"DiracSimplify: diracSimplifyEval: Leaving with: ", res, FCDoControl->dsVerbose];

		res


	]/;Head[expr]=!=DiracChain;

FCPrint[1,"DiracSimplify.m loaded."];
End[]
