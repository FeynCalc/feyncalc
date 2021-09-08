(* ::Package:: *)



(* :Title: DiracTrick                                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Contraction and simplification rules for Dirac matrices                                        *)

(* ------------------------------------------------------------------------ *)

DiracTrick::usage =
"DiracTrick[exp] contracts Dirac matrices with each other and performs several
simplifications but no expansions.There are not many cases when a user will
need to call this function directly. Use DiracSimplify to achieve maximal
simplification of Dirac matrix chains. Regarding the treatment of $\\gamma^5$
in $D$-dimensional expressions or the evaluation of expressions with tensors
living in different dimensions, see the explanations on the help pages for
DiracSimplify and DiracTrace.";

DiracTrick::failmsg =
"Error! DiracTrick has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"


(* ------------------------------------------------------------------------ *)

Begin["`Package`"]

diracTrickEvalFastFromDiracSimplifyList;
diracTrickEvalFastFromDiracSimplifySingle;
(*TODO: Need to add an option for moving all g^5 to the left!*)
End[]

Begin["`DiracTrick`Private`"]

diTrVerbose::usage="";
diracTraceSimplify::usage="";
insideDiracTrace::usage="";
diracOrder::usage="";
optJoin::usage="";
diga::usage="";
tmpli::usage="";

Options[DiracTrick] = {
	Evaluate			-> True,
	DiracChain			-> True,
	DiracGammaCombine	-> False,
	Expanding 			-> False,
	FCDiracIsolate		-> True,
	FCE					-> False,
	FCI					-> False,
	FCJoinDOTs			-> False,
	FCVerbose			-> False,
	InsideDiracTrace	-> False,
	ToDiracGamma67		-> True
};

diracTrickEvalFastFromDiracSimplifyList[diracObjects_List, {optInsideDiracTrace_, optDiracOrder_}]:=
	Block[{tmp1,tmp2,res},
		tmp1 = insideDiracTrace;
		tmp2 = diracOrder;
		insideDiracTrace = optInsideDiracTrace;
		diracOrder = optDiracOrder;
		res = (diracTrickEvalFast/@diracObjects) /. diracTrickEvalFast->Identity;
		insideDiracTrace = tmp1;
		diracOrder = tmp2;
		res
	];

diracTrickEvalFastFromDiracSimplifySingle[diracObject_, {tmpHead_, optInsideDiracTrace_, optDiracOrder_}]:=
	Block[{tmp1,tmp2,res},
		tmp1 = insideDiracTrace;
		tmp2 = diracOrder;
		insideDiracTrace = optInsideDiracTrace;
		diracOrder = optDiracOrder;
		res = diracTrickEvalFast[diracObject] /. diracTrickEvalFast->tmpHead;
		insideDiracTrace = tmp1;
		diracOrder = tmp2;
		res
	]/; Head[diracObject]=!=DiracChain;

diracTrickEvalFastFromDiracSimplifySingle[DiracChain[diracObject_,i_,j_], {tmpHead_, optInsideDiracTrace_, optDiracOrder_}]:=
	DiracChain[diracTrickEvalFastFromDiracSimplifySingle[diracObject, {tmpHead, optInsideDiracTrace, optDiracOrder}],i,j]

DiracTrick[expr_,OptionsPattern[]] :=
	Block[{	res, tmp, ex, null1, null2, holdDOT, freePart, dsPart, diracObjects,
			diracObjectsEval, repRule, time, dsHead, optFCDiracIsolate},

		(*	Algorithm of DiracTrick:

			x)	Isolate all Dirac objects and handle them separately. Of course there is an extra option
				to skip this if the input is already a single object (e.g. when DiracTrick is called by
				DiracTrace or DiracSimplify) Then apply the evaluating function to each of the objects,
				create replacement rules (standard)	and substitute the results back.

			x) The inner working of the evaluating function:
				xx)		Check the dimension of the chain, could be purely 4-dim, purely D-dim or mixed (BMHV).
						Also check that BMHV was indeed activated if mixed dimensions appear

				xx)		Then we
							1) 	Handle g^5 if it is present
							2) 	Ensure that the expression is of the form .... g^5 or contains no g^5 at all,
								unless there are unknown non-commutative objects inside the chain or we use BMHV
							3)	Handle everything apart from g^5.
							4)	If we use BMHV, handle g^5 again, this time using anticommuting rules.
							5)	Notice that we use different functions for 4-dim, D-dim and mixed expressions
								to save time and ensure that e.g. no rules for mixed expressions are unsuccesfully
								applied to a purely 4-dim chain

				xx)		The handling of g^5 of course depends a lot on dimensions, the scheme and whether we are
						inside a trace or not.

						xxx) If we are purely 4-dim, then we do all the g^5 related simplifications in this step.
						xxx) If we are purely D-dim, not inside trace, and use NDR, then g^5 is anticommuted
						xxx) If we are purely D-dim, not inside trace, and use Larin or BMHV, then g^5 is left alone
						xxx) If we are purely D-dim, inside trace and use Larin, then g^5 is anticommuted and cyclicity is used
						xxx) If we are purely D-dim, inside trace and use BMHV, then only cyclicity is used

						xxx) If we are mixed (always BMHV), not inside trace, then g^5 is left alone
						xxx) If we are mixed (always BMHV), inside trace, then only cyclicity is used
						xxx) Notice that in all cases we use some generic properties of g^5, namely that
						(g^5)^2 = 1, g^6 g^7 = 0 etc.


		*)

		If [OptionValue[FCVerbose]===False,
			diTrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				diTrVerbose=OptionValue[FCVerbose]
			];
		];


		optJoin 			= OptionValue[FCJoinDOTs];
		optFCDiracIsolate	= OptionValue[FCDiracIsolate];
		insideDiracTrace	= OptionValue[InsideDiracTrace];


		FCPrint[1, "DiracTrick. Entering.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: Entering with ", expr, FCDoControl->diTrVerbose];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];



		If[ FreeQ2[ex,DiracHeadsList],
			Return[ex]
		];


		If[	optFCDiracIsolate===True || Head[optFCDiracIsolate]===List,
			(*	This is the standard mode for calling DiracTrick	*)
			FCPrint[1,"DiracTrick: Normal mode.", FCDoControl->diTrVerbose];
			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: Extracting Dirac objects.", FCDoControl->diTrVerbose];
			(* 	First of all we need to extract all the Dirac structures in the input. *)
			If[	TrueQ[Head[optFCDiracIsolate]===List],
				ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, optFCDiracIsolate],
				ex = FCDiracIsolate[ex,FCI->True,Head->dsHead, DotSimplify->True, DiracGammaCombine->OptionValue[DiracGammaCombine],
					FCJoinDOTs -> optJoin, LorentzIndex->True, ToDiracGamma67-> OptionValue[ToDiracGamma67], DiracChain->OptionValue[DiracChain]];
			];

			{freePart,dsPart} = FCSplit[ex,{dsHead}];
			FCPrint[3,"DiracTrick: dsPart: ",dsPart , FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrick: freePart: ",freePart , FCDoControl->diTrVerbose];
			FCPrint[1, "DiracTrick: Done extracting Dirac objects, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];

			diracObjects = Cases[dsPart+null1+null2, dsHead[_], Infinity]//Union;
			FCPrint[3,"DiracTrick: diracObjects: ",diracObjects , FCDoControl->diTrVerbose];

			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: Applying diracTrickEvalFast", FCDoControl->diTrVerbose];

			diracObjectsEval = diracTrickEvalFast/@(diracObjects/.dsHead->Identity);

			FCPrint[1,"DiracTrick: diracTrickEvalFast done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrick: After diracTrickEvalFast: ", diracObjectsEval, FCDoControl->diTrVerbose];


			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: Doing contractions", FCDoControl->diTrVerbose];

			If[	!FreeQ[diracObjectsEval,Pair],
				diracObjectsEval = diracObjectsEval /. diracTrickEvalFast[zzz_] :> diracTrickEvalFast[Expand2[zzz,Pair]/. Pair->PairContract /. PairContract->Pair]
			];

			If[	!FreeQ[diracObjectsEval,CartesianPair],
				diracObjectsEval = diracObjectsEval /. diracTrickEvalFast[zzz_] :> diracTrickEvalFast[Expand2[zzz,CartesianPair]/.
					CartesianPair->CartesianPairContract /. CartesianPairContract->CartesianPair]
			];

			FCPrint[1,"DiracTrick: Contractions done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3,"DiracTrick: After contractions: ", diracObjectsEval, FCDoControl->diTrVerbose];


			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: Applying diracTrickEval", FCDoControl->diTrVerbose];

			If[	OptionValue[Evaluate],
				diracObjectsEval = diracObjectsEval /. diracTrickEvalFast -> diracTrickEval,
				diracObjectsEval = diracObjectsEval /. diracTrickEvalFast -> Identity
			];

			FCPrint[3,"DiracTrick: After diracTrickEval: ", diracObjectsEval, FCDoControl->diTrVerbose];
			FCPrint[1,"DiracTrick: diracTrickEval done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];


			If[ !FreeQ2[diracObjectsEval,{diracTrickEvalFast,diracTrickEval,holdDOT}],
				Message[DiracTrick::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			];

			FCPrint[1, "DiracTrick: Inserting Dirac objects back.", FCDoControl->diTrVerbose];
			time=AbsoluteTime[];
			repRule = Thread[Rule[diracObjects,diracObjectsEval]];
			FCPrint[3,"DiracTrick: repRule: ",repRule , FCDoControl->diTrVerbose];
			res = freePart + ( dsPart /. Dispatch[repRule]);
			FCPrint[1, "DiracTrick: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose],

			FCPrint[1,"DiracTrick: Fast mode.", FCDoControl->diTrVerbose];

			(* 	This is a fast mode for input that is already isolated, e.g. for calling DiracTrick/@exprList
				from internal functions	*)
			res = diracTrickEvalFast[ex];

			If[	!FreeQ[diracObjectsEval,Pair],
				res = res /. diracTrickEvalFast[zzz_] :> diracTrickEvalFast[Expand2[zzz,Pair]/. Pair->PairContract /. PairContract->Pair]
			];

			If[	!FreeQ[diracObjectsEval,CartesianPair],
				res = res /. diracTrickEvalFast[zzz_] :> diracTrickEvalFast[Expand2[zzz,CartesianPair]/.
					CartesianPair->CartesianPairContract /. CartesianPairContract->CartesianPair]
			];

			(* It might happen that after diracTrickEvalFast there are no Dirac matrices left.*)

			FCPrint[3,"DiracTrick: After diracTrickEvalFast: ", res , FCDoControl->diTrVerbose];

			If[ OptionValue[ToDiracGamma67],
				res = res /. diracTrickEvalFast[zzz_]/;!FreeQ[{zzz}, DiracGamma[5]] :> diracTrickEvalFast[ToDiracGamma67[zzz,FCI->True]]
			];

			If[ !FreeQ[res,diracTrickEvalFast],
				res = res /. diracTrickEvalFast -> diracTrickEval;
				FCPrint[3,"DiracTrick: After diracTrickEval: ", res , FCDoControl->diTrVerbose]
			];

			If[ !FreeQ2[res,{diracTrickEvalFast,diracTrickEval,holdDOT}],
				Message[DiracTrick::failmsg,"Evaluation of isolated objects failed."];
				Abort[]
			]
		];


		If[	OptionValue[Expanding],
			time=AbsoluteTime[];
			FCPrint[1, "DiracTrick: Expanding the result.", FCDoControl->diTrVerbose];
			res = Expand[res];
			FCPrint[1,"DiracTrick: Expanding done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3, "DiracTrick: After expanding: ", res, FCDoControl->diTrVerbose]
		];

		If[	OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "DiracTrick. Leaving.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: Leaving with ", res, FCDoControl->diTrVerbose];

		res
	];

(*	Here we can quickly handle trivial contractions of short expressions. Usually such expressions appear
	when we call DiracTrick from DiracTrace after DotSimplify. Hence, the rules should be as simple, as possible	*)


diracTrickEvalFast[DiracChain[x_, i_, j_]]:=
	DiracChain[diracTrickEvalFast[x],i,j];

diracTrickEvalFast[x_DiracGamma]:=
	x/; !insideDiracTrace && !diracOrder;


diracTrickEvalFast[x_DiracGamma]:=
	0/; insideDiracTrace && FreeQ2[x, {DiracGamma[6],DiracGamma[7]}];

diracTrickEvalFast[DiracGamma[6|7]]:=
	(1/2)/; insideDiracTrace;


diracTrickEvalFast[DOT[DiracGamma[ExplicitLorentzIndex[0]],DiracGamma[ExplicitLorentzIndex[0]]]]:=
	1;

diracTrickEvalFast[DOT[DiracGamma[ExplicitLorentzIndex[(i: 1|2|3)]], DiracGamma[ExplicitLorentzIndex[(i: 1|2|3)]]]] :=
	-1;

diracTrickEvalFast[DOT[x_DiracGamma,y__DiracGamma]]:=
	DOT[x,y]/; FreeQ2[{x,y},{DiracGamma[5],DiracGamma[6],DiracGamma[7], ExplicitLorentzIndex[0], ExplicitLorentzIndex[1], ExplicitLorentzIndex[2], ExplicitLorentzIndex[3]}] &&
	(Sort[Cases[{x,y}, (LorentzIndex | Momentum | CartesianIndex | CartesianMomentum)[a_, ___] :> a, Infinity]] ===
	Union[Cases[{x,y}, (LorentzIndex | Momentum | CartesianIndex | CartesianMomentum)[a_, ___] :> a, Infinity]])/; !insideDiracTrace && !diracOrder;

(*	Dirac trace related simplifications	*)

diracTrickEvalFast[DOT[b___DiracGamma,di_DiracGamma,c__DiracGamma]] :=
	diracTrickEvalFast[DOT[c,b, di]]/; !FreeQ2[{di},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] &&
	FreeQ2[{b,c},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}]/; insideDiracTrace;


diracTrickEvalFast[DOT[x__DiracGamma, DiracGamma[5]]]:=
	0/; FreeQ2[{x},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && EvenQ[Length[{x}]] && insideDiracTrace &&
	MatchQ[FCGetDimensions[{x}],{_Symbol}] && 	(FeynCalc`Package`DiracGammaScheme === "NDR-Discard");


diracTrickEvalFast[DOT[x__DiracGamma, DiracGamma[6|7]]]:=
	1/2 diracTrickEvalFast[DOT[x]]/; FreeQ2[{x},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && EvenQ[Length[{x}]] && insideDiracTrace &&
	MatchQ[FCGetDimensions[{x}],{_Symbol}] && 	(FeynCalc`Package`DiracGammaScheme === "NDR-Discard");

diracTrickEvalFast[DOT[DiracGamma[(arg1:_[_, dim1___]),dim1___], DiracGamma[(arg2:_[_, dim2___]), dim2___]]] :=
	FCUseCache[ExpandScalarProduct,{PairContract[arg1,arg2] /. PairContract->Pair},{}]/; insideDiracTrace;

diracTrickEvalFast[DOT[DiracGamma[(arg1:_[_, dim1___]),dim1___], DiracGamma[(arg2:_[_, dim2___]), dim2___], DiracGamma[6|7]]]:=
	1/2 FCUseCache[ExpandScalarProduct,{PairContract[arg1,arg2] /. PairContract->Pair},{}]/; insideDiracTrace;

diracTrickEvalFast[DOT[x_DiracGamma,y__DiracGamma]]:=
	0/; FreeQ2[{x,y},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && OddQ[Length[{x,y}]] && insideDiracTrace;

diracTrickEvalFast[DOT[x___DiracGamma,DiracGamma[5],y___DiracGamma]]:=
	0/; Length[{x,y}]<4 && FreeQ2[{x,y},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && insideDiracTrace;

diracTrickEvalFast[DOT[x___DiracGamma,DiracGamma[6|7],y___DiracGamma]]:=
	0/; OddQ[Length[{x,y}]] && FreeQ2[{x,y},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && insideDiracTrace;

diracTrickEvalFast[DiracGamma[5]]:=
	0/; insideDiracTrace;

diracTrickEvalFast[DiracGamma[6|7]]:=
	1/2/; insideDiracTrace;

diracTrickEvalFast[DOT[DiracGamma[(a:6|7)],b___,DiracGamma[(a:6|7)]]] :=
	diracTrickEvalFast[DOT[b,DiracGamma[a]]]/; insideDiracTrace;

diracTrickEvalFast[DOT[DiracGamma[(h1:5|6|7)],b___,DiracGamma[(h2:5|6|7)]]] :=
	ga67MatSign[h1,h2] diracTrickEvalFast[DOT[b,ga67Mat[h1,h2]]]/; h1=!=h2 && insideDiracTrace;

(*	Generic simplifications	*)

diracTrickEvalFast[DOT[b___,DiracGamma[l_LorentzIndex], DiracGamma[l_LorentzIndex], d___]] :=
	4 diracTrickEvalFast[DOT[ b,d ]];

diracTrickEvalFast[DOT[b___,DiracGamma[l_LorentzIndex, dim_], DiracGamma[l_LorentzIndex, dim_], d___]] :=
	dim diracTrickEvalFast[DOT[ b,d ]];

diracTrickEvalFast[DOT[]]:=
	1;

diracTrickEvalFast[DOT[b___,DiracGamma[c_Momentum], DiracGamma[c_Momentum], d___ ]] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracTrickEvalFast[DOT[b,d]];

diracTrickEvalFast[DOT[b___,DiracGamma[c_Momentum, dim_], DiracGamma[c_Momentum, dim_], d___]] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracTrickEvalFast[DOT[b,d]];

diracTrickEvalFast[DOT[b___,DiracGamma[l_LorentzIndex, dim_:4], DiracGamma[c_Momentum, dim_:4], DiracGamma[l_LorentzIndex, dim_:4], d___]] :=
	(2 - dim) diracTrickEvalFast[DOT[ b, DiracGamma[c,  dim], d ]];

diracTrickEvalFast[DOT[b___,DiracGamma[5], DiracGamma[5], d___]] :=
	diracTrickEvalFast[DOT[ b,d ]];

diracTrickEvalFast[DOT[b___, DiracGamma[5], DiracGamma[(hh:6|7)], c___]] :=
	ga67Val1[hh] diracTrickEvalFast[DOT[b,DiracGamma[hh],c]];

diracTrickEvalFast[DOT[b___, DiracGamma[(hh:6|7)], DiracGamma[5], c___]] :=
	ga67Val1[hh]  diracTrickEvalFast[DOT[b,DiracGamma[hh],c]];

diracTrickEvalFast[DOT[b___, DiracGamma[(hh1:6|7)],DiracGamma[(hh2:6|7)], c___]] :=
	ga67Val2[hh1,hh2] diracTrickEvalFast[DOT[b, DiracGamma[hh2], c]];


diracTrickEvalFast[DOT[b___,DiracGamma[5], c:DiracGamma[_[_,_],_].. , d___]] :=
	(-1)^Length[{c}] diracTrickEvalFast[DOT[ b,c,DiracGamma[5],d]]/; MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] &&
		MatchQ[FCGetDimensions[{c}],{_Symbol}];

diracTrickEvalFast[DOT[b___,DiracGamma[(h:6|7)], c:DiracGamma[_[_,_],_].. ,d___]] :=
	diracTrickEvalFast[DOT[ b,c,DiracGamma[h],d]]/; EvenQ[Length[{c}]] && MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] &&
		MatchQ[FCGetDimensions[{c}],{_Symbol}];

diracTrickEvalFast[DOT[b___,DiracGamma[(h:6|7)], c:DiracGamma[_[_,_],_].. ,d___]] :=
	diracTrickEvalFast[DOT[ b,c,DiracGamma[ga67Switch1[h]],d]]/; OddQ[Length[{c}]] && MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] &&
		MatchQ[FCGetDimensions[{c}],{_Symbol}];

diracTrickEvalFast[DOT[___,DiracGamma[(h:6|7)],DiracGamma[_[_,_],_], DiracGamma[(h:6|7)], ___]] :=
	0/; MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme];

diracTrickEvalFast[DOT[b___,DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_,_],_]), DiracGamma[(h2:6|7)], c___]] :=
	diracTrickEvalFast[DOT[b, dg, DiracGamma[h2], c]]/; h1=!=h2 && MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] &&
		MatchQ[FCGetDimensions[{dg}],{_Symbol}];

diracTrickEvalFast[DOT[b___, DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_,_],_]), xy:DiracGamma[_[_] ].. , DiracGamma[(h2:6|7)], c___]] :=
	diracTrickEvalFast[DOT[b, dg, xy, DiracGamma[h2], c]]/; EvenQ[Length[{xy}]] && h1=!=h2 && MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] &&
		MatchQ[FCGetDimensions[{dg,xy}],{_Symbol}];

diracTrickEvalFast[DOT[___, DiracGamma[(h:6|7)],(dg:DiracGamma[_[_,_],_]), xy:DiracGamma[_[_] ].. , DiracGamma[(h:6|7)], ___]] :=
	0/; EvenQ[Length[{xy}]] && MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme]  &&
		MatchQ[FCGetDimensions[{dg,xy}],{_Symbol}];


diracTrickEvalFast[DOT[___, DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_,_],_]), xy:DiracGamma[_[_,_],_].. , DiracGamma[(h2:6|7)], ___]] :=
	0/; OddQ[Length[{xy}]] && h1=!=h2 && MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme]  &&
		MatchQ[FCGetDimensions[{dg,xy}],{_Symbol}];

diracTrickEvalFast[DOT[b___, DiracGamma[(h:6|7)],(dg:DiracGamma[_[_,_],_]), xy:DiracGamma[_[_,_],_].. , DiracGamma[(h:6|7)], c___]] :=
	diracTrickEvalFast[DOT[b, dg, xy, DiracGamma[h], c]]/; OddQ[Length[{xy}]]  && MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme]  &&
		MatchQ[FCGetDimensions[{dg,xy}],{_Symbol}];


diracTrickEvalFast[DOT[b___,DiracGamma[5], c:DiracGamma[_[_]].. , d___]] :=
	(-1)^Length[{c}] diracTrickEvalFast[DOT[ b,c,DiracGamma[5],d]];

diracTrickEvalFast[DOT[b___,DiracGamma[(h:6|7)], c:DiracGamma[_[_]].. ,d___]] :=
	diracTrickEvalFast[DOT[ b,c,DiracGamma[h],d]]/; EvenQ[Length[{c}]];

diracTrickEvalFast[DOT[b___,DiracGamma[(h:6|7)], c:DiracGamma[_[_]].. ,d___]] :=
	diracTrickEvalFast[DOT[ b,c,DiracGamma[ga67Switch1[h]],d]]/; OddQ[Length[{c}]];

diracTrickEvalFast[DOT[___,DiracGamma[(h:6|7)],DiracGamma[_[_]], DiracGamma[(h:6|7)], ___]] :=
	0;

diracTrickEvalFast[DOT[b___,DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_]]), DiracGamma[(h2:6|7)], c___]] :=
	diracTrickEvalFast[DOT[b, dg, DiracGamma[h2], c]]/; h1=!=h2;

diracTrickEvalFast[DOT[b___, DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_]]), xy:DiracGamma[_[_] ].. , DiracGamma[(h2:6|7)], c___]] :=
	diracTrickEvalFast[DOT[b, dg, xy, DiracGamma[h2], c]]/; EvenQ[Length[{xy}]] && h1=!=h2;

diracTrickEvalFast[DOT[___, DiracGamma[(h:6|7)],DiracGamma[_[_]], xy:DiracGamma[_[_] ].. , DiracGamma[(h:6|7)], ___]] :=
	0/; EvenQ[Length[{xy}]];

diracTrickEvalFast[DOT[___, DiracGamma[(h1:6|7)],DiracGamma[_[_]], xy:DiracGamma[_[_]].. , DiracGamma[(h2:6|7)], ___]] :=
	0/; OddQ[Length[{xy}]] && h1=!=h2;

diracTrickEvalFast[DOT[b___, DiracGamma[(h:6|7)],(dg:DiracGamma[_[_]]), xy:DiracGamma[_[_]].. , DiracGamma[(h:6|7)], c___]] :=
	diracTrickEvalFast[DOT[b, dg, xy, DiracGamma[h], c]]/; OddQ[Length[{xy}]];

diracTrickEval[0]:=
	0;

diracTrickEval[ex_DiracGamma]:=
	ex/; !insideDiracTrace;

diracTrickEval[ex_DiracGamma]:=
	0/; insideDiracTrace && !MatchQ[ex,DiracGamma[6|7]];

diracTrickEval[ex_DiracGamma]:=
	1/2/; insideDiracTrace && MatchQ[ex, DiracGamma[6|7]];

diracTrickEval[ex_/;Head[ex]=!=DiracGamma]:=
	Which[
		(*	NDR, inside Trace	*)
		MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme] && insideDiracTrace,
		diracTrickEvalCachedNDRInsideTrace[ex],
		(*	Larin, inside Trace	*)
		(FeynCalc`Package`DiracGammaScheme === "Larin") && insideDiracTrace,
		diracTrickEvalCachedLarinInsideTrace[ex],
		(* BMHV, inside Trace *)
		(FeynCalc`Package`DiracGammaScheme === "BMHV") && insideDiracTrace,
		diracTrickEvalCachedBMHVInsideTrace[ex],
		(*	NDR	*)
		MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme],
		diracTrickEvalCachedNDR[ex],
		(*	Larin	*)
		(FeynCalc`Package`DiracGammaScheme === "Larin"),
		diracTrickEvalCachedLarin[ex],
		(* BMHV *)
		(FeynCalc`Package`DiracGammaScheme === "BMHV"),
		diracTrickEvalCachedBMHV[ex],
		(* Else *)
		True,
		Message[DiracTrace::mixmsg];
		Abort[]
	]/; ex=!=0;

diracTrickEvalCachedNDRInsideTrace[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedLarinInsideTrace[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedBMHVInsideTrace[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedNDR[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedLarin[x_] :=
	diracTrickEvalInternal[x];

diracTrickEvalCachedBMHV[x_] :=
	diracTrickEvalInternal[x];

(*
diracTrickEvalCachedNDRInsideTrace[x_] :=
	MemSet[diracTrickEvalCachedNDRInsideTrace[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedLarinInsideTrace[x_] :=
	MemSet[diracTrickEvalCachedLarinInsideTrace[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedBMHVInsideTrace[x_] :=
	MemSet[diracTrickEvalCachedBMHVInsideTrace[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedNDR[x_] :=
	MemSet[diracTrickEvalCachedNDR[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedLarin[x_] :=
	MemSet[diracTrickEvalCachedLarin[x], diracTrickEvalInternal[x]];

diracTrickEvalCachedBMHV[x_] :=
	MemSet[diracTrickEvalCachedBMHV[x], diracTrickEvalInternal[x]];
*)

diracTrickEvalInternal[ex_/;Head[ex]=!=DiracGamma]:=
	Block[{res=ex, holdDOT, time, dim, gamma5Present,noncommPresent,null1,null2,dsHead},

		FCPrint[2, "DiracTrick: diracTrickEval: Entering.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: diracTrickEval: Entering with: ", ex , FCDoControl->diTrVerbose];

		gamma5Present = !FreeQ2[ex,{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];
		noncommPresent = !NonCommFreeQ[ex/.DiracGamma->diga];

		FCPrint[3, "DiracTrick: diracTrickEval: g^5 present: ", gamma5Present, FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: diracTrickEval: unknown non-commutative objects present: ", noncommPresent, FCDoControl->diTrVerbose];

		res = res/. DOT -> holdDOT;

		If[	gamma5Present,
			res = res /. holdDOT -> commonGamma5Properties /. commonGamma5Properties -> holdDOT;
			gamma5Present = !FreeQ2[res,{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];
			FCPrint[3, "DiracTrick: diracTrickEval: after applying simplifications related to g^5: ", res, FCDoControl->diTrVerbose];
		];

		If[ FreeQ2[res,DiracHeadsList],
			Return[res]
		];

		If[	insideDiracTrace,
			time=AbsoluteTime[];
			FCPrint[2, "DiracTrick: diracTrickEval: Applying diracTraceSimplify ", FCDoControl->diTrVerbose];
			res = diracTraceSimplify[res] /. DOT -> holdDOT /. diracTraceSimplify[holdDOT[x__]] :> diracTraceSimplify[x]/.
			diracTraceSimplify -> commonGamma5Properties /. commonGamma5Properties -> holdDOT;
			FCPrint[1,"DiracTrick: diracTrickEval: diracTraceSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3, "DiracTrick: diracTrickEval: After diracTraceSimplify ", res, FCDoControl->diTrVerbose],
			res = res /. DOT -> holdDOT;
		];

		If[	!FreeQ[res,Pair],
			res = Expand2[res,Pair]/. Pair->PairContract /. PairContract->Pair;
		];

		If[	!FreeQ[res,CartesianPair],
			res = Expand2[res,CartesianPair]/. CartesianPair->CartesianPairContract /. CartesianPairContract->CartesianPair;
		];


		If[ FreeQ2[res,DiracHeadsList],
			Return[res]
		];

		dim = FCGetDimensions[res, FreeQ->{DiracGamma[5],DiracGamma[6],DiracGamma[7]}, ChangeDimension->True];


		noncommPresent = !NonCommFreeQ[res/.DiracGamma->diga];

		FCPrint[3, "DiracTrick: diracTrickEval: Dimensions: ", dim, FCDoControl->diTrVerbose];

		If[	gamma5Present,

			time=AbsoluteTime[];
			FCPrint[2, "DiracTrick: diracTrickEval: Applying chiralTrick ", FCDoControl->diTrVerbose];
			Which[
					(* Purely 4-dimensional -> use anticommuting g^5 *)
					MatchQ[dim,{4}],
						FCPrint[2, "DiracTrick: diracTrickEval: Purely 4-dim.", FCDoControl->diTrVerbose];
						res = res /. holdDOT -> chiralTrickAnticommuting4DimRight;
						res = FixedPoint[(# /. chiralTrickAnticommuting4DimRight -> commonGamma5Properties /.
							commonGamma5Properties -> chiralTrickAnticommuting4DimRight)&,res];
						res = res /. chiralTrickAnticommuting4DimRight -> holdDOT,
					(* Purely D-dimensional and NDR -> use anticommuting g^5 *)
					MatchQ[dim,{_Symbol}] && MemberQ[{"NDR","NDR-Discard"},FeynCalc`Package`DiracGammaScheme],
						FCPrint[2, "DiracTrick: diracTrickEval: Purely D-dim, NDR.", FCDoControl->diTrVerbose];
						res = res /. holdDOT -> chiralTrickAnticommutingDDimRight;
						res = FixedPoint[(# /. chiralTrickAnticommutingDDimRight -> commonGamma5Properties /.
							commonGamma5Properties -> chiralTrickAnticommutingDDimRight)&,res];
						res = res /. chiralTrickAnticommutingDDimRight -> holdDOT,
					(* Purely D-dimensional and BMHV -> don't move anything around *)
					MatchQ[dim,{_Symbol}] && (FeynCalc`Package`DiracGammaScheme === "BMHV"),
						FCPrint[2, "DiracTrick: diracTrickEval: Purely D-dim and BMHV.", FCDoControl->diTrVerbose];
						Null,
					(* Purely D-dimensional and Larin use the substitution rule to eliminate g^5 that are not on the left of the trace *)
					MatchQ[dim,{_Symbol}] && (FeynCalc`Package`DiracGammaScheme === "Larin"),
						FCPrint[2, "DiracTrick: diracTrickEval: Purely D-dim and Larin.", FCDoControl->diTrVerbose];
						res = res /. holdDOT -> chiralTrickLarin /. chiralTrickLarin -> holdDOT,
					(* Mixed and BMHV -> don't move g^5 around *)
					dim=!={} && (FeynCalc`Package`DiracGammaScheme === "BMHV"),
						FCPrint[1, "DiracTrick: diracTrickEval: Mixed and BMHV.", FCDoControl->diTrVerbose],
					(* special case that the expression contains only chiral or g^0 matrices*)
					dim==={} && !FreeQ2[res,{DiracGamma[5],DiracGamma[6],DiracGamma[7],DiracGamma[ExplicitLorentzIndex[0]]}] && FreeQ[(res/.DiracGamma[5|6|7|ExplicitLorentzIndex[0]]:>diga),DiracGamma],
					FCPrint[2, "DiracTrick: diracTrickEval: Chiral or g^0 only.", FCDoControl->diTrVerbose],
					(* Anything else is most likely an error *)
					True,
						Message[DiracTrace::mixmsg];
						Abort[]
			];

			FCPrint[2,"DiracTrick: diracTrickEval: chiralTrick done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3, "DiracTrick: diracTrickEval: After chiralTrick ", res, FCDoControl->diTrVerbose];
			gamma5Present = !FreeQ2[res,{DiracGamma[5],DiracGamma[6],DiracGamma[7]}]
		];

		(* Here we do all the simplifications not realted to g^5	*)
		FCPrint[2, "DiracTrick: diracTrickEval: Doing simplifications unrelated to g^5.", FCDoControl->diTrVerbose];
		Which[
			(* Purely 4-dimensional *)
			MatchQ[dim,{4}],
				FCPrint[2, "DiracTrick: diracTrickEval: Purely 4-dim.", FCDoControl->diTrVerbose];
				FCPrint[2, "DiracTrick: diracTrickEval: Applying diracology4Dim.", FCDoControl->diTrVerbose];
				res = res /. holdDOT -> diracology4Dim /. diracology4Dim -> diracology4DimFinalOrdering /. diracology4DimFinalOrdering -> holdDOT;
				FCPrint[3, "DiracTrick: diracTrickEval: After diracology4Dim: ", res, FCDoControl->diTrVerbose],
			(* Purely D-dimensional *)
			MatchQ[dim,{_Symbol}],
				FCPrint[2, "DiracTrick: diracTrickEval: Purely D-dim.", FCDoControl->diTrVerbose];
				res = res /. holdDOT -> diracologyDDim;
				res = FixedPoint[(# /. diracologyDDim -> diracologyDDim2 /. diracologyDDim2 -> diracologyDDim)&,res];
				res = res /. diracologyDDim -> holdDOT,
			(* Purely D-4-dimensional and BMHV *)
			MatchQ[dim,{_Symbol - 4}] && (FeynCalc`Package`DiracGammaScheme === "BMHV"),
				FCPrint[2, "DiracTrick: diracTrickEval: Purely D-4-dim.", FCDoControl->diTrVerbose];
				res = res /. holdDOT -> diracologyDDim;
				res = FixedPoint[(# /. diracologyDDim -> diracologyDDim2 /. diracologyDDim2 -> diracologyDDim)&,res];
				res = res /. diracologyDDim -> holdDOT,
			(* Mixed and BMHV *)
			dim=!={} && (FeynCalc`Package`DiracGammaScheme === "BMHV"),
				FCPrint[2, "DiracTrick: diracTrickEval: Mixed and BMHV.", FCDoControl->diTrVerbose];
				res = res /. holdDOT -> diracologyBMHV1;
				res = FixedPoint[(# /. diracologyBMHV1 -> diracologyBMHV2 /. diracologyBMHV2 -> diracologyBMHV1)&,res];
				res = res /. diracologyBMHV1 -> holdDOT,
			(* special case that the expression contains only g^0 matrices*)
			dim==={} && !FreeQ2[res,{DiracGamma[ExplicitLorentzIndex[0]]}] && FreeQ[(res/.DiracGamma[ExplicitLorentzIndex[0]]:>diga),DiracGamma],
				FCPrint[2, "DiracTrick: diracTrickEval: Only g^0 matrices.", FCDoControl->diTrVerbose];
				FCPrint[2, "DiracTrick: diracTrickEval: Applying diracology4Dim.", FCDoControl->diTrVerbose];
				res = res /. holdDOT -> diracology4Dim/. diracology4Dim -> diracology4DimFinalOrdering /. diracology4DimFinalOrdering -> holdDOT;
				FCPrint[3, "DiracTrick: diracTrickEval: After diracology4Dim: ", res, FCDoControl->diTrVerbose],
			(* special case that the expression contains only chiral or g^0 matrices*)
					dim==={} !FreeQ2[res,{DiracGamma[5],DiracGamma[6],DiracGamma[7],DiracGamma[ExplicitLorentzIndex[0]]}] && FreeQ[(res/.DiracGamma[5|6|7|ExplicitLorentzIndex[0]]:>diga),DiracGamma],
					FCPrint[2, "DiracTrick: diracTrickEval: Chiral  or g^0 only.", FCDoControl->diTrVerbose],
					(* Anything else is most likely an error *)
					True,
						Message[DiracTrace::mixmsg];
						Abort[]
		];
		FCPrint[2, "DiracTrick: diracTrickEval: Done with simplifications unrelated to g^5.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: diracTrickEval: After simplifications unrelated to g^5: ", res , FCDoControl->diTrVerbose];

		If[ FreeQ2[res,DiracHeadsList],
			Return[res/. holdDOT -> DOT]
		];

		(*	For BMHV we need to handle g^5 again here*)
		If[	gamma5Present && (FeynCalc`Package`DiracGammaScheme === "BMHV"),
			FCPrint[2, "DiracTrick: diracTrickEval: Doing special simplifications for the BMHV scheme.", FCDoControl->diTrVerbose];
			res = res /. holdDOT -> gamma5MoveBMHVRight;
						res = FixedPoint[(# /. gamma5MoveBMHVRight -> commonGamma5Properties /.
							commonGamma5Properties -> chiralTrickAnticommuting4DimRight /. chiralTrickAnticommuting4DimRight -> gamma5MoveBMHVRight)&,res];
			FCPrint[2, "DiracTrick: diracTrickEval: Additional simplifications of g^5 in BMHV.", FCDoControl->diTrVerbose];
			res = res /. gamma5MoveBMHVRight -> diracologyBMHV1;
			res = FixedPoint[(# /. diracologyBMHV1 -> diracologyBMHV2 /. diracologyBMHV2 -> diracologyBMHV1)&,res];
			res = res /. diracologyBMHV1 -> holdDOT;
			FCPrint[2, "DiracTrick: diracTrickEval: Done with special simplifications for the BMHV scheme.", FCDoControl->diTrVerbose];
			FCPrint[3, "DiracTrick: diracTrickEval: After special simplifications for the BMHV scheme: ", res , FCDoControl->diTrVerbose];
		];

		If[ FreeQ2[res,DiracHeadsList],
			Return[res/. holdDOT -> DOT]
		];

		res = res /. holdDOT -> DOT;

		If[	insideDiracTrace && res=!=0,
			time=AbsoluteTime[];
			FCPrint[2, "DiracTrick: diracTrickEval: Applying diracTraceSimplify again ", FCDoControl->diTrVerbose];
			res = FCDiracIsolate[res, DotSimplify->False, FCI->True, DiracGammaCombine->False, FCJoinDOTs->optJoin, Head->dsHead];
			res = res  /. {dsHead[DiracGamma[5]] :> 0, dsHead[DiracGamma[6|7]] :> 1/2 } /. DOT-> holdDOT/.
			dsHead[holdDOT[x__]] :>  diracTraceSimplify[x];
			res = res /. diracTraceSimplify -> DOT /. dsHead-> Identity /. holdDOT -> DOT;
			FCPrint[2,"DiracTrick: diracTrickEval: diracTraceSimplify done, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->diTrVerbose];
			FCPrint[3, "DiracTrick: diracTrickEval: After diracTraceSimplify ", res, FCDoControl->diTrVerbose]
		];

		FCPrint[2, "DiracTrick: diracTrickEval: Leaving.", FCDoControl->diTrVerbose];
		FCPrint[3, "DiracTrick: diracTrickEval: Leaving with ", res, FCDoControl->diTrVerbose];
		res
	];


(* ------------------------------------------------------------------------ *)

(*	g^0123 g^0123' : Order matrices with explicit indices as 0123	*)
diracology4Dim[b___,DiracGamma[ExplicitLorentzIndex[(i: 0|1|2|3)]], DiracGamma[ExplicitLorentzIndex[(j: 0|1|2|3)]], d___] :=
	- diracology4Dim[b, DiracGamma[ExplicitLorentzIndex[j]], DiracGamma[ExplicitLorentzIndex[i]], d]/; i>j;

diracology4Dim[]:=
	1;

(*	g^mu g_mu	*)
diracology4Dim[b___,DiracGamma[l_LorentzIndex], DiracGamma[l_LorentzIndex], d___] :=
	4 diracology4Dim[ b,d ];

(*	g^i g^i	*)
diracology4Dim[b___,DiracGamma[l_CartesianIndex], DiracGamma[l_CartesianIndex], d___] :=
	3 FeynCalc`Package`MetricS diracology4Dim[ b,d ];

(*	g^0 g^0	*)
diracology4Dim[b___,DiracGamma[ExplicitLorentzIndex[0]], DiracGamma[ExplicitLorentzIndex[0]], d___] :=
	diracology4Dim[ b,d ];

(*	g^123 g^123	*)
diracology4Dim[b___,DiracGamma[ExplicitLorentzIndex[(i: 1|2|3)]], DiracGamma[ExplicitLorentzIndex[(i: 1|2|3)]], d___] :=
	- diracology4Dim[ b,d ];

(*	g^0 g^i_1 ... g^i_n  -> (-1)^n g^i_1 ... g^i_n g^0	*)
diracology4Dim[ b___,  DiracGamma[ExplicitLorentzIndex[0]], Longest[ch : DiracGamma[(_CartesianIndex | _CartesianMomentum)]..], f___ ] :=
	(-1)^Length[{ch}] diracology4Dim[b,ch,DiracGamma[ExplicitLorentzIndex[0]],f];

(*	g^mu g^nu g_mu	*)
diracology4Dim[b___ , DiracGamma[c_LorentzIndex],
			DiracGamma[x: (_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)],
			DiracGamma[c_LorentzIndex], d___] :=
	- 2 diracology4Dim[b,DiracGamma[x], d];

(*	g^mu g^nu g^rho g_mu	*)
diracology4Dim[b___ , DiracGamma[c_LorentzIndex],
			DiracGamma[x1: (_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)],
			DiracGamma[x2: (_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)],
			DiracGamma[c_LorentzIndex], d___] :=
	4 FCUseCache[FCFastContract,{Pair[x1,x2] diracology4Dim[b, d]},{}];

(*	g^mu g^nu g^rho g^sigma g_mu	*)
diracology4Dim[b___ , DiracGamma[c_LorentzIndex],
			(dg1: DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]),
			(dg2: DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]),
			(dg3: DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]), DiracGamma[c_LorentzIndex], d___] :=
	- 2 diracology4Dim[b, dg3, dg2, dg1, d];

diracology4Dim[b___ , DiracGamma[c_LorentzIndex],
			(dg1: DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]),
			(dg2: DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]),
			(dg3: DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]),
			(dg4: DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]),
			DiracGamma[c_LorentzIndex], d___] :=
	2 diracology4Dim[b, dg3, dg2, dg1, dg4, d] + 2 diracology4Dim[b, dg4, dg1, dg2, dg3, d];

(*	g^mu g^nu_1 ... g^nu_i g_mu  -> -2 g^nu_i ... g^nu_1, where i is odd	*)
diracology4Dim[ b___,  DiracGamma[c_LorentzIndex],
		ch : DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)].., DiracGamma[c_LorentzIndex], f___ ] :=
	-2 diracology4Dim @@ Join[ {b},Reverse[{ch}],{f} ] /; OddQ[Length[{ch}]] && Length[{ch}]>3;

(*	g^mu g^nu_1 ... g^nu_i g_mu  -> 2 g^nu_i-1 ... g^nu_1 g^nu_i + 2 g^nu_i g^nu_1 ... g^nu_i-1, where i is even	*)
diracology4Dim[ b___,  DiracGamma[c_LorentzIndex],
		ch : DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex  | _CartesianMomentum)]..,
		end : DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)],
		DiracGamma[c_LorentzIndex], f___ ] :=
	(2 diracology4Dim @@ Join[ {b},Reverse[{ch}],{end}, {f}] + 2 diracology4Dim[ b,end,ch,f ])/; OddQ[Length[{ch}]]  && Length[{ch}]>4;

(*
	g^alpha g^nu_1 ... g^nu_i (where alpha is an explicit index)  -> essentially the same formula as for the slashes
	Important: No matrices with explicit indices among g^nu_1 ... g^nu! Chains where all matrices are explicit are handled by ordering
	the matrices as 0 1 2 3.
*)
diracology4Dim[ b___,  DiracGamma[ExplicitLorentzIndex[(i:0|1|2|3)]],
		Longest[ch : DiracGamma[(_LorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]..], f___ ] :=
		Block[{len=Length[{ch}],iVar},
			(-1)^len diracology4Dim[b,ch,DiracGamma[ExplicitLorentzIndex[i]],f] +
			Sum[2 (-1)^(iVar+1) Pair[ExplicitLorentzIndex[i],({ch}[[iVar]])[[1]]] * diracology4Dim@@Join[{b},Drop[{ch}, {iVar, iVar}],{f}],{iVar,1,len}]
		];

(*
diracology4Dim[ b___,  DiracGamma[ExplicitLorentzIndex[(i:0|1|2|3)]],
		ch : DiracGamma[(_LorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)].., DiracGamma[ExplicitLorentzIndex[(j:0|1|2|3)]], f___ ] :=
		Block[{len=Length[{ch}],iVar},
			(-1)^len diracology4Dim[b,ch,DiracGamma[ExplicitLorentzIndex[i]],DiracGamma[ExplicitLorentzIndex[j]],f] +
			Sum[2 (-1)^(iVar+1) Pair[ExplicitLorentzIndex[i],({ch}[[iVar]])[[1]]] * diracology4Dim@@Join[{b},Drop[{ch}, {iVar, iVar}],{DiracGamma[ExplicitLorentzIndex[j]],f}],{iVar,1,len}]
		];
*)
(*	g^i g^nu_1 ... g^nu_i g^i  -> s ( g^mu g^nu_1 ... g^nu_i g_mu - t g^0  g^nu_1 ... g^nu_i g^0   )	*)
diracology4Dim[ b___,  DiracGamma[c_CartesianIndex],
		ch : DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)].., DiracGamma[c_CartesianIndex], f___ ] :=
	(
	tmpli= LorentzIndex[$MU[Unique[]]];
	FeynCalc`Package`MetricS (diracology4Dim[b,DiracGamma[tmpli],ch,DiracGamma[tmpli],f] -
	FeynCalc`Package`MetricT diracology4Dim[b,DiracGamma[ExplicitLorentzIndex[0]],ch,DiracGamma[ExplicitLorentzIndex[0]],f] )
	);

(*	Slash(p).Slash(p)	*)
diracology4Dim[b___,DiracGamma[c_Momentum], DiracGamma[c_Momentum], d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracology4Dim[b,d];

(*	Slash(p) g^nu Slash(p)	*)
diracology4Dim[b___ , DiracGamma[c_Momentum], DiracGamma[x: (_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)],
		DiracGamma[c_Momentum], d___] :=
	- FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracology4Dim[b,DiracGamma[x], d] + 2 FCUseCache[FCFastContract,{Pair[c,x] diracology4Dim[b, DiracGamma[c], d]},{}];

(* Slash(p) g^nu_1 ... g^nu_n Slash(p), purely 4-dim; Eq 2.10 of R. Mertig, M. Boehm, A. Denner. Comp. Phys. Commun., 64 (1991) *)
diracology4Dim[b___, DiracGamma[c_Momentum],ch:DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]..,
			DiracGamma[c_Momentum],f___ ] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracology4Dim[b,ch,f]
		+ 2 Sum[(-1)^(iVar+1)FCUseCache[FCFastContract,{Pair[c,{ch}[[iVar,1]]] diracology4Dim@@Join[{b},
			Drop[{ch},{iVar, iVar}],{DiracGamma[c],f}]},{}],{iVar, 1,len}]
	]/; (Length[{ch}]>0);

(*	g^i g^i p^i p^j *)
diracology4Dim[b___,DiracGamma[c_CartesianMomentum], DiracGamma[c_CartesianMomentum], d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracology4Dim[b,d];

(*	g^i g^nu_1 ... g^nu_n g^i p^i p^j *)
diracology4Dim[b___, DiracGamma[c_CartesianMomentum],ch:DiracGamma[(_LorentzIndex | _ExplicitLorentzIndex | _Momentum | _CartesianIndex | _CartesianMomentum)]..,
			DiracGamma[c_CartesianMomentum],f___ ] :=
	Block[{len=Length[{ch}],iVar},
			(-1)^len FeynCalc`Package`MetricS CartesianPair[c,c] diracology4Dim[b,ch,f] +
				Sum[2 (-1)^(iVar+1) Pair[c,({ch}[[iVar]])[[1]]] *
				diracology4Dim@@Join[{b},Drop[{ch}, {iVar, iVar}],{DiracGamma[c],f}],{iVar,1,len}]
	];

(* ------------------------------------------------------------------------ *)


diracologyDDim[]:=
	1;

(*	g^mu g^mu	*)
diracologyDDim[b___,DiracGamma[l_LorentzIndex, dim_], DiracGamma[l_LorentzIndex, dim_], d___] :=
	dim diracologyDDim[ b,d ];

(*	g^i g^i	*)
diracologyDDim[b___,DiracGamma[l_CartesianIndex, dim_], DiracGamma[l_CartesianIndex, dim_], d___] :=
	(dim-1) FeynCalc`Package`MetricS diracologyDDim[ b,d ];


(*	g^mu g^nu g_mu	*)
diracologyDDim[b___ , DiracGamma[c_LorentzIndex, dim_],
			DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim_], dim_],
			DiracGamma[c_LorentzIndex, dim_], d___] :=
	(2 - dim) diracologyDDim[b,DiracGamma[x[y, dim], dim], d];

(*	g^mu g^nu g^rho g_mu *)
diracologyDDim[b___ , DiracGamma[c_LorentzIndex, dim_ ],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim_], dim_],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim_], dim_],
			DiracGamma[c_LorentzIndex, dim_], d___] :=
	(dim - 4) diracologyDDim[b,DiracGamma[x1[y1, dim], dim], DiracGamma[x2[y2, dim], dim], d] +
	4 FCUseCache[FCFastContract,{Pair[x1[y1,dim],x2[y2,dim]] diracologyDDim[b, d]},{}];

(*	g^mu g^nu g^rho g^sigma g_mu *)
diracologyDDim[b___ , DiracGamma[c_LorentzIndex, dim_],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim_], dim_],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim_], dim_],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim_], dim_],
			DiracGamma[c_LorentzIndex, dim_], d___] :=
	-(dim - 4) diracologyDDim[b, DiracGamma[x1[y1, dim], dim], DiracGamma[x2[y2, dim], dim], DiracGamma[x3[y3, dim], dim], d] -
	2 diracologyDDim[b, DiracGamma[x3[y3, dim], dim], DiracGamma[x2[y2, dim], dim], DiracGamma[x1[y1, dim], dim], d];

diracologyDDim[b___ , DiracGamma[c_LorentzIndex, dim_],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim_], dim_],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim_], dim_],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim_], dim_],
			DiracGamma[(x4: LorentzIndex | ExplicitLorentzIndex | Momentum)[y4_, dim_], dim_],
			DiracGamma[c_LorentzIndex, dim_], d___] :=
	(dim - 4) diracologyDDim[b, DiracGamma[x1[y1, dim], dim], DiracGamma[x2[y2, dim], dim], DiracGamma[x3[y3, dim], dim],
					DiracGamma[x4[y4, dim], dim], d] +
					2 diracologyDDim[b, DiracGamma[x3[y3, dim], dim], DiracGamma[x2[y2, dim], dim], DiracGamma[x1[y1, dim], dim],
							DiracGamma[x4[y4, dim], dim], d] +
				2 diracologyDDim[b, DiracGamma[x4[y4, dim], dim], DiracGamma[x1[y1, dim], dim], DiracGamma[x2[y2, dim], dim],
							DiracGamma[x3[y3, dim], dim], d];

(*	Simplification for g^mu g^nu_1 ... g^nu_n g_mu where all matrices are in D-dims;
	Eq 2.9 of R. Mertig, M. Boehm, A. Denner. Comp. Phys. Commun., 64 (1991)	*)
diracologyDDim2[ b___,DiracGamma[c_LorentzIndex,dim_],
	ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_],dim_]..,
	DiracGamma[c_LorentzIndex,dim_],f___ ] :=
	Block[ {iVar,jVar,len = Length[{ch}],dsTemp},
		FCUseCache[FCFastContract,{(-1)^len ( dim - 2 len ) dsTemp[b,ch,f] - 4 (-1)^len Sum[ (-1)^(jVar-iVar) *  Pair[{ch}[[iVar,1]],
			{ch}[[jVar,1]] ]*dsTemp@@Join[{b}, Delete[{ch}, {{iVar},{jVar}}], {f}],{iVar,1,len-1},{jVar,iVar+1,len}]},{}]/.
			dsTemp->diracologyDDim/. Pair[z__]/;!FreeQ[{z},Momentum] :> FCUseCache[ExpandScalarProduct,{Pair[z]},{}]
	] /; (Length[{ch}]>4);

(*	Slash(p) Slash(p)	*)
diracologyDDim[b___,DiracGamma[c_Momentum, dim_], DiracGamma[c_Momentum, dim_], d___] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracologyDDim[b,d];

(*	g^i g^i p^i p^j *)
diracologyDDim[b___,DiracGamma[c_CartesianMomentum, dim_], DiracGamma[c_CartesianMomentum, dim_], d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracologyDDim[b,d];

(*	Slash(p) g^nu Slash(p)	*)
diracologyDDim[b___ , DiracGamma[c_Momentum, dim_],
		DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim_] ,dim_],
		DiracGamma[c_Momentum, dim_], d___] :=
	- FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracologyDDim[b,DiracGamma[x[y, dim], dim], d] +
	2 FCUseCache[FCFastContract,{Pair[c,x[y,dim]] diracologyDDim[b, DiracGamma[c, dim], d]},{}];


(* Slash(p) g^nu_1 ... g^nu_n Slash(p), purely D-dim; Eq 2.10 of R. Mertig, M. Boehm, A. Denner. Comp. Phys. Commun., 64 (1991) *)
diracologyDDim2[b___, DiracGamma[c_Momentum, dim_], ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, _], dim_]..,
			DiracGamma[c_Momentum, dim_],f___] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len FCUseCache[ExpandScalarProduct,{Pair[c,c]},{}] diracologyDDim[b,ch,f]
		+ 2 Sum[(-1)^(iVar+1) FCUseCache[FCFastContract,{Pair[c,{ch}[[iVar,1]]] diracologyDDim@@Join[{b},Drop[{ch},{iVar, iVar}],{DiracGamma[c,dim],f}]},{}],
			{iVar, 1,len}]]/; (Length[{ch}]>0);

(* ------------------------------------------------------------------------ *)

diracTraceSimplify[] :=
	1;

diracTraceSimplify[___,0,___] :=
	0;

gaAlphaSq[0,0] =  1;
gaAlphaSq[1,1] = -1;
gaAlphaSq[2,2] = -1;
gaAlphaSq[3,3] = -1;

(* Trace cyclicity*)

ga67Mat[5,6]=
	DiracGamma[6];

ga67Mat[5,7]=
	DiracGamma[7];

ga67Mat[6,5]=
	DiracGamma[6];

ga67Mat[6,7]=
	0;

ga67Mat[7,5]=
	DiracGamma[7];

ga67Mat[7,6]=
	0;

ga67MatSign[5,6]=
	1;

ga67MatSign[5,7]=
	-1;

ga67MatSign[6,5]=
	1;

ga67MatSign[6,7]=
	0;

ga67MatSign[7,5]=
	-1;

ga67MatSign[7,6]=
	0;

diracTraceSimplify[b___,di_,c__] :=
	diracTraceSimplify[c,b, di]/; !FreeQ2[{di},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] &&
	FreeQ2[{b,c},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

diracTraceSimplify[DiracGamma[5],b___,DiracGamma[5]] :=
	diracTraceSimplify[b];

diracTraceSimplify[DiracGamma[(a:6|7)],b___,DiracGamma[(a:6|7)]] :=
	diracTraceSimplify[b,DiracGamma[a]];

diracTraceSimplify[DiracGamma[(h1:5|6|7)],b___,DiracGamma[(h2:5|6|7)]] :=
	ga67MatSign[h1,h2] diracTraceSimplify[b,ga67Mat[h1,h2]]/; h1=!=h2;

diracTraceSimplify[DOT[x__DiracGamma]]:=
	0/; FreeQ2[{x},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && OddQ[Length[{x}]]


diracTraceSimplify[DiracGamma[(arg1:_[_, dim1___]),dim1___], DiracGamma[(arg2:_[_, dim2___]), dim2___]] :=
	FCUseCache[ExpandScalarProduct,{Pair[arg1,arg2]},{}];

diracTraceSimplify[DOT[x___DiracGamma,DiracGamma[6|7],y___DiracGamma]]:=
	0/; OddQ[Length[{x,y}]] && FreeQ2[{x,y},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];


diracTraceSimplify[ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,___],___].., DiracGamma[5|6|7]] :=
	0/; OddQ[Length[{ch}]];

diracTraceSimplify[DOT[x___DiracGamma,DiracGamma[5],y___DiracGamma]]:=
	0/; Length[{x,y}]<4 && FreeQ2[{x,y},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}];


diracTraceSimplify[DiracGamma[_[_,___],___], DiracGamma[_[_,___],___], DiracGamma[5]] :=
	0;

diracTraceSimplify[a:DiracGamma[_[_,___],___], b:DiracGamma[_[_,___],___], DiracGamma[6|7]] :=
	diracTraceSimplify[a,b]/2;

diracTraceSimplify[cc_. DiracGamma[5]]:=
	0/; NonCommFreeQ[{cc}];

diracTraceSimplify[cc_. DiracGamma[6|7]]:=
	1/2 cc/; NonCommFreeQ[{cc}];

diracTraceSimplify[cc1_. DiracGamma[6] + cc2_. DiracGamma[7]]:=
	1/2 (cc1+cc2)/; NonCommFreeQ[{cc1,cc2}];


(* ------------------------------------------------------------------------ *)

ga67Val1[7]=
	-1;

ga67Val1[6]=
	1;

ga67Val2[6,6]=
	1;

ga67Val2[7,7]=
	1;

ga67Val2[6,7]=
	0;

ga67Val2[7,6]=
	0;

commonGamma5Properties[]:=
	1;

commonGamma5Properties[___,0,___]:=
	0;

commonGamma5Properties[b___, DiracGamma[5],cc1_. DiracGamma[5] + cc2_:0, c___] :=
	cc1 commonGamma5Properties[ b,c ] + cc2 commonGamma5Properties[ b,DiracGamma[5],c ]/; NonCommFreeQ[{cc1,cc2}];

commonGamma5Properties[b___ ,cc1_. DiracGamma[5] + cc2_:0,DiracGamma[5], c___] :=
	cc1 commonGamma5Properties[ b,c ] + cc2 commonGamma5Properties[ b,DiracGamma[5],c ]/; NonCommFreeQ[{cc1,cc2}];

commonGamma5Properties[b___, DiracGamma[5],cc1_. DiracGamma[(hh:6|7)] + cc2_:0, c___] :=
	ga67Val1[hh] cc1 commonGamma5Properties[b,DiracGamma[hh],c] + cc2 commonGamma5Properties[b,DiracGamma[5],c]/; NonCommFreeQ[{cc1,cc2}];

commonGamma5Properties[b___, cc1_. DiracGamma[(hh:6|7)] + cc2_:0,DiracGamma[5], c___] :=
	ga67Val1[hh] cc1 commonGamma5Properties[b,DiracGamma[hh],c] + cc2 commonGamma5Properties[b,DiracGamma[5],c]/; NonCommFreeQ[{cc1,cc2}];

commonGamma5Properties[b___, DiracGamma[(hh:6|7)], cc1_. DiracGamma[5] + cc2_:0, c___] :=
	ga67Val1[hh] cc1 commonGamma5Properties[b,DiracGamma[hh],c] + cc2 commonGamma5Properties[b,DiracGamma[hh],c]/; NonCommFreeQ[{cc1,cc2}];

commonGamma5Properties[b___, cc1_. DiracGamma[5] + cc2_:0,DiracGamma[(hh:6|7)], c___] :=
	ga67Val1[hh] cc1 commonGamma5Properties[b,DiracGamma[hh],c] + cc2 commonGamma5Properties[b,DiracGamma[hh],c]/; NonCommFreeQ[{cc1,cc2}];

commonGamma5Properties[b___, DiracGamma[(hh1:6|7)],cc1_. DiracGamma[(hh2:6|7)] + cc2_:0, c___] :=
	(ga67Val2[hh1,hh2] cc1 + cc2) commonGamma5Properties[b, DiracGamma[hh1], c]/; NonCommFreeQ[{cc1,cc2}];

commonGamma5Properties[b___, cc1_. DiracGamma[(hh1:6|7)] + cc2_:0,DiracGamma[(hh2:6|7)], c___] :=
	(ga67Val2[hh1,hh2] cc1 + cc2) commonGamma5Properties[b, DiracGamma[hh2], c]/; NonCommFreeQ[{cc1,cc2}];

commonGamma5Properties[b___,cc_. DiracGamma[6]+ cc_. DiracGamma[7],c___] :=
	cc commonGamma5Properties[b, c]/; NonCommFreeQ[{cc}];

(* ------------------------------------------------------------------------ *)

ga67Switch1[7]=
	6;

ga67Switch1[6]=
	7;

(* Move g^5 to the right: *)

chiralTrickAnticommuting4DimRight[b___,DiracGamma[5], c:DiracGamma[_[_]].. , d___] :=
	(-1)^Length[{c}] chiralTrickAnticommuting4DimRight[ b,c,DiracGamma[5],d];

chiralTrickAnticommuting4DimRight[b___, DiracGamma[5], (dd1_. (dg:DiracGamma[_[_]]) + dd2_:0 ),d___ ] :=
	chiralTrickAnticommuting4DimRight[b,(- dd1 dg + dd2), DiracGamma[5],d ]/; NonCommFreeQ[{dd1,dd2}];

chiralTrickAnticommuting4DimRight[b___,DiracGamma[(h:6|7)], c:DiracGamma[_[_]].. ,d___] :=
	chiralTrickAnticommuting4DimRight[ b,c,DiracGamma[h],d]/; EvenQ[Length[{c}]];

chiralTrickAnticommuting4DimRight[b___,DiracGamma[(h:6|7)], c:DiracGamma[_[_]].. ,d___] :=
	chiralTrickAnticommuting4DimRight[ b,c,DiracGamma[ga67Switch1[h]],d]/; OddQ[Length[{c}]];

chiralTrickAnticommuting4DimRight[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]), (dd1_. DiracGamma[6] + dd2_. DiracGamma[7]),d___ ] :=
	chiralTrickAnticommuting4DimRight[b, (cc1 dd1 DiracGamma[6] + cc2 dd2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4DimRight[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]), dd1_. (c:DiracGamma[_[_]]) + dd2_:0, d___ ] :=
	(
	dd1 chiralTrickAnticommuting4DimRight[b, c, (cc1 DiracGamma[7] + cc2 DiracGamma[6]), d] +
	dd2 chiralTrickAnticommuting4DimRight[b, (cc1 DiracGamma[6] + cc2 DiracGamma[7]), d]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4DimRight[b___, (cc1_. + cc2_. DiracGamma[5]), dd1_. (c:DiracGamma[_[_]]) + dd2_:0, d___ ] :=
	(
	cc1 chiralTrickAnticommuting4DimRight[b,  dd1 c + dd2 , d] -
	cc2 chiralTrickAnticommuting4DimRight[b,  dd1 c - dd2 , DiracGamma[5], d]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4DimRight[b___,(cc2_. DiracGamma[(h:6|7)] + cc1_:0),(dd1_. (dg:DiracGamma[_[_]]) + dd2_:0),d___ ] :=
	(
	dd1 chiralTrickAnticommuting4DimRight[b,dg, (cc1 + cc2 DiracGamma[ga67Switch1[h]]),d ] +
	dd2 chiralTrickAnticommuting4DimRight[b, (cc1 + cc2 DiracGamma[h]),d ]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}]

chiralTrickAnticommuting4DimRight[b___, DiracGamma[(h1:6|7)],DiracGamma[_[_]] + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[(h2:6|7)], c___] :=
	mass chiralTrickAnticommuting4DimRight[b, xy, DiracGamma[h2], c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommuting4DimRight[b___, DiracGamma[(h:6|7)],(dg:DiracGamma[_[_]]) + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[(h:6|7)], c___] :=
	chiralTrickAnticommuting4DimRight[b, dg, xy, DiracGamma[h], c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4DimRight[b___, DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_]]) + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[(h2:6|7)], c___] :=
	chiralTrickAnticommuting4DimRight[b, dg, xy, DiracGamma[h2], c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommuting4DimRight[b___, DiracGamma[(h:6|7)],DiracGamma[_[_]] + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[(h:6|7)], c___] :=
	mass chiralTrickAnticommuting4DimRight[b, xy, DiracGamma[h], c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4DimRight[b___,DiracGamma[(h:6|7)],DiracGamma[_[_]] + mass_:0, DiracGamma[(h:6|7)], c___] :=
	mass chiralTrickAnticommuting4DimRight[b, DiracGamma[h], c]/; NonCommFreeQ[mass];

chiralTrickAnticommuting4DimRight[b___,DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_]]) + mass_:0, DiracGamma[(h2:6|7)], c___] :=
	chiralTrickAnticommuting4DimRight[b, dg, DiracGamma[h2], c]/; NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommuting4DimRight[b___,DiracGamma[(h:6|7)],(dg:DiracGamma[_[_]]) + mass_:0, c___] :=
	(chiralTrickAnticommuting4DimRight[b, dg, DiracGamma[ga67Switch1[h]], c] +
	mass chiralTrickAnticommuting4DimRight[b, DiracGamma[h], c])/; NonCommFreeQ[mass];


(* Move g^5 to the left: *)

chiralTrickAnticommuting4DimLeft[b___, c:DiracGamma[_[_]].. , DiracGamma[5], d___] :=
	(-1)^Length[{c}] chiralTrickAnticommuting4DimLeft[ b,DiracGamma[5],c,d];

chiralTrickAnticommuting4DimLeft[b___, (dd1_. (dg:DiracGamma[_[_]]) + dd2_:0 ), DiracGamma[5], d___ ] :=
	chiralTrickAnticommuting4DimLeft[b, DiracGamma[5], (- dd1 dg + dd2), d ]/; NonCommFreeQ[{dd1,dd2}];

chiralTrickAnticommuting4DimLeft[b___, c:DiracGamma[_[_]].. , DiracGamma[(h:6|7)], d___] :=
	chiralTrickAnticommuting4DimLeft[ b,DiracGamma[h],c,d]/; EvenQ[Length[{c}]];

chiralTrickAnticommuting4DimLeft[b___, c:DiracGamma[_[_]].. , DiracGamma[(h:6|7)], d___] :=
	chiralTrickAnticommuting4DimLeft[ b,DiracGamma[ga67Switch1[h]],c,d]/; OddQ[Length[{c}]];

chiralTrickAnticommuting4DimLeft[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]), (dd1_. DiracGamma[6] + dd2_. DiracGamma[7]),d___ ] :=
	chiralTrickAnticommuting4DimLeft[b, (cc1 dd1 DiracGamma[6] + cc2 dd2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4DimLeft[b___, dd1_. (c:DiracGamma[_[_]]) + dd2_:0, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]), d___ ] :=
	(
	dd1 chiralTrickAnticommuting4DimLeft[b, (cc1 DiracGamma[7] + cc2 DiracGamma[6]), c, d] +
	dd2 chiralTrickAnticommuting4DimLeft[b, (cc1 DiracGamma[6] + cc2 DiracGamma[7]), d]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4DimLeft[b___, dd1_. (c:DiracGamma[_[_]]) + dd2_:0, (cc1_. + cc2_. DiracGamma[5]), d___ ] :=
	(
	cc1 chiralTrickAnticommuting4DimLeft[b,  dd1 c + dd2 , d] -
	cc2 chiralTrickAnticommuting4DimLeft[b,  DiracGamma[5], dd1 c - dd2, d]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommuting4DimLeft[b___,(dd1_. (dg:DiracGamma[_[_]]) + dd2_:0),(cc2_. DiracGamma[(h:6|7)] + cc1_:0),d___ ] :=
	(
	dd1 chiralTrickAnticommuting4DimLeft[b,(cc1 + cc2 DiracGamma[ga67Switch1[h]]), dg, d ] +
	dd2 chiralTrickAnticommuting4DimLeft[b, (cc1 + cc2 DiracGamma[h]),d ]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}]

chiralTrickAnticommuting4DimLeft[b___, DiracGamma[(h1:6|7)],DiracGamma[_[_]] + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[(h2:6|7)], c___] :=
	mass chiralTrickAnticommuting4DimLeft[b, DiracGamma[h1], xy, c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommuting4DimLeft[b___, DiracGamma[(h:6|7)],(dg:DiracGamma[_[_]]) + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[(h:6|7)], c___] :=
	chiralTrickAnticommuting4DimLeft[b, DiracGamma[h], dg, xy, c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4DimLeft[b___, DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_]]) + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[(h2:6|7)], c___] :=
	chiralTrickAnticommuting4DimLeft[b, DiracGamma[h1], dg, xy, c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommuting4DimLeft[b___, DiracGamma[(h:6|7)],DiracGamma[_[_]] + mass_:0, xy:DiracGamma[_[_]].. , DiracGamma[(h:6|7)], c___] :=
	mass chiralTrickAnticommuting4DimLeft[b, DiracGamma[h], xy, c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommuting4DimLeft[b___,DiracGamma[(h:6|7)],DiracGamma[_[_]] + mass_:0, DiracGamma[(h:6|7)], c___] :=
	mass chiralTrickAnticommuting4DimLeft[b, DiracGamma[h], c]/; NonCommFreeQ[mass];

chiralTrickAnticommuting4DimLeft[b___,DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_]]) + mass_:0, DiracGamma[(h2:6|7)], c___] :=
	chiralTrickAnticommuting4DimLeft[b, DiracGamma[h1], dg, c]/; NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommuting4DimLeft[b___,(dg:DiracGamma[_[_]]) + mass_:0, DiracGamma[(h:6|7)], c___] :=
	(chiralTrickAnticommuting4DimLeft[b, DiracGamma[ga67Switch1[h]], dg, c] +
	mass chiralTrickAnticommuting4DimLeft[b, DiracGamma[h], c])/; NonCommFreeQ[mass];


(* ------------------------------------------------------------------------ *)

(* Move g^5 to the right: *)

chiralTrickAnticommutingDDimRight[b___,DiracGamma[5], c:DiracGamma[_[_,_],_].. , d___] :=
	(-1)^Length[{c}] chiralTrickAnticommutingDDimRight[ b,c,DiracGamma[5],d];

chiralTrickAnticommutingDDimRight[b___, DiracGamma[5], (dd1_. (dg:DiracGamma[_[_,_],_]) + dd2_:0 ),d___ ] :=
	chiralTrickAnticommutingDDimRight[b,(- dd1 dg + dd2), DiracGamma[5],d ]/; NonCommFreeQ[{dd1,dd2}];

chiralTrickAnticommutingDDimRight[b___,DiracGamma[(h:6|7)], c:DiracGamma[_[_,_],_].. ,d___] :=
	chiralTrickAnticommutingDDimRight[ b,c,DiracGamma[h],d]/; EvenQ[Length[{c}]];

chiralTrickAnticommutingDDimRight[b___,DiracGamma[(h:6|7)], c:DiracGamma[_[_,_],_].. ,d___] :=
	chiralTrickAnticommutingDDimRight[ b,c,DiracGamma[ga67Switch1[h]],d]/; OddQ[Length[{c}]];

chiralTrickAnticommutingDDimRight[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]), (dd1_. DiracGamma[6] + dd2_. DiracGamma[7]),d___ ] :=
	chiralTrickAnticommutingDDimRight[b, (cc1 dd1 DiracGamma[6] + cc2 dd2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDimRight[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]), dd1_. (c:DiracGamma[_[_,_],_]) + dd2_:0, d___ ] :=
	(
	dd1 chiralTrickAnticommutingDDimRight[b, c, (cc1 DiracGamma[7] + cc2 DiracGamma[6]), d] +
	dd2 chiralTrickAnticommutingDDimRight[b, (cc1 DiracGamma[6] + cc2 DiracGamma[7]), d]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDimRight[b___, (cc1_. + cc2_. DiracGamma[5]), dd1_. (c:DiracGamma[_[_,_],_]) + dd2_:0, d___ ] :=
	(
	cc1 chiralTrickAnticommutingDDimRight[b,  dd1 c + dd2 , d] - cc2 chiralTrickAnticommutingDDimRight[b,  dd1 c - dd2 , DiracGamma[5], d]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDimRight[b___, (cc2_. DiracGamma[(h:6|7)] + cc1_:0),(dd1_. (dg:DiracGamma[_[_,_],_]) + dd2_:0),d___ ] :=
	(
	dd1 chiralTrickAnticommutingDDimRight[b,dg, (cc1 + cc2 DiracGamma[ga67Switch1[h]]),d ] +
	dd2 chiralTrickAnticommutingDDimRight[b, (cc1 + cc2 DiracGamma[h]),d ]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}]

chiralTrickAnticommutingDDimRight[b___, DiracGamma[(h1:6|7)],DiracGamma[_[_,_],_] + mass_:0, xy:DiracGamma[_[_,_],_].. , DiracGamma[(h2:6|7)], c___] :=
	mass chiralTrickAnticommutingDDimRight[b, xy, DiracGamma[h2], c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommutingDDimRight[b___, DiracGamma[(h:6|7)],(dg:DiracGamma[_[_,_],_]) + mass_:0, xy:DiracGamma[_[_,_],_].. , DiracGamma[(h:6|7)], c___] :=
	chiralTrickAnticommutingDDimRight[b, dg, xy, DiracGamma[h], c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDimRight[b___, DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_,_],_]) + mass_:0, xy:DiracGamma[_[_] ].. , DiracGamma[(h2:6|7)], c___] :=
	chiralTrickAnticommutingDDimRight[b, dg, xy, DiracGamma[h2], c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommutingDDimRight[b___, DiracGamma[(h:6|7)],DiracGamma[_[_,_],_] + mass_:0, xy:DiracGamma[_[_] ].. , DiracGamma[(h:6|7)], c___] :=
	mass chiralTrickAnticommutingDDimRight[b, xy, DiracGamma[h], c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDimRight[b___,DiracGamma[(h:6|7)],DiracGamma[_[_,_],_] + mass_:0, DiracGamma[(h:6|7)], c___] :=
	mass chiralTrickAnticommutingDDimRight[b, DiracGamma[h], c]/; NonCommFreeQ[mass];

chiralTrickAnticommutingDDimRight[b___,DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_,_],_]) + mass_:0, DiracGamma[(h2:6|7)], c___] :=
	chiralTrickAnticommutingDDimRight[b, dg, DiracGamma[h2], c]/; NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommutingDDimRight[b___,DiracGamma[(h:6|7)],(dg:DiracGamma[_[_,_],_]) + mass_:0, c___] :=
	(chiralTrickAnticommutingDDimRight[b, dg, DiracGamma[ga67Switch1[h]], c] +
	mass chiralTrickAnticommutingDDimRight[b, DiracGamma[h], c])/; NonCommFreeQ[mass];

(* Move g^5 to the left: *)

chiralTrickAnticommutingDDimLeft[b___, c:DiracGamma[_[_,_],_].. , DiracGamma[5], d___] :=
	(-1)^Length[{c}] chiralTrickAnticommutingDDimLeft[ b,DiracGamma[5],c,d];

chiralTrickAnticommutingDDimLeft[b___, (dd1_. (dg:DiracGamma[_[_,_],_]) + dd2_:0 ), DiracGamma[5], d___ ] :=
	chiralTrickAnticommutingDDimLeft[b, DiracGamma[5], (- dd1 dg + dd2), d ]/; NonCommFreeQ[{dd1,dd2}];

chiralTrickAnticommutingDDimLeft[b___, c:DiracGamma[_[_,_],_].. , DiracGamma[(h:6|7)], d___] :=
	chiralTrickAnticommutingDDimLeft[ b,DiracGamma[h],c,d]/; EvenQ[Length[{c}]];

chiralTrickAnticommutingDDimLeft[b___, c:DiracGamma[_[_,_],_].. , DiracGamma[(h:6|7)], d___] :=
	chiralTrickAnticommutingDDimLeft[ b,DiracGamma[ga67Switch1[h]],c,d]/; OddQ[Length[{c}]];

chiralTrickAnticommutingDDimLeft[b___, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]), (dd1_. DiracGamma[6] + dd2_. DiracGamma[7]),d___ ] :=
	chiralTrickAnticommutingDDimLeft[b, (cc1 dd1 DiracGamma[6] + cc2 dd2 DiracGamma[7]),d ]/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDimLeft[b___, dd1_. (c:DiracGamma[_[_,_],_]) + dd2_:0, (cc1_. DiracGamma[6] + cc2_. DiracGamma[7]), d___ ] :=
	(
	dd1 chiralTrickAnticommutingDDimLeft[b, (cc1 DiracGamma[7] + cc2 DiracGamma[6]), c, d] +
	dd2 chiralTrickAnticommutingDDimLeft[b, (cc1 DiracGamma[6] + cc2 DiracGamma[7]), d]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDimLeft[b___, dd1_. (c:DiracGamma[_[_,_],_]) + dd2_:0, (cc1_. + cc2_. DiracGamma[5]), d___ ] :=
	(
	cc1 chiralTrickAnticommutingDDimLeft[b,  dd1 c + dd2 , d] -
	cc2 chiralTrickAnticommutingDDimLeft[b,  DiracGamma[5], dd1 c - dd2 , d]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}];

chiralTrickAnticommutingDDimLeft[b___, (dd1_. (dg:DiracGamma[_[_,_],_]) + dd2_:0), (cc2_. DiracGamma[(h:6|7)] + cc1_:0), d___ ] :=
	(
	dd1 chiralTrickAnticommutingDDimLeft[b, (cc1 + cc2 DiracGamma[ga67Switch1[h]]), dg,d ] +
	dd2 chiralTrickAnticommutingDDimLeft[b, (cc1 + cc2 DiracGamma[h]),d ]
	)/; NonCommFreeQ[{cc1,cc2,dd1,dd2}]

chiralTrickAnticommutingDDimLeft[b___, DiracGamma[(h1:6|7)],DiracGamma[_[_,_],_] + mass_:0, xy:DiracGamma[_[_,_],_].. , DiracGamma[(h2:6|7)], c___] :=
	mass chiralTrickAnticommutingDDimLeft[b, DiracGamma[h1], xy, c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommutingDDimLeft[b___, DiracGamma[(h:6|7)],(dg:DiracGamma[_[_,_],_]) + mass_:0, xy:DiracGamma[_[_,_],_].. , DiracGamma[(h:6|7)], c___] :=
	chiralTrickAnticommutingDDimLeft[b, DiracGamma[h], dg, xy,  c]/; OddQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDimLeft[b___, DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_,_],_]) + mass_:0, xy:DiracGamma[_[_] ].. , DiracGamma[(h2:6|7)], c___] :=
	chiralTrickAnticommutingDDimLeft[b, DiracGamma[h1], dg, xy, c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommutingDDimLeft[b___, DiracGamma[(h:6|7)],DiracGamma[_[_,_],_] + mass_:0, xy:DiracGamma[_[_] ].. , DiracGamma[(h:6|7)], c___] :=
	mass chiralTrickAnticommutingDDimLeft[b, DiracGamma[h], xy, c]/; EvenQ[Length[{xy}]] && NonCommFreeQ[mass];

chiralTrickAnticommutingDDimLeft[b___,DiracGamma[(h:6|7)],DiracGamma[_[_,_],_] + mass_:0, DiracGamma[(h:6|7)], c___] :=
	mass chiralTrickAnticommutingDDimLeft[b, DiracGamma[h], c]/; NonCommFreeQ[mass];

chiralTrickAnticommutingDDimLeft[b___,DiracGamma[(h1:6|7)],(dg:DiracGamma[_[_,_],_]) + mass_:0, DiracGamma[(h2:6|7)], c___] :=
	chiralTrickAnticommutingDDimLeft[b, DiracGamma[h1], dg, c]/; NonCommFreeQ[mass] && h1=!=h2;

chiralTrickAnticommutingDDimLeft[b___,(dg:DiracGamma[_[_,_],_]) + mass_:0, DiracGamma[(h:6|7)], c___] :=
	(chiralTrickAnticommutingDDimLeft[b, DiracGamma[ga67Switch1[h]], dg, c] +
	mass chiralTrickAnticommutingDDimLeft[b, DiracGamma[h], c])/; NonCommFreeQ[mass];

(* ------------------------------------------------------------------------ *)

chiralTrickLarin[b___,((dg:DiracGamma[_[_,_], dim_]) + mass_:0),DiracGamma[5],d__] :=
Block[{li1,li2,li3},
	{li1,li2,li3} = LorentzIndex[#,dim]& /@ Unique[{"dtlarLia","dtlarLib","dtlarLic"}];
		I/6 $LeviCivitaSign Eps[dg[[1]], li1, li2, li3] chiralTrickLarin[b,DiracGamma[li1,dim],
			DiracGamma[li2,dim],DiracGamma[li3,dim],d] + mass chiralTrickLarin[b,DiracGamma[5],d]
	]/; !FreeQ2[{d},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && NonCommFreeQ[mass];

chiralTrickLarin[b___,((dg:DiracGamma[_[_,_], dim_]) + mass_:0),DiracGamma[6],d__] :=
Block[{li1,li2,li3},
	{li1,li2,li3} = LorentzIndex[#,dim]& /@ Unique[{"dtlarLia","dtlarLib","dtlarLic"}];
		mass chiralTrickLarin[b,DiracGamma[6],d] +
		1/2 chiralTrickLarin[b,dg,d] +
		I/12 $LeviCivitaSign Eps[dg[[1]], li1, li2, li3] chiralTrickLarin[b,DiracGamma[li1,dim],DiracGamma[li2,dim],DiracGamma[li3,dim],d]
	]/; !FreeQ2[{d},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && NonCommFreeQ[mass];

chiralTrickLarin[b___,((dg:DiracGamma[_[_,_], dim_]) + mass_:0),DiracGamma[7],d__] :=
Block[{li1,li2,li3},
	{li1,li2,li3} = LorentzIndex[#,dim]& /@ Unique[{"dtlarLia","dtlarLib","dtlarLic"}];
		mass chiralTrickLarin[b,DiracGamma[7],d] +
		1/2 chiralTrickLarin[b,dg,d] +
		I/12 $LeviCivitaSign Eps[dg[[1]], li1, li2, li3] chiralTrickLarin[b,DiracGamma[li1,dim],DiracGamma[li2,dim],DiracGamma[li3,dim],d]
	]/; !FreeQ2[{d},{DiracGamma[5],DiracGamma[6],DiracGamma[7]}] && NonCommFreeQ[mass];

(* ------------------------------------------------------------------------ *)

(* Move g^5 to the right: *)

gamma5MoveBMHVRight[]:=
	1;

gamma5MoveBMHVRight[b___, DiracGamma[(h:5|6|7)],DiracGamma[x_[y__],d_Symbol -4] + mass_:0,f___] :=
	gamma5MoveBMHVRight[ b,mass + DiracGamma[x[y],d-4],DiracGamma[h],f ]/; NonCommFreeQ[mass];

gamma5MoveBMHVRight[b___,DiracGamma[5],DiracGamma[x_[y__],d_Symbol], f___] :=
	2 gamma5MoveBMHVRight[b,DiracGamma[x[y],d-4],DiracGamma[5],f] -
	gamma5MoveBMHVRight[b,DiracGamma[x[y],d],DiracGamma[5],f];

gamma5MoveBMHVRight[b___,DiracGamma[5],DiracGamma[x_[y__],d_Symbol] + mass_, f___] :=
	2 gamma5MoveBMHVRight[b,DiracGamma[x[y],d-4],DiracGamma[5],f] +
	gamma5MoveBMHVRight[b,(mass - DiracGamma[x[y],d]),DiracGamma[5],f]/; NonCommFreeQ[mass];

gamma5MoveBMHVRight[b___,DiracGamma[(h:6|7)],DiracGamma[x_[y_,_],d_Symbol] + mass_:0, f___] :=
	(
	gamma5MoveBMHVRight[b,DiracGamma[x[y,d-4],d-4] + mass,DiracGamma[h],f] +
	gamma5MoveBMHVRight[b,DiracGamma[x[y]],DiracGamma[ga67Switch1[h]],f]
	)/; NonCommFreeQ[mass];

(* Move g^5 to the left: *)

gamma5MoveBMHVLeft[]:=
	1;

gamma5MoveBMHVLeft[b___, DiracGamma[x_[y__],d_Symbol -4] + mass_:0, DiracGamma[(h:5|6|7)], f___] :=
	gamma5MoveBMHVLeft[ b,DiracGamma[h], mass + DiracGamma[x[y],d-4],f ]/; NonCommFreeQ[mass];

gamma5MoveBMHVLeft[b___,DiracGamma[x_[y__],d_Symbol], DiracGamma[5], f___] :=
	2 gamma5MoveBMHVLeft[b,DiracGamma[5],DiracGamma[x[y],d-4],f] -
	gamma5MoveBMHVLeft[b,DiracGamma[5],DiracGamma[x[y],d],f]

gamma5MoveBMHVLeft[b___,DiracGamma[x_[y__],d_Symbol] + mass_, DiracGamma[5], f___] :=
	2 gamma5MoveBMHVLeft[b,DiracGamma[5],DiracGamma[x[y],d-4],f] +
	gamma5MoveBMHVLeft[b,DiracGamma[5],(mass - DiracGamma[x[y],d]),f]/; NonCommFreeQ[mass];

gamma5MoveBMHVLeft[b___,DiracGamma[x_[y_,_],d_Symbol] + mass_:0, DiracGamma[(h:6|7)], f___] :=
	(
	gamma5MoveBMHVLeft[b,DiracGamma[h],DiracGamma[x[y,d-4],d-4] + mass,f] +
	gamma5MoveBMHVLeft[b,DiracGamma[ga67Switch1[h]],DiracGamma[x[y]],f]
	)/; NonCommFreeQ[mass];

(* ------------------------------------------------------------------------ *)

diracologyBMHV1[]:=
	1;

diracologyBMHV2[]:=
	1;

(* Simplification for g^mu ... g_mu where the first and the last
	matrix are in different dimensions                                         *)

(* D and 4  -> 4 *)
diracologyBMHV1[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], d:DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_]], f___ ] :=
	diracologyBMHV1[b, DiracGamma[LorentzIndex[c]], d, DiracGamma[LorentzIndex[c]], f];

(* D-1 and 3  -> 3 *)
diracologyBMHV1[ b___, DiracGamma[CartesianIndex[c_, dimD_Symbol-1], dimD_Symbol], d:DiracGamma[__].. ,
	DiracGamma[CartesianIndex[c_]], f___ ] :=
	diracologyBMHV1[b, DiracGamma[CartesianIndex[c]], d, DiracGamma[CartesianIndex[c]], f];

(* 4 and D -> 4 *)
diracologyBMHV1[ b___, DiracGamma[LorentzIndex[c_]], d:DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], f___ ] :=
	diracologyBMHV1[b, DiracGamma[LorentzIndex[c]], d, DiracGamma[LorentzIndex[c]], f];

(* 3 and D-1 -> 3 *)
diracologyBMHV1[ b___, DiracGamma[CartesianIndex[c_]], d:DiracGamma[__].. ,
	DiracGamma[CartesianIndex[c_, dimD_Symbol-1], dimD_Symbol], f___ ] :=
	diracologyBMHV1[b, DiracGamma[CartesianIndex[c]], d, DiracGamma[CartesianIndex[c]], f];

(* D and D-4 -> D-4 *)
diracologyBMHV1[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol], dimD_Symbol], d:DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], f___ ] :=
	diracologyBMHV1[b,DiracGamma[LorentzIndex[c, dimD-4], dimD-4], d,DiracGamma[LorentzIndex[c, dimD-4], dimD-4], f];

(* D-1 and D-4 -> D-4 *)
diracologyBMHV1[ b___, DiracGamma[CartesianIndex[c_, dimD_Symbol-1], dimD_Symbol], d:DiracGamma[__].. ,
	DiracGamma[CartesianIndex[c_, dimD_Symbol-4], dimD_Symbol-4], f___ ] :=
	diracologyBMHV1[b,DiracGamma[CartesianIndex[c, dimD-4], dimD-4], d,DiracGamma[CartesianIndex[c, dimD-4], dimD-4], f];

(* D-4 and D -> D-4 *)
diracologyBMHV1[ b___, DiracGamma[LorentzIndex[c_, dimD_Symbol-4], dimD_Symbol-4], d:DiracGamma[__].. ,
	DiracGamma[LorentzIndex[c_, dimD_Symbol],dimD_Symbol],f___ ] :=
	diracologyBMHV1[b,DiracGamma[LorentzIndex[c, dimD-4], dimD-4], d, DiracGamma[LorentzIndex[c, dimD-4], dimD-4],f];

(* D-4 and D-1 -> D-4 *)
diracologyBMHV1[ b___, DiracGamma[CartesianIndex[c_, dimD_Symbol-4], dimD_Symbol-4], d:DiracGamma[__].. ,
	DiracGamma[CartesianIndex[c_, dimD_Symbol-1],dimD_Symbol],f___ ] :=
	diracologyBMHV1[b,DiracGamma[CartesianIndex[c, dimD-4], dimD-4], d, DiracGamma[CartesianIndex[c, dimD-4], dimD-4],f];

(* 4 and D-4 -> 0 *)
diracologyBMHV1[ ___, DiracGamma[(h:LorentzIndex|CartesianIndex)[c_, dimD_Symbol-4], dimD_Symbol-4], DiracGamma[__].. ,
	DiracGamma[(h:LorentzIndex|CartesianIndex)[c_]], ___ ] :=
	0;

(* D-4 and 4 -> 0 *)
diracologyBMHV1[ ___, DiracGamma[(h:LorentzIndex|CartesianIndex)[c_]], DiracGamma[__].. ,
	DiracGamma[(h:LorentzIndex|CartesianIndex)[c_, dimD_Symbol-4], dimD_Symbol-4], ___ ] :=
	0;

(* Contractions of neighbouring Dirac matrices in D, 4 and D-4 dimensions     *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___,DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_:  4],
		DiracGamma[LorentzIndex[c_, dim2_ : 4], dim2_: 4], d___] :=
	(PairContract[LorentzIndex[c, dim1],LorentzIndex[c, dim2]]/. PairContract->Pair) diracologyBMHV2[ b,d ];

diracologyBMHV2[b___,DiracGamma[CartesianIndex[c_, dim1_ : 3], ___],
		DiracGamma[CartesianIndex[c_, dim2_ : 3], ___], d___] :=
	(FeynCalc`Package`MetricS CartesianPairContract[CartesianIndex[c, dim1],CartesianIndex[c, dim2]]/. CartesianPairContract->CartesianPair) diracologyBMHV2[ b,d ];

(* Simplifications for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in 4 and
	g^nu_i are in D-4 dimensions or vice versa.                                *)
(* ------------------------------------------------------------------------ *)

(* 4, (... D-4 ... ), 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_]],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol-4] , dim_Symbol-4]..,
			DiracGamma[LorentzIndex[c_]], d___] :=
	4 (-1)^Length[{ch}] diracologyBMHV2[b,ch, d];

(* D-4, (... 4 ... ), D-4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4],
			ch : DiracGamma[((LorentzIndex | ExplicitLorentzIndex | Momentum)[_])]..,
			DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
	(dim - 4) (-1)^Length[{ch}] diracologyBMHV2[b,ch, d];

(* D, (... 4 ... ), D *)
(* Simplification for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in D and
	g^nu_i are in 4 dimensions. Applies for n>4, since for n<=4 we have
	explicit expressions in the code. However, if g^nu_i contain g^0, we
	apply this formula independently of the length of the chain.	*)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
			DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol], d___] :=
	diracologyBMHV2[b,DiracGamma[LorentzIndex[c]], ch, DiracGamma[LorentzIndex[c]], d] +
	(dim - 4) (-1)^Length[{ch}] diracologyBMHV2[b,ch, d]/; Length[{ch}]>4;

(* D, (... D-4 ... ), D *)
(* Simplification for g^mu g^nu_1 ... g^nu_n g_mu where g^mu is in D and
	g^nu_i are in D-4 dimensions. Applies for n>4, since for n<=4 we have
	explicit expressions in the code                                        *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,
			dim_Symbol-4],dim_Symbol-4]..,
			DiracGamma[LorentzIndex[c_, dim_Symbol], dim_Symbol], d___] :=
	diracologyBMHV2[b,DiracGamma[LorentzIndex[c,dim-4],dim-4], ch, DiracGamma[LorentzIndex[c,dim-4],dim-4], d] +
	4 (-1)^Length[{ch}] diracologyBMHV2[b,ch, d]/; Length[{ch}]>4;


(* Evaluation of g^mu g^nu g_mu for Dirac matrices in different
	dimensions                                                                *)
(* ------------------------------------------------------------------------ *)


(* D, D, D or 4, 4, 4 or D-4, D-4, D-4
	or D, D-4, D or D, 4, D *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim2_ : 4] ,dim2_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	(2 - dim1) diracologyBMHV2[b,DiracGamma[x[y, dim2], dim2], d]/;
					(dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
					MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
					MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D-4 or 4, D, 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim2_Symbol], dim2_Symbol],
			DiracGamma[LorentzIndex[c_, dim1_ : 4],  dim1_ : 4], d___] :=
	- dim1 diracologyBMHV2[b,DiracGamma[x[y,dim2],dim2],d] +
	2 diracologyBMHV2[b,DiracGamma[x[y,dim1],dim1],d]/; (dim1 === dim2-4 || dim1 ===4);

(* Evaluation of g^mu g^nu g^rho g_mu for Dirac matrices in different
	dimensions                                                                *)
(* ------------------------------------------------------------------------ *)


(* D, D, D, D or 4, 4, 4, 4 or D-4, D-4, D-4, D-4
	or D, D-4, D-4, D or D, 4, 4, D *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_ : 4] ,dim2_ : 4],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_ : 4] ,dim2_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	(dim1 - 4) diracologyBMHV2[b,DiracGamma[x1[y1, dim2], dim2],
					DiracGamma[x2[y2, dim2], dim2], d] +
					4 FCUseCache[FCFastContract,{Pair[x1[y1,dim1],x2[y2,dim2]] diracologyBMHV2[b, d]},{}]/;
					(dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
					MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
					MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D, D-4 or 4, D, D, 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_Symbol], dim2_Symbol],
			DiracGamma[LorentzIndex[c_, dim1_: 4], dim1_ : 4], d___] :=
	dim1 diracologyBMHV2[b,DiracGamma[x1[y1, dim2], dim2],DiracGamma[x2[y2, dim2], dim2], d] -
	2 diracologyBMHV2[b,DiracGamma[x1[y1, dim1], dim1],DiracGamma[x2[y2, dim2], dim2], d] +
	2 diracologyBMHV2[b,DiracGamma[x2[y2, dim1], dim1],DiracGamma[x1[y1, dim2], dim2], d]/;
	(dim1 === dim2-4 || dim1 ===4);


(* Evaluation of g^mu g^nu g^rho g^sigma g_mu for Dirac matrices in different
	dimensions                                                                *)
(* ------------------------------------------------------------------------ *)

(* D, D, D, D, D or 4, 4, 4, 4, 4 or D-4, D-4, D-4, D-4, D-4
	or D, D-4, D-4, D-4, D or D, 4, 4, 4, D *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	-(dim1 - 4) diracologyBMHV2[b, DiracGamma[x1[y1, dim2], dim2],
						DiracGamma[x2[y2, dim2], dim2],
						DiracGamma[x3[y3, dim2], dim2], d] -
					2 diracologyBMHV2[b, DiracGamma[x3[y3, dim2], dim2],
							DiracGamma[x2[y2, dim2], dim2],
							DiracGamma[x1[y1, dim2], dim2], d]/;
							(dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
							MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
							MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D, D, D-4 or 4, D, D, D, 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_Symbol], dim2_Symbol],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	-dim1 diracologyBMHV2[b, DiracGamma[x1[y1, dim2], dim2],
				DiracGamma[x2[y2, dim2], dim2],
				DiracGamma[x3[y3, dim2], dim2], d] +
	2 diracologyBMHV2[b, DiracGamma[x1[y1, dim1], dim1],
			DiracGamma[x2[y2, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2], d] -
	2 diracologyBMHV2[b, DiracGamma[x2[y2, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2], d] +
	2 diracologyBMHV2[b, DiracGamma[x3[y3, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x2[y2, dim2], dim2], d]/;
			(dim1 === dim2-4 || dim1 ===4);

(* Evaluation of g^mu g^nu g^rho g^sigma g^tau g_mu for Dirac matrices
	in different dimensions                                                    *)
(* ------------------------------------------------------------------------ *)

(* D, D, D, D, D, D or 4, 4, 4, 4, 4, 4 or D-4, D-4, D-4, D-4, D-4, D-4
	or D, D-4, D-4, D-4, D-4, D or D, 4, 4, 4, 4, D *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[(x4: LorentzIndex | ExplicitLorentzIndex | Momentum)[y4_, dim2_ : 4] , dim2_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	(dim1 - 4) diracologyBMHV2[b, DiracGamma[x1[y1, dim2], dim2],
					DiracGamma[x2[y2, dim2], dim2],
					DiracGamma[x3[y3, dim2], dim2],
					DiracGamma[x4[y4, dim2], dim2], d] +
					2 diracologyBMHV2[b, DiracGamma[x3[y3, dim2], dim2],
							DiracGamma[x2[y2, dim2], dim2],
							DiracGamma[x1[y1, dim2], dim2],
							DiracGamma[x4[y4, dim2], dim2], d] +
				2 diracologyBMHV2[b, DiracGamma[x4[y4, dim2], dim2],
							DiracGamma[x1[y1, dim2], dim2],
							DiracGamma[x2[y2, dim2], dim2],
							DiracGamma[x3[y3, dim2], dim2], d]/;
							(dim1===dim2 || dim2 === dim1-4 || dim2 ===4) &&
							MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
							MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* D-4, D, D, D, D, D-4 or 4, D, D, D, D, 4 *)
diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],
			DiracGamma[(x1: LorentzIndex | ExplicitLorentzIndex | Momentum)[y1_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x3: LorentzIndex | ExplicitLorentzIndex | Momentum)[y3_, dim2_Symbol], dim2_Symbol],
			DiracGamma[(x4: LorentzIndex | ExplicitLorentzIndex | Momentum)[y4_, dim2_Symbol], dim2_Symbol],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	dim1  diracologyBMHV2[b, DiracGamma[x1[y1, dim2], dim2],
				DiracGamma[x2[y2, dim2], dim2],
				DiracGamma[x3[y3, dim2], dim2],
				DiracGamma[x4[y4, dim2], dim2], d] -
	2 diracologyBMHV2[b, DiracGamma[x1[y1, dim1], dim1],
			DiracGamma[x2[y2, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2],
			DiracGamma[x4[y4, dim2], dim2], d] +
	2 diracologyBMHV2[b, DiracGamma[x2[y2, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2],
			DiracGamma[x4[y4, dim2], dim2], d] -
	2 diracologyBMHV2[b, DiracGamma[x3[y3, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x2[y2, dim2], dim2],
			DiracGamma[x4[y4, dim2], dim2], d] +
	2 diracologyBMHV2[b, DiracGamma[x4[y4, dim1], dim1],
			DiracGamma[x1[y1, dim2], dim2],
			DiracGamma[x2[y2, dim2], dim2],
			DiracGamma[x3[y3, dim2], dim2], d]/;
			(dim1 === dim2-4 || dim1 ===4);

(* Evaluation of a string of 4 dimensional Dirac matrices
	g^mu g^nu_1 ... g^nu_i g_mu  -> -2 g^nu_i ... g^nu_1,
	where i is odd *)
diracologyBMHV2[ b___,  DiracGamma[LorentzIndex[c_]],
		ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
		DiracGamma[LorentzIndex[c_]], f___ ] :=
	-2 diracologyBMHV2 @@ Join[ {b},Reverse[{ch}],{f} ] /; OddQ[Length[{ch}]];


(* Evaluation of a string of 4 dimensional Dirac matrices
	g^mu g^nu_1 ... g^nu_i g_mu  ->
	2 g^nu_i-1 ... g^nu_1 g^nu_i + 2 g^nu_i g^nu_1 ... g^nu_i-1,
	where i is even *)
diracologyBMHV2[ b___,  DiracGamma[LorentzIndex[c_]],
		ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
		end : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]],
		DiracGamma[LorentzIndex[c_]], f___ ] :=
	( 2 diracologyBMHV2 @@ Join[ {b},Reverse[{ch}],{end}, {f}] +
	2 diracologyBMHV2[ b,end,ch,f ]
	)/; OddQ[Length[{ch}]];

(* Slash(p).Slash(p), where both objects have the same dimension *)
diracologyBMHV2[b___,DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],
		DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],d___ ] :=
	FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim],Momentum[c,dim]]},{}] diracologyBMHV2[b,d];

(* Simplifications for Slash(p) g^nu_1 ... g^nu_n Slash(p) where the slashes
	are in 4 and g^nu_i are in D-4 dimensions or vice versa.                 *)
(* ------------------------------------------------------------------------ *)

(* 4, (... D-4 ... ), 4 *)
diracologyBMHV2[b___ , DiracGamma[Momentum[c_]],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol-4] , dim_Symbol-4]..,
			DiracGamma[Momentum[c_]], d___] :=
	(-1)^Length[{ch}] FCUseCache[ExpandScalarProduct,{Pair[Momentum[c],Momentum[c]]},{}] diracologyBMHV2[b,ch, d];

(* D-4, (... 4 ... ), D-4 *)
diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
			DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
	(-1)^Length[{ch}] FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim-4],Momentum[c,dim-4]]},{}] diracologyBMHV2[b,ch, d];

(* Simplification for Slash(p) g^nu_1 ... g^nu_n Slash(p) where the slashes
	are in D and g^nu_i are in 4 dimensions. This applies for n>1.           *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_]]..,
			DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol], d___] :=
	diracologyBMHV2[b,DiracGamma[Momentum[c]], ch, DiracGamma[Momentum[c]], d] +
	(-1)^Length[{ch}] FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim-4],Momentum[c,dim-4]]},{}] diracologyBMHV2[b,ch, d]/; Length[{ch}]>1;

(* Simplification for Slash(p) g^nu_1 ... g^nu_n Slash(p) where the slashes are
	in D and g^nu_i are in D-4 dimensions. This applies for n>1.             *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol],
			ch : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,
			dim_Symbol-4],dim_Symbol-4]..,
			DiracGamma[Momentum[c_, dim_Symbol], dim_Symbol], d___] :=
	diracologyBMHV2[b,DiracGamma[Momentum[c,dim-4],dim-4], ch, DiracGamma[Momentum[c,dim-4],dim-4], d] +
	(-1)^Length[{ch}] FCUseCache[ExpandScalarProduct,{Pair[Momentum[c],Momentum[c]]},{}]  diracologyBMHV2[b,ch, d]/; Length[{ch}]>1;


(* Evaluation of Slash(p) g^nu Slash(p) for Dirac matrices in different
	dimensions                                                               *)
(* ------------------------------------------------------------------------ *)

diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4],
		DiracGamma[(x: LorentzIndex | ExplicitLorentzIndex | Momentum)[y_, dim2_ : 4] ,dim2_ : 4],
		DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4], d___] :=
	- FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim1],Momentum[c,dim1]]},{}] diracologyBMHV2[b,DiracGamma[x[y, dim2], dim2], d] +
	2 FCUseCache[FCFastContract,{Pair[Momentum[c,dim1],x[y,dim2]] diracologyBMHV2[b, DiracGamma[Momentum[c, dim1], dim1], d]},{}]/;
					(dim1===dim2 || dim2 === dim1-4 || dim2 ===4 || dim1 === dim2-4 || dim1 ===4) &&
					MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
					MatchQ[dim2, _Symbol | _Symbol-4 | 4 ];

(* Simplification for g^mu g^nu_1 ... g^nu_n g_mu where all
	matrices are  in D or D-4 dimensions. Applies for n>5, since
	for n<=5 we have explicit expressions in the code. The
	formula is given in Eq 2.9 of R. Mertig, M. Boehm,
	A. Denner. Comp. Phys. Commun., 64 (1991)                *)
diracologyBMHV2[ b___,DiracGamma[LorentzIndex[c_,dim_],dim_],
	ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_,
	dim_],dim_]..,
	DiracGamma[LorentzIndex[c_,dim_],dim_],f___ ] :=
	Block[ {iVar,jVar,len = Length[{ch}],dsTemp},
		FCUseCache[FCFastContract,{(-1)^len ( dim - 2 len ) dsTemp[b,ch,f] - 4 (-1)^len *
		Sum[ (-1)^(jVar-iVar) *  Pair[{ch}[[iVar,1]],
			{ch}[[jVar,1]] ]*dsTemp@@Join[{b},
			Delete[{ch}, {{iVar},{jVar}}], {f}],{iVar,1,len-1},{jVar,iVar+1,len}
		]},{}]/.dsTemp->diracologyBMHV2 /. Pair[z__] :> FCUseCache[ExpandScalarProduct,{Pair[z]},{}]
	] /;(Length[{ch}]>4) && MatchQ[dim, _Symbol | _Symbol-4 ];


(* Simplification for Slash(p) g^nu_1 ... g^nu_n Slash(p) where all
	matrices are in D, 4 or D-4 dimensions. The
	formula is given in Eq 2.10 of R. Mertig, M. Boehm,
	A. Denner. Comp. Phys. Commun., 64 (1991)                *)
diracologyBMHV2[b___, DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],
			ch:DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_ : 4], dim_ : 4]..,
			DiracGamma[Momentum[c_, dim_ : 4], dim_ : 4],f___
	] :=
	Block[ {iVar, len = Length[{ch}]},
		(-1)^len FCUseCache[ExpandScalarProduct,{Pair[Momentum[c,dim],Momentum[c,dim]]},{}] diracologyBMHV2[b,ch,f]
		+ 2 Sum[(-1)^(iVar+1) FCUseCache[FCFastContract,{ Pair[Momentum[c, dim],{ch}[[iVar,1]] ]
				* diracologyBMHV2@@Join[{b},Drop[{ch},{iVar, iVar}],{DiracGamma[Momentum[c, dim],dim],f}]
										},{}],{iVar, 1,len}]
	]/;(Length[{ch}]>0) && MatchQ[dim, _Symbol | _Symbol-4 | 4 ];

(* g^mu g^nu_1 ... g^nu_n g_mu, where g^mu is in 4 or D-4
	and g^nu_i are in D dimensions is simplfied by repeatedly
	applying anticommuation relations. Applies for n>5,
	since for n<=5 we have explicit expressions in the code *)

diracologyBMHV2[b___, DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4],
		ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
		ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
		DiracGamma[LorentzIndex[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
	(-diracologyBMHV2[b, DiracGamma[LorentzIndex[c, dim-4],dim-4], ch1, DiracGamma[LorentzIndex[c, dim-4], dim-4], ch2, d]
	+ 2 diracologyBMHV2[b, DiracGamma[h[x,dim-4],dim-4], ch1, d] );

diracologyBMHV2[b___, DiracGamma[LorentzIndex[c_]],
		ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
		ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
		DiracGamma[LorentzIndex[c_]], d___] :=
	(-diracologyBMHV2[b, DiracGamma[LorentzIndex[c]], ch1, DiracGamma[LorentzIndex[c]], ch2, d]
	+ 2 diracologyBMHV2[b, DiracGamma[h[x]], ch1, d] );

(* Slash(p) g^nu_1 ... g^nu_n Slash(p), where the slashes
	are in 4 or D-4 and g^nu_i are in D dimensions is simplfied
	by repeatedly applying anticommuation relations.             *)

diracologyBMHV2[b___, DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4],
		ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
		ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
		DiracGamma[Momentum[c_, dim_Symbol-4], dim_Symbol-4], d___] :=
	(-diracologyBMHV2[b, DiracGamma[Momentum[c, dim-4],dim-4], ch1, DiracGamma[Momentum[c, dim-4], dim-4], ch2, d]
	+ 2 FCUseCache[FCFastContract,{Pair[Momentum[c,dim-4],h[x,dim-4]] diracologyBMHV2[b, DiracGamma[Momentum[c, dim-4],dim-4], ch1, d]},{}]);

diracologyBMHV2[b___, DiracGamma[Momentum[c_]],
		ch1 : DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim_Symbol] , dim_Symbol]..,
		ch2 : DiracGamma[(h : LorentzIndex | ExplicitLorentzIndex | Momentum)[x_, dim_Symbol] , dim_Symbol],
		DiracGamma[Momentum[c_]], d___] :=
	(-diracologyBMHV2[b, DiracGamma[Momentum[c]], ch1, DiracGamma[Momentum[c]], ch2, d]
	+ 2 FCUseCache[FCFastContract,{Pair[Momentum[c],h[x]] diracologyBMHV2[b, DiracGamma[Momentum[c]], ch1, d]},{}] );

(* g^mu ... g^nu g^rho g_mu, where g^mu is in D, 4, or D-4
	and g^nu and g^rho are in different dimensions is simplfied
	by repeatedly applying anticommuation relations.            *)

diracologyBMHV2[b___ , DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4],    w___,
			DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim3_ : 4] ,dim3_ : 4],
			DiracGamma[LorentzIndex[c_, dim1_ : 4], dim1_ : 4], d___] :=
	((
	-diracologyBMHV2[b,  DiracGamma[LorentzIndex[c, dim1], dim1], w,
			DiracGamma[LorentzIndex[c, dim1], dim1],
			DiracGamma[x2[y2,dim3],dim3], d]
		+ 2 FCUseCache[FCFastContract,{Pair[LorentzIndex[c, dim1], x2[y2,dim3]] diracologyBMHV2[b, DiracGamma[LorentzIndex[c, dim1], dim1], w, d]},{}]))/;
				(dim1===dim3 || dim3 === dim1-4 || dim1 === dim3-4 || dim3 === 4 || dim1 === 4) &&
				MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
				MatchQ[dim3, _Symbol | _Symbol-4 | 4 ] &&
				!MatchQ[{w, DiracGamma[x2[y2,dim3],dim3]},{DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim3] , dim3]..} |
					{___, DiracGamma[a__], ___, DiracGamma[a__], ___}] && NonCommFreeQ[{w}/.DiracGamma->diga];

(* Slash(p) ... g^nu g^rho Slash(p), where the slashes are in D, 4, or D-4
	and g^nu and g^rho are in different dimensions is simplfied
	by repeatedly applying anticommuation relations.            *)

diracologyBMHV2[b___ , DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4], w___,
	DiracGamma[(x2: LorentzIndex | ExplicitLorentzIndex | Momentum)[y2_, dim3_ : 4] ,dim3_ : 4],
	DiracGamma[Momentum[c_, dim1_ : 4], dim1_ : 4], d___] :=
	((-diracologyBMHV2[b,  DiracGamma[Momentum[c, dim1], dim1], w,
	DiracGamma[Momentum[c, dim1], dim1],
	DiracGamma[x2[y2,dim3],dim3], d]
	+ 2 FCUseCache[FCFastContract,{Pair[Momentum[c, dim1], x2[y2,dim3]] diracologyBMHV2[b, DiracGamma[Momentum[c, dim1], dim1], w, d]},{}]))/;
		(dim1===dim3 || dim3 === dim1-4 || dim1 === dim3-4 || dim3 === 4 || dim1 === 4) &&
		MatchQ[dim1, _Symbol | _Symbol-4 | 4 ] &&
		MatchQ[dim3, _Symbol | _Symbol-4 | 4 ] &&
		!MatchQ[{w, DiracGamma[x2[y2,dim3],dim3]},{DiracGamma[(LorentzIndex | ExplicitLorentzIndex | Momentum)[_, dim3] , dim3]..} |
			{___, DiracGamma[a__], ___, DiracGamma[a__], ___}] && NonCommFreeQ[{w}/.DiracGamma->diga];

FCPrint[1,"DiracTrick.m loaded."];
End[]
