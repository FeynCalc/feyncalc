(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpinorChainTranspose										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Switch u and v spinors in a spinor chain						*)

(* ------------------------------------------------------------------------ *)

SpinorChainTranspose::usage =
"SpinorChainTranspose[exp] transposes particular spinor chains in exp, which
effectively switches the $u$ and $v$ spinors and reverses the order of the
Dirac matrices using charge conjugation operator. This operation is often
required in calculations that involve Majorana particles. By default, the
function will tranpose all chains of the form $\\bar{v}.x.u$ and $\\bar{v}.x.v$.
A different or more fine grained choice can be obtained via the option Select.";

SpinorChainTranspose::failmsg =
"Error! SpinorChainTranspose has encountered a fatal problem and must abort the computation. \
The problem reads: `1`"

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`SpinorChainTranspose`Private`"]

holdDOT::usage="";
sctrVerbose::usage="";

Options[SpinorChainTranspose] = {
	Collecting 		-> True,
	DotSimplify 	-> True,
	FCE 			-> False,
	FCI 			-> False,
	FCVerbose 		-> False,
	Factoring 		-> Factor,
	Head			-> Identity,
	Select 			-> {
						{SpinorVBar[_,_], SpinorU[_,_]},
						{SpinorVBar[_,_], SpinorV[_,_]},
						{SpinorVBarD[_,_], SpinorUD[_,_]},
						{SpinorVBarD[_,_], SpinorVD[_,_]}
					},
	TimeConstrained -> 3
};

SpinorChainTranspose[a_ == b_, opts:OptionsPattern[]] :=
	SpinorChainTranspose[a,opts] == SpinorChainTranspose[b,opts];

SpinorChainTranspose[expr_List, opts:OptionsPattern[]]:=
	SpinorChainTranspose[#, opts]&/@expr;

SpinorChainTranspose[expr_/; !MemberQ[{List,Equal},expr], OptionsPattern[]] :=
	Block[{ex, tmp, dsHead, freePart,dsPart, null1, null2, time, diracObjects, diracObjectsEval,
		repRule, res, optHead, optSelect, chainPatterns},

		optHead	= OptionValue[Head];
		optSelect = FCI[OptionValue[Select]];

		If[	!MatchQ[optSelect, {{Spinor[__], Spinor[__]}..}],
			Message[SpinorChainTranspose::failmsg,"The value of the Select option is not a valid list of filters."];
			Abort[]
		];

		chainPatterns = Map[holdDOT[#[[1]], ___, #[[2]]] &, optSelect];

		If[	Length[chainPatterns]===1,
			chainPatterns = First[chainPatterns],
			chainPatterns = Alternatives@@chainPatterns
		];

		If[ OptionValue[FCI],
			ex = expr,
			ex = FCI[expr]
		];

		If[	FreeQ[ex,Spinor],
			Return[ex]
		];

		If[	OptionValue[FCVerbose]===False,
			sctrVerbose=$VeryVerbose,
			If[MatchQ[OptionValue[FCVerbose], _Integer],
				sctrVerbose=OptionValue[FCVerbose]
			];
		];

		FCPrint[1, "SpinorChainTranspose. Entering.", FCDoControl->sctrVerbose];
		FCPrint[3, "SpinorChainTranspose: Entering with ", ex, FCDoControl->sctrVerbose];

		If[ !FreeQ[ex,DiracChain],
			ex = ex//. DiracChain[x___, a_Spinor, b_Spinor] :> DOT[a,x,b]
		];

		If[	MatchQ[ex/.DOT->holdDOT, holdDOT[a_Spinor,b___,c_Spinor]/;FreeQ[{b},Spinor]],

			FCPrint[1, "SpinorChainTranspose: Recognized standalone expression.", FCDoControl->sctrVerbose];
			tmp = dsHead[ex];
			tmp = tmp /. {
				dsHead[DOT[a_Spinor,b___,c_Spinor]]/;!MatchQ[holdDOT[a,b,c],chainPatterns] :> DOT[a,b,c]
			},

			FCPrint[1, "SpinorChainTranspose: Isolating spinor chains.", FCDoControl->sctrVerbose];
			time=AbsoluteTime[];
			tmp = FCDiracIsolate[ex,FCI->True, Head->dsHead, Spinor->True, Collecting-> OptionValue[Collecting], DiracGamma->False,
				Factoring -> OptionValue[Factoring], TimeConstrained -> OptionValue[TimeConstrained]];
			FCPrint[1, "SpinorChainTranspose: Done isolating spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->sctrVerbose];
			FCPrint[3, "SpinorChainTranspose: After FCDiracIsolate ", tmp, FCDoControl->sctrVerbose];

			tmp = tmp /. {
				dsHead[DOT[a_Spinor,b___,c_Spinor]]/;!MatchQ[holdDOT[a,b,c],chainPatterns] :> DOT[a,b,c]
			};
		];

		diracObjects = Cases[tmp+null1+null2, dsHead[_], Infinity]//Sort//DeleteDuplicates;

		FCPrint[3,"SpinorChainTranspose: diracObjects: ", diracObjects , FCDoControl->sctrVerbose];

		time=AbsoluteTime[];
		FCPrint[1, "SpinorChainTranspose: Checking the spinor syntax.", FCDoControl->sctrVerbose];
		If[	FeynCalc`Package`spinorSyntaxCorrectQ[diracObjects]=!=True,
			Message[SpinorChainTranspose::failmsg, "The input contains Spinor objects with incorrect syntax."];
			Abort[]
		];
		FCPrint[1,"SpinorChainTranspose: Checks done, timing: ", N[AbsoluteTime[] - time, 4] , FCDoControl->sctrVerbose];

		FCPrint[1, "SpinorChainTranspose: Trasnposing spinor chains.", FCDoControl->sctrVerbose];

		time=AbsoluteTime[];
		diracObjectsEval = (diracObjects/.dsHead->Identity /. DOT->holdDOT)/. {
			holdDOT[Spinor[p1_,r1__], b___, Spinor[p2_,r2__]] :>
			-1*optHead[DOT[Spinor[-p2,r2], FCChargeConjugateTransposed[DOT[b],FCI->True,Explicit->True,DotSimplify->False],Spinor[-p1,r1]]]
		} /. holdDOT -> DOT;

		FCPrint[1, "SpinorChainTranspose: Done transposing spinor chains, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->sctrVerbose];
		FCPrint[3, "SpinorChainTranspose: diracObjectsEval: ", diracObjectsEval, FCDoControl->sctrVerbose];

		If[ OptionValue[DotSimplify],
				diracObjectsEval = DotSimplify[#,FCI->True,Expanding->False]&/@diracObjectsEval
		];

		FCPrint[1, "SpinorChainTranspose: Inserting Dirac objects back.", FCDoControl->sctrVerbose];
		time=AbsoluteTime[];
		repRule = Thread[Rule[diracObjects,diracObjectsEval]];
		FCPrint[3,"SpinorChainTranspose: repRule: ",repRule , FCDoControl->sctrVerbose];
		res =  (tmp /. Dispatch[repRule]);
		FCPrint[1, "SpinorChainTranspose: Done inserting Dirac objects back, timing: ", N[AbsoluteTime[] - time, 4], FCDoControl->sctrVerbose];

		If[ OptionValue[FCE],
			res = FCE[res]
		];

		FCPrint[1, "SpinorChainTranspose: Leaving.", FCDoControl->sctrVerbose];
		FCPrint[3, "SpinorChainTranspose: Leaving with ", res, FCDoControl->sctrVerbose];

		res

	];

FCPrint[1,"SpinorChainTranspose.m loaded"];
End[]
