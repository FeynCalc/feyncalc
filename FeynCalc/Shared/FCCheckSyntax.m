(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCCheckSyntax											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Checks if the expression contains dummy indices		*)

(* ------------------------------------------------------------------------ *)



FCCheckSyntax::usage =
"FCCheckSyntax[exp] attempts to detect mistakes and inconsistencies in the user
input. The function returns the original expression but will abort the
evaluation if it thinks that the input is incorrect. Notice that false
positives are possible and it is not guaranteed that the input which passes
FCCheckSyntax is indeed fully correct.

FCCheckSyntax is also an option for several FeynCalc routines. If set to True,
those functions will try to check the syntax of the input expressions to
detect possible inconsistencies. However, on large expressions such checks may
cost a lot of performance, which is why this option is set to False by
default.";

FCCheckSyntax::failmsg =
"Error! FCCheckSyntax has found an inconsistency in your input expression \
and must abort the evaluation. The problem reads: `1` `2`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];
End[]

Begin["`FCCheckSyntax`Private`"];

indexHeads = {LorentzIndex, CartesianIndex, SUNIndex, SUNFIndex};
nonCommHeads = {DiracGamma, PauliSigma, SUNT};

Options[FCCheckSyntax] = {
	DotSimplify	-> True,
	FCI 		-> False
};


getFreeIndices[ex_, head_]:=
	Cases[ex, Blank[head], Infinity]//ReplaceAll[#, head[z_, ___] :> z] & // Tally // Cases[#, {z_, 1} :> z]  & // Sort;

getMoreThanTwoIndices[ex_, head_]:=
	Cases[ex, Blank[head], Infinity]//ReplaceAll[#, head[z_, ___] :> z] & // Tally // Cases[#, {z_, n_}/;(n>2) :> z] &;

getAllIndices[ex_, head_]:=
	Cases[ex, Blank[head], Infinity] // DeleteDuplicates // Sort // ReplaceAll[#, head[z_, ___] :> z] & // DeleteDuplicates // Sort

checkEinsteinSummation[expr0_, head_]:=
	Block[{expr=expr0, ex,times,freeLiList,fullLiList,dummyLiList,null1,null2,listExpr},

		If[	!FreeQ[expr, Power],
			expr = expr /. Power[a_, b_Integer?Positive] /; !FreeQ[a, LorentzIndex] :>
				Apply[times, Table[a, {b}]];
		];

		If[Head[expr]===Plus,
			ex=First[expr];
			listExpr = List@@expr,
			ex=expr;
			listExpr = {expr}
		];



		(* List of the free indices *)
		freeLiList = getFreeIndices[ex,head];

		(* List of all indices that appear in the expression *)
		fullLiList = getAllIndices[ex,head];

		(* List of all dummy Lorentz indices that appear in the expression *)
		dummyLiList = Complement[fullLiList, freeLiList];

		(* Check that there is only one dsHead per term and no nested dsHeads *)

			Scan[
				(


				Which[

					freeLiList=!=getFreeIndices[#,head],
					Message[FCCheckSyntax::failmsg, "Different number of free indices in ", ex];
					Abort[],

					getMoreThanTwoIndices[#,head]=!={},
					Message[FCCheckSyntax::failmsg, "More than two repeating indices in ", ToString[#,InputForm]];
					Abort[],

					True,
					Null
			])&, listExpr];

		];

checkIndices[ex_,head_]:=
	If[!FreeQ[ex,head],
			checkEinsteinSummation[ex,head]
	];

nonComRulePrep[head_,dotHead_,powerHead_,timesHead_]:=
	{
		a_head b_head -> timesHead[a b],
		a_head b_powerHead/; !FreeQ[b,head] -> timesHead[a b],
		a_head (b: dotHead[_head,___]) -> timesHead[a b],
		a_head (b: dotHead[___,_head]) -> timesHead[a b],
		(a: dotHead[_head,___]) (b: dotHead[_head,___]) -> timesHead[a b],
		(a: dotHead[_head,___]) (b: dotHead[___,_head]) -> timesHead[a b],
		(a: dotHead[___,_head]) (b: dotHead[_head,___]) -> timesHead[a b],
		(a: dotHead[___,_head]) (b: dotHead[___,_head]) -> timesHead[a b]
	};

nonComCheck[ex_,head_,powerHead_,timesHead_]:=
	If[	!FreeQ[ex,timesHead],
				Message[FCCheckSyntax::failmsg, "Commutative products of "<> ToString[head] <> " in ",
					SelectNotFree2[ex,timesHead]/.timesHead->Identity/.powerHead->Times];
				Abort[]
	];

FCCheckSyntax[expr_, OptionsPattern[]] :=
	Block[{fullLiList, freeLiList, ex, times, i, sel, sel2, tmp, timesGamma, timesSUNT,
		timesSigma,dotHold},


		FCPrint[1, "FCCheckSyntax: Entering."];
		FCPrint[3, "FCCheckSyntax: Entering with: ", expr];

		If[ !OptionValue[FCI],
			ex = FCI[expr],
			ex = expr
		];
		(*
		If[	!FreeQ[expr,DOT] && OptionValue[DotExpand],
			ex = DotExpand[ex]
		];*)

		If[	!FreeQ[expr,DOT] && OptionValue[DotSimplify],
			ex = DotSimplify[ex]
		];

		ex = Expand2[ex,indexHeads];



		(* Check Einstein summation for the relevant indices *)
		Scan[checkIndices[ex,#]&,indexHeads];



		(* Syntax errors for noncommutative objects *)
		If[!FreeQ2[ex,nonCommHeads],

			If[	!FreeQ[ex, Power],
			ex = ex /. Power[a_, b_Integer?Positive] /; !FreeQ2[a, nonCommHeads] :>
				Apply[times, Table[a, {b}]];
			];

			ex = ex /. DOT -> dotHold;

			ex = ex /. Dispatch[nonComRulePrep[DiracGamma,dotHold,times,timesGamma]];

			ex = ex /. Dispatch[nonComRulePrep[PauliSigma,dotHold,times,timesSigma]];
			ex = ex /. Dispatch[nonComRulePrep[SUNT,dotHold,times,timesSUNT]];

			nonComCheck[ex,DiracGamma,times,timesGamma];
			nonComCheck[ex,PauliSigma,times,timesSigma];

			nonComCheck[ex,SUNT,times,timesSUNT];


		];



		expr

	];

FCPrint[1,"FCCheckSyntax.m loaded."];
End[]
