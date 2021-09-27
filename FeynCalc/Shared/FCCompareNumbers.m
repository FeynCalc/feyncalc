(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCCompareNumbers														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: 	Compares numbers											*)

(* ------------------------------------------------------------------------ *)

FCCompareNumbers::usage=
"FCCompareNumbers[x, y] compares two purely numerical or semi-numerical
expressions x and y and returns the number of agreeing significant digits
calculated from the relative differences.";

FCCompareNumbers::failmsg =
"FCCompareNumbers has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

Begin["`Package`"];

End[]
(* ------------------------------------------------------------------------ *)

Begin["`FCCompareNumbers`Private`"];

fccnVerbose::usage="";

Options[FCCompareNumbers]={
	Chop					->	10^(-10),
	Complex					-> 	FCGV["I"],
	DigitCount				->	6,
	FCVerbose				->	False,
	Head					->	FCGV["CommonDigits"],
	InitialSubstitutions	->	{},
	Unequal					->	FCGV["Unmatched"]
};

FCCompareNumbers[x_List,y_List, opts:OptionsPattern[]]:=
	MapThread[FCCompareNumbers[#1,#2,opts]&,{x,y}];

FCCompareNumbers[xRaw_/;Head[xRaw]=!=List,yRaw_/;Head[yRaw]=!=List,OptionsPattern[]]:=
	Block[{	num1Pref, num2Pref, lhs, rhs, vars, headRaw, diff, lhsRe,lhsIm,rhsRe, rhsIm,
			comp, varsX, varsY, x, y, xNums, yNums, optChop, xChopped, yChopped,
			optHead, optDigitCount, null1, null2, tmpHead, dummy, ruleRemoveZero,
			zeroHold1, zeroHold2, optIntialSubstitutions},

		If [OptionValue[FCVerbose]===False,
				fccnVerbose=$VeryVerbose,
				If[MatchQ[OptionValue[FCVerbose], _Integer],
					fccnVerbose=OptionValue[FCVerbose]
				];
			];

		optChop					= OptionValue[Chop];
		optHead					= OptionValue[Head];
		optDigitCount			= OptionValue[DigitCount];
		optIntialSubstitutions	= OptionValue[InitialSubstitutions];

		ruleRemoveZero = {0. +0. I -> 0, 0. -> 0};

		FCPrint[1, "FCCompareNumbers: Entering.", FCDoControl->fccnVerbose];
		FCPrint[3, "FCCompareNumbers: Entering with, lhs: ", xRaw, FCDoControl->fccnVerbose];
		FCPrint[3, "FCCompareNumbers: Entering with, rhs: ", yRaw, FCDoControl->fccnVerbose];

		If[	!FreeQ2[{xRaw,yRaw},{I,Complex}],
			{x,y} = {ComplexExpand[ExpandAll[dummy xRaw]],ComplexExpand[ExpandAll[dummy yRaw]]},
			{x,y} = {ExpandAll[dummy xRaw],ExpandAll[dummy yRaw]}
		];

		FCPrint[3, "FCCompareNumbers: After ComplexExpand and ExpandAll, lhs: ", x, FCDoControl->fccnVerbose];
		FCPrint[3, "FCCompareNumbers: After ComplexExpand and ExpandAll, rhs: ", y, FCDoControl->fccnVerbose];

		If[	optIntialSubstitutions=!={},
			{x,y} = {x,y} /. optIntialSubstitutions;
			FCPrint[3, "FCCompareNumbers: After applying initial substitutions, lhs: ", x, FCDoControl->fccnVerbose];
			FCPrint[3, "FCCompareNumbers: After applying initial substitutions, rhs: ", y, FCDoControl->fccnVerbose]
		];

		xNums = Union[Cases[(x/. ruleRemoveZero)+null1+null2,_?NumberQ,Infinity]];
		yNums = Union[Cases[(y/. ruleRemoveZero)+null1+null2,_?NumberQ,Infinity]];
		xNums = Sort[Union[xNums /. Complex[a_,b_]:>comp[a,b] //. {r1___,comp[a_,b_],r2___}:> {r1,a, Complex[0,1] b,r2}]/. {r1___,0,r2___} :> {r1,r2}];
		yNums = Sort[Union[yNums /. Complex[a_,b_]:>comp[a,b] //. {r1___,comp[a_,b_],r2___}:> {r1,a, Complex[0,1] b,r2}]/. {r1___,0,r2___} :> {r1,r2}];

		FCPrint[3, "FCCompareNumbers: Numbers present on the lhs: ", xNums, FCDoControl->fccnVerbose];
		FCPrint[3, "FCCompareNumbers: Numbers present on the rhs: ", yNums, FCDoControl->fccnVerbose];

		FCPrint[0, Style["FCCompareNumbers: Minimal number of significant digits to agree in: ", {Black, Bold}], optDigitCount, FCDoControl->fccnVerbose];

		If[	optChop=!=False && NumberQ[optChop],

			FCPrint[0, Style["FCCompareNumbers: Chop is set to ", {Black, Bold}], N[optChop], FCDoControl->fccnVerbose];
			xChopped = Select[xNums,(Chop[#,optChop]==0.)&];
			yChopped = Select[yNums,(Chop[#,optChop]==0.)&];
			If[	xChopped=!={},
				FCPrint[0, Style["FCCompareNumbers: Following numbers on the l.h.s. are set to 0. by Chop: ", {Darker[Yellow,0.55], Bold}], xChopped, FCDoControl->fccnVerbose];
			];
			If[	yChopped=!={},
				FCPrint[0, Style["FCCompareNumbers: Following numbers on the r.h.s. are set to 0. by Chop: ", {Darker[Yellow,0.55], Bold}], yChopped, FCDoControl->fccnVerbose];
			];
			If[	xChopped==={} && yChopped==={},
				FCPrint[0, Style["FCCompareNumbers: No number is set to 0. by Chop at this stage. ", {Darker[Green,0.55], Bold}], FCDoControl->fccnVerbose];
			];
			{x,y} = Chop[{x,y}/.{0. -> zeroHold1, 0. +0. I -> zeroHold2},optChop]/. {zeroHold1 -> 0. , zeroHold2 -> 0. +0. I}
		];

		varsX = Variables2[x] /. dummy->Unevaluated[Sequence[]];
		varsY = Variables2[y] /. dummy->Unevaluated[Sequence[]];
		vars = Union[varsX, varsY];

		FCPrint[3, "FCCompareNumbers: Variables on the lhs: ", varsX, FCDoControl->fccnVerbose];
		FCPrint[3, "FCCompareNumbers: Variables on the rhs: ", varsY, FCDoControl->fccnVerbose];

		If[varsX=!=varsY,
			FCPrint[0, Style["FCCompareNumbers: The two expressions contain different number of variables!", {Darker[Red,0.55], Bold}], FCDoControl->fccnVerbose];
			FCPrint[0, Style["FCCompareNumbers: Variables present only on the l.h.s. : ", {Darker[Red,0.55], Bold}], Complement[vars, varsX], FCDoControl->fccnVerbose];
			FCPrint[0, Style["FCCompareNumbers: Variables present only on the r.h.s. : ", {Darker[Red,0.55], Bold}], Complement[vars, varsY], FCDoControl->fccnVerbose];
		];
		vars = Join[vars,{comp}];


		lhsRe=SelectFree[x+null1+null2,Complex,I] /. null1|null2->0 /. ruleRemoveZero  /. dummy->1;
		lhsIm=SelectNotFree[x+null1+null2,Complex,I]/. {Complex->comp,I->comp[0,1]}  /. null1|null2->0 /. ruleRemoveZero  /. dummy->1;

		rhsRe=SelectFree[y+null1+null2,Complex,I] /. null1|null2->0 /. ruleRemoveZero  /. dummy->1;
		rhsIm=SelectNotFree[y+null1+null2,Complex,I]/. {Complex->comp,I->comp[0,1]}  /. null1|null2->0 /. ruleRemoveZero  /. dummy->1;


		FCPrint[3, "FCCompareNumbers: Real part of the lhs: ", lhsRe, FCDoControl->fccnVerbose];
		FCPrint[3, "FCCompareNumbers: Imaginary part of the lhs: ", lhsIm, FCDoControl->fccnVerbose];

		FCPrint[3, "FCCompareNumbers: Real part of the rhs: ", rhsRe, FCDoControl->fccnVerbose];
		FCPrint[3, "FCCompareNumbers: Imaginary part of the rhs: ", rhsIm, FCDoControl->fccnVerbose];

		If[	!MatchQ[lhsIm,(comp[0,1] c_/;FreeQ[c,comp]) | 0],
				Message[FCCompareNumbers::failmsg,"The imaginary part of the l.h.s doesn't look right."];
				Print[lhsIm];
				Abort[]
		];

		If[	!MatchQ[rhsIm,(comp[0,1] c_/;FreeQ[c,comp]) | 0],
				Message[FCCompareNumbers::failmsg,"The imaginary part of the r.h.s doesn't look right."];
				Print[rhsIm];
				Abort[]
		];

		lhs=Collect2[lhsRe+lhsIm,vars,Factoring->num1Pref];
		rhs=Collect2[rhsRe+rhsIm,vars,Factoring->num2Pref];
		diff=Collect2[lhs-rhs,vars,Factoring->headRaw];

		If[	!FreeQ[diff,Complex],
			Message[FCCompareNumbers::failmsg,"Something went wrong when separating real and imaginary parts."];
			Abort[]
		];

		diff = diff //. headRaw[num1Pref[aa_?NumericQ]-num2Pref[bb_?NumericQ]]/; (bb!=0) :> tmpHead[Abs[(aa-bb)]/Abs[bb]];

		FCPrint[3, "FCCompareNumbers: diff after calculating relative differences: ", diff, FCDoControl->fccnVerbose];

		diff = diff /. tmpHead[0.|0]->0 /. tmpHead[aa_?NumericQ]/; aa!=0. :> optHead[-N[Log[10,aa]]];

		FCPrint[3, "FCCompareNumbers: diff after calculating agreeing significant digits: ", diff, FCDoControl->fccnVerbose];

		diff = diff /. optHead[aa_?NumericQ]/; aa>=optDigitCount -> 0;

		FCPrint[3, "FCCompareNumbers: diff after removing terms that agree in the required number of significant digits: ", diff, FCDoControl->fccnVerbose];

		diff = diff /. {comp[0,1] -> OptionValue[Complex], headRaw -> OptionValue[Unequal]} /. num1Pref|num2Pref->Identity;

		FCPrint[3, "FCCompareNumbers: Leaving with: ", diff, FCDoControl->fccnVerbose];
		FCPrint[1, "FCCompareNumbers: Leaving.", FCDoControl->fccnVerbose];

		diff
	];



FCPrint[1,"FCCompareNumbers.m loaded"];
End[]
