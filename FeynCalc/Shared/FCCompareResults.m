(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FCCompareResults													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary: Compare given results to the known values						*)

(* ------------------------------------------------------------------------ *)

FCCompareResults::usage =
"FCCompareResults[{res1, res2, ...}, {res1Known, res2Known, ...}] compares the
given list of expression {res1,res2,...} to the list of expressions
{res1Known,res2Known,...} that represents the correct results. This is handy
for checking both intermediate and final results of calculations, where you
know what should come out at the end.";

FCCompareResults::failmsg =
"Error! FCCompareResults has encountered a fatal problem and must abort the computation. \
The problem reads: `1`";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"];

End[]

Begin["`FCCompareResults`Private`"];

Options[FCCompareResults] = {
	Differences 		-> False,
	ExpandScalarProduct -> True,
	Factoring 			-> Factor,
	FCI 				-> False,
	Function 			-> {
							Function[Print[Style[#1,"Text",Bold], " ", If[TrueQ[#4],Style[#2,"Text",Darker[Green],Bold],Style[#3,"Text",Red,Bold]]]],
							Function[WriteString["stdout","\033[1m"<>#1<>"\033[0m "<> If[TrueQ[#4],
								"\033[1m \033[32m"<>#2<>"\033[0m \033[0;39m","\033[1m \033[31m"<>#3<>"\033[0m \033[0;39m"]<>"\n"]]
						},
	Interrupt 			-> {Hold[Abort[]], Automatic},
	Text 				-> {"Check of the results:", "The results agree.", "The results disagree."}
};

FCCompareResults[r1_, r2:Except[_?OptionQ], opts:OptionsPattern[]]:=
	FCCompareResults[{r1}, {r2}, opts]/; Head[r1]=!=List && Head[r2]=!=List;

FCCompareResults[obtainedResult_List, knownResult_List/;(knownResult==={} || !OptionQ[knownResult]), OptionsPattern[]]:=
	Block[{	res1, res2, diff, factoring, match=False, text, function, interrupt,
			interruptCommand, noFrontEnd, tmp, set},

		noFrontEnd = TrueQ[$FrontEnd===Null];

		factoring						= OptionValue[Factoring];
		text 							= OptionValue[Text];
		{interruptCommand, interrupt}	= OptionValue[Interrupt];

		If[	TrueQ[interrupt===Automatic] && noFrontEnd,
			interrupt=True,

			If[	TrueQ[interrupt],
				interrupt=True,
				interrupt=False
			]
		];

		If[	OptionValue[Function]=!=False,
			If[ noFrontEnd,
				function = OptionValue[Function][[2]],
				function = OptionValue[Function][[1]]
			]
		];

		If[ Length[res1]=!=Length[res2],
			Message[FCCompareResults::failmsg, "The list of the obtained and the list of the known results are of different lengths."];
			Abort[]
		];

		If[	!MatchQ[text, {_String,_String,_String}],
			Message[FCCompareResults::failmsg, "The supplied text messages are not strings."];
			Abort[]
		];

		If[!OptionValue[FCI],
			{res1,res2} = FCI[{obtainedResult, knownResult}],
			{res1,res2} = {obtainedResult, knownResult}
		];

		If[	OptionValue[ExpandScalarProduct],
			{res1,res2} = ExpandScalarProduct[#,FCI->True]&/@{res1, res2}
		];

		diff = res1 - res2;

		If[	factoring=!=False,
			diff = factoring/@diff
		];

		If[	TrueQ[MatchQ[diff, {0..}] || MatchQ[diff, {{0}..}] || diff==={} || Union[Flatten[diff]]==={0} ],
			match = True
		];

		If[	OptionValue[Function]=!=False,
			function[text[[1]],text[[2]],text[[3]],match];
		];

		If[	interrupt && match=!=True,
			ReleaseHold[interruptCommand]
		];

		If[ TrueQ[OptionValue[Differences]],
			match = diff
		];

		match

	];

FCPrint[1,"FCCompareResults.m loaded."];
End[]
