(* :Title: Feynman.mt														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "Feynman" directory			*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "Feynman"}]]
Get/@tests;

If[	$OnlySubTest=!="",
	testNames = "Tests`Feynman`*";
	removeTests=Complement[Names[testNames],Flatten[StringCases[Names[testNames],Alternatives@@$OnlySubTest]]];
	Remove/@removeTests;
	Print["Only following subtests will be checked: ", Names[testNames]];
	Remove[testNames]
];

stringCompare[a_,b_]:=If[ToString[a]===ToString[b],True,False];

stringCompareIgnore[_,_]:=
	True;

fcCompare[a_/;Head[a]=!=List,b_/;Head[b]=!=List]:=
	Together[FCTraceExpand[a-b]]===0;

fcCompare[a_List,b_List]:=
	a===b;

FCClearScalarProducts[];
ClearAll[M];

tmpTest = Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]],
	EquivalenceFunction -> fcCompare]&,
	Join@@(ToExpression/@Select[Names["Tests`Feynman`*"], !StringMatchQ[#, "*fcstAbort*"] &])];


If[ Names["Tests`Feynman`fcstAbort*"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],(#[[4]]),testID->#[[1]],
		MessagesEquivalenceFunction->stringCompareIgnore]&,
		Join@@(ToExpression/@Names["Tests`Feynman`fcstAbort*"])];
	tmpTest = tmpTest /. testID->TestID /. test->Test
];
