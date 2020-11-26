(* :Title: Feynman.mt														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
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

FCClearScalarProducts[];
ClearAll[M];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Select[Names["Tests`Feynman`*"], !StringMatchQ[#, "*fcstAbort*"] &])];

If[ Names["Tests`Feynman`fcstAbort*"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],(#[[4]]),testID->#[[1]],
		MessagesEquivalenceFunction->stringCompareIgnore]&,
		Join@@(ToExpression/@Names["Tests`Feynman`fcstAbort*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];
