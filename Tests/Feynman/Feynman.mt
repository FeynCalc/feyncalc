(* :Title: Feynman.mt														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
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

FCClearScalarProducts[];
ClearAll[M];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Feynman`*"])];
