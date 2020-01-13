(* :Title: LoopIntegrals.mt													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "LoopIntegrals" directory		*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];
$FCAdvice = False;
$VeryVerbose = -1;

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "LoopIntegrals"}]];
Get/@tests;

If[	$OnlySubTest=!="",
	testNames = "Tests`LoopIntegrals`*";
	removeTests=Complement[Names[testNames],Flatten[StringCases[Names[testNames],Alternatives@@$OnlySubTest]]];
	Remove/@removeTests;
	Print["Only following subtests will be checked: ", Names[testNames]];
	Remove[testNames]
];

FCClearScalarProducts[];

SetOptions[Tdec,UseParallelization->False];
SetOptions[CTdec,UseParallelization->False];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Select[Names["Tests`LoopIntegrals`*"],
	!StringMatchQ[#, "*fcstLogDivergentScaleless"] &])];



$FCAdvice = True;

If[ Names["Tests`LoopIntegrals`fcstLogDivergentScaleless"]=!={},
	$KeepLogDivergentScalelessIntegrals=True;
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&, Join@@(ToExpression/@Names["Tests`LoopIntegrals`fcstLogDivergentScaleless"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	$KeepLogDivergentScalelessIntegrals=False
];

$VeryVerbose = 0;
