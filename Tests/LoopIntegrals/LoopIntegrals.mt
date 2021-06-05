(* :Title: LoopIntegrals.mt													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "LoopIntegrals" directory		*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];
$FCAdvice = False;
$VeryVerbose = -1;

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "LoopIntegrals"}]];
Get/@tests;

fcCompare[a_/;Head[a]=!=List,b_/;Head[b]=!=List]:=
	Together[a-b]===0;

fcCompare[a_List,b_List]:=
	a===b;

If[	$OnlySubTest=!="",
	testNames = "Tests`LoopIntegrals`*";
	removeTests=Complement[Names[testNames],Flatten[StringCases[Names[testNames],Alternatives@@$OnlySubTest]]];
	Remove/@removeTests;
	Print["Only following subtests will be checked: ", Names[testNames]];
	Remove[testNames]
];

FCClearScalarProducts[];

SetOptions[Tdec,Parallelize->False];
SetOptions[CTdec,Parallelize->False];
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]], EquivalenceFunction -> fcCompare]&,
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
