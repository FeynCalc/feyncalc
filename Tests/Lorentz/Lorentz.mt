(* :Title: Lorentz.mt														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "Lorentz" directory			*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "Lorentz"}]]
Get/@tests;

If[	$OnlySubTest=!="",
	testNames = "Tests`Lorentz`*";
	removeTests=Complement[Names[testNames],Flatten[StringCases[Names[testNames],Alternatives@@$OnlySubTest]]];
	Remove/@removeTests;
	Print["Only following subtests will be checked: ", Names[testNames]];
	Remove[testNames]
];

FCClearScalarProducts[];
ClearAll[xsp1,xsp2,ysp,zsp,X,abval,abval2,abval3,a,b,a1,a2,b1,b2,p,q,xxx,tmp,i,li];
DataType[a1,FCVariable]=True;
DataType[a2,FCVariable]=True;

$FCDefaultLightconeVectorN = FCGV["n"];
$FCDefaultLightconeVectorNB = FCGV["nb"];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Lorentz`*"])];

FCClearScalarProducts[];
DataType[a1,FCVariable]=False;
DataType[a2,FCVariable]=False;
