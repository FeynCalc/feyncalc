(* :Title: NonCommAlgebra.mt												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "NonCommAlgebra" directory	*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "NonCommAlgebra"}]]
Get/@tests;

If[	$OnlySubTest=!="",
	testNames = "Tests`NonCommAlgebra`*";
	removeTests=Complement[Names[testNames],Flatten[StringCases[Names[testNames],Alternatives@@$OnlySubTest]]];
	Remove/@removeTests;
	Print["Only following subtests will be checked: ", Names[testNames]];
	Remove[testNames]
];

If[ Names["Tests`NonCommAlgebra`fcstAntiCommutator*"]=!={},
	DeclareNonCommutative[a,b];
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`NonCommAlgebra`fcstAntiCommutator*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	UnDeclareNonCommutative[a,b]

];

If[ Names["Tests`NonCommAlgebra`fcstCommutator*"]=!={},
	DeclareNonCommutative[a,b,c,d];
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`NonCommAlgebra`fcstCommutator*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	UnDeclareNonCommutative[a,b,c,d]
];

If[ Names["Tests`NonCommAlgebra`*"]=!={} &&
	Select[Names["Tests`NonCommAlgebra`*"], !StringMatchQ[#, "*fcstCommutator" | "*fcstAntiCommutator" | "*fcstCommutatorExplicit" ] &]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Select[Names["Tests`NonCommAlgebra`*"], !StringMatchQ[#, "*fcstCommutator" | "*fcstAntiCommutator" | "*fcstCommutatorExplicit" ] &])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];
