(* :Title: Feynman.mt														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "Feynman" directory			*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "ExportImport"}]]
Get/@tests;

If[	$OnlySubTest=!="",
	testNames = "Tests`ExportImport`*";
	removeTests=Complement[Names[testNames],Flatten[StringCases[Names[testNames],Alternatives@@$OnlySubTest]]];
	Remove/@removeTests;
	Print["Only following subtests will be checked: ", Names[testNames]];
	Remove[testNames]
];

FCClearScalarProducts[];

Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Select[Names["Tests`ExportImport`*"],
	!StringMatchQ[#, "*fcstFCToTeXPreviewTermOrder"] &])

	];

holdFormCompare[x_,y_]:=
	Block[	{lhs,rhs},
			{lhs,rhs} = StringReplace[ToString[#,InputForm],{" "|"\n"->"","(-12^(-1))"->"-1/12"}]&/@{x,y};
			lhs===rhs
	]

If[ Names["Tests`ExportImport`fcstFCToTeXPreviewTermOrder"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],{},testID->#[[1]],
		(*MessagesEquivalenceFunction->Function[{x,y},True],*)
		EquivalenceFunction->holdFormCompare]&,
		Join@@(ToExpression/@Names["Tests`ExportImport`fcstFCToTeXPreviewTermOrder"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];
