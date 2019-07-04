(* :Title: Pauli.mt															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "Pauli" directory				*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "Pauli"}]]
Get/@tests;

If[	$OnlySubTest=!="",
	testNames = "Tests`Pauli`*";
	removeTests=Complement[Names[testNames],Flatten[StringCases[Names[testNames],Alternatives@@$OnlySubTest]]];
	Remove/@removeTests;
	Print["Only following subtests will be checked: ", Names[testNames]];
	Remove[testNames]
];

If[ Names["Tests`Pauli`fcstPauliSigmaCombine*"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Pauli`fcstPauliSigmaCombine*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
];

If[ Names["Tests`Pauli`fcstPauliSigmaExpand*"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Pauli`fcstPauliSigmaExpand*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
];

If[ Names["Tests`Pauli`fcstFCPauliIsolate*"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Pauli`fcstFCPauliIsolate*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
];

If[ Names["Tests`Pauli`fcstPauliTrick*"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Pauli`fcstPauliTrick*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
];

If[ Names["Tests`Pauli`fcstPauliSimplify*"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Pauli`fcstPauliSimplify*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
];

If[ Names["Tests`Pauli`fcstPauliOrder*"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Pauli`fcstPauliOrder*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
];
