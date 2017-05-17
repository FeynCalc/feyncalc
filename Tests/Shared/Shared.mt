(* :Title: Shared.mt														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Unit tests for functions in the "Shared" directory			*)

(* ------------------------------------------------------------------------ *)

Needs["FeynCalc`"];

(*TODO Need tests for Collect3, Factor1, FeynCalcForm, ILimit, Series3,  MemSet, XYT, TimedIntegrate*)

ClearAll[tests];
tests = FileNames["*.test",FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "Shared"}]]
Get/@tests;

If[	$OnlySubTest=!="",
	testNames = "Tests`Shared`*";
	removeTests=Complement[Names[testNames],Flatten[StringCases[Names[testNames],Alternatives@@$OnlySubTest]]];
	Remove/@removeTests;
	Print["Only following subtests will be checked: ", Names[testNames]];
	Remove[testNames]
];

stingCompare[a_,b_]:=If[ToString[a]===ToString[b],True,False];

stingCompareIgnore[_,_]:=
	True;

If[ Names["Tests`Shared`fcstSharedObjectsMessages"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],(#[[4]]),testID->#[[1]],
		MessagesEquivalenceFunction->stingCompare]&,
		Join@@(ToExpression/@Names["Tests`Shared`fcstSharedObjectsMessages"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];

If[ Names["Tests`Shared`fcstSharedObjectsCheckAbort"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],(#[[4]]),testID->#[[1]],
		MessagesEquivalenceFunction->stingCompare]&,
		Join@@(ToExpression/@Names["Tests`Shared`fcstSharedObjectsCheckAbort"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];

If[ Names["Tests`Shared`fcstSharedObjectsTypesetting"]=!={} && $VersionNumber >= 10,
	tmpTest = Map[test[StringReplace[ToString[(ToExpression[#[[2]]]),InputForm, CharacterEncoding -> "Unicode"]," " ->""],StringReplace[(#[[3]])," " ->""],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Shared`fcstSharedObjectsTypesetting"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];

If[ Names["Tests`Shared`fcstFCCheckSyntax*"]=!={},

	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],(#[[4]]),testID->#[[1]],
		MessagesEquivalenceFunction->stingCompareIgnore]&,
		Join@@(ToExpression/@Names["Tests`Shared`fcstFCCheckSyntax*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];

nms=Names["Tests`Shared`*"];
If[ nms && Select[nms, !StringMatchQ[#, "*fcstSharedObjectsMessages" | "*fcstSharedObjectsTypesetting" | "*fcstSharedObjectsCheckAbort" | "*fcstFCCheckSyntax*" ] &]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Select[nms, !StringMatchQ[#, "*fcstSharedObjectsMessages" | "*fcstSharedObjectsTypesetting" | "*fcstSharedObjectsCheckAbort" | "*fcstFCCheckSyntax*" ] &])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];

(*
Map[Test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],TestID->#[[1]]]&,
	Join@@(ToExpression/@Select[Names["Tests`Shared`*"],
	!StringMatchQ[#, "*fcstSharedObjectsMessages" | "*fcstSharedObjectsTypesetting" | "*fcstSharedObjectsCheckAbort"] &])];

*)
