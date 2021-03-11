(* :Title: Shared.mt														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
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

stringCompare[a_,b_]:=If[ToString[a]===ToString[b],True,False];

stringCompareIgnore[_,_]:=
	True;

If[ Names["Tests`Shared`fcstSharedObjectsCheckAbort"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],(#[[4]]),testID->#[[1]],
		MessagesEquivalenceFunction->stringCompareIgnore]&,
		Join@@(ToExpression/@Names["Tests`Shared`fcstSharedObjectsCheckAbort"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];

If[ Names["Tests`Shared`fcstSharedObjectsTypesetting*"]=!={} && $VersionNumber >= 10,

	FCAttachTypesettingRule[im[{"f_em","3P2"}],"Im \!\(\*SubscriptBox[\(f\), \(em\)]\)\!\(\*SuperscriptBox[\"\", \
3]\)\!\(\*SuperscriptBox[P, 0]\)"];
	FCAttachTypesettingRule[pp1,{SubscriptBox,p,1}];
	FCAttachTypesettingRule[n0,{SubscriptBox,n,0}];
	FCAttachTypesettingRule[m12,{SubsuperscriptBox,m,1,2}];

	tmpTest = Map[test[StringReplace[ToString[(ToExpression[#[[2]]]),InputForm, CharacterEncoding -> "Unicode"]," " ->""],StringReplace[(#[[3]])," " ->""],testID->#[[1]]]&,
	Join@@(ToExpression/@Names["Tests`Shared`fcstSharedObjectsTypesetting*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test;
	FCRemoveTypesettingRules[{im[{"f_em","3P2"}], pp1, n0, m12}]
];

If[ Names["Tests`Shared`fcstFCCheckSyntax*"]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],(#[[4]]),testID->#[[1]],
		MessagesEquivalenceFunction->stringCompareIgnore]&,
		Join@@(ToExpression/@Names["Tests`Shared`fcstFCCheckSyntax*"])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];

nms=Names["Tests`Shared`*"];
If[ nms=!={} && Select[nms, !StringMatchQ[#, "*fcstSharedObjectsMessages" | "*fcstSharedObjectsTypesetting*" | "*fcstSharedObjectsCheckAbort" | "*fcstFCCheckSyntax*" ] &]=!={},
	tmpTest = Map[test[ToExpression[(#[[2]])],ToExpression[(#[[3]])],testID->#[[1]]]&,
	Join@@(ToExpression/@Select[nms, !StringMatchQ[#, "*fcstSharedObjectsMessages" | "*fcstSharedObjectsTypesetting*" | "*fcstSharedObjectsCheckAbort" | "*fcstFCCheckSyntax*" ] &])];
	tmpTest = tmpTest /. testID->TestID /. test -> Test
];
