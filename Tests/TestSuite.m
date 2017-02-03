(* :Title: TestSuite.m                                              		*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  	Test Suite for FeynCalc via MUnit. Doesn't require Wolfram
				Worbench to run												*)

(* ------------------------------------------------------------------------ *)

<< MUnit`
$FeynCalcStartupMessages = False;

If[testType===3,
	$LoadTARCER=True;
	WriteString["stdout","Tests for TARCER.\n"]
];

<<FeynCalc`

testRunner[test_String]:=
	(time=AbsoluteTime[]; If[$VersionNumber < 10,
		FCPrint[0,"Testing ", FileNameTake[test], " ", UseWriteString->True]
	];
	If[!MUnit`TestRun[test,Loggers->{VerbosePrintLogger[]}],
		FCPrint[0,"\n ERROR! Some tests from ", test, " failed! Test run aborted!\n",UseWriteString->True];
		Exit[1],
		FCPrint[0,"\nTiming: ", N[AbsoluteTime[] - time, 4], " seconds",UseWriteString->True];
		FCPrint[0,"\n\n",UseWriteString->True]
	]);

FCPrint[0,"Starting FeynCalc Test Suite on Mathematica ", $VersionNumber, "\n", UseWriteString->True];

Which[
testType===1,
fcTestList = Select[(FileNames["*.mt", FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "*"}]]),
	StringFreeQ[#, ___ ~~ "TARCER" ~~ ___] &],
testType===2,
fcTestList = Select[StringCases[
FileNames["*.mt",
FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "*"}],
Infinity], RegularExpression[".*IntegrationTests.*"]]//Flatten,  StringFreeQ[#, ___ ~~ "TARCER" ~~ ___] &],
(* TARCER *)
testType===3,
fcTestList =Select[(FileNames["*.mt", FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", "*"}]]),
	!StringFreeQ[#, ___ ~~ "TARCER" ~~ ___] &],
True,
FCPrint[0,"Error! Uknown test type",UseWriteString->True];
Exit[]
];

If[	onlyTest=!="" && Head[onlyTest]===String,
	strList = StringSplit[ToString[onlyTest],"|"];
	str = RegularExpression[(".*" <> # <> ".*")]& /@ strList;
	fcTestList = StringCases[fcTestList,Alternatives@@str]//Flatten;
	FCPrint[0,"Only following tests will be checked: ", fcTestList,UseWriteString->True];
	FCPrint[0,"\n",UseWriteString->True]
];

If[	onlySubTest=!="" && Head[onlySubTest]===String,
	strList = StringSplit[ToString[onlySubTest],"|"];
	$OnlySubTest = RegularExpression[(".*" <> # <> ".*")]& /@ strList,
	$OnlySubTest=""
];

SetAttributes[test, HoldAllComplete];

testRunner/@fcTestList;
FCPrint[0,"\n",UseWriteString->True];
FCPrint[0,"Done!\n",UseWriteString->True];
Return[1]
