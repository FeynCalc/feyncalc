(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FUnitTools														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary: 	Tools for working with FeynCalc's unit tests				*)

(* ------------------------------------------------------------------------ *)


FUnitExtractUnitTests::usage = \
"FUnitExtractUnitTests[{Directory,TestFile},TestName] extracts testing \
expressions from existing unit tests for FeynCalc. This very \
useful to quickly update the correct results of existing tests. Exampe: \
FUnitExtractUnitTests[{\"Shared\",\"SharedObjects\"},\"fcstSharedObjectsOwnValues\"]"

FUnitCreateUnitTests::usage =
"FUnitCreateUnitTests[TestName,Tests] creates a list of unit tests for \
FeynCalc with the id of type TestName-ID# from the given list Tests. \
The elements of this list are strings, where each string denotes a \
valid FeynCalc expression."

FUnitCreateUnitTestsTypesetting::usage = \
"FUnitCreateUnitTestsTypesetting[TestName,Tests] is like FUnitCreateUnitTests \
but with the sole purpose of creating unit tests for typesetting, \
i.e. the TraditionalForm output. Notice that when creating typesetting tests, \
the list elements must not be strings, i.e. li = {FV[p,mu], FV[q,nu]}. Then \
FUnitCreateUnitTestsTypesetting[fcstFV,li] does the job and the output (copied as\
Input Text) can be directly pasted into the test file."

AddMakeBoxes::usage =
"AddMakeBoxes is an option for FUnitCreateUnitTestsTypesetting, which \
determines whether one should wrap the elements of the Tests list \
into MakeBoxes[#,TraditionalForm] or not."

Begin["`Package`"]
End[]

Begin["`FUnitTools`Private`"];

$FUnitToolsVersion="0.2";

$FUnitToolsDirectory =
ToFileName[{$FeynCalcDirectory, "AddOns", "FUnitTools"}];

Options[FUnitCreateUnitTestsTypesetting] = {
	AddMakeBoxes -> True,
	"ZeroIDValue" -> 0
};

Options[FUnitCreateUnitTests] = {
	AddMakeBoxes -> True,
	"ZeroIDValue" -> 0
};

Options[FUnitExtractUnitTests] = {
	Names -> False
};

FUnitCreateUnitTests[n_String, l_List, OptionsPattern[]] :=
	MapIndexed[{n <> "-ID" <>
		If[ StringQ[OptionValue["ZeroIDValue"]],
				ToString[OptionValue["ZeroIDValue"]]<>ToString[First[#2]],
				ToString[OptionValue["ZeroIDValue"]+First[#2]]
		],
		#, ToString[ToExpression[#], FormatType -> InputForm]} &, l]



FUnitCreateUnitTestsTypesetting[n_String, l_List, OptionsPattern[]] :=
	MapIndexed[{n <> "-ID" <>

		If[ StringQ[OptionValue["ZeroIDValue"]],
				ToString[OptionValue["ZeroIDValue"]]<>ToString[First[#2]],
				ToString[OptionValue["ZeroIDValue"]+First[#2]]
		],
		If[OptionValue[AddMakeBoxes],
			StringReplace[ToString[System`MakeBoxees[#, TraditionalForm], InputForm],
			"MakeBoxees" -> "MakeBoxes"],
			ToString[#]
		],
		ToString[
			If[OptionValue[AddMakeBoxes],
				MakeBoxes[#, TraditionalForm],
				ToExpression[#]
			],
		InputForm, CharacterEncoding -> "Unicode"]} &, l];

FUnitExtractUnitTests[{dir__, file_}, testVar_, OptionsPattern[]] :=
	Module[{li},
		Get[FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", dir,	file <> ".test"}]];
		li = "Tests`" <> First[{dir}] <> "`" <> testVar;
		li = ToExpression[li];
		If[	OptionValue[Names],
			Map[#[[1;;2]] &, li],
			Map[#[[2]] &, li]
		]
	];


(* Print startup message *)
If[ $FeynCalcStartupMessages =!= False,
	Print[Style["FUnitTools ", "Text", Bold], Style[$FUnitToolsVersion <> " loaded.", "Text"]]
];


End[]
