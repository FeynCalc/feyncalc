(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FUnitTools														*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary: 	Tools for working with FeynCalc's unit tests				*)

(* ------------------------------------------------------------------------ *)


ExtractUnitTests::usage = \
"ExtractUnitTests[{Directory,TestFile},TestName] extracts testing \
expressions from existing unit tests for FeynCalc. This very \
useful to quickly update the correct results of existing tests. Exampe: \
ExtractUnitTests[{\"Shared\",\"SharedObjects\"},\"fcstSharedObjectsOwnValues\"]"

CreateUnitTests::usage =
"CreateUnitTests[TestName,Tests] creates a list of unit tests for \
FeynCalc with the id of type TestName-ID# from the given list Tests. \
The elements of this list are strings, where each string denotes a \
valid FeynCalc expression."

CreateUnitTestsTypesetting::usage = \
"CreateUnitTestsTypesetting[TestName,Tests] is like CreateUnitTests \
but with the sole purpose of creating unit tests for typesetting, \
i.e. the TraditionalForm output."

AddMakeBoxes::usage =
"AddMakeBoxes is an option for CreateUnitTestsTypesetting, which \
determines whether one should wrap the elements of the Tests list \
into MakeBoxes[#,TraditionalForm] or not."

Begin["`Package`"]
End[]

Begin["`FUnitTools`Private`"];

$FUnitToolsVersion="0.0.1";

$FUnitToolsDirectory =
ToFileName[{$FeynCalcDirectory, "AddOns", "FUnitTools"}];

Options[CreateUnitTestsTypesetting] = {
	AddMakeBoxes -> True
};

CreateUnitTests[n_String, l_List] :=
	MapIndexed[{n <> "-ID" <> ToString[First[#2]], #, ToString[ToExpression[#], FormatType -> InputForm]} &, l];

CreateUnitTestsTypesetting[n_String, l_List, OptionsPattern[]] :=
	MapIndexed[{n <> "-ID" <> ToString[First[#2]],
		If[OptionValue[AddMakeBoxes],
			StringReplace[ToString[MakeBoxees[#, TraditionalForm]],
			"MakeBoxees" -> "MakeBoxes"],
			ToString[#]
		],
		ToString[
			If[OptionValue[AddMakeBoxes],
				MakeBoxes[#, TraditionalForm],
				ToExpression[#]
			],
		InputForm, CharacterEncoding -> "Unicode"]} &, l];

ExtractUnitTests[{dir__, file_}, testVar_] :=
	Module[{li},
		Get[FileNameJoin[{ParentDirectory@$FeynCalcDirectory, "Tests", dir,	file <> ".test"}]];
		li = "Tests`" <> First[{dir}] <> "`" <> testVar;
		li = ToExpression[li];
		Map[#[[2]] &, li]
	];


(* Print startup message *)
If[ Global`$FeynCalcStartupMessages =!= False,
	Print[Style["FUnitTools ", "Text", Bold], Style[$FUnitToolsVersion <> " loaded.", "Text"]]
];


End[]
