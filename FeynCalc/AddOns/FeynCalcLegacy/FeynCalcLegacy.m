(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalcLegacy													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: 	Legacy functions and symbols								*)

(* ------------------------------------------------------------------------ *)

$FeynCalcLegacyVersion::usage=
"$FeynCalcLegacyVersion is a string that represents the version of FeynCalcLegacy.";

$FeynCalcLegacyDirectory::usage=
"$FeynCalcLegacyDirectory specifies the location of FeynCalcLegacy.";

Begin["`Package`"]



End[]

Begin["`FeynCalcLegacy`Private`"];

$FeynCalcLegacyVersion="1.0.0";

$FeynCalcLegacyDirectory = FileNameJoin[{$FeynCalcDirectory, "AddOns", "FeynCalcLegacy"}];

(* Load the intefaces *)
BeginPackage["FeynCalc`"];

load = "";


FCDeclareHeader[FileNameJoin[{$FeynCalcLegacyDirectory,"LegacyFunctions","Shared.m"}]];
Get[FileNameJoin[{$FeynCalcLegacyDirectory,"LegacyFunctions","Shared.m"}]];

load = FileNames[{"*.m"},FileNameJoin[{$FeynCalcLegacyDirectory,"LegacyFunctions"}]];
FCDeclareHeader/@load;
Get/@load

Remove["FeynCalc`load"];
EndPackage[]


fcVersion = StringSplit[$FeynCalcVersion, "."];
tooOldString = "Your FeynCalc version is too old. FeynCalcLegacy "<> $FeynCalcLegacyVersion <> " requires at least FeynCalc 10.2";

If[ (fcVersion[[1]]<10),
	Print[tooOldString];
	Abort[],
	If[ fcVersion[[2]]<2,
		Print[tooOldString];
		Abort[]
	];
];

FeynCalc`Package`FeynCalcLegacyLoaded = True;


(* Print the startup message *)
If[ $FeynCalcStartupMessages =!= False,
	Print[Style["FeynCalcLegacy ", "Text", Bold], Style[$FeynCalcLegacyVersion, ". For help, use the ", "Text"]];
	Print[Style["FeynCalcLegacy contains legacy functions and symbols that were removed in FeynCalc 10.2.1","Text"]];
];





End[]


