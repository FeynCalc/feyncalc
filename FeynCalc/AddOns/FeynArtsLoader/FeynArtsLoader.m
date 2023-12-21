(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynArtsLoader													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary: 	A simple loader to load the patched FeynArts from
				$FeynArtsDirectory											*)

(* ------------------------------------------------------------------------ *)


Begin["`Package`"]
End[]

Begin["`FeynArtsLoader`Private`"];


If[ $FeynCalcStartupMessages,
	PrintTemporary[Style["Loading FeynArts from " <>  $FeynArtsDirectory, "Text"]];
];

(* Load the .m files *)
Block[ {loadfa, fafiles, strm, patch=True, str},
	If[	$FAPatch,
		(* Check if FeynArts needs to be patched *)
		If[(fafiles = FileNames["FeynArts.m", $FeynArtsDirectory])=!={},
			strm = OpenRead[First[fafiles]];
			If[ Head[strm] =!= InputStream,
				Message[General::noopen, First[fafiles]];
				Abort[]
			];
			While[	ToString[str] != "EndOfFile",
					str = Read[strm, String];
					If[ StringMatchQ[ToString[str], "*patched for use with FeynCalc*", IgnoreCase -> True],
						patch = False
					]
			];
			Close[First[fafiles]],
			Message[General::noopen, FileNameJoin[{$FeynArtsDirectory, "FeynArts.m"}]];
			Message[FeynCalc::faerror, $FeynArtsDirectory];
			patch = False
		];
		(* Apply the patch *)
		If[ patch,
			FAPatch[]
		]
	];
	loadfa=Block[ {Print= System`Print},Get[FileNameJoin[{$FeynArtsDirectory, "FeynArts.m"}]]];
	If[loadfa =!=$Failed,
		(* If everything went fine *)
		If[ $FeynCalcStartupMessages,
			Print[	Style["FeynArts ", "Text", Bold],
					Style[StringReplace[ToString[FeynArts`$FeynArtsVersion],"FeynArts "->""] <>" patched for use with FeynCalc, for documentation see the ",
						"Text"],
					Style[DisplayForm@ButtonBox["manual", BaseStyle -> "Hyperlink",	ButtonFunction :>
						SystemOpen[First@FileNames[{"*.pdf", "*.PDF"}, FileNameJoin[{$FeynArtsDirectory, "manual"}]]],
						Evaluator -> Automatic, Method -> "Preemptive"], "Text"],
					Style[" or visit ", "Text"],
					Style[DisplayForm@ButtonBox["www.feynarts.de.",	ButtonData :> {URL["http://www.feynarts.de/"], None},
						BaseStyle -> "Hyperlink", ButtonNote -> "www.feynarts.de/"],"Text"]
			];
			Print[ Style["If you use FeynArts in your research, please cite","Text"]];
			Print [Style[" \[Bullet] T. Hahn, Comput. Phys. Commun., 140, 418-431, 2001, arXiv:hep-ph/0012260","Text"]];
		],
		(* If FeynArts didn't load *)
		Message[FeynCalc::faerror, $FeynArtsDirectory];
	];
];

FeynCalc`Private`AddToTheContextPath = Join[FeynCalc`Private`AddToTheContextPath,{"FeynArts`"}];

End[]


