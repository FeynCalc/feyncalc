(* ::Package:: *)

QuitAbort[]:=
If[$FrontEnd===Null,
	Quit[],
	Abort[]
];


examplesDir="/media/Data/Projects/VS/FeynCalc/FeynCalc/Examples"
galleryFile="/media/Data/Projects/VS/feyncalc.github.io/_pages/examples.md"


If[!DirectoryQ[examplesDir],
	Print["ERROR! The directory ", examplesDir, " does not exist!" ];
	QuitAbort[]
];


noMDConversion={"WIP","MasterIntegrals","Misc","ReductionTable","ValidateModel","Raw"};
mFiles=Select[FileNames["*.m",FileNameJoin[{examplesDir}],Infinity],StringFreeQ[#,noMDConversion]&];
mdFiles=Select[FileNames["*.md",FileNameJoin[{examplesDir}],Infinity],StringFreeQ[#,"README"]&];


aux=Complement[Sort[StringReplace[mFiles,{".m"->".md","Mathematica"->"Markdown"}]],Sort[mdFiles]];
If[aux=!={},
Print["ERROR! Missing some markdown files that should be generated from .m files: ", aux];
QuitAbort[]
]


aux=Complement[Sort[StringReplace[Flatten@StringCases[mdFiles,Shortest["FeynCalc/Examples/"~~x__~~".md"]:>x,Infinity],{"Markdown/"->""}]],
Sort[StringCases[Import[galleryFile,"String"],Shortest["(FeynCalcExamples/"~~x__~~")"]:>x,Infinity]]];
If[aux=!={},
Print["ERROR! Missing some files in the example gallery: ", aux];
QuitAbort[]
]
