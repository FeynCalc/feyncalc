(* ::Package:: *)

(*Print[inputNB];
Print[outputDir];
Print[""];
Print[Head[inputNB]];
Print[Head[outputDir]]
Quit[]*)


QuitAbort[]:=
If[$FrontEnd===Null,
	Quit[],
	Abort[]
];


<<Developer`;
Developer`InstallFrontEnd["Server"->False];
UsingFrontEnd[FE=$FrontEnd];
UsingFrontEnd[FS=$FrontEndSession];


Unprotect[$FrontEnd];
Unprotect[$FrontEndSession];
$FrontEnd=FE;
$FrontEndSession=FS;


(*For debugging purposes*)
(*
force=True;
$fcDEBUG=True;
inputNB="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Scripts";
outputDir=FileNameJoin[{$HomeDirectory,"Downloads","outputMD"}];
*)


outputMD=FileNameJoin[{outputDir,FileBaseName[inputNB]<>".md"}]


SetDirectory[outputDir];


If[!FileExistsQ[inputNB],
	Print["ERROR! The file ", inputNB, " does not exist!" ];
	QuitAbort[]
];
If[!DirectoryQ[outputDir],
	Print["ERROR! The directory ", outputDir, " does not exist!" ];
	QuitAbort[]
];
(*If[FileExistsQ[outputMD] && force=!=True,
	Print["Markdown file ", outputMD, " already exists, skipping." ];
	QuitAbort[]
];*)


(*
This useful piece of code for determining when SelectionEvaluate 
finished comes from Arnoud Buzing
<https://stackoverflow.com/questions/7626491/
uncaught-throw-generated-by-jlink-or-usefrontend/8054270#8054270>
*)
NotebookPauseForEvaluation[nb_] := 
	Module[{},		
		While[NotebookEvaluatingQ[nb],WriteString["stdout","."];Pause[.5]];
		WriteString["stdout","\n"]
	];
	
NotebookEvaluatingQ[nb_]:=
	Module[{},
		SelectionMove[nb,All,Notebook];
		Or@@Map["Evaluating"/.#&,Developer`CellInformation[nb]]
	];	


nb = NotebookOpen[inputNB, WindowSize -> {1366, 800},
     Visible -> False, Magnification->1.5];
Print["Evaluating the notebook ", inputNB];
SetOptions[nb,CreateCellID->False];
SetOptions[nb,PrivateNotebookOptions->{"FileOutlineCache"->False},"TrackCellChangeTimes"->False];
time=AbsoluteTime[];
SetSelectedNotebook[nb];


(*Insert the FeynCalc cell*)
NotebookWrite[nb,"$FeynCalcStartupMessages=False; \n <<FeynCalc`"]
SelectionMove[nb, Next, Cell];
SelectionMove[nb, Previous, Cell];
SelectionEvaluate[nb];


NotebookPauseForEvaluation[nb];


(*Delete the FeynCalc cell*)
SelectionMove[nb,All,Notebook];
SelectionMove[nb, Before, Cell];
SelectionMove[nb, Next, Cell];
NotebookDelete[nb]


SelectionMove[nb, First, Cell];
NotebookDelete[nb];
SelectionMove[nb, Last, Cell];
NotebookFind[nb, "Code", All, CellStyle];
(*FrontEndExecute[FrontEndToken[nb, "SelectionConvert", "RawInputForm"]];*)
SelectionMove[nb, All, Notebook];
SelectionEvaluate[nb];


NotebookPauseForEvaluation[nb];
Print["Evaluation done, timing ", N[AbsoluteTime[] - time, 2]];


(*
Load the M2MD converter of Mathematica notebooks to markdown
by Kuba Podkalicki <https://github.com/kubaPod/M2MD>
*)
Get["M2MD`"];


(*
Need to replace 
M2MD[style_, cell:_[BoxData @ FormBox[_, TraditionalForm], ___], OptionsPattern[]
] := MDElement["LaTeXBlock", BoxesToTeXString @ cell ]

with

M2MD[style_, cell:_[BoxData @ FormBox[_, TraditionalForm], ___], OptionsPattern[]
] := MDElement["LaTeXBlock", BoxesToTeXString @ cell ]/; FreeQ[cell,DynamicModuleBox|"\[SpanFromLeft]"|"MessageTemplate2"]

and


M2MD[style_, cell:_[_BoxData, ___], opt : OptionsPattern[]] :=
  ToImageElement[res, opt]

with 

cellFixRule={
TemplateBox[{s1__String, i1_Integer, i2_Integer, i3_Integer, 
    i4_Integer, s2__String}, "MessageTemplate2"] /; ! 
   MemberQ[{i1, i2, i3, i4}, 0] :> TemplateBox[{s1, 0, 0, 0, 0, s2}, "MessageTemplate2"]
};

M2MD[style_, cell:_[_BoxData, ___], opt : OptionsPattern[]] :=
Block[{res},
  res = cell/.cellFixRule;
  Print["Sending this to ToImageElement"];
  Print[""];
  Print[res];
  ToImageElement[res, opt]
]







also rename *.png to *.svg everywhere in the source
*)


M2MD[style_, cell : _[BoxData @ FractionBox[__], ___], 
   OptionsPattern[]
   ] := M2MD`Private`MDElement["LaTeXBlock", M2MD`BoxesToTeXString @ cell ];


M2MD[style_, cell : _[BoxData @ RowBox[__], ___], 
   OptionsPattern[]
   ] := M2MD`Private`MDElement["LaTeXBlock", M2MD`BoxesToTeXString @ cell ];


System`Convert`TeXFormDump`maketex[i_Integer] := ToString[i];


fixTitle[cell_, ___]:= "---\ntitle: "<>
	M2MD`Private`BoxesToString[cell, "PlainText"]<>"\n---\n";
Print["Exporting the notebook to ", outputMD];
MDExport[outputMD, nb,"CellStyleRules"-> <|
  "Title"->{"Text",fixTitle}|>];
Print["Export done."];



