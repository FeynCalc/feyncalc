(* ::Package:: *)

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
inputNB="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Mathematica/Tutorials/LightCone.m";
outputDir=FileNameJoin[{$HomeDirectory,"Downloads","outputMD"}];
loadAddOns={};
*)
(*
inputNB="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation/Mathematica/FIRE/Functions/FIRECreateConfigFile.m";
outputDir="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation/Markdown";
loadAddOns="FeynHelpers";
*)


outputMD=FileNameJoin[{outputDir,FileBaseName[inputNB]<>".md"}];


Quiet[CreateDirectory[outputDir]];
SetDirectory[outputDir];


If[!FileExistsQ[inputNB],
	Print["ERROR! The file ", inputNB, " does not exist!" ];
	QuitAbort[]
];
If[!DirectoryQ[outputDir],
	Print["ERROR! The directory ", outputDir, " does not exist!" ];
	QuitAbort[]
];


If[loadAddOns==="{}" || inputNB===loadAddOns,
	loadAddOns={},	
	loadAddOns=ToString/@ToExpression[loadAddOns]
];


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


If[DoNotLoadFeynCalc=!="True",
(*Insert the FeynCalc cell*)
NotebookWrite[nb,"$FeynCalcStartupMessages=False; \n $LoadAddOns = "<>
ToString[loadAddOns,InputForm]<>"; \n <<FeynCalc`"];
SelectionMove[nb, Next, Cell];
SelectionMove[nb, Previous, Cell];
SelectionEvaluate[nb];
];


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

add 

M2MD[ "Print", Cell[inputForm_String, ___], ___]:= MDElement["Output", inputForm, $CodeLanguage];
after
M2MD[ "Output", Cell[inputForm_String, ___], ___]:= MDElement["CodeBlock", inputForm, $CodeLanguage];

also rename *.png to *.svg everywhere in the source
*)


(*
Load the M2MD converter of Mathematica notebooks to markdown
by Kuba Podkalicki <https://github.com/kubaPod/M2MD>
*)
Get["M2MD`"];


M2MD[style_, cell : _[BoxData @ FractionBox[__], ___], 
   OptionsPattern[]
   ] := M2MD`Private`MDElement["LaTeXBlock", M2MD`BoxesToTeXString @ cell ];


M2MD[style_, cell : _[BoxData @ RowBox[__], ___], 
   OptionsPattern[]
   ] := M2MD`Private`MDElement["LaTeXBlock", M2MD`BoxesToTeXString @ cell ];


System`Convert`TeXFormDump`maketex[i_Integer] := ToString[i];


(*M2MD[Cell["\<\
#compressor zstd
#threads 8
#fthreads s16
#lthreads 4
#sthreads 8
#variables d, mb, mg
#bucket 29
#start
#folder ./
#problem 4242 asyR1prop2Ltopo01310X11111N1.start
#integrals LIs.m
#output asyR1prop2Ltopo01310X11111N1.tables\
\>", "Print",
 CellLabel->"During evaluation of In[58]:="]]*)


fixTitle[cell_, ___]:= "---\ntitle: "<>
	M2MD`Private`BoxesToString[cell, "PlainText"]<>"\n---\n";
Print["Exporting the notebook to ", outputMD];
MDExport[outputMD, nb,"CellStyleRules"-> <|
  "Title"->{"Text",fixTitle}|>];
Print["Export done."];



