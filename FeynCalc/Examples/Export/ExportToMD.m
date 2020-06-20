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
(*$fcDEBUG=True;
inputNB=FileNameJoin[{$UserBaseDirectory, "Applications", "FeynCalc","Examples",
"QCD","Tree","ElAel-QQbar.m"}];
outputDir=FileNameJoin[{$HomeDirectory,"Downloads","outputMD"}];*)


If[!FileExistsQ[inputNB],
	Print["ERROR! The file ", inputNB, " does not exist!" ];
	QuitAbort[]
];
If[!DirectoryQ[outputDir],
	Print["ERROR! The directory ", outputDir, " does not exist!" ];
	QuitAbort[]
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
	Module[{},DirectoryName
		SelectionMove[nb,All,Notebook];
		Or@@Map["Evaluating"/.#&,Developer`CellInformation[nb]]
	];	


nb = NotebookOpen[inputNB, WindowSize -> {1366, 800},
     Visible -> False, Magnification->1.5];
     
Print["Evaluating the notebook ", inputNB];
time=AbsoluteTime[];
SetSelectedNotebook[nb];
SelectionMove[nb, First, Cell];
NotebookDelete[nb];
SelectionMove[nb, Last, Cell];
NotebookFind[nb, "Code", All, CellStyle];

FrontEndExecute[FrontEndToken[nb, "SelectionConvert", "RawInputForm"]];
SelectionMove[nb, All, Notebook];
SelectionEvaluate[nb];
NotebookPauseForEvaluation[nb];
Print["Evaluation done, timing ", N[AbsoluteTime[] - time, 2]];


(*
Load the M2MD converter of Mathematica notebooks to markdown
by Kuba Podkalicki <https://github.com/kubaPod/M2MD>
*)
Get["M2MD`"];


fixTitle[cell_, ___]:= "---\ntitle: "<>
	M2MD`Private`BoxesToString[cell, "PlainText"]<>"\n---\n";
outputMD=FileNameJoin[{outputDir,FileBaseName[inputNB]<>".md"}]
Print["Exporting the notebook to", outputMD];
MDExport[outputMD, nb,"OverwriteImages"->"True","CellStyleRules"-> <|
  "Title"->{"Text",fixTitle}|>];
Print["Export done."];
