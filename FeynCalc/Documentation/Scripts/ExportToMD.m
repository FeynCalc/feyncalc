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
inputNB="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Mathematica/LoopIntegrals/FCLoopGraphPlot.m";
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


fixTitle[cell_, ___]:= "---\ntitle: "<>
	M2MD`Private`BoxesToString[cell, "PlainText"]<>"\n---\n";
Print["Exporting the notebook to ", outputMD];
MDExport[outputMD, nb,"CellStyleRules"-> <|
  "Title"->{"Text",fixTitle}|>];
Print["Export done."];


Options[svgExport] = {"CommentString" -> "Created by Mathematica", 
   AspectRatio -> Automatic, Background -> Automatic};
Clear[svgExport];
svgExport[name_String, gr_, opts : OptionsPattern[]] := 
 Module[{svgCode = 
    StringReplace[
       ExportString[
        First@ImportString[
          ExportString[gr, "PDF", 
           Background -> OptionValue[Background]], "PDF"], "SVG", 
        Background -> OptionValue[Background]], 
       "<svg " -> 
        "<svg " <> 
         If[OptionValue[AspectRatio] === Full, 
          " preserveAspectRatio='none' ", " "]] &[
     ToString /@ 
      ImageDimensions[
       Rasterize[Show[gr, ImagePadding -> 0], "Image"]]]}, 
  Export[name, 
   StringReplace[svgCode, 
    RegularExpression["(<svg\\b[^>]*>)"] :> 
     "$1" <> "\n<!-- ***Exported Comment***\n" <> 
      OptionValue["CommentString"] <> "\n***Exported Comment*** -->"],
    "Text"]]



?ImportString


ExportString[XXX,"SVG"]


ImportString[ExportString[XXX,"PDF"],"PDF"]


Export["tt.svg",ExportString[XXX,"SVG"],"Text"]


Export["tt.svg",ExportString[ImportString[ExportString[XXX,"PDF"],"PDF"],"SVG"],"Text"]


svgExport["tt.svg",XXX]


Options[Export]


XXX//InputForm


?Graph


XXX


(* ::Output:: *)
(*Cell[BoxData[*)
(* FormBox[*)
(*  TagBox[Cell[BoxData[*)
(*    FormBox[*)
(*     GraphicsBox[*)
(*      NamespaceBox["NetworkGraphics",*)
(*       DynamicModuleBox[{Typeset`graph = HoldComplete[*)
(*         Graph[{-3, 3, -2, 1, -1, 2}, {Null, {{1, 2}, {3, 4}, {5, 6}, {4, 6}, {4, 2}, {6, 2}}, {1, 1, 1, 1, 1, 1}}, {*)
(*          EdgeStyle -> {UndirectedEdge[-2, 1, 1] -> {{*)
(*                Thickness[Large], *)
(*                GrayLevel[0]}}, UndirectedEdge[2, 3, 1] -> {{*)
(*                Dashing[{Small, Small}], *)
(*                Thickness[Large], *)
(*                GrayLevel[0]}}, UndirectedEdge[1, 3, 1] -> {{*)
(*                Dashing[{Small, Small}], *)
(*                Thickness[Large], *)
(*                GrayLevel[0]}}, UndirectedEdge[-3, 3, 1] -> {{*)
(*                Thickness[Large], *)
(*                GrayLevel[0]}}, UndirectedEdge[-1, 2, 1] -> {{*)
(*                Thickness[Large], *)
(*                GrayLevel[0]}}, UndirectedEdge[1, 2, 1] -> {{*)
(*                Dashing[{Small, Small}], *)
(*                Thickness[Large], *)
(*                GrayLevel[0]}}}}]]}, *)
(*        TagBox[GraphicsGroupBox[*)
(*          GraphicsComplexBox[{{1.343726388522676, 0.}, {1.3560507330282476`, 1.021552696390113}, {2.74960390125566, 2.358141948500332}, {1.855730226260601,*)
(*            1.8605568750353632`}, {0., 2.3976396495913717`}, {0.878214825988157, 1.8738525581311334`}}, {*)
(*            {Hue[0.6, 0.7, 0.5], Opacity[0.7], *)
(*             {GrayLevel[0], Thickness[Large], *)
(*              {Arrowheads[0.], ArrowBox[{1, 2}, 0.02841652528500503]}}, *)
(*             {GrayLevel[0], Thickness[Large], Dashing[{Small, Small}], *)
(*              {Arrowheads[0.], ArrowBox[{2, 4}, 0.02841652528500503]}}, *)
(*             {GrayLevel[0], Thickness[Large], Dashing[{Small, Small}], *)
(*              {Arrowheads[0.], ArrowBox[{2, 6}, 0.02841652528500503]}}, *)
(*             {GrayLevel[0], Thickness[Large], *)
(*              {Arrowheads[0.], ArrowBox[{3, 4}, 0.02841652528500503]}}, *)
(*             {GrayLevel[0], Thickness[Large], Dashing[{Small, Small}], *)
(*              {Arrowheads[0.], ArrowBox[{4, 6}, 0.02841652528500503]}}, *)
(*             {GrayLevel[0], Thickness[Large], *)
(*              {Arrowheads[0.], ArrowBox[{5, 6}, 0.02841652528500503]}}}, *)
(*            {Hue[0.6, 0.2, 0.8], EdgeForm[{GrayLevel[0], Opacity[0.7]}], DiskBox[1, 0.02841652528500503], DiskBox[2, 0.02841652528500503], *)
(*             DiskBox[3, 0.02841652528500503], DiskBox[4, 0.02841652528500503], DiskBox[5, 0.02841652528500503], DiskBox[6, 0.02841652528500503]}}]],*)
(*         MouseAppearanceTag["NetworkGraphics"]],*)
(*        AllowKernelInitialization->False]],*)
(*      DefaultBaseStyle->{"NetworkGraphics", FrontEnd`GraphicsHighlightColor -> Hue[0.8, 1., 0.6]},*)
(*      FormatType->TraditionalForm,*)
(*      FrameTicks->None], TraditionalForm]], "Output"],*)
(*   DisplayForm], TraditionalForm]], "Output",*)
(* CellLabel->"Out[233]//DisplayForm="]*)
