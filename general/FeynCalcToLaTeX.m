(* :Title: FeynCalcToLaTeX *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: useful for documentation, but probably also otherwise *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`FeynCalcToLaTeX`",
             "HighEnergyPhysics`FeynCalc`"];

FeynCalcToLaTeX::usage="FeynCalcToLaTeX[expr] generates LaTeX with line-breaking 
for expr.
FeynCalcToLaTeX[expr, 500] generates LaTeX for expr with 500 being the Window width
 setting for the Mathematica frontend. Increasing its value will generate less
line breaks.";

F2L::usage="F2L is the same as FeynCalcToLaTeX.";

Begin["`Private`"];

MakeContext[StringChomp];

F2L = FeynCalcToLaTeX;

FeynCalcToLaTeX[expr_, width_:500] := If[!$Notebooks,
   Needs["JLink`"];
   JLink`InstallJava[];
   JLink`UseFrontEnd[f2tex[expr, width]],
   f2tex[expr,width]
];

(* this is of course heuristics; should change to java.util.regexp or so ... *)

f2tex[expr_, width_:500] := Module[{r, n, w,y,z}, 
   r = Cell[BoxData[FormBox[(MakeBoxes[#1, TraditionalForm] & )[expr], TraditionalForm]], 
      "Output"]; n = NotebookPut[Notebook[{r}, WindowSize -> {width, Inherited}, 
       Visible -> False]]; o = StringJoin[$TemporaryPrefix, ToString[Random[Integer, 10^8]], 
      "fc2latex.tex"]; TeXSave[o, n]; t = Import[o, "Text"]; 
    w = StringDrop[StringChomp[ StringReplace[StringDrop[t, 
         StringPosition[t, "dispSFoutmath"][[1,1]] + 13],
          {"\\end{document}" -> "", 
         "\\MathBegin{MathArray}{l}"->"",
         "\\MathBegin{MathArray}[p]{l}"->"",
         "\\MathEnd{MathArray}"->"",
         "\\NoBreak" -> ""}
         ]], -2]; DeleteFile[o]; NotebookClose[n]; 
    y=StringChomp[StringReplace[StringJoin@@(Characters[
        w]//.{in___,"\\",_,"s","p","a","c","e","{",___,"}",
          fi___}\[RuleDelayed]
        {in,fi}) ,"\\noalign{}"->""]];    
    z = StringReplace[FixedPoint[StringReplace[#, "  "->" "]&,y],"\n \n"->"\n"];
    z=StringReplace[StringReplace[z,"\\\\\n"->"\n\\\\\n"],"\n\n"->""];
    z = StringReplace[z,"\n\\\\ (\n"-> "\n\\\\ \n( "];
    If[StringMatchQ[z,"*\\\\"], z=StringDrop[z,-2]];
    If[StringLength[z]>1,
       If[StringTake[z,1]==="{" && StringTake[z,-1]==="}", 
           z = StringDrop[StringDrop[z, 1], -1]]
      ];
    (*z = "$\n"<>z<>"\n$";*)
    (*not always useful, comment it out for now
     CellPrint[Cell[z, "Program"]]
    *);z]

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynCalcToLaTeX| \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

