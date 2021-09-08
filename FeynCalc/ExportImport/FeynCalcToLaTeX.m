(* :Title: FeynCalcToLaTeX *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: useful for documentation, but probably also otherwise *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

FeynCalcToLaTeX::usage =
"FeynCalcToLaTeX[exp] generates LaTeX with line-breaking  for exp.

FeynCalcToLaTeX[expr, 500] generates LaTeX for exp where 500 is the Window
width  setting for the Mathematica frontend. Increasing its value will
generate less line breaks.

NB: This function appears to be broken in the recent Mathematica versions,
most likely it will be rewritten from scratch in a future version of FeynCalc.";

Begin["`Package`"]
End[]

Begin["`FeynCalcToLaTeX`Private`"]

F2L = FeynCalcToLaTeX;

FeynCalcToLaTeX[expr_, width_:500] :=
	(
	If[	!FreeQ2[expr, FeynCalc`Package`NRStuff],
			Message[FeynCalc::nrfail];
			Abort[]
	];

	If[ !$Notebooks,
		Needs["JLink`"];
		JLink`InstallJava[];
		JLink`UseFrontEnd[f2tex[expr, width]],
		f2tex[expr,width]
	]);

(* this is of course heuristics; should change to java.util.regexp or so ... *)

f2tex[expr_, width_:500] :=
	Module[ {r, n, w,y,z, tt},
		r = Cell[BoxData[FormBox[(MakeBoxes[#1, TraditionalForm] & )[expr], TraditionalForm]],
			"Output"];
		n = NotebookPut[Notebook[{r}, WindowSize -> {width, Inherited},
			Visible -> False]];
		o = StringJoin[$TemporaryPrefix, ToString[Random[Integer, 10^8]],
			"fc2latex.tex"];
		If[ $VersionNumber<=5.1,
			TeXSave[o, n],
			Export[o, r, "TeX"]
		];
		t = Import[o, "Text"];
		w = If[ (tt = StringPosition[t, "dispSFoutmath"])!={},
				StringDrop[StringChomp[
					StringReplace[
						StringDrop[t,
							StringPosition[t, "dispSFoutmath"][[1,1]] + 13],
				{"\\end{document}" -> "",
				"\\MathBegin{MathArray}{l}"->"",
				"\\MathBegin{MathArray}[p]{l}"->"",
				"\\MathEnd{MathArray}"->"",
				"\\NoBreak" -> ""}
				]], -2],
				StringChomp[
					StringReplace[
						t,
					{"\\end{document}" -> "",
					"\\MathBegin{MathArray}{l}"->"",
					"\\MathBegin{MathArray}[p]{l}"->"",
					"\\MathEnd{MathArray}"->"",
					"\\NoBreak" -> ""}
					]]
			];
		DeleteFile[o];
		NotebookClose[n];
		y = StringChomp[StringReplace[StringJoin@@(Characters[
					w]//.{in___,"\\",_,"s","p","a","c","e","{",___,"}",
						fi___}\[RuleDelayed]
					{in,fi}) ,"\\noalign{}"->""]];
		z = StringReplace[FixedPoint[StringReplace[#, "  "->" "]&,y],"\n \n"->"\n"];
		z = StringReplace[StringReplace[z,"\\\\\n"->"\n\\\\\n"],"\n\n"->""];
		z = StringReplace[z,"\n\\\\ (\n"-> "\n\\\\ \n( "];
		If[ StringMatchQ[z,"*\\\\"],
			z = StringDrop[z,-2]
		];
		If[ StringLength[z]>1,
			If[ StringTake[z,1]==="{" && StringTake[z,-1]==="}",
				z = StringDrop[StringDrop[z, 1], -1]
			]
		];
		(*z = "$\n"<>z<>"\n$";*)
		(*not always useful, comment it out for now
		CellPrint[Cell[z, "Program"]]
		;*)
		z
	]

FCPrint[1,"FeynCalcToLaTeX.m loaded."];
End[]
