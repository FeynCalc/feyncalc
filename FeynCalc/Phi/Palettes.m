(* Wolfram Language package *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Palettes (Phi)													*)

(*
	This software is covered by the GNU Lesser General Public License 3.
	Copyright (C) 1990-2015 Rolf Mertig
	Copyright (C) 1997-2015 Frederik Orellana
	Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:	Palettes for PHI											*)

(* ------------------------------------------------------------------------ *)


LoadConfiguration::usage =
"LoadConfiguration[c] loads a configuration file c.conf. c must be a string like e.g. \
\"QED\".";

LoadLagrangian::usage =
"LoadLagrangian[l] loads the lagrangian l. l must be a non-string \
like e.g. QED[1] (or a string like e.g. \"QED1\").";

ReloadPhiFA::usage =
"ReloadPhi[conf] reloads Phi with configuration conf, where conf must \
be given as a string, like e.g. \"QED\", and then reloads FeynArts.";

RebuildConfigurationsPalette::usage =
"RebuildConfigurationsPalette rebuilds, saves and \
(re)opens the configurations palette.";

RebuildLagrangiansPalette::usage =
"RebuildLagrangiansPalette rebuilds, saves and \
(re)opens the lagrangians palette.";

Begin["`Package`"]
End[]

Begin["`Palettes`Private`"];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Configurations *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* LoadConfiguration is called in PhiStart.m *)

LoadConfiguration["None"] :=
	Null;
LoadConfiguration[None] :=
	Null;
LoadConfiguration[conf_] :=
	Block[ {olddir},
		olddir = Directory[];
		FCPrint[3, "Storing current directory ", olddir];
		SetDirectory[Phi`$HEPDir];
		SetDirectory["FeynCalc"];
		SetDirectory["Phi"];
		SetDirectory["Configurations"];
		FCPrint[3, "Loading configuration ", conf];
		Global`$Configuration = conf;
		$PaletteConfiguration = conf;
			(* Set context for the name of the configuration *)
		Evaluate[ToExpression["Phi`Objects`"<>conf]];
		Get[conf <> ".conf"];
		FCPrint[3, "Resetting to directory ", olddir];
		SetDirectory[olddir];
	];


ReloadPhiFA[conf_] :=
	(
	$PaletteConfiguration = conf;
	Get["Phi`Phi`"];
	Get["FeynArts`"]);

RebuildConfigurationsPalette :=
	Block[ {names, width, height, olddir, nb, bbc},

		(* Close the clicked notebook *)
		NotebookClose[ButtonNotebook[]];

		(* The directory is set *)
		olddir = Directory[];
		SetDirectory[Phi`$HEPDir];
		SetDirectory["FeynCalc"];
		SetDirectory["Phi"];
		SetDirectory["Configurations"];

		(* A list of the configuration files present: *)

		(*Change 14/5 - 1999, Mac filenames start with colon*)
		names = If[ StringMatchQ[#, ":*"] && StringMatchQ[$System, "MacOS*"],
					StringDrop[#, 1],
					#
				] & /@ (StringDrop[#, -5] & /@
							FileNames["*.conf"]);

		(* The corresponding list of button expressions.   For each
		configuration, a name is generated and assigned a definition: *)
		bbc = List /@ (
		ButtonBox[#, ButtonFunction :> LoadConfiguration[#],
		ButtonEvaluator :> Automatic] & /@ names);
		width = Ceiling[95/11*Max[Join[StringLength /@ names, {9}]]];
		height = Ceiling[140/7*Length[names] + 100];

		(* A notebook is created: *)
		nb =
				Notebook[

		(* Headline *)
		{Cell[BoxData[StyleBox["Load \n configuration:"]], NotebookDefault,
		CellMargins -> {{Inherited, Inherited}, {5, Inherited}},
		Background -> GrayLevel[0.9], CellFrame -> False, Evaluatable -> False,
		CellHorizontalScrolling -> False, LineSpacing -> {1.0, 0},
		FormatType -> InputForm, ScriptMinSize -> 9,
		ShowStringCharacters -> False, FontFamily -> "Times",
		FontWeight -> "Bold"],

				(* Configuration buttons *)
				Cell[BoxData[
						StyleBox[
							GridBox[bbc, RowSpacings -> 0, ColumnSpacings -> 0,
								GridDefaultElement :> ButtonBox[ "\[Placeholder]"]],
							ButtonBoxOptions -> {ButtonEvaluator -> Automatic, Active -> True,
						ButtonStyle -> "Evaluate"}]], NotebookDefault,
		CellMargins -> {{Inherited, Inherited}, {5, Inherited}},
		Evaluatable -> True, CellGroupingRules -> "InputGrouping",
		CellHorizontalScrolling -> True, PageBreakAbove -> True,
		PageBreakWithin -> False, GroupPageBreakWithin -> False,
		CellLabelMargins -> {{11, Inherited}, {Inherited, Inherited}},
		DefaultFormatType -> DefaultInputFormatType, LineSpacing -> {1.25, 0},
		FormatType -> InputForm, ScriptMinSize -> 9,
		ShowStringCharacters -> True, NumberMarks -> True,
		CounterIncrements -> "Input", StyleMenuListing -> None,
		FontFamily -> "Courier", FontWeight -> "Bold"],

				(* Save and reload buttons*)
				Cell[BoxData[
						StyleBox[
							GridBox[{{ButtonBox["Rebuild", ButtonFunction :> RebuildConfigurationsPalette,
											ButtonEvaluator :> Automatic]}, {ButtonBox["Reload Phi",
											ButtonFunction :> ReloadPhiFA[$PaletteConfiguration],
					ButtonEvaluator :> Automatic]}},
								RowSpacings -> 0, ColumnSpacings -> 0],
							ButtonBoxOptions -> {ButtonEvaluator -> Automatic, Active -> True,
						ButtonStyle -> "Evaluate"}]], NotebookDefault,
		CellMargins -> {{Inherited, Inherited}, {5, Inherited}},
		Evaluatable -> True, CellGroupingRules -> "InputGrouping",
		CellHorizontalScrolling -> True, PageBreakAbove -> True,
		PageBreakWithin -> False, GroupPageBreakWithin -> False,
		CellLabelMargins -> {{11, Inherited}, {Inherited, Inherited}},
		DefaultFormatType -> DefaultInputFormatType, LineSpacing -> {1.25, 0},
		AutoItalicWords -> {}, FormatType -> InputForm, ScriptMinSize -> 9,
		ShowStringCharacters -> True, NumberMarks -> True,
		SingleLetterItalics -> False, CounterIncrements -> "Input",
		StyleMenuListing -> None, FontFamily -> "Courier",
		FontWeight -> "Bold"]},

		Background -> GrayLevel[0.9],
						WindowTitle -> "Configurations", (*ScreenRectangle -> {{0, 1152}, {0, 850}},*)
						Editable -> False, WindowToolbars -> {}, PageWidth -> 342,
						WindowSize -> {width, height},
						WindowMargins -> {{0, Automatic}, {Automatic, 0}},
						WindowFrame -> "Palette", WindowElements -> {},
						WindowFrameElements -> "CloseBox", WindowClickSelect -> False,
						ScrollingOptions -> {"PagewiseScrolling" -> True},
						ShowCellBracket -> False, CellMargins -> {{0, 0}, {Inherited, 0}},
						Active -> True, CellOpen -> True, ShowCellLabel -> False,
						ShowCellTags -> False,
						ImageMargins -> {{0, Inherited}, {Inherited, 0}},
						Magnification -> 1];

		(* The notebook is saved and opened *)
		ResetDirectory[];
		SetDirectory["Palettes"];
		Put[nb, "PhiConfigurations.nb"];
		NotebookOpen[ToFileName[{Phi`$HEPDir,"FeynCalc",
							"Phi","Palettes"},"PhiConfigurations.nb"]];

		(* The directory is reset *)
		SetDirectory[olddir];
	];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Lagrangians *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



LoadLagrangian[] :=
	None;

LoadLagrangian[fn_] /; Depth[fn] == 1 :=
	Block[ {olddir},
		olddir = Directory[];
		FCPrint[3, "Storing current directory ", olddir];
		SetDirectory[Phi`$HEPDir];
		SetDirectory["FeynCalc"];
		SetDirectory["Phi"];
		SetDirectory["Lagrangians"];
			(*Allow using strings. 6/4-2002*)
			(*Allow not using strings, keep context Global`. 13/5-2003*)
			(*FCPrint[3,
			"Putting the following in context Phi`Objects`: ",
				Global`$Lagrangians//FullForm];
			Global`$Lagrangians =
				Union[ToExpression["Phi`Objects`"<>ToString[#]] & /@ Global`$Lagrangians];*)
		Get[ToString[fn] <> ".m"];
		FCPrint[2,"$Lagrangians is now ", Global`$Lagrangians//FullForm];
		FCPrint[3, "Resetting to directory ", olddir];
		SetDirectory[olddir];
		FAUpdate;
	];

LoadLagrangian[fn_] /; Depth[fn] == 2 :=
	Block[ {olddir},
		olddir = Directory[];
		FCPrint[3, "Storing current directory ", olddir];
		SetDirectory[Phi`$HEPDir];
		SetDirectory["FeynCalc"];
		SetDirectory["Phi"];
		SetDirectory["Lagrangians"];
			(*Get[StringDrop[ToString[fn], -3] <> *) (*Change 17/9-2000*)
			(*Allow using strings. 6.4.2002*)
			(*FCPrint[3,
			"Putting the following in context Phi`Objects`: ",
				Global`$Lagrangians//FullForm];
			Global`$Lagrangians =
				Union[ToExpression["Phi`Objects`"<>ToString[#[[0]]]][#[[1]]] & /@ Global`$Lagrangians];*)
		Get[ToString[ToExpression[ToString[fn]][[0]]] <>
				ToString[ToExpression[ToString[fn]][[1]]] <> ".m"];
		FCPrint[2,"$Lagrangians is now ", Global`$Lagrangians//FullForm];
		FCPrint[3, "Resetting to directory ", olddir];
		SetDirectory[olddir];
		FAUpdate;
	];

LoadLagrangian[fn__] /; Length[{fn}]>1 :=
	(LoadLagrangian /@ {fn};);



RebuildLagrangiansPalette :=
	Block[ {names, width, height, olddir, nb, bbc},

		(* Close the clicked notebook *)
		NotebookClose[ButtonNotebook[]];

		(* The directory is set *)
		olddir = Directory[];
		SetDirectory[Phi`$HEPDir];
		SetDirectory["FeynCalc"];
		SetDirectory["Phi"];
		SetDirectory["Lagrangians"];

		(* A list of the configuration files present: *)
		names = If[ StringMatchQ[#, ":*"]&& StringMatchQ[$System, "MacOS*"],
					StringDrop[#, 1],
					#
				] & /@ (StringDrop[#, -2] & /@
							Select[Select[
									FileNames["*.m"], ! StringMatchQ[#, "d.*"] &],
									!StringMatchQ[#, "exp.*"] &]);

		(* The corresponding list of button expressions.   For each
		configuration, a name is generated and assigned a definition: *)
		bbc = List /@ (
		ButtonBox[#, ButtonFunction :> LoadLagrangian[#],
		ButtonEvaluator :> Automatic] & /@ names);
		width = Ceiling[95/11*Max[Join[StringLength /@ names, {9}]]];
		height = Ceiling[140/7*Length[names] + 70];

		(* A notebook is created: *)
		nb =
				Notebook[

		(* Headline *)
		{Cell[BoxData[StyleBox["Load \n lagrangian:"]], NotebookDefault,
					CellMargins -> {{Inherited, Inherited}, {5, Inherited}},
					Background -> GrayLevel[0.9], CellFrame -> False, Evaluatable -> False,
					CellHorizontalScrolling -> False, LineSpacing -> {1.0, 0},
					FormatType -> InputForm, ScriptMinSize -> 9,
					ShowStringCharacters -> False, FontFamily -> "Times",
					FontWeight -> "Bold"],

				(* Langrangian buttons *)
				Cell[BoxData[
					StyleBox[
						GridBox[bbc, RowSpacings -> 0, ColumnSpacings -> 0,
							GridDefaultElement :> ButtonBox[ "\\[Placeholder]"]],
						ButtonBoxOptions -> {ButtonEvaluator -> Automatic, Active -> True,
								ButtonStyle -> "Evaluate"}]], NotebookDefault,
				CellMargins -> {{Inherited, Inherited}, {5, Inherited}},
				Evaluatable -> True, CellGroupingRules -> "InputGrouping",
				CellHorizontalScrolling -> True, PageBreakAbove -> True,
				PageBreakWithin -> False, GroupPageBreakWithin -> False,
				CellLabelMargins -> {{11, Inherited}, {Inherited, Inherited}},
				DefaultFormatType -> DefaultInputFormatType, LineSpacing -> {1.25, 0},
				FormatType -> InputForm, ScriptMinSize -> 9,
				ShowStringCharacters -> True, NumberMarks -> True,
				CounterIncrements -> "Input", StyleMenuListing -> None,
				FontFamily -> "Courier", FontWeight -> "Bold"],

				(* Save and reload buttons*)
				Cell[BoxData[
						StyleBox[
							GridBox[{{ButtonBox["Rebuild", ButtonFunction :> RebuildLagrangiansPalette,
											ButtonEvaluator :> Automatic]}},
								RowSpacings -> 0, ColumnSpacings -> 0],
							ButtonBoxOptions -> {ButtonEvaluator -> Automatic, Active -> True,
						ButtonStyle -> "Evaluate"}]], NotebookDefault,
		CellMargins -> {{Inherited, Inherited}, {5, Inherited}},
		Evaluatable -> True, CellGroupingRules -> "InputGrouping",
		CellHorizontalScrolling -> True, PageBreakAbove -> True,
		PageBreakWithin -> False, GroupPageBreakWithin -> False,
		CellLabelMargins -> {{11, Inherited}, {Inherited, Inherited}},
		DefaultFormatType -> DefaultInputFormatType, LineSpacing -> {1.25, 0},
		AutoItalicWords -> {}, FormatType -> InputForm, ScriptMinSize -> 9,
		ShowStringCharacters -> True, NumberMarks -> True,
		SingleLetterItalics -> False, CounterIncrements -> "Input",
		StyleMenuListing -> None, FontFamily -> "Courier",
		FontWeight -> "Bold"]},

		Background -> GrayLevel[0.9],
					WindowTitle -> "Load lagrangian",
					(*ScreenRectangle -> {{0, 1152}, {0, 850}},*) Editable -> False,
					WindowToolbars -> {}, PageWidth -> 342,
					WindowSize -> {width, height},
					WindowMargins -> {{147, Automatic}, {Automatic, 0}},
					WindowFrame -> "Palette", WindowElements -> {},
					WindowFrameElements -> "CloseBox", WindowClickSelect -> False,
					ScrollingOptions -> {"PagewiseScrolling" -> True},
					ShowCellBracket -> False, CellMargins -> {{0, 0}, {Inherited, 0}},
					Active -> True, CellOpen -> True, ShowCellLabel -> False,
					ShowCellTags -> False,
					ImageMargins -> {{0, Inherited}, {Inherited, 0}},
					Magnification -> 1];

		(* The notebook is saved and opened *)
		ResetDirectory[];
		SetDirectory["Palettes"];
		Put[nb, "PhiLagrangians.nb"];
		NotebookOpen[ToFileName[{Phi`$HEPDir,"FeynCalc",
							"Phi","Palettes"},"PhiLagrangians.nb"]];

		(* The directory is reset *)
		SetDirectory[olddir];
	];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

FCPrint[1,"Palettes.m loaded"];
End[];
