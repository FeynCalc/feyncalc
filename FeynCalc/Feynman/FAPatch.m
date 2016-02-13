(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FAPatch															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Patch for FeynArts										    *)

(* ------------------------------------------------------------------------ *)

FAPatch::usage =
"If an unpatched copy of FeynArts is present in $FeynArtsDirectory, \
evaluating FAPatch causes the files making up FeynArts to be modified in \
order for FeynArts to be compatible with FeynCalc.";

FilePatch::usage =
"FilePatch[f, rp] replaces the patterns given by rp in the file f.  \
rp should be a list of the form {string -> string, ..}.";

(* ------------------------------------------------------------------------ *)

Begin["`Package`"]
End[]

Begin["`FAPatch`Private`"]

(*	Grab FeynArts files that we want to patch	*)
part1 = FileNames[{"*.m"}, FileNameJoin[{$FeynArtsDirectory}]];
part2 = FileNames[{"*.mod", "*.gen"},FileNameJoin[{$FeynArtsDirectory, "Models"}], Infinity];
part3 = FileNames[{"*.m", "*.m-ok"},  FileNameJoin[{$FeynArtsDirectory, "FeynArts"}]];

(*	Drop symlinks, otherwise we would patch the same file twice	*)

part1 = Map[If[FileNameTake[FileInformation[#, "AbsoluteFileName"]] ===
		FileNameTake[#], #, Unevaluated@Sequence[]] &, part1];
part2 = Map[If[FileNameTake[FileInformation[#, "AbsoluteFileName"]] ===
		FileNameTake[#], #, Unevaluated@Sequence[]] &, part2];
part3 = Map[If[FileNameTake[FileInformation[#, "AbsoluteFileName"]] ===
		FileNameTake[#], #, Unevaluated@Sequence[]] &, part3];

(*	Create the final list of files to be patched	*)
mods = Flatten[Join[{
			FileNameTake/@part1,
			Map[("Models" <> $PathnameSeparator <> #)&,FileNameTake /@ part2],
			Map[("FeynArts" <> $PathnameSeparator <> #) &,FileNameTake /@ part3]
		}]];

(*The list of files to be modified*)
Options[FAPatch] = {
	File -> mods,

	(*The list of replacements*)
	(*Some regular expression utilities would be VERY nice...*)

	Replace -> Join[
			{
			"FourVector" -> "FAFourVector",
			"DiracSpinor" -> "FADiracSpinor",
			"DiracSlash" -> "FADiracSlash",
			"DiracMatrix" -> "FADiracMatrix",
			"DiracTrace" -> "FADiracTrace",
			"MetricTensor" -> "FAMetricTensor",
			"ScalarProduct" -> "FAScalarProduct",
			"ChiralityProjector" -> "FAChiralityProjector",
			"GS" -> "FAGS",
			"MM" -> "FAMass[\"Muon\"]",
			"SUNT" -> "FASUNT",
			"SUNF" -> "FASUNF",
			"InferFormat" -> "tmpInfer",
			"Loops" -> "tmploops",
			"SetLoop" -> "tmpsetloop",
			"LoopNumber"->"tmploopnumber",
			"LoopPD"->"tmplooppd",
			"LoopFields" -> "tmploopfields",
			"CreateFeynAmp" -> "tmpcreatefeynamp",
			"FeynAmpDenominator"->"tmpfeynampdenominator",
			"FeynAmpList"->"tmpfeynamplist",
			"FeynAmpCases"->"tmpfeynampcases",
			"FeynAmpExpr"->"tmpfeynampexpr"
			},
			{
			"LeviCivita" -> "FALeviCivita",
			"Loop" -> "FALoop",
			"NonCommutative" -> "FANonCommutative",
			"PolarizationVector"-> "FAPolarizationVector",
			"FeynAmp"-> "FAFeynAmp",
			"PropagatorDenominator" -> "FAPropagatorDenominator",
			"GaugeXi" -> "FAGaugeXi"
			},
			{
			"tmploops"->"Loops",
			"tmpsetloop" -> "SetLoop",
			"tmploopnumber"->"LoopNumber",
			"tmplooppd"->"LoopPD",
			"tmploopfields"->"LoopFields",
			"tmpcreatefeynamp"->"CreateFeynAmp",
			"tmpfeynampdenominator"->"FAFeynAmpDenominator",
			"tmpfeynamplist"->"FAFeynAmpList",
			"tmpfeynampcases"->"FeynAmpCases",
			"tmpfeynampexpr"->"FeynAmpExpr"
			}
					]
};

(*Error message*)
$ok = True;
checkok :=
		If[ $ok =!= True,
				If[ Global`$FeynCalcStartupMessages =!= False,
						If[ $Notebooks===True,
								CellPrint[Cell[TextData[{
								"WARNING! Your FeynArts installation is not complete or the version you have cannot be used with this version of FeynCalc.\nFeynArts can be downloaded at ", ButtonBox["www.feynarts.de", ButtonData:>{
								URL[ "http://www.feynarts.de"], None},
								ButtonStyle->"Hyperlink", ButtonNote->"http://www.feynarts.de"]}
								],"Text"]],
								WriteString["stdout", "Your FeynArts installation is not complete or the version you have cannot be used with this version of FeynCalc.\nFeynArts can be downloaded at http://www.feynarts.de/.\n"];
						];
				];
				Return[False]
		];


(* ------------------------------------------------------------------------------ *)
(*Generic patching function*)

patchPrint[x__] :=
		WriteString["stdout", StringJoin@@{x,"\n"}];

FilePatch[filename_, replacements_List] :=
		(
		(*Read and patch the file*)
		fname = ToFileName[{$FeynArtsDirectory}, filename];
		patchPrint[fname];
		str = "";
		linelist = {};
		foundrev = False;
		strm = OpenRead[fname];
		While[ToString[str] != "EndOfFile",
				str = Read[strm, String];
				str1 = str;
				Do[
					If[ StringMatchQ[ToString[str1], "*" <> replacements[[i, 1]] <> "*"],
							foundrev = True;
							repl = str1;
							str1 = StringReplace[
											StringReplace[
												str1, {replacements[[i, 1]] -> replacements[[i, 2]]},
											MetaCharacters -> Automatic], "$1" -> repl]
					],
				{i, 1, Length[replacements]}];
				If[ ToString[str] != "EndOfFile",
						If[ str =!= str1,
								patchPrint["Old: ", str, ", \nNew: ", str1]
						];
						linelist = Append[linelist, str1]
				]];
		Close[strm];
(*Write the file*)
		strm = OpenWrite[fname, PageWidth -> Infinity];
		Do[WriteString[strm, linelist[[i]], "\n"], {i, 1, Length[linelist]}];
		Close[strm];
		);


FAPatch[opts___Rule] :=
		(
		(*Check that files are there*)
		If[ StringQ[$FeynArtsDirectory] =!= True,
				$ok = False;
				Print[$ok]
		];
		If[ !checkok,
				Print[$ok];
				Return[]
		];
		If[ FileNames["FeynArts.m", $FeynArtsDirectory] === {},
				Print[$ok];
				$ok = False
		];
		If[ FileNames["Setup.m", $FeynArtsDirectory] === {},
				Print[$ok];
				$ok = False
		];
		If[ !checkok,
				Return[]
		];

		(*Check version number; must be >= 3*)
		$ok = False;

		str = "";
		linelist = {};
		strm =
		OpenRead[ToFileName[{$FeynArtsDirectory}, "FeynArts.m"]];
		While[ToString[str] != "EndOfFile",
				str = Read[strm, String];
			If[ StringMatchQ[ToString[str], "*FeynArts*Version*"],
					Do[If[ SyntaxQ[StringTake[str, -i]],
								If[ NumberQ[num1 = ToExpression[StringTake[str, -i]]],
										num = num1
								]
						], {i, 1, 7}];
					If[ num >= 3,
							$ok = True
					]
			]];
		Close[strm];
		If[ !checkok,
				Return[]
		];

		(*Check that patch has not already been applied*)
		str = "";
		If[ FileNames["FeynArts.m", $FeynArtsDirectory] =!= {},
				strm = OpenRead[$FeynArtsDirectory <> $PathnameSeparator <> "FeynArts.m"];
				While[ToString[str] != "EndOfFile",
						str = Read[strm, String];
					If[ StringMatchQ[ToString[str],
							"*FeynCalc*",
							IgnoreCase -> True],
							Close[strm];
							Return[]
					]];
				Close[strm],
				patchPrint["Cannot find FeynArts.m!"];
				Close[strm];
				Return[]
		];

(*Launch confirm dialog*)
		If[ $ok,
			If[	ChoiceDialog["An installation of FeynArts has been found in \"" <> $FeynArtsDirectory <>
				"\". This program will now patch FeynArts to allow interoperation with FeynCalc. Continue?"],
				patchPrint["OK, starting.."],
				patchPrint["OK, no files have been modified."];
				Return[]
			],
			patchPrint["Your FeynArts installation is not complete or the version you have \
			cannot be handled by this program"];
			Return[]
		];


		(* ------------------------------------------------------------------------------ *)
		(* Make it known that the FA code has been patched, change context and
			change to formatting in TraditionalForm only *)
		FeynArtsChanges = {
		"Print[\"by Hagen Eck, Sepp Kueblbeck, and Thomas Hahn\"]"->
		"$1;\nPrint[\"(patched for use with FeynCalc)\"];\n\n$1"};
		FilePatch["FeynArts.m", FeynArtsChanges];
		FilePatch["FeynArts39.m", FeynArtsChanges];

		(* The files loop *)
		filelist = File /. {opts} /. Options[FAPatch];
		replacelist = Replace /. {opts} /. Options[FAPatch];
		Do[FilePatch[filelist[[i]], replacelist], {i, 1, Length[filelist]}];

		(* Include the PHI-specific settings in Setup.m *)
		patchPrint[ "Writing PHI-specific settings to Setup.m.\n"];
		strm = OpenAppend[$FeynArtsDirectory <> $PathnameSeparator <> "Setup.m"];
		WriteString[strm,
		"\nP$Generic = Flatten[P$Generic | Phi`Objects`$ParticleHeads];\n
If[ Phi`Objects`$FermionHeads=!=None,\n
P$NonCommuting=Flatten[P$NonCommuting | Phi`Objects`$FermionHeads]];"];
		Close[strm];

		(* The following changes were done in old FAPatch.m. Currently they are not applied. *)

		(*
				in Analytic.m (small fixes?)

		"SequenceForm[StringTake[ToString[type], 3]" ->
		"SequenceForm[StringTake[ToString[type],Min[3,StringLength[ToString[type]]]]"

		"Cases[p, PropagatorDenominator[__]]" ->
		"Cases[p, HoldPattern[PropagatorDenominator[__]]]"

				in Insert.m (allow one-vertices?)

		"DeleteCases[Take[#, 2], Vertex[1, ___][_]]&/@ top," ->
		"(DeleteCases[Take[#,2],Vertex[1][_]]&/@(top/.p:Propagator[Internal][___,Vertex[1,___][_],___]:>(p/.Vertex[1]->Vertex[vertexone])))/.vertexone -> 1,"

		"MapIndexed[ Append[#1, Field@@ #2]&, top" ->
		"MapIndexed[Append[#1,Field@@#2]&,Sort[Sort[Take[#,2]]&/@ top/. {Incoming->AAA,Outgoing->AAB}]/. {AAA->Incoming,AAB->Outgoing}"

				in Utilities.m (allow one-vertices?)

		"Union[ Cases[top, Vertex[n__][_] /; {n} =!= {1}, {2}] ]" ->
		"Union[Join[Cases[Cases[top,Propagator[Internal][__]],Vertex[n__][_],Infinity],Cases[top,Vertex[n__][_]/;{n}=!={1},{2}]]]"

				in Graphics.m (small fixes?)

		"Orientation[ p1_, p2_ ] := N[ArcTan@@ (p2 - p1)]" ->
		"Orientation[ p1_, p2_ ] := N[(If[{##}=={0,0},0,ArcTan[##]]&)@@ (p2 - p1)]"

		*)
		patchPrint["\nFinished!\n"];

		);

FCPrint[1, "FAPatch.m loaded"];
End[];



