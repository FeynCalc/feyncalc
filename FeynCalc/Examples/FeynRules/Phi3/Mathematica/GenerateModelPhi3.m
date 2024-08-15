(* ::Package:: *)

(* :Title: GenerateModelPhi3											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Generates FeynArts model for the scalar phi^3 theory 		*)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*phi^3 model for FeynArts*)


(* ::Subsection:: *)
(*Load FeynRules*)


FR$Parallel=False;
$FeynRulesPath=FileNameJoin[{$UserBaseDirectory,"Applications","FeynRules"}];
<<FeynRules`;


(* ::Subsection:: *)
(*Load FeynRules model*)


If[$FrontEnd===Null,
nbDir=DirectoryName[$InputFileName],
nbDir=NotebookDirectory[]
];


frModelPath=FileNameJoin[{nbDir,"Phi3.fr"}];
LoadModel[frModelPath];


(* ::Subsection:: *)
(*Create FeynArts model*)


FR$Loop=True;
SetDirectory[FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts","Models"}]];
WriteFeynArtsOutput[LPhi3,Output->"Phi3",CouplingRename->False];
