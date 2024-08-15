(* ::Package:: *)

(* :Title: GenerateModelQCD													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Generates FeynArts model for QCD 								*)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*QCD model for FeynArts*)


(* ::Subsection:: *)
(*Load FeynRules*)


FR$Parallel=False;
$FeynRulesPath=FileNameJoin[{$UserBaseDirectory,"Applications","FeynRules"}]
<<FeynRules`;


(* ::Subsection:: *)
(*Load FeynRules model*)


If[$FrontEnd===Null,
nbDir=DirectoryName[$InputFileName],
nbDir=NotebookDirectory[]
];


frModelPath=FileNameJoin[{nbDir,"QCD.fr"}];
LoadModel[frModelPath];


FR$Loop=True;
SetDirectory[FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts","Models"}]];
WriteFeynArtsOutput[LQCD,Output->"QCD",CouplingRename->False];


input=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts","Models","QCD","QCD.gen"}];
tmp = Import[input, "Text"] <> "\n";
res=StringJoin[{tmp,"GaugeXi[V[5,___]]=GaugeXi[\"G\"]; \n GaugeXi[U[5,___]]=GaugeXi[\"G\"]; \n"}];
Export[input, res, "Text"];
