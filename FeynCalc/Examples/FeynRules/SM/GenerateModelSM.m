(* ::Package:: *)

(* :Title: GenerateModelSM												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Generates FeynArts model for the SM that comes with FeynRules *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*SM model for FeynArts*)


(* ::Subsection:: *)
(*Load FeynRules*)


FR$Parallel=False;
$FeynRulesPath=SetDirectory[$UserBaseDirectory<>"/Applications/FeynRules"];
<<FeynRules`;


(* ::Subsection:: *)
(*Load FeynRules model*)


frModelPath=FileNameJoin[{$FeynRulesPath,"Models","SM","SM.fr"}];
LoadModel[frModelPath];


(* ::Subsection:: *)
(*Create FeynArts model*)


SetDirectory[FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts","Models"}]];
WriteFeynArtsOutput[LSM,Output->"SM",CouplingRename->False];



