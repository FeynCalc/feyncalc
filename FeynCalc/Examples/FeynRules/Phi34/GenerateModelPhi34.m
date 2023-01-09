(* ::Package:: *)

(* :Title: GenerateModelPhi34												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Generates FeynArts model for the scalar theory 		
	with phi^3 and phi^4 interactions								*)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*phi^3+phi^4 model for FeynArts*)


(* ::Subsection:: *)
(*Load FeynRules*)


FR$Parallel=False;
$FeynRulesPath=SetDirectory[$UserBaseDirectory<>"/Applications/FeynRules"];
<<FeynRules`;


(* ::Subsection:: *)
(*Load FeynRules model*)


frModelPath=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","Examples","FeynRules","Phi34","Phi34.fr"}];
LoadModel[frModelPath];


(* ::Subsection:: *)
(*Create FeynArts model*)


FR$Loop=True;
SetDirectory[FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts","Models"}]];
WriteFeynArtsOutput[LPhi34,Output->"Phi34",CouplingRename->False];









