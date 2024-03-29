(* ::Package:: *)

(* ::Section:: *)
(*Yukawa model for FeynArts*)


(* ::Subsection:: *)
(*Load FeynRules*)


FR$Parallel=False;
$FeynRulesPath=SetDirectory[$UserBaseDirectory<>"/Applications/FeynRules"];
<<FeynRules`;


(* ::Subsection:: *)
(*Load FeynRules model*)


SetDirectory[NotebookDirectory[]];
LoadModel["Yukawa.fr"];


LY


FeynmanRules[LY]


(* ::Subsection:: *)
(*Create FeynArts model*)


(*Without FR$Loop=True; the counter-terms will not be included into the model file *)
FR$Loop=True;
SetDirectory[FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts","Models"}]];
WriteFeynArtsOutput[LY,Output->"LY",CouplingRename->False]



