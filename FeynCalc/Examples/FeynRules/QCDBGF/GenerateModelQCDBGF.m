(* ::Package:: *)

(* :Title: GenerateModelQCDBGF												*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2019 Rolf Mertig
	Copyright (C) 1997-2019 Frederik Orellana
	Copyright (C) 2014-2019 Vladyslav Shtabovenko
*)

(* :Summary:  Generates FeynArts model for QCD in background field 
				formalism                                                   *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*QCD BGF model for FeynArts*)


(* ::Subsection:: *)
(*Load FeynRules*)


FR$Parallel=False;
$FeynRulesPath=SetDirectory[$UserBaseDirectory<>"/Applications/FeynRules"];
<<FeynRules`;


(* ::Subsection:: *)
(*Load FeynRules model*)


frModelPath=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","Examples","FeynRules","QCDBGF","QCDBGF.fr"}];
LoadModel[frModelPath];


(* ::Subsection:: *)
(*Generate Feynman rules*)


fRules=FeynmanRules[LQCD]


(* ::Subsection:: *)
(*Create FeynArts model*)


SetDirectory[FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","FeynArts","Models"}]];
WriteFeynArtsOutput[LQCD,Output->"QCDBGF",CouplingRename->False,SelectParticles->{
{ghG,ghGbar,B},{ghG,ghGbar,B,B},{B,G,G},
{ghG,ghGbar,G},{ghG,ghGbar,B,G},{B,B,G,G},
{G,G,G},{B,G,G,G},{G,G,G,G},{uqbar,uq,G},{dqbar,dq,G},
{uqbar,uq,B},{dqbar,dq,B}}];






