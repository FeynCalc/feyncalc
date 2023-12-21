(* ::Package:: *)

(* :Title: Ga-Ga															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Ga -> Ga, massless QED, 2-loops								*)

(* ------------------------------------------------------------------------ *)


(* ::Title:: *)
(*QED photon self-energy*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Ga -> Ga, massless QED, 2-loops";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[10,0,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


diags=InsertFields[CreateTopologies[2, 1 -> 1,ExcludeTopologies -> {Tadpoles}], {V[1]} -> {V[1]},
InsertionLevel -> {Particles}, ExcludeParticles->{V[2|3],S[_],U[_],F[1|3|4]}];


Paint[DiagramExtract[diags,{1,4,7}],ColumnsXRows->{3,1},SheetHeader -> False,   
Numbering -> None,ImageSize->{768,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


ampRaw = FCFAConvert[CreateFeynAmp[DiagramExtract[diags,{1,4,7}], Truncated -> True, GaugeRules->{},
	PreFactor->1], IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{q1,q2},
	UndoChiralSplittings->True, ChangeDimension->D, List->True, SMP->True,
	DropSumOver->True]//SMPToSymbol;


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
ScalarProduct[p,p]=pp;


(* ::Section:: *)
(*Calculate the amplitude*)


AbsoluteTiming[ampSimp=DiracSimplify[ampRaw/.me->0];]


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amp,topos}=FCLoopFindTopologies[ampSimp,{q1,q2}];


subtopos=FCLoopFindSubtopologies[topos];


mappings=FCLoopFindTopologyMappings[topos,PreferredTopologies->subtopos];


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amp,topos];]


AbsoluteTiming[ampPreFinal=FCLoopApplyTopologyMappings[ampReduced,mappings];]


AbsoluteTiming[ampFinal=ampPreFinal//DiracSimplify;]


(*FCReloadAddOns[{"FeynHelpers"}];
FIREPrepareStartFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateStartFile[FCGetNotebookDirectory[],mappings[[2]]]
FIRECreateConfigFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateIntegralFile[Cases2[ampPreFinal,GLI],mappings[[2]],FCGetNotebookDirectory[]]
FIRERunReduction[FCGetNotebookDirectory[],mappings[[2]]]
tables=FIREImportResults[mappings[[2]],FCGetNotebookDirectory[]]//Flatten;
Put[tables,FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-Ga-Ga.m"}]];*)


reductionTable=Get[FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-Ga-Ga.m"}]];


resPreFinal=Collect2[Total[ampFinal/.reductionTable],GLI]


integralMappings=FCLoopFindIntegralMappings[Cases2[resPreFinal,GLI],mappings[[2]]]


resFinal=Collect2[resPreFinal/.integralMappings[[1]],GLI]


(* ::Section:: *)
(*Check the final results*)


resGrozinVacuumPol=-I FCI[e^4 2(D-2)/((D-1)(D-4))(-(D^2-7D+16)GLI["fctopology1", {1, 1, 0, 1, 1}]+
4(D-3)(D^2-4D+8)/(D-4)(1/SPD[p,p])GLI["fctopology1", {0, 1, 1, 0, 1}])(-(FVD[p, Lor1]*FVD[p, Lor2]) + 
pp*MTD[Lor1, Lor2])];
FCCompareResults[resFinal,resGrozinVacuumPol,
Text->{"\tCompare to Grozin's Lectures on QED and QCD, hep-ph/0508242, Eq. 5.18:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic},Factoring->Simplify];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
