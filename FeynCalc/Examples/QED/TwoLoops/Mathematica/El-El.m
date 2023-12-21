(* ::Package:: *)

(* :Title: El-El															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  El -> El, massless QED, 2-loops								*)

(* ------------------------------------------------------------------------ *)


(* ::Title:: *)
(*QED electron self-energy*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="El -> El, massless QED, 2-loops";
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


diags=InsertFields[CreateTopologies[2, 1 -> 1,ExcludeTopologies -> {Tadpoles}], {F[2,{1}]} -> {F[2,{1}]},
InsertionLevel -> {Particles}, ExcludeParticles->{V[2|3],S[_],U[_],F[1|3|4]}];


Paint[DiagramExtract[diags,{1,2,5}],ColumnsXRows->{3,1},SheetHeader -> False,   
Numbering -> None,ImageSize->{768,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


ampRaw = FCFAConvert[CreateFeynAmp[DiagramExtract[diags,{1,2,5}], Truncated -> True, GaugeRules->{},
	PreFactor->1], IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{q1,q2},
	UndoChiralSplittings->True, ChangeDimension->D, List->True, SMP->True,
	DropSumOver->True]//SMPToSymbol;


ampRaw = {ampRaw[[1]],Nf ampRaw[[2]],ampRaw[[3]]};


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


reductionTable=Get[FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-El-El.m"}]];


resPreFinal=Collect2[Total[ampFinal/.reductionTable],GLI];


integralMappings=FCLoopFindIntegralMappings[Cases2[resPreFinal,GLI],mappings[[2]]]


resFinal=Collect2[resPreFinal/.integralMappings[[1]],GLI]


(* ::Section:: *)
(*Check the final results*)


resGrozinSE=I FCI[e^4(D-2)( 2(D-2)/(D-6)(1/SPD[p,p])GLI["fctopology1", {0, 1, 1, 0, 1}]Nf-
1/4((D-2)(GaugeXi[A])^2+D-6)GLI["fctopology1", {1, 1, 0, 1, 1}]
+(1/2)(D-3)/(D-4)((3D-8)(GaugeXi[A])^2-D-4)(1/SPD[p,p])GLI["fctopology1", {0, 1, 1, 0, 1}])GSD[p]];
FCCompareResults[resFinal,resGrozinSE,
Text->{"\tCompare to Grozin's Lectures on QED and QCD, hep-ph/0508242, Eq. 5.51:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic},Factoring->Simplify];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];




