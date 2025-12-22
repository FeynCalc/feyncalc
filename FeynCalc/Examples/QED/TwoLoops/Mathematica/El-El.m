(* ::Package:: *)

(* :Title: El-El															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
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
LaunchKernels[4];
$LoadAddOns={"FeynArts","FeynHelpers"};
<<FeynCalc`
$FAVerbose = 0;
$ParallelizeFeynCalc=True;

FCCheckVersion[10,2,0];
If[ToExpression[StringSplit[$FeynHelpersVersion,"."]][[1]]<2,
	Print["You need at least FeynHelpers 2.0 to run this example."];
	Abort[];
]


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


AbsoluteTiming[ampSimp=DiracSimplify[ampRaw/.me->0,
FCParallelize->True];]


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amp,topos}=FCLoopFindTopologies[ampSimp,{q1,q2},FCParallelize->True];


subtopos=FCLoopFindSubtopologies[topos,FCParallelize->True];


mappings=FCLoopFindTopologyMappings[topos,
PreferredTopologies->subtopos,FCParallelize->True];


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amp,topos,FCParallelize->True];]


AbsoluteTiming[ampPreFinal=FCLoopApplyTopologyMappings[ampReduced,
mappings,FCParallelize->True];]


AbsoluteTiming[ampFinal=ampPreFinal//DiracSimplify[#,FCParallelize->True]&;]


dir=FileNameJoin[{$TemporaryDirectory,"Reduction-ElToEl2"}];
Quiet[CreateDirectory[dir]];


FIREPrepareStartFile[mappings[[2]],dir];


FIRECreateLiteRedFiles[dir,mappings[[2]]];


FIRECreateStartFile[dir,mappings[[2]]];


FIRECreateIntegralFile[Cases2[ampPreFinal,GLI],mappings[[2]],dir];


FIRECreateConfigFile[mappings[[2]],dir];


FIRERunReduction[dir,mappings[[2]]];


reductionTable=FIREImportResults[mappings[[2]],dir]//Flatten;


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



