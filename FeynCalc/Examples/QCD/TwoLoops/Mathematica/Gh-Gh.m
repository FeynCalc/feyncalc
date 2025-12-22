(* ::Package:: *)

(* :Title: Gh-Gh															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Gh -> Gh, massless QCD, 2-loops								*)

(* ------------------------------------------------------------------------ *)


(* ::Title:: *)
(*QCD ghost self-energy*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gh -> Gh, massless QCD, 2-loops";
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


diags=InsertFields[CreateTopologies[2, 1 -> 1,ExcludeTopologies -> {Tadpoles}], {U[5]} -> {U[5]},
InsertionLevel -> {Classes}, ExcludeParticles->{V[1|2|3],S[_]},Model->SMQCD];


Paint[diags,ColumnsXRows->{4,1},SheetHeader -> False,   
Numbering -> True,ImageSize->{1024,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


ampRaw = FCFAConvert[CreateFeynAmp[DiagramExtract[diags,{2,3,4,6,7,8,9}], Truncated -> True,
	PreFactor->1,GaugeRules->{}], IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{q1,q2},
	UndoChiralSplittings->True, ChangeDimension->D, List->True, SMP->True,
	DropSumOver->True,FinalSubstitutions->{MQU[Index[Generation, 3]]->0,GaugeXi[_]->1-GaugeXi}]//SMPToSymbol;


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
ScalarProduct[p,p]=pp;


(* ::Section:: *)
(*Calculate the amplitude*)


AbsoluteTiming[ampSimp=(ampRaw)//Contract[#,FCParallelize->True]&//
DiracSimplify[#,FCParallelize->True]&//SUNSimplify[#,FCParallelize->True]&;]


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amp,topos}=FCLoopFindTopologies[ampSimp,{q1,q2},FCParallelize->True];


subtopos=FCLoopFindSubtopologies[topos,FCParallelize->True];


mappings=FCLoopFindTopologyMappings[topos,PreferredTopologies->subtopos,
FCParallelize->True];


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amp,topos,FCParallelize->True];]


AbsoluteTiming[ampPreFinal=FCLoopApplyTopologyMappings[ampReduced,mappings];]


AbsoluteTiming[ampFinal=ampPreFinal//
DiracSimplify[#,FCParallelize->True]&//SUNSimplify[#,FCParallelize->True]&;]


dir=FileNameJoin[{$TemporaryDirectory,"Reduction-GhToGh-2L"}];
Quiet[CreateDirectory[dir]];


FIREPrepareStartFile[mappings[[2]],dir];


FIRECreateLiteRedFiles[dir,mappings[[2]]];


FIRECreateStartFile[dir,mappings[[2]]];


FIRECreateIntegralFile[Cases2[ampPreFinal,GLI],mappings[[2]],dir];


FIRECreateConfigFile[mappings[[2]],dir];


FIRERunReduction[dir,mappings[[2]]];


reductionTable=FIREImportResults[mappings[[2]],dir]//Flatten;


resPreFinal=Collect2[Total[ampFinal/.reductionTable]//FeynAmpDenominatorExplicit,GLI]


integralMappings=FCLoopFindIntegralMappings[Cases2[resPreFinal,GLI],mappings[[2]]]


resFinal=Collect2[resPreFinal/.integralMappings[[1]],GLI]


ruleMasters={
GLI["fctopology1", {0, 1, 1, 0, 1}] -> (-pp)^(1 - 2*ep)*(13/8 + 1/(4*ep) + (115*ep)/16 + (49*ep^2)/2 - (ep*Zeta2)/4 - (13*ep^2*Zeta2)/8 + (9*ep^2*(9/4 - 2*Zeta[3]))/8 - (5*ep^2*Zeta[3])/12), 
 GLI["fctopology1", {1, 1, 0, 1, 1}] -> (2 + ep^(-1) + 4*ep + (16*ep^2)/3 - (ep*Zeta2)/2 - ep^2*Zeta2 + (4*ep^2*(2 - 2*Zeta[3]))/3 + (ep^2*Zeta[3])/3)^2/(-pp)^(2*ep)
}


resEpPre=FCReplaceD[resFinal/.ruleMasters,D->4-2ep]


(* ::Text:: *)
(*To bring our result into the suitable form comparable with the literature, we  must divide it  (1- Zeta2/2 ep^2)^2  and again expand it in ep. This yields a prefactor called eta^2. *)
(*We also factor out the prefactor (-pp)^(-2ep)*)


resEp=Collect2[Series[eta^2/(1- Zeta2/2 ep^2)^2 FCReplaceD[Cancel[resEpPre/(-pp)^(-2ep)],D->4-2ep],{ep,0,0}]//Normal//SUNSimplify,ep,CA,ep]


(* ::Section:: *)
(*Check the final results*)


G2xPaper=(((CA^2*eta^2*gs^4)/(-pp)^(2*ep)) * ((83/16 + 7/32*GaugeXi)/ep + (5/4 + 7/16*GaugeXi - 1/32 GaugeXi^2)/ep^2 + 
599/32 - 3/4 Zeta[3] - 9/64 GaugeXi + 3/8 GaugeXi^2 - 3/16 GaugeXi^2 Zeta[3] )/(4*Pi)^D);


G2qPaper=(((CA*eta^2*gs^4*Tf)/(-pp)^(2*ep))*(-53/8 - 1/(2*ep^2) - 7/(4*ep))/(4*Pi)^D);


G2xPaperFinal=pp FCI@SUNDelta[Glu1,Glu2]Collect2[Series[FCReplaceD[I (4Pi)^D Cancel[(G2xPaper)/(-pp)^(-2ep)],{D->4-2ep}],{ep,0,0}]//Normal//PowerExpand,ep]
G2qPaperFinal=pp FCI@SUNDelta[Glu1,Glu2]Collect2[Series[FCReplaceD[I (4Pi)^D Cancel[(G2qPaper)/(-pp)^(-2ep)],{D->4-2ep}],{ep,0,0}]//Normal//PowerExpand,ep]/.Tf->1/2


resLit=G2xPaperFinal+G2qPaperFinal


FCCompareResults[resLit, resEp, 
     Text -> {"\tCompare to Davydychev, Osland and Tarasov, \
    hep-ph/9801380, Eqs. 6.14-6.15:", "CORRECT.", "WRONG!"}, 
     Interrupt -> {Hold[Quit[1]], Automatic}, 
     Factoring -> Simplify]; 
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], 
     " s."];

