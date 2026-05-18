(* ::Package:: *)

(* :Title: Renormalization													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, phi^3, MSbar, 1-loop							*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*1-loop phi^3 renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom phi^3 model created with FeynRules.*)


description="Renormalization, phi^3, MSbar, 1-loop";
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
(*Configure some options*)


modelName="Phi3";


modelDir=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","Examples","Models",modelName}];


FAPatch[PatchModelsOnly->True,FAModelsDirectory->modelDir];


(* ::Text:: *)
(*Here we define all Z-factors for renormalization constants present in the Lagrangian*)


renConstants=Zg|Zphi|Zmphi


(* ::Section:: *)
(*Generate Feynman diagrams*)


diagScalarSE=InsertFields[CreateTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"Phi3"}]];


diagScalar3VTX=InsertFields[CreateTopologies[1, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1],S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"Phi3"}]];


diagScalarSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"Phi3"}]];


diagScalar3VTXCT=InsertFields[CreateCTTopologies[1, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1],S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"Phi3"}]];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagScalarSE, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagScalar3VTX, ColumnsXRows -> {4,1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Counter-term diagrams*)


Paint[diagScalarSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagScalar3VTXCT, ColumnsXRows -> {4,1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Section:: *)
(*Master integrals*)


(* ::Text:: *)
(*The only required masters are 1-loop tadpoles*)
(**)


tadpoleMaster=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]];


tadpoleMaster1=tadpoleMaster/.m1->mx/.tad1LxFx1x1xxEp999x->"tad1Lv1";
tadpoleMaster2=tadpoleMaster/.m1-> mphi/.tad1LxFx1x1xxEp999x->"tad1Lv2";


tadpoleMaster1


tadpoleMaster2


(* ::Section:: *)
(*Obtain the amplitudes*)


{scalarSE$RawAmp,scalarSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	DropSumOver->True,
	LoopMomenta->{k}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{}]&/@{
	diagScalarSE,diagScalarSECT};


{scalar3VTX$RawAmp,scalar3VTXCT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{q1},
	DropSumOver->True,
	LoopMomenta->{k}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{}]&/@{
	diagScalar3VTX,diagScalar3VTXCT};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Scalar self-energy*)


(* ::Text:: *)
(*The 1-loop scalar self-energy has superficial degree of divergence equal to 2.*)


FCClearScalarProducts[];
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[scalarSE$RawAmp,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


scalarSE$StrName=StringReplace[ToString[Hold[scalarSE$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[scalarSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[scalarSE$Amp1=Collect2[scalarSE$Amp,p,IsolateNames->KK];]
AbsoluteTiming[scalarSE$Amp2=FourSeries[scalarSE$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[scalarSE$Amp3=Collect2[FRH[scalarSE$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{scalarSE$Amp4,scalarSE$Topos}=FCLoopFindTopologies[scalarSE$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->scalarSEtopo];


AbsoluteTiming[scalarSE$Amp5=FCLoopTensorReduce[scalarSE$Amp4,scalarSE$Topos,FCParallelize->True];]


{scalarSE$Amp6,scalarSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[scalarSE$Amp5,scalarSE$Topos];


scalarSE$SubTopos=FCLoopFindSubtopologies[scalarSE$Topos2,Flatten->True,Remove->True]


{scalarSE$TopoMappings,scalarSE$FinalTopos}=FCLoopFindTopologyMappings[scalarSE$Topos2,PreferredTopologies->scalarSE$SubTopos];


scalarSE$AmpGLI=FCLoopApplyTopologyMappings[scalarSE$Amp6,{scalarSE$TopoMappings,scalarSE$FinalTopos},FCParallelize->True];


scalarSE$GLIs=Cases2[scalarSE$AmpGLI,GLI];


scalarSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>modelName<>"-"<>scalarSE$StrName<>"-1L"}];
Quiet[CreateDirectory[scalarSE$dir]];


KiraCreateJobFile[scalarSE$FinalTopos, scalarSE$GLIs, scalarSE$dir]


KiraCreateIntegralFile[scalarSE$GLIs, scalarSE$FinalTopos, scalarSE$dir]
KiraCreateConfigFiles[scalarSE$FinalTopos, scalarSE$GLIs, scalarSE$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1,mx->1,gxi->0}]


KiraRunReduction[scalarSE$dir, scalarSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


scalarSE$ReductionTables=KiraImportResults[scalarSE$FinalTopos, scalarSE$dir]//Flatten;


scalarSE$resPreFinal=Collect2[Total[scalarSE$AmpGLI/.Dispatch[scalarSE$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,D,FCParallelize->True];


scalarSE$masters=Cases2[scalarSE$resPreFinal,GLI];


scalarSE$MIMappings=FCLoopFindIntegralMappings[scalarSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
scalarSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


scalarSE$resFinal=Collect2[scalarSE$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,scalarSE$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Print["Unsubstituted GLIs!"];Abort[],#]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


scalarSECT$RawAmp


scalarSE$RenConstants=(scalarSE$resFinal+Total[scalarSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//
	ReplaceAll[#,alpha->1]&//Collect2[#,pp,Pair]&//
	FCMatchSolve[#,{ep,g,mphi,pp}]&//ExpandAll


(* ::Subsection:: *)
(*Three-scalar vertex*)


(* ::Text:: *)
(*The 1-loop three-scalar-vertex has superficial degree of divergence equal to 0. We set q1=0, so that p1+p2=0 yields p1=-p2*)


FCClearScalarProducts[];
divDegree=0;
aux1=FCLoopGetFeynAmpDenominators[scalar3VTX$RawAmp/.q1->0/.p2->-p1/.p1->p,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


scalar3VTX$StrName=StringReplace[ToString[Hold[scalar3VTX$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[scalar3VTX$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[scalar3VTX$Amp1=Collect2[scalar3VTX$Amp,p,IsolateNames->KK];]
AbsoluteTiming[scalar3VTX$Amp2=FourSeries[scalar3VTX$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[scalar3VTX$Amp3=Collect2[FRH[scalar3VTX$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{scalar3VTX$Amp4,scalar3VTX$Topos}=FCLoopFindTopologies[scalar3VTX$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->scalar3VTXtopo];


AbsoluteTiming[scalar3VTX$Amp5=FCLoopTensorReduce[scalar3VTX$Amp4,scalar3VTX$Topos,FCParallelize->True];]


{scalar3VTX$Amp6,scalar3VTX$Topos2}=FCLoopRewriteOverdeterminedTopologies[scalar3VTX$Amp5,scalar3VTX$Topos];


scalar3VTX$SubTopos=FCLoopFindSubtopologies[scalar3VTX$Topos2,Flatten->True,Remove->True]


{scalar3VTX$TopoMappings,scalar3VTX$FinalTopos}=FCLoopFindTopologyMappings[scalar3VTX$Topos2,PreferredTopologies->scalar3VTX$SubTopos];


scalar3VTX$AmpGLI=FCLoopApplyTopologyMappings[scalar3VTX$Amp6,{scalar3VTX$TopoMappings,scalar3VTX$FinalTopos},FCParallelize->True];


scalar3VTX$GLIs=Cases2[scalar3VTX$AmpGLI,GLI];


scalar3VTX$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>modelName<>"-"<>scalar3VTX$StrName<>"-1L"}];
Quiet[CreateDirectory[scalar3VTX$dir]];


KiraCreateJobFile[scalar3VTX$FinalTopos, scalar3VTX$GLIs, scalar3VTX$dir]


KiraCreateIntegralFile[scalar3VTX$GLIs, scalar3VTX$FinalTopos, scalar3VTX$dir]
KiraCreateConfigFiles[scalar3VTX$FinalTopos, scalar3VTX$GLIs, scalar3VTX$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1,mx->1,gxi->0}]


KiraRunReduction[scalar3VTX$dir, scalar3VTX$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


scalar3VTX$ReductionTables=KiraImportResults[scalar3VTX$FinalTopos, scalar3VTX$dir]//Flatten;


scalar3VTX$resPreFinal=Collect2[Total[scalar3VTX$AmpGLI/.Dispatch[scalar3VTX$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


scalar3VTX$masters=Cases2[scalar3VTX$resPreFinal,GLI];


scalar3VTX$MIMappings=FCLoopFindIntegralMappings[scalar3VTX$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
scalar3VTX$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


scalar3VTX$resFinal=Collect2[scalar3VTX$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,scalar3VTX$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Print["Unsubstituted GLIs!"];Abort[],#]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


If[FreeQ[scalar3VTX$resFinal,ep],
scalar3VTX$resFinal=0]


scalar3VTX$RenConstants=(scalar3VTX$resFinal+Total[scalar3VTXCT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,Join[scalarSE$RenConstants,scalarSE$RenConstants]]&//
	ReplaceAll[#,alpha->1]&//Collect2[#,pp,Pair]&//
	FCMatchSolve[#,{ep,g,gxi,la,DiracGamma,mphi,mx,pp}]&//ExpandAll


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final phi^3 1-loop renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ rc[ToExpression["del"<>ToString[h]],1])//ReplaceAll[#,Join[scalarSE$RenConstants,
scalar3VTX$RenConstants]]&]]


Join[scalarSE$RenConstants,scalar3VTX$RenConstants]//InputForm


knownResult ={rc[delZphi, 1] -> 0, rc[delZmphi, 1] -> g^2/(32*ep*mphi^2*Pi^2), rc[delZg, 1] -> 0};


(* ::Text:: *)
(*Here are the literature results*)


FCCompareResults[Join[scalarSE$RenConstants,scalar3VTX$RenConstants]/.Rule->Equal,knownResult/.Rule->Equal,
Text->{"\tCompare to Cheng and Li, Gauge theory of elementary particle physics, Problems and Solutions, Eq. 2.120:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



