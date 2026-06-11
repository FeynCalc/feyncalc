(* ::Package:: *)

(* :Title: Renormalization													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, phi^4, MSbar, 1-loop							*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*1-loop phi^4 renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom phi^4 model created with FeynRules.*)


description="Renormalization, phi^4, MSbar, 1-loop";
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


modelName="Phi4";


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
GenericModel -> FileNameJoin[{modelDir,"Phi4"}]];


diagScalar4VTX=InsertFields[CreateTopologies[1, 2 -> 2,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1],S[1]} -> {S[1],S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"Phi4"}]];


diagScalarSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"Phi4"}]];


diagScalar4VTXCT=InsertFields[CreateCTTopologies[1, 2 -> 2,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1],S[1]} -> {S[1],S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"Phi4"}]];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagScalarSE, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagScalar4VTX, ColumnsXRows -> {4,1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Counter-term diagrams*)


Paint[diagScalarSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagScalar4VTXCT, ColumnsXRows -> {4,1},SheetHeader->None,
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


{scalar4VTX$RawAmp,scalar4VTXCT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{q1,q2},
	DropSumOver->True,
	LoopMomenta->{k}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{}]&/@{
	diagScalar4VTX,diagScalar4VTXCT};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Scalar self-energy*)


(* ::Text:: *)
(*The 1-loop scalar self-energy has superficial degree of divergence equal to 2.*)


FCClearScalarProducts[];
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[scalarSE$RawAmp/.k->k-p,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


scalarSE$StrName=StringReplace[ToString[Hold[scalarSE$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[scalarSE$Amp=(aux1[[1]]/.aux2);]


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
 KiraMassDimensions -> {pp -> 2,mphi->1}]


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


scalarSE$RenConstants=(scalarSE$resFinal+Total[scalarSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+g rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{g,0,1}]&//Normal//Collect2[#,pp,Pair]&//FCMatchSolve[#,{ep,g,mphi,pp}]&//ExpandAll


(* ::Subsection:: *)
(*Four-scalar vertex*)


(* ::Text:: *)
(*The 1-loop four-scalar-vertex has superficial degree of divergence equal to 0. We set q1=q2=0, so that p1+p2=0 yields p1=-p2*)


FCClearScalarProducts[];
divDegree=0;
aux1=FCLoopGetFeynAmpDenominators[scalar4VTX$RawAmp/.q1|q2->0/.p2->-p1/.p1->p,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


scalar4VTX$StrName=StringReplace[ToString[Hold[scalar4VTX$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[scalar4VTX$Amp=(aux1[[1]]/.aux2);]


AbsoluteTiming[scalar4VTX$Amp1=Collect2[scalar4VTX$Amp,p,IsolateNames->KK];]
AbsoluteTiming[scalar4VTX$Amp2=FourSeries[scalar4VTX$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[scalar4VTX$Amp3=Collect2[FRH[scalar4VTX$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{scalar4VTX$Amp4,scalar4VTX$Topos}=FCLoopFindTopologies[scalar4VTX$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->scalar4VTXtopo];


AbsoluteTiming[scalar4VTX$Amp5=FCLoopTensorReduce[scalar4VTX$Amp4,scalar4VTX$Topos,FCParallelize->True];]


{scalar4VTX$Amp6,scalar4VTX$Topos2}=FCLoopRewriteOverdeterminedTopologies[scalar4VTX$Amp5,scalar4VTX$Topos];


scalar4VTX$SubTopos=FCLoopFindSubtopologies[scalar4VTX$Topos2,Flatten->True,Remove->True]


{scalar4VTX$TopoMappings,scalar4VTX$FinalTopos}=FCLoopFindTopologyMappings[scalar4VTX$Topos2,PreferredTopologies->scalar4VTX$SubTopos];


scalar4VTX$AmpGLI=FCLoopApplyTopologyMappings[scalar4VTX$Amp6,{scalar4VTX$TopoMappings,scalar4VTX$FinalTopos},FCParallelize->True];


scalar4VTX$GLIs=Cases2[scalar4VTX$AmpGLI,GLI];


scalar4VTX$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>modelName<>"-"<>scalar4VTX$StrName<>"-1L"}];
Quiet[CreateDirectory[scalar4VTX$dir]];


KiraCreateJobFile[scalar4VTX$FinalTopos, scalar4VTX$GLIs, scalar4VTX$dir]


KiraCreateIntegralFile[scalar4VTX$GLIs, scalar4VTX$FinalTopos, scalar4VTX$dir]
KiraCreateConfigFiles[scalar4VTX$FinalTopos, scalar4VTX$GLIs, scalar4VTX$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1}]


KiraRunReduction[scalar4VTX$dir, scalar4VTX$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


scalar4VTX$ReductionTables=KiraImportResults[scalar4VTX$FinalTopos, scalar4VTX$dir]//Flatten;


scalar4VTX$resPreFinal=Collect2[Total[scalar4VTX$AmpGLI/.Dispatch[scalar4VTX$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,D,DiracGamma,FCParallelize->True];


scalar4VTX$masters=Cases2[scalar4VTX$resPreFinal,GLI];


scalar4VTX$MIMappings=FCLoopFindIntegralMappings[scalar4VTX$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
scalar4VTX$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


scalar4VTX$resFinal=Collect2[scalar4VTX$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,scalar4VTX$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Print["Unsubstituted GLIs!"];Abort[],#]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


scalar4VTX$RenConstants=(scalar4VTX$resFinal+Total[scalar4VTXCT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+g rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{g,0,1}]&//Normal//ReplaceAll[#,Join[scalarSE$RenConstants,scalarSE$RenConstants]]&//Collect2[#,pp,Pair]&//
	FCMatchSolve[#,{ep,g,mphi,pp}]&//ExpandAll


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final phi^4 1-loop renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ rc[ToExpression["del"<>ToString[h]],1])//ReplaceAll[#,Join[scalarSE$RenConstants,
scalar4VTX$RenConstants]]&]]


Join[scalarSE$RenConstants,scalar4VTX$RenConstants]//InputForm


knownResult ={rc[delZphi, 1] -> 0, rc[delZmphi, 1] -> 1/(32*ep*Pi^2), rc[delZg, 1] -> (3)/(32*ep*Pi^2)};


(* ::Text:: *)
(*Compare to the literature results*)


FCCompareResults[Join[scalarSE$RenConstants,scalar4VTX$RenConstants]/.Rule->Equal,knownResult/.Rule->Equal,
Text->{"\tCompare to Bailin and Love, Introduction to Gauge Field Theory, Eqs. 7.73-7.74 and Eqs. 7.76-7.77:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



