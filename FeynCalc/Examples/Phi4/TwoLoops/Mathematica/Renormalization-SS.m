(* ::Package:: *)

(* :Title: Renormalization2L-SS										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, phi^4, MSbar, Scalar self-energy, 2-loop *)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*2-loop phi^4 renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QCD model created with FeynRules.*)


description="Renormalization, phi^4, MSbar, 2-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
LaunchKernels[8];
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


diagScalarSE=InsertFields[CreateTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,modelName}]];


diagScalarSECT=InsertFields[CreateCTTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,modelName}]];


diagScalarTreeSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,modelName}]];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagScalarSE, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Counter-term diagrams*)


Paint[diagScalarSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


Paint[diagScalarTreeSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Section:: *)
(*Master integrals*)


(* ::Text:: *)
(*The only required masters are 1- and 2-loop tadpoles*)


tadpoleMaster=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]];


tadpoleMaster1=tadpoleMaster/.m1->mphi/.tad1L->"tad1Lv1";


tadpoleMaster2=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxFx111x111xxEp1x.m"}]]/.m1->mphi/.tad2LxFx111x111xxEp1x->"tad2Lv2";


(* ::Section:: *)
(*Obtain the amplitudes*)


{scalarSE$RawAmp,scalarSECT$RawAmp,diagScalarTreeSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	DropSumOver->True,
	LoopMomenta->{k1,k2}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{}]&/@{
	diagScalarSE,diagScalarSECT,diagScalarTreeSECT};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Scalar self-energy at 2 loops*)


(* ::Text:: *)
(*The 2-loop scalar self-energy has superficial degree of divergence equal to 2*)


scalarSE$RawAmp


FCClearScalarProducts[];
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[scalarSE$RawAmp,{k1,k2},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1,k2},-mxt^2,0,Head->denHead]


AbsoluteTiming[scalarSE$PreAmp1=Contract[(aux1[[1]]/.aux2),FCParallelize->True];]


AbsoluteTiming[scalarSE$Amp=scalarSE$PreAmp1;]


isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]


AbsoluteTiming[scalarSE$Amp1=Collect2[scalarSE$Amp,p,IsolateNames->isoSymbols,FCParallelize->True];]


AbsoluteTiming[scalarSE$Amp2=FourSeries[scalarSE$Amp1,{p,0,divDegree},FCParallelize->True];]


AbsoluteTiming[scalarSE$Amp3=Collect2[FRH2[scalarSE$Amp2,isoSymbols],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[]
SPD[p]=pp;


AbsoluteTiming[{scalarSE$Amp4,scalarSE$Topos}=FCLoopFindTopologies[scalarSE$Amp3,{k1,k2},FCI->True,FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp}];]


AbsoluteTiming[scalarSE$Amp5=FCLoopTensorReduce[scalarSE$Amp4,scalarSE$Topos,FCParallelize->True];]


AbsoluteTiming[{scalarSE$Amp6,scalarSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[scalarSE$Amp5,scalarSE$Topos,FCParallelize->True];]


AbsoluteTiming[{scalarSE$Amp7,scalarSE$Topos3}=FCLoopRewriteIncompleteTopologies[scalarSE$Amp6,scalarSE$Topos2,FCParallelize->True];]


AbsoluteTiming[scalarSE$SubTopos=FCLoopFindSubtopologies[scalarSE$Topos3,Flatten->True,Remove->True,FCParallelize->True];]


{scalarSE$TopoMappings,
scalarSE$FinalTopos}=FCLoopFindTopologyMappings[scalarSE$Topos3,PreferredTopologies->scalarSE$SubTopos,FCParallelize->True];


AbsoluteTiming[scalarSE$AmpGLI=FCLoopApplyTopologyMappings[scalarSE$Amp7,{scalarSE$TopoMappings,
scalarSE$FinalTopos},FCParallelize->True];]


scalarSE$GLIs=Cases2[scalarSE$AmpGLI,GLI];


scalarSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-scalarSE-2L-massless"}];
Quiet[CreateDirectory[scalarSE$dir]];


KiraCreateJobFile[scalarSE$FinalTopos, scalarSE$GLIs, scalarSE$dir]


KiraCreateIntegralFile[scalarSE$GLIs, scalarSE$FinalTopos, scalarSE$dir]
KiraCreateConfigFiles[scalarSE$FinalTopos, scalarSE$GLIs, scalarSE$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1}]


KiraRunReduction[scalarSE$dir, scalarSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


scalarSE$ReductionTables=KiraImportResults[scalarSE$FinalTopos, scalarSE$dir]//Flatten;


AbsoluteTiming[scalarSE$resPreFinal1=(scalarSE$AmpGLI/.Dispatch[scalarSE$ReductionTables]);]


AbsoluteTiming[scalarSE$resPreFinal2=Map[Collect2[#,GLI,FCParallelize->True]&,scalarSE$resPreFinal1];]


scalarSE$masters=Cases2[scalarSE$resPreFinal1,GLI];


scalarSE$MIMappings=FCLoopFindIntegralMappings[scalarSE$masters,Join[tadpoleMaster1[[2]],{tadpoleMaster2[[2]]},
scalarSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


isoSymbols1=FCMakeSymbols[LL,Range[1,$KernelCount],List];
isoSymbols2=FCMakeSymbols[LM,Range[1,$KernelCount],List];


AbsoluteTiming[scalarSE$resPreFinal2=Collect2[scalarSE$resPreFinal1,D,GLI,IsolateNames->isoSymbols1,FCParallelize->True]//FCReplaceD[#,D->4-2ep]&//ReplaceAll[#,scalarSE$MIMappings[[1]]]&//
ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//Collect2[#,ep,IsolateNames->isoSymbols2,FCParallelize->True]&;]


AbsoluteTiming[scalarSE$resPreFinal3=scalarSE$resPreFinal2//Series[#,{ep,0,-1}]&//Normal//Series[(I*(4*Pi)^(-2 + ep))^2 #,{ep,0,-1}]&//Normal;]


AbsoluteTiming[scalarSE$resPreFinal4=Collect2[FRH2[FRH2[scalarSE$resPreFinal3,isoSymbols2],isoSymbols1],DiracGamma,pp,mxt,ep,FCParallelize->True];]


isoSymbols3=FCMakeSymbols[LH,Range[1,$KernelCount],List];


AbsoluteTiming[scalarSE$resPreFinal5=Series[Total[Collect2[scalarSE$resPreFinal4,mxt,IsolateNames->isoSymbols3,FCParallelize->True]],{mxt,0,2}]//Normal;]


AbsoluteTiming[scalarSE$resPreFinal6=Collect2[FRH2[scalarSE$resPreFinal5,isoSymbols3]//ReplaceAll[#,Log[m_Symbol^2]:>2Log[m]]&,DiracGamma,pp,mxt,ep,FCParallelize->True];]


scalarSE$resFinal=Collect2[Collect2[scalarSE$resPreFinal6,ep,g,Factoring->FullSimplify],ep,mphi,mxt]


(* ::Subsection:: *)
(*Scalar self-energy 1-loop CT*)


FCClearScalarProducts[];
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[scalarSECT$RawAmp/.k1->k1+p,{k1},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1},-mxt^2,0,Head->denHead]


scalarSECT$StrName=StringReplace[ToString[Hold[scalarSECT$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[scalarSECT$Amp=(aux1[[1]]/.aux2);]


AbsoluteTiming[scalarSECT$Amp1=Collect2[scalarSECT$Amp,p,IsolateNames->KK];]
AbsoluteTiming[scalarSECT$Amp2=FourSeries[scalarSECT$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[scalarSECT$Amp3=Collect2[FRH[scalarSECT$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{scalarSECT$Amp4,scalarSECT$Topos}=FCLoopFindTopologies[scalarSECT$Amp3,{k1},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->scalarSEtopo];


AbsoluteTiming[scalarSECT$Amp5=FCLoopTensorReduce[scalarSECT$Amp4,scalarSECT$Topos,FCParallelize->True];]


AbsoluteTiming[scalarSECT$Amp6=DiracSimplify[scalarSECT$Amp5,FCParallelize->True];]


{scalarSECT$Amp7,scalarSECT$Topos2}=FCLoopRewriteOverdeterminedTopologies[scalarSECT$Amp6,scalarSECT$Topos,FCParallelize->True];


{scalarSECT$Amp8,scalarSECT$Topos3}=FCLoopRewriteIncompleteTopologies[scalarSECT$Amp7,scalarSECT$Topos2,FCParallelize->True];


AbsoluteTiming[scalarSECT$SubTopos=FCLoopFindSubtopologies[scalarSECT$Topos2,Flatten->True,Remove->True,FCParallelize->True];]


AbsoluteTiming[{scalarSECT$TopoMappings,scalarSECT$FinalTopos}=FCLoopFindTopologyMappings[scalarSECT$Topos2,PreferredTopologies->scalarSECT$SubTopos,FCParallelize->True];]


AbsoluteTiming[scalarSECT$AmpGLI=FCLoopApplyTopologyMappings[scalarSECT$Amp8,{scalarSECT$TopoMappings,scalarSECT$FinalTopos},FCParallelize->True];]


scalarSECT$GLIs=Cases2[scalarSECT$AmpGLI,GLI];


scalarSECT$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>scalarSECT$StrName<>"-1L-massive"}];
Quiet[CreateDirectory[scalarSECT$dir]];


KiraCreateJobFile[scalarSECT$FinalTopos, scalarSECT$GLIs, scalarSECT$dir]


KiraCreateIntegralFile[scalarSECT$GLIs, scalarSECT$FinalTopos, scalarSECT$dir]
KiraCreateConfigFiles[scalarSECT$FinalTopos, scalarSECT$GLIs, scalarSECT$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1}]


KiraRunReduction[scalarSECT$dir, scalarSECT$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


scalarSECT$ReductionTables=KiraImportResults[scalarSECT$FinalTopos, scalarSECT$dir]//Flatten;


scalarSECT$resPreFinal1=Collect2[Total[scalarSECT$AmpGLI/.Dispatch[scalarSECT$ReductionTables]],GLI,D,FCParallelize->True];


scalarSECT$masters=Cases2[scalarSECT$resPreFinal1,GLI];


scalarSECT$MIMappings=FCLoopFindIntegralMappings[scalarSECT$masters,Join[tadpoleMaster1[[2]],
scalarSECT$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


(* ::Text:: *)
(*At this point we need to insert the 1-loop renormalization constants*)


knownResults1L ={rc[delZphi, 1] -> 0, rc[delZmphi, 1] -> 1/(32*ep*Pi^2), rc[delZg, 1] -> (3)/(32*ep*Pi^2)};


Collect2[scalarSECT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,scalarSECT$MIMappings[[1]]]&


AbsoluteTiming[scalarSECT$resPreFinal2=Collect2[scalarSECT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,scalarSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,1}]&//Normal//FCLoopAddMissingHigherOrdersWarning[#,ep,epHelp]&//FRH//
ReplaceAll[#,Log[m_^2]:>2Log[m]]&;]


AbsoluteTiming[scalarSECT$resPreFinal2=Collect2[scalarSECT$resPreFinal1,Join[{g},List@@renConstants],IsolateNames->KK]//ReplaceAll[#,{
	(h:renConstants):>1+(g rc[ToExpression["del"<>ToString[h]],1]+g^2 rc[ToExpression["del"<>ToString[h]],2])}]&//Series[#,{g,0,2}]&//Normal;]


AbsoluteTiming[scalarSECT$resPreFinal3=Collect2[scalarSECT$resPreFinal2//FRH,{rc,D,GLI},IsolateNames->KK]//FCReplaceD[#,{D->4-2ep}]&//ReplaceRepeated[#,knownResults1L]&//
ReplaceAll[#,scalarSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Abort[],#]&//Collect2[#,ep,IsolateNames->KK]&;]


scalarSECT$resFinal=scalarSECT$resPreFinal3//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//
ReplaceAll[#,{Log[m_^2]:>2Log[m],Log[4Pi]->Log[4]+Log[Pi]}]&//Collect2[#,ep,mphi]&


(* ::Subsection:: *)
(*Determination of renormalization constants*)


diagGhostTreeSECT$Amp=(Total[diagScalarTreeSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+(g rc[ToExpression["del"<>ToString[h]],1]+g^2 rc[ToExpression["del"<>ToString[h]],2])}]&//
	Series[#,{g,0,2}]&//Normal//ReplaceRepeated[#,knownResults1L]&


scalarSE$RenConstants2L=Coefficient[scalarSE$resFinal+ scalarSECT$resFinal+ diagGhostTreeSECT$Amp,g,2]//Collect2[#,pp]&//FCMatchSolve[#,{ep,mphi,pp}]&


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final phi^4 2-loop wave-function renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ g rc[ToExpression["del"<>ToString[h]],1]+ g^2 rc[ToExpression["del"<>ToString[h]],2])//
ReplaceAll[#,Join[SUNSimplify[knownResults1L,SUNNToCACF->False],scalarSE$RenConstants2L]]&]]//SelectNotFree[#,Zphi,Zmphi]&//ExpandAll


finalResults//TableForm


knownResult = {rc[delZmphi, 2] -> (12 - 5*ep)/(6144*ep^2*Pi^4), rc[delZphi, 2] -> -1/6144*1/(ep*Pi^4)};


FCCompareResults[scalarSE$RenConstants2L,knownResult,
Text->{"\tCompare to the known result:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



