(* ::Package:: *)

(* :Title: Renormalization2L-SSSS										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, phi^4, MSbar, Scalar vertex, 2-loop *)

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


diagScalar4VTX=InsertFields[CreateTopologies[2, 2 -> 2,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1],S[1]} -> {S[1],S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,modelName}]];


diagScalar4VTXCT=InsertFields[CreateCTTopologies[2, 2 -> 2,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1],S[1]} -> {S[1],S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,modelName}]];


diagScalarTree4VTX=InsertFields[CreateCTTopologies[1, 2 -> 2,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1],S[1]} -> {S[1],S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,modelName}]];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagScalar4VTX, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Counter-term diagrams*)


Paint[diagScalar4VTXCT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


Paint[diagScalarTree4VTX, ColumnsXRows -> {4, 1},SheetHeader->None,
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


{scalar4VTX$RawAmp,scalar4VTXCT$RawAmp,diagScalarTree4VTX$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{q1,q2},
	DropSumOver->True,
	LoopMomenta->{k1,k2}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{}]&/@{
	diagScalar4VTX,diagScalar4VTXCT,diagScalarTree4VTX};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Scalar vertex at 2 loops*)


(* ::Text:: *)
(*The 1-loop four-scalar-vertex has superficial degree of divergence equal to 0. We set q1=q2=0, so that p1+p2=0 yields p1=-p2*)


FCClearScalarProducts[];
divDegree=0;
aux1=FCLoopGetFeynAmpDenominators[scalar4VTX$RawAmp/.q1|q2->0/.p2->-p1/.p1->p,{k1,k2},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1,k2},-mxt^2,0,Head->denHead]


AbsoluteTiming[scalar4VTX$PreAmp1=Contract[(aux1[[1]]/.aux2),FCParallelize->True];]


AbsoluteTiming[scalar4VTX$Amp=scalar4VTX$PreAmp1;]


isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]


AbsoluteTiming[scalar4VTX$Amp1=Collect2[scalar4VTX$Amp,p,IsolateNames->isoSymbols,FCParallelize->True];]


AbsoluteTiming[scalar4VTX$Amp2=FourSeries[scalar4VTX$Amp1,{p,0,divDegree},FCParallelize->True];]


AbsoluteTiming[scalar4VTX$Amp3=Collect2[FRH2[scalar4VTX$Amp2,isoSymbols],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[]
SPD[p]=pp;


AbsoluteTiming[{scalar4VTX$Amp4,scalar4VTX$Topos}=FCLoopFindTopologies[scalar4VTX$Amp3,{k1,k2},FCI->True,FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp}];]


AbsoluteTiming[scalar4VTX$Amp5=FCLoopTensorReduce[scalar4VTX$Amp4,scalar4VTX$Topos,FCParallelize->True];]


AbsoluteTiming[{scalar4VTX$Amp6,scalar4VTX$Topos2}=FCLoopRewriteOverdeterminedTopologies[scalar4VTX$Amp5,scalar4VTX$Topos,FCParallelize->True];]


AbsoluteTiming[{scalar4VTX$Amp7,scalar4VTX$Topos3}=FCLoopRewriteIncompleteTopologies[scalar4VTX$Amp6,scalar4VTX$Topos2,FCParallelize->True];]


AbsoluteTiming[scalar4VTX$SubTopos=FCLoopFindSubtopologies[scalar4VTX$Topos3,Flatten->True,Remove->True,FCParallelize->True];]


{scalar4VTX$TopoMappings,
scalar4VTX$FinalTopos}=FCLoopFindTopologyMappings[scalar4VTX$Topos3,PreferredTopologies->scalar4VTX$SubTopos,FCParallelize->True];


AbsoluteTiming[scalar4VTX$AmpGLI=FCLoopApplyTopologyMappings[scalar4VTX$Amp7,{scalar4VTX$TopoMappings,
scalar4VTX$FinalTopos},FCParallelize->True];]


scalar4VTX$GLIs=Cases2[scalar4VTX$AmpGLI,GLI];


scalar4VTX$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-scalar4VTX-2L-massless"}];
Quiet[CreateDirectory[scalar4VTX$dir]];


KiraCreateJobFile[scalar4VTX$FinalTopos, scalar4VTX$GLIs, scalar4VTX$dir]


KiraCreateIntegralFile[scalar4VTX$GLIs, scalar4VTX$FinalTopos, scalar4VTX$dir]
KiraCreateConfigFiles[scalar4VTX$FinalTopos, scalar4VTX$GLIs, scalar4VTX$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1}]


KiraRunReduction[scalar4VTX$dir, scalar4VTX$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


scalar4VTX$ReductionTables=KiraImportResults[scalar4VTX$FinalTopos, scalar4VTX$dir]//Flatten;


AbsoluteTiming[scalar4VTX$resPreFinal1=(scalar4VTX$AmpGLI/.Dispatch[scalar4VTX$ReductionTables]);]


AbsoluteTiming[scalar4VTX$resPreFinal2=Map[Collect2[#,GLI,FCParallelize->True]&,scalar4VTX$resPreFinal1];]


scalar4VTX$masters=Cases2[scalar4VTX$resPreFinal1,GLI];


scalar4VTX$MIMappings=FCLoopFindIntegralMappings[scalar4VTX$masters,Join[tadpoleMaster1[[2]],{tadpoleMaster2[[2]]},
scalar4VTX$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


isoSymbols1=FCMakeSymbols[LL,Range[1,$KernelCount],List];
isoSymbols2=FCMakeSymbols[LM,Range[1,$KernelCount],List];


AbsoluteTiming[scalar4VTX$resPreFinal2=Collect2[scalar4VTX$resPreFinal1,D,GLI,IsolateNames->isoSymbols1,FCParallelize->True]//FCReplaceD[#,D->4-2ep]&//ReplaceAll[#,scalar4VTX$MIMappings[[1]]]&//
ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//Collect2[#,ep,IsolateNames->isoSymbols2,FCParallelize->True]&;]


AbsoluteTiming[scalar4VTX$resPreFinal3=scalar4VTX$resPreFinal2//Series[#,{ep,0,-1}]&//Normal//Series[(I*(4*Pi)^(-2 + ep))^2 #,{ep,0,-1}]&//Normal;]


AbsoluteTiming[scalar4VTX$resPreFinal4=Collect2[FRH2[FRH2[scalar4VTX$resPreFinal3,isoSymbols2],isoSymbols1],DiracGamma,pp,mxt,ep,FCParallelize->True];]


isoSymbols3=FCMakeSymbols[LH,Range[1,$KernelCount],List];


AbsoluteTiming[scalar4VTX$resPreFinal5=Series[Total[Collect2[scalar4VTX$resPreFinal4,mxt,IsolateNames->isoSymbols3,FCParallelize->True]],{mxt,0,2}]//Normal;]


AbsoluteTiming[scalar4VTX$resPreFinal6=Collect2[FRH2[scalar4VTX$resPreFinal5,isoSymbols3]//ReplaceAll[#,Log[m_Symbol^2]:>2Log[m]]&,pp,mxt,ep,FCParallelize->True];]


scalar4VTX$resFinal=Collect2[Collect2[scalar4VTX$resPreFinal6,ep,g,Factoring->FullSimplify],ep,mphi,mxt]//ReplaceAll[#,Log[m_Symbol^2]:>2Log[m]]&


(* ::Subsection:: *)
(*Scalar self-energy 1-loop CT*)


FCClearScalarProducts[];
divDegree=0;
aux1=FCLoopGetFeynAmpDenominators[scalar4VTXCT$RawAmp/.q1|q2->0/.p2->-p1/.p1->p,{k1},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1},-mxt^2,0,Head->denHead]


scalar4VTXCT$StrName=StringReplace[ToString[Hold[scalar4VTXCT$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[scalar4VTXCT$Amp=(aux1[[1]]/.aux2);]


AbsoluteTiming[scalar4VTXCT$Amp1=Collect2[scalar4VTXCT$Amp,p,IsolateNames->KK];]
AbsoluteTiming[scalar4VTXCT$Amp2=FourSeries[scalar4VTXCT$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[scalar4VTXCT$Amp3=Collect2[FRH[scalar4VTXCT$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{scalar4VTXCT$Amp4,scalar4VTXCT$Topos}=FCLoopFindTopologies[scalar4VTXCT$Amp3,{k1},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->scalar4VTXtopo];


AbsoluteTiming[scalar4VTXCT$Amp5=FCLoopTensorReduce[scalar4VTXCT$Amp4,scalar4VTXCT$Topos,FCParallelize->True];]


{scalar4VTXCT$Amp6,scalar4VTXCT$Topos2}=FCLoopRewriteOverdeterminedTopologies[scalar4VTXCT$Amp5,scalar4VTXCT$Topos,FCParallelize->True];


{scalar4VTXCT$Amp7,scalar4VTXCT$Topos3}=FCLoopRewriteIncompleteTopologies[scalar4VTXCT$Amp6,scalar4VTXCT$Topos2,FCParallelize->True];


AbsoluteTiming[scalar4VTXCT$SubTopos=FCLoopFindSubtopologies[scalar4VTXCT$Topos2,Flatten->True,Remove->True,FCParallelize->True];]


AbsoluteTiming[{scalar4VTXCT$TopoMappings,scalar4VTXCT$FinalTopos}=FCLoopFindTopologyMappings[scalar4VTXCT$Topos2,PreferredTopologies->scalar4VTXCT$SubTopos,FCParallelize->True];]


AbsoluteTiming[scalar4VTXCT$AmpGLI=FCLoopApplyTopologyMappings[scalar4VTXCT$Amp7,{scalar4VTXCT$TopoMappings,scalar4VTXCT$FinalTopos},FCParallelize->True];]


scalar4VTXCT$GLIs=Cases2[scalar4VTXCT$AmpGLI,GLI];


scalar4VTXCT$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>scalar4VTXCT$StrName<>"-1L-massive"}];
Quiet[CreateDirectory[scalar4VTXCT$dir]];


KiraCreateJobFile[scalar4VTXCT$FinalTopos, scalar4VTXCT$GLIs, scalar4VTXCT$dir]


KiraCreateIntegralFile[scalar4VTXCT$GLIs, scalar4VTXCT$FinalTopos, scalar4VTXCT$dir]
KiraCreateConfigFiles[scalar4VTXCT$FinalTopos, scalar4VTXCT$GLIs, scalar4VTXCT$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1}]


KiraRunReduction[scalar4VTXCT$dir, scalar4VTXCT$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


scalar4VTXCT$ReductionTables=KiraImportResults[scalar4VTXCT$FinalTopos, scalar4VTXCT$dir]//Flatten;


scalar4VTXCT$resPreFinal1=Collect2[Total[scalar4VTXCT$AmpGLI/.Dispatch[scalar4VTXCT$ReductionTables]],GLI,D,FCParallelize->True];


scalar4VTXCT$masters=Cases2[scalar4VTXCT$resPreFinal1,GLI];


scalar4VTXCT$MIMappings=FCLoopFindIntegralMappings[scalar4VTXCT$masters,Join[tadpoleMaster1[[2]],
scalar4VTXCT$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


(* ::Text:: *)
(*At this point we need to insert the 1-loop renormalization constants*)


knownResults1L ={rc[delZphi, 1] -> 0, rc[delZmphi, 1] -> 1/(32*ep*Pi^2), rc[delZg, 1] -> (3)/(32*ep*Pi^2)};


knownResults2L ={rc[delZphi, 2] -> -1/6144*1/(ep*Pi^4)};


Collect2[scalar4VTXCT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,scalar4VTXCT$MIMappings[[1]]]&


AbsoluteTiming[scalar4VTXCT$resPreFinal2=Collect2[scalar4VTXCT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,scalar4VTXCT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,1}]&//Normal//FCLoopAddMissingHigherOrdersWarning[#,ep,epHelp]&//FRH//
ReplaceAll[#,Log[m_^2]:>2Log[m]]&;]


AbsoluteTiming[scalar4VTXCT$resPreFinal2=Collect2[scalar4VTXCT$resPreFinal1,Join[{g},List@@renConstants],IsolateNames->KK]//ReplaceAll[#,{
	(h:renConstants):>1+(g rc[ToExpression["del"<>ToString[h]],1]+g^2 rc[ToExpression["del"<>ToString[h]],2])}]&//Series[#,{g,0,3}]&//Normal;]


AbsoluteTiming[scalar4VTXCT$resPreFinal3=Collect2[scalar4VTXCT$resPreFinal2//FRH,{rc,D,GLI},IsolateNames->KK]//FCReplaceD[#,{D->4-2ep}]&//ReplaceRepeated[#,knownResults1L]&//
ReplaceAll[#,scalar4VTXCT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Abort[],#]&//Collect2[#,ep,IsolateNames->KK]&;]


scalar4VTXCT$resFinal=scalar4VTXCT$resPreFinal3//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//
ReplaceAll[#,{Log[m_^2]:>2Log[m],Log[4Pi]->Log[4]+Log[Pi]}]&//Collect2[#,ep,mphi]&


(* ::Subsection:: *)
(*Determination of renormalization constants*)


diagScalarTree4VTX$Amp=(Total[diagScalarTree4VTX$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+(g rc[ToExpression["del"<>ToString[h]],1]+g^2 rc[ToExpression["del"<>ToString[h]],2])}]&//
	Series[#,{g,0,3}]&//Normal//ReplaceRepeated[#,Join[knownResults1L,knownResults2L]]&


Coefficient[scalar4VTX$resFinal+ scalar4VTXCT$resFinal+ diagScalarTree4VTX$Amp,g,3]//Collect2[#,pp]&


scalar4VTX$RenConstants2L=Coefficient[scalar4VTX$resFinal+ scalar4VTXCT$resFinal+ diagScalarTree4VTX$Amp,g,3]//Collect2[#,pp]&//FCMatchSolve[#,{ep,mphi,pp}]&//FullSimplify


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final phi^4 2-loop wave-function renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ g rc[ToExpression["del"<>ToString[h]],1]+ g^2 rc[ToExpression["del"<>ToString[h]],2])//
ReplaceAll[#,Join[SUNSimplify[knownResults1L,SUNNToCACF->False],scalar4VTX$RenConstants2L]]&]]//SelectNotFree[#,Zg]&//ExpandAll


finalResults//TableForm


knownResult = {rc[delZg, 2] -> (27 - 17*ep)/(3072*ep^2*Pi^4)};


FCCompareResults[scalar4VTX$RenConstants2L,knownResult,
Text->{"\tCompare to the known result:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



