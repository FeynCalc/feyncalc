(* ::Package:: *)

(* :Title: Renormalization2L-GhGh										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QCD, MSbar, Ghost self-energy, massless, 2-loop *)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*2-loop QCD renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QCD model created with FeynRules.*)


description="Renormalization, QCD, MSbar, 2-loop";
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


modelDir=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","Examples","Models","QCD"}]


FAPatch[PatchModelsOnly->True,FAModelsDirectory->modelDir];


renConstants=Zm|Zpsi|ZA|ZAmxt|Zu|Zumxt|Zg|Zxi


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[mu,"\[Mu]"];
FCAttachTypesettingRule[nu,"\[Nu]"];
FCAttachTypesettingRule[rho,"\[Rho]"];
FCAttachTypesettingRule[si,"\[Sigma]"];


diagGhostSE=InsertFields[CreateTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {U[5]} -> {U[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagGhostSECT=InsertFields[CreateCTTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {U[5]} -> {U[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagGhostTreeSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {U[5]} -> {U[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagGhostSE, ColumnsXRows -> {6, 2},SheetHeader->None,
Numbering -> Simple, ImageSize->128{6, 2}];


(* ::Text:: *)
(*1-loop counter-term diagrams*)


Paint[diagGhostSECT, ColumnsXRows -> {4,1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Tree-level counter-term diagrams*)


Paint[diagGhostTreeSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Section:: *)
(*Master integrals*)


(* ::Text:: *)
(*The only required masters are 1- and 2-loop tadpoles*)


tadpoleMaster=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]];


tadpoleMaster1=tadpoleMaster/.m1->mxt/.tad1L->"tad1Lv1";


tadpoleMaster2=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxFx111x111xxEp1x.m"}]]/.m1->mxt/.tad2LxFx111x111xxEp1x->"tad2Lv2";


(* ::Section:: *)
(*Obtain the amplitudes*)


{ghostSE$RawAmp,ghostSECT$RawAmp,diagGhostTreeSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k1,k2}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->0,SMP["g_s"]->4 Pi Sqrt[as4]}]&/@{
	diagGhostSE,diagGhostSECT,diagGhostTreeSECT};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Ghost self-energy at 2 loops*)


(* ::Text:: *)
(*The 2-loop ghost self-energy has superficial degree of divergence equal to 2*)


FCClearScalarProducts[];
divDegree=2;
ghostSE$RawAmp2=Join[ghostSE$RawAmp[[1;;3]],Nf ghostSE$RawAmp[[4;;4]],ghostSE$RawAmp[[5;;7]]];
aux1=FCLoopGetFeynAmpDenominators[ghostSE$RawAmp2,
{k1,k2},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1,k2},-mxt^2,0,Head->denHead]


AbsoluteTiming[ghostSE$PreAmp1=Contract[(aux1[[1]]/.aux2),FCParallelize->True];]


AbsoluteTiming[ghostSE$Amp=ghostSE$PreAmp1//
SUNSimplify[#,FCI->True,FCParallelize->True]&//DiracSimplify[#,FCI->True,FCParallelize->True]&;]


isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]


AbsoluteTiming[ghostSE$Amp1=Collect2[ghostSE$Amp,p,IsolateNames->isoSymbols,FCParallelize->True];]


AbsoluteTiming[ghostSE$Amp2=FourSeries[ghostSE$Amp1,{p,0,divDegree},FCParallelize->True];]


AbsoluteTiming[ghostSE$Amp3=Collect2[FRH2[ghostSE$Amp2,isoSymbols],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[]
SPD[p]=pp;


AbsoluteTiming[{ghostSE$Amp4,ghostSE$Topos}=FCLoopFindTopologies[ghostSE$Amp3,{k1,k2},FCI->True,FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp}];]


AbsoluteTiming[ghostSE$Amp5=FCLoopTensorReduce[ghostSE$Amp4,ghostSE$Topos,FCParallelize->True];]


AbsoluteTiming[ghostSE$Amp6=DiracSimplify[ghostSE$Amp5,FCParallelize->True];]


AbsoluteTiming[{ghostSE$Amp7,ghostSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[ghostSE$Amp6,ghostSE$Topos,FCParallelize->True];]


AbsoluteTiming[{ghostSE$Amp8,ghostSE$Topos3}=FCLoopRewriteIncompleteTopologies[ghostSE$Amp7,ghostSE$Topos2,FCParallelize->True];]


AbsoluteTiming[ghostSE$SubTopos=FCLoopFindSubtopologies[ghostSE$Topos3,Flatten->True,Remove->True,FCParallelize->True];]


{ghostSE$TopoMappings,
ghostSE$FinalTopos}=FCLoopFindTopologyMappings[ghostSE$Topos3,PreferredTopologies->ghostSE$SubTopos,FCParallelize->True];


AbsoluteTiming[ghostSE$AmpGLI=FCLoopApplyTopologyMappings[ghostSE$Amp8,{ghostSE$TopoMappings,
ghostSE$FinalTopos},FCParallelize->True];]


ghostSE$GLIs=Cases2[ghostSE$AmpGLI,GLI];


ghostSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-ghostSE-2L-massless"}];
Quiet[CreateDirectory[ghostSE$dir]];


KiraCreateJobFile[ghostSE$FinalTopos, ghostSE$GLIs, ghostSE$dir]


KiraCreateIntegralFile[ghostSE$GLIs, ghostSE$FinalTopos, ghostSE$dir]
KiraCreateConfigFiles[ghostSE$FinalTopos, ghostSE$GLIs, ghostSE$dir, 
 KiraMassDimensions -> {pp -> 2,mxt->1}]


KiraRunReduction[ghostSE$dir, ghostSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


ghostSE$ReductionTables=KiraImportResults[ghostSE$FinalTopos, ghostSE$dir]//Flatten;


AbsoluteTiming[ghostSE$resPreFinal1=(ghostSE$AmpGLI/.Dispatch[ghostSE$ReductionTables]);]


AbsoluteTiming[ghostSE$resPreFinal2=Map[Collect2[#,GLI,DiracGamma,FCParallelize->True]&,ghostSE$resPreFinal1];]


ghostSE$masters=Cases2[ghostSE$resPreFinal1,GLI];


ghostSE$MIMappings=FCLoopFindIntegralMappings[ghostSE$masters,Join[tadpoleMaster1[[2]],{tadpoleMaster2[[2]]},
ghostSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


isoSymbols1=FCMakeSymbols[LL,Range[1,$KernelCount],List];
isoSymbols2=FCMakeSymbols[LM,Range[1,$KernelCount],List];


AbsoluteTiming[ghostSE$resPreFinal2=Collect2[ghostSE$resPreFinal1,D,GLI,IsolateNames->isoSymbols1,FCParallelize->True]//FCReplaceD[#,D->4-2ep]&//ReplaceAll[#,ghostSE$MIMappings[[1]]]&//
ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//Collect2[#,ep,IsolateNames->isoSymbols2,FCParallelize->True]&;]


AbsoluteTiming[ghostSE$resPreFinal3=ghostSE$resPreFinal2//Series[#,{ep,0,-1}]&//Normal//Series[(I*(4*Pi)^(-2 + ep))^2 #,{ep,0,-1}]&//Normal;]


AbsoluteTiming[ghostSE$resPreFinal4=Collect2[FRH2[FRH2[ghostSE$resPreFinal3,isoSymbols2],isoSymbols1],DiracGamma,pp,mxt,ep,FCParallelize->True];]


isoSymbols3=FCMakeSymbols[LH,Range[1,$KernelCount],List];


AbsoluteTiming[ghostSE$resPreFinal5=Series[Total[Collect2[ghostSE$resPreFinal4,mxt,IsolateNames->isoSymbols3,FCParallelize->True]],{mxt,0,2}]//Normal;]


AbsoluteTiming[ghostSE$resPreFinal6=Collect2[FRH2[ghostSE$resPreFinal5,isoSymbols3]//ReplaceAll[#,Log[m_Symbol^2]:>2Log[m]]&,DiracGamma,pp,mxt,ep,FCParallelize->True];]


ghostSE$resFinal=Collect2[Collect2[ghostSE$resPreFinal6,ep,CA,CF,mq,Nf,SUNFDelta,as4,DiracGamma,GaugeXi,Factoring->FullSimplify],ep,mq,mxt]


(* ::Subsection:: *)
(*Gluon self-energy 1-loop CT*)


FCClearScalarProducts[];
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[ghostSECT$RawAmp,{k1},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1},-mxt^2,0,Head->denHead]


ghostSECT$StrName=StringReplace[ToString[Hold[ghostSECT$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[ghostSECT$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[ghostSECT$Amp1=Collect2[ghostSECT$Amp,p,IsolateNames->KK];]
AbsoluteTiming[ghostSECT$Amp2=FourSeries[ghostSECT$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[ghostSECT$Amp3=Collect2[FRH[ghostSECT$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{ghostSECT$Amp4,ghostSECT$Topos}=FCLoopFindTopologies[ghostSECT$Amp3,{k1},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->quarkSEtopo];


AbsoluteTiming[ghostSECT$Amp5=FCLoopTensorReduce[ghostSECT$Amp4,ghostSECT$Topos,FCParallelize->True];]


AbsoluteTiming[ghostSECT$Amp6=DiracSimplify[ghostSECT$Amp5,FCParallelize->True];]


{ghostSECT$Amp7,ghostSECT$Topos2}=FCLoopRewriteOverdeterminedTopologies[ghostSECT$Amp6,ghostSECT$Topos,FCParallelize->True];


{ghostSECT$Amp8,ghostSECT$Topos3}=FCLoopRewriteIncompleteTopologies[ghostSECT$Amp7,ghostSECT$Topos2,FCParallelize->True];


AbsoluteTiming[ghostSECT$SubTopos=FCLoopFindSubtopologies[ghostSECT$Topos2,Flatten->True,Remove->True,FCParallelize->True];]


AbsoluteTiming[{ghostSECT$TopoMappings,ghostSECT$FinalTopos}=FCLoopFindTopologyMappings[ghostSECT$Topos2,PreferredTopologies->ghostSECT$SubTopos,FCParallelize->True];]


AbsoluteTiming[ghostSECT$AmpGLI=FCLoopApplyTopologyMappings[ghostSECT$Amp8,{ghostSECT$TopoMappings,ghostSECT$FinalTopos},FCParallelize->True];]


ghostSECT$GLIs=Cases2[ghostSECT$AmpGLI,GLI];


ghostSECT$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>ghostSECT$StrName<>"-1L-massive"}];
Quiet[CreateDirectory[ghostSECT$dir]];


KiraCreateJobFile[ghostSECT$FinalTopos, ghostSECT$GLIs, ghostSECT$dir]


KiraCreateIntegralFile[ghostSECT$GLIs, ghostSECT$FinalTopos, ghostSECT$dir]
KiraCreateConfigFiles[ghostSECT$FinalTopos, ghostSECT$GLIs, ghostSECT$dir, 
 KiraMassDimensions -> {pp -> 2,mxt->1}]


KiraRunReduction[ghostSECT$dir, ghostSECT$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


ghostSECT$ReductionTables=KiraImportResults[ghostSECT$FinalTopos, ghostSECT$dir]//Flatten;


ghostSECT$resPreFinal1=Collect2[Total[ghostSECT$AmpGLI/.Dispatch[ghostSECT$ReductionTables]],GLI,
GaugeXi,D,DiracGamma,FCParallelize->True];


ghostSECT$masters=Cases2[ghostSECT$resPreFinal1,GLI];


ghostSECT$MIMappings=FCLoopFindIntegralMappings[ghostSECT$masters,Join[tadpoleMaster1[[2]],
ghostSECT$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


(* ::Text:: *)
(*At this point we need to insert the 1-loop renormalization constants*)


knownResults1L = {
rc[delZA, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
 rc[delZAmxt, 1] -> -1/8*(CA + 8*Nf + 3*CA*GaugeXi["G"])/ep, 
 rc[delZxi, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
 rc[delZpsi, 1] -> -((CF*GaugeXi["G"])/ep), 
 rc[delZumxt, 1] -> 0, rc[delZu, 1] -> (CA*(3 - GaugeXi["G"]))/(4*ep), 
 rc[delZg, 1] -> -1/6*(11*CA - 2*Nf)/ep};


AbsoluteTiming[ghostSECT$resPreFinal2=Collect2[ghostSECT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,ghostSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,1}]&//Normal//FCLoopAddMissingHigherOrdersWarning[#,ep,epHelp]&//FRH//
ReplaceAll[#,{Log[mxt^2]->2Log[mxt]}]&;]


AbsoluteTiming[ghostSECT$resPreFinal2=Collect2[ghostSECT$resPreFinal1,Join[{as4},List@@renConstants],IsolateNames->KK]//ReplaceAll[#,Zxi->ZA]&//ReplaceAll[#,{
	(h:renConstants):>1+(as4 rc[ToExpression["del"<>ToString[h]],1]+as4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//Series[#,{as4,0,2}]&//Normal;]


AbsoluteTiming[ghostSECT$resPreFinal3=Collect2[ghostSECT$resPreFinal2//FRH,{rc,D,GLI},IsolateNames->KK]//FCReplaceD[#,{D->4-2ep}]&//ReplaceRepeated[#,knownResults1L]&//
ReplaceAll[#,ghostSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Abort[],#]&//Collect2[#,ep,IsolateNames->KK]&;]


ghostSECT$resFinal=ghostSECT$resPreFinal3//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//
Collect2[#,mxt,IsolateNames->KK]&//Series[#,{mxt,0,2}]&//Normal//FRH//ReplaceAll[#,Log[m_^2]:>2Log[m]]&//Collect2[#,ep,mq,mxt]&


(* ::Subsection:: *)
(*Determination of renormalization constants*)


diagGhostTreeSECT$Amp=(Total[diagGhostTreeSECT$RawAmp])//ReplaceAll[#,Zxi->ZA]&//ReplaceRepeated[#,{
	(h:renConstants):>1+(as4 rc[ToExpression["del"<>ToString[h]],1]+as4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//
	Series[#,{as4,0,2}]&//Normal//ReplaceRepeated[#,knownResults1L]&


ghostSE$RenConstants2L=Collect2[Coefficient[SUNSimplify[ ghostSE$resFinal+ ghostSECT$resFinal+ diagGhostTreeSECT$Amp,SUNNToCACF->False],as4,2],
as4,mxt,DiracGamma,Factoring->Simplify]//FCMatchSolve[#,{ep,CF,DiracGamma,mq,mxt,SUNDelta,SUNTF,SUNFDelta,CA,GaugeXi,as4,Pair,pp,Nf,SUNN}]&//Collect2[#,ep]&


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final QCD 2-loop wave-function renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ as4 rc[ToExpression["del"<>ToString[h]],1]+ as4^2 rc[ToExpression["del"<>ToString[h]],2])//
ReplaceAll[#,Join[SUNSimplify[knownResults1L,SUNNToCACF->False],ghostSE$RenConstants2L]]&]]//SelectNotFree[#,Zu,Zumxt]&;


finalResults//TableForm


knownResult = {rc[delZumxt, 2] -> 0, 
rc[delZu, 2] -> -1/96*(SUNN*(20*Nf - 95*SUNN - 3*SUNN*GaugeXi["G"]))/ep + (SUNN*(8*Nf - 35*SUNN + 3*SUNN*GaugeXi["G"]^2))/(32*ep^2)};


FCCompareResults[ghostSE$RenConstants2L,knownResult,
Text->{"\tCompare to Chetyrkin, Four-loop renormalization of QCD: full set of renormalization constants and anomalous dimensions, \
arXiv:hep-ph/0405193:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



