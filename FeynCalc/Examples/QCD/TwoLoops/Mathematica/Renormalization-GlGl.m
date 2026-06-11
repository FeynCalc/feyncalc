(* ::Package:: *)

(* :Title: Renormalization-GlGl										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QCD, MSbar, Gluon self-energy, massless, 2-loop *)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*2-loop QCD renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QCD model created with FeynRules.*)


description="Renormalization, QCD, MSbar, Gluon self-energy, massless, 2-loop";
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


diagGluonSE=InsertFields[CreateTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[5]} -> {V[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagGluonSECT=InsertFields[CreateCTTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[5]} -> {V[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagGluonTreeSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[5]} -> {V[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagGluonSE, ColumnsXRows -> {6, 4},SheetHeader->None,
Numbering -> Simple, ImageSize->128{6, 4}];


(* ::Text:: *)
(*1-loop counter-term diagrams*)


Paint[diagGluonSECT, ColumnsXRows -> {6, 3},SheetHeader->None,
Numbering -> Simple, ImageSize->128{6, 3}];


(* ::Text:: *)
(*Tree-level counter-term diagrams*)


Paint[diagGluonTreeSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
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


{gluonSE$RawAmp,gluonSECT$RawAmp,diagGluonTreeSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k1,k2}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->0,SMP["g_s"]->4 Pi Sqrt[as4]}]&/@{
	diagGluonSE,diagGluonSECT,diagGluonTreeSECT};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Gluon self-energy at 2 loops*)


(* ::Text:: *)
(*The 2-loop gluon self-energy has superficial degree of divergence equal to 2*)


FCClearScalarProducts[];
divDegree=2;
gluonSE$RawAmp2=Join[gluonSE$RawAmp[[1;;3]],Nf gluonSE$RawAmp[[4;;4]],gluonSE$RawAmp[[5;;5]],Nf gluonSE$RawAmp[[6;;7]],
gluonSE$RawAmp[[8;;10]],Nf gluonSE$RawAmp[[11;;12]],gluonSE$RawAmp[[13;;14]],
Nf gluonSE$RawAmp[[15;;15]],gluonSE$RawAmp[[16;;19]],Nf gluonSE$RawAmp[[20;;20]],gluonSE$RawAmp[[21;;23]]];
aux1=FCLoopGetFeynAmpDenominators[gluonSE$RawAmp2,
{k1,k2},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1,k2},-mxt^2,0,Head->denHead]


AbsoluteTiming[gluonSE$PreAmp1=Contract[(aux1[[1]]/.aux2),FCParallelize->True];]


AbsoluteTiming[gluonSE$Amp=gluonSE$PreAmp1//
SUNSimplify[#,FCI->True,FCParallelize->True]&//DiracSimplify[#,FCI->True,FCParallelize->True]&;]


isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]


AbsoluteTiming[gluonSE$Amp1=Collect2[gluonSE$Amp,p,IsolateNames->isoSymbols,FCParallelize->True];]


AbsoluteTiming[gluonSE$Amp2=FourSeries[gluonSE$Amp1,{p,0,divDegree},FCParallelize->True];]


AbsoluteTiming[gluonSE$Amp3=Collect2[FRH2[gluonSE$Amp2,isoSymbols],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[]
SPD[p]=pp;


AbsoluteTiming[{gluonSE$Amp4,gluonSE$Topos}=FCLoopFindTopologies[gluonSE$Amp3,{k1,k2},FCI->True,FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp}];]


AbsoluteTiming[gluonSE$Amp5=FCLoopTensorReduce[gluonSE$Amp4,gluonSE$Topos,FCParallelize->True];]


AbsoluteTiming[gluonSE$Amp6=DiracSimplify[gluonSE$Amp5,FCParallelize->True];]


AbsoluteTiming[{gluonSE$Amp7,gluonSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[gluonSE$Amp6,gluonSE$Topos,FCParallelize->True];]


AbsoluteTiming[{gluonSE$Amp8,gluonSE$Topos3}=FCLoopRewriteIncompleteTopologies[gluonSE$Amp7,gluonSE$Topos2,FCParallelize->True];]


AbsoluteTiming[gluonSE$SubTopos=FCLoopFindSubtopologies[gluonSE$Topos3,Flatten->True,Remove->True,FCParallelize->True];]


{gluonSE$TopoMappings,
gluonSE$FinalTopos}=FCLoopFindTopologyMappings[gluonSE$Topos3,PreferredTopologies->gluonSE$SubTopos,FCParallelize->True];


AbsoluteTiming[gluonSE$AmpGLI=FCLoopApplyTopologyMappings[gluonSE$Amp8,{gluonSE$TopoMappings,
gluonSE$FinalTopos},FCParallelize->True];]


gluonSE$GLIs=Cases2[gluonSE$AmpGLI,GLI];


gluonSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-gluonSE-2L-massless"}];
Quiet[CreateDirectory[gluonSE$dir]];


KiraCreateJobFile[gluonSE$FinalTopos, gluonSE$GLIs, gluonSE$dir]


KiraCreateIntegralFile[gluonSE$GLIs, gluonSE$FinalTopos, gluonSE$dir]
KiraCreateConfigFiles[gluonSE$FinalTopos, gluonSE$GLIs, gluonSE$dir, 
 KiraMassDimensions -> {pp -> 2,mxt->1}]


KiraRunReduction[gluonSE$dir, gluonSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


gluonSE$ReductionTables=KiraImportResults[gluonSE$FinalTopos, gluonSE$dir]//Flatten;


AbsoluteTiming[gluonSE$resPreFinal1=(gluonSE$AmpGLI/.Dispatch[gluonSE$ReductionTables]);]


AbsoluteTiming[gluonSE$resPreFinal2=Map[Collect2[#,GLI,DiracGamma,FCParallelize->True]&,gluonSE$resPreFinal1];]


gluonSE$masters=Cases2[gluonSE$resPreFinal1,GLI];


gluonSE$MIMappings=FCLoopFindIntegralMappings[gluonSE$masters,Join[tadpoleMaster1[[2]],{tadpoleMaster2[[2]]},
gluonSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


isoSymbols1=FCMakeSymbols[LL,Range[1,$KernelCount],List];
isoSymbols2=FCMakeSymbols[LM,Range[1,$KernelCount],List];


AbsoluteTiming[gluonSE$resPreFinal2=Collect2[gluonSE$resPreFinal1,D,GLI,IsolateNames->isoSymbols1,FCParallelize->True]//FCReplaceD[#,D->4-2ep]&//ReplaceAll[#,gluonSE$MIMappings[[1]]]&//
ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//Collect2[#,ep,IsolateNames->isoSymbols2,FCParallelize->True]&;]


AbsoluteTiming[gluonSE$resPreFinal3=gluonSE$resPreFinal2//Series[#,{ep,0,-1}]&//Normal//Series[(I*(4*Pi)^(-2 + ep))^2 #,{ep,0,-1}]&//Normal;]


AbsoluteTiming[gluonSE$resPreFinal4=Collect2[FRH2[FRH2[gluonSE$resPreFinal3,isoSymbols2],isoSymbols1],DiracGamma,pp,mxt,ep,FCParallelize->True];]


isoSymbols3=FCMakeSymbols[LH,Range[1,$KernelCount],List];


AbsoluteTiming[gluonSE$resPreFinal5=Series[Total[Collect2[gluonSE$resPreFinal4,mxt,IsolateNames->isoSymbols3,FCParallelize->True]],{mxt,0,2}]//Normal;]


AbsoluteTiming[gluonSE$resPreFinal6=Collect2[FRH2[gluonSE$resPreFinal5,isoSymbols3]//ReplaceAll[#,Log[m_Symbol^2]:>2Log[m]]&,DiracGamma,pp,mxt,ep,FCParallelize->True];]


gluonSE$resFinal=Collect2[Collect2[gluonSE$resPreFinal6,ep,CA,CF,mq,Nf,SUNFDelta,as4,DiracGamma,GaugeXi,Factoring->FullSimplify,FCParallelize->True],ep,mq,mxt]


(* ::Subsection:: *)
(*Gluon self-energy 1-loop CT*)


FCClearScalarProducts[];
divDegree=2;
gluonSECT$RawAmp2=Join[Nf gluonSECT$RawAmp[[1;;2]],gluonSECT$RawAmp[[3;;6]],Nf gluonSECT$RawAmp[[7;;7]],gluonSECT$RawAmp[[8;;10]],
Nf gluonSECT$RawAmp[[11;;11]],gluonSECT$RawAmp[[12;;]]];
aux1=FCLoopGetFeynAmpDenominators[gluonSECT$RawAmp2,{k1},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1},-mxt^2,0,Head->denHead]


gluonSECT$StrName=StringReplace[ToString[Hold[gluonSECT$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[gluonSECT$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[gluonSECT$Amp1=Collect2[gluonSECT$Amp,p,IsolateNames->KK];]
AbsoluteTiming[gluonSECT$Amp2=FourSeries[gluonSECT$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[gluonSECT$Amp3=Collect2[FRH[gluonSECT$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{gluonSECT$Amp4,gluonSECT$Topos}=FCLoopFindTopologies[gluonSECT$Amp3,{k1},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->quarkSEtopo];


AbsoluteTiming[gluonSECT$Amp5=FCLoopTensorReduce[gluonSECT$Amp4,gluonSECT$Topos,FCParallelize->True];]


AbsoluteTiming[gluonSECT$Amp6=DiracSimplify[gluonSECT$Amp5,FCParallelize->True];]


{gluonSECT$Amp7,gluonSECT$Topos2}=FCLoopRewriteOverdeterminedTopologies[gluonSECT$Amp6,gluonSECT$Topos,FCParallelize->True];


{gluonSECT$Amp8,gluonSECT$Topos3}=FCLoopRewriteIncompleteTopologies[gluonSECT$Amp7,gluonSECT$Topos2,FCParallelize->True];


AbsoluteTiming[gluonSECT$SubTopos=FCLoopFindSubtopologies[gluonSECT$Topos2,Flatten->True,Remove->True,FCParallelize->True];]


AbsoluteTiming[{gluonSECT$TopoMappings,gluonSECT$FinalTopos}=FCLoopFindTopologyMappings[gluonSECT$Topos2,PreferredTopologies->gluonSECT$SubTopos,FCParallelize->True];]


AbsoluteTiming[gluonSECT$AmpGLI=FCLoopApplyTopologyMappings[gluonSECT$Amp8,{gluonSECT$TopoMappings,gluonSECT$FinalTopos},FCParallelize->True];]


gluonSECT$GLIs=Cases2[gluonSECT$AmpGLI,GLI];


gluonSECT$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>gluonSECT$StrName<>"-1L-massive"}];
Quiet[CreateDirectory[gluonSECT$dir]];


KiraCreateJobFile[gluonSECT$FinalTopos, gluonSECT$GLIs, gluonSECT$dir]


KiraCreateIntegralFile[gluonSECT$GLIs, gluonSECT$FinalTopos, gluonSECT$dir]
KiraCreateConfigFiles[gluonSECT$FinalTopos, gluonSECT$GLIs, gluonSECT$dir, 
 KiraMassDimensions -> {pp -> 2,mxt->1}]


KiraRunReduction[gluonSECT$dir, gluonSECT$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


gluonSECT$ReductionTables=KiraImportResults[gluonSECT$FinalTopos, gluonSECT$dir]//Flatten;


gluonSECT$resPreFinal1=Collect2[Total[gluonSECT$AmpGLI/.Dispatch[gluonSECT$ReductionTables]],GLI,
GaugeXi,D,DiracGamma,FCParallelize->True];


gluonSECT$masters=Cases2[gluonSECT$resPreFinal1,GLI];


gluonSECT$MIMappings=FCLoopFindIntegralMappings[gluonSECT$masters,Join[tadpoleMaster1[[2]],
gluonSECT$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]}]


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


AbsoluteTiming[gluonSECT$resPreFinal2=Collect2[gluonSECT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,gluonSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,1}]&//Normal//FCLoopAddMissingHigherOrdersWarning[#,ep,epHelp]&//FRH//
ReplaceAll[#,{Log[mxt^2]->2Log[mxt]}]&;]


AbsoluteTiming[gluonSECT$resPreFinal3=Collect2[gluonSECT$resPreFinal2,Join[{as4},List@@renConstants],IsolateNames->KK]//ReplaceAll[#,Zxi->ZA]&//
ReplaceAll[#,{(h:renConstants):>1+(as4 rc[ToExpression["del"<>ToString[h]],1]+as4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//Series[#,{as4,0,2}]&//Normal;]


gluonSECT$resPreFinal4=Collect2[gluonSECT$resPreFinal3//FRH,{rc,D,GLI},IsolateNames->KK]//FCReplaceD[#,{D->4-2ep}]&//
ReplaceRepeated[#,knownResults1L]&//FRH//Series[#,{ep,0,-1}]&//Normal//Collect2[#,ep,mxt,Factoring->Simplify,FCParallelize->True,Pair]&;


gluonSECT$resFinal=Collect2[gluonSECT$resPreFinal4,mxt,IsolateNames->KK]//Series[#,{mxt,0,2}]&//Normal//FRH//Collect2[#,ep,mxt,Pair,Factoring->Simplify,FCParallelize->True]&


(* ::Subsection:: *)
(*Determination of renormalization constants*)


diagGluonTreeSECT$Amp=(Total[diagGluonTreeSECT$RawAmp])//ReplaceAll[#,Zxi->ZA]&//ReplaceRepeated[#,{
	(h:renConstants):>1+(as4 rc[ToExpression["del"<>ToString[h]],1]+as4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//
	Series[#,{as4,0,2}]&//Normal//ReplaceRepeated[#,knownResults1L]&


gluonSE$RenConstants2L=Collect2[Coefficient[SUNSimplify[gluonSE$resFinal+gluonSECT$resFinal+ diagGluonTreeSECT$Amp,SUNNToCACF->False],as4,2],
as4,mxt,DiracGamma,Factoring->Simplify]//FCMatchSolve[#,{ep,CF,DiracGamma,mq,mxt,SUNDelta,SUNTF,SUNFDelta,CA,GaugeXi,as4,Pair,pp,Nf,SUNN}]&//Collect2[#,ep]&


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final QCD 2-loop wave-function renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ as4 rc[ToExpression["del"<>ToString[h]],1]+ as4^2 rc[ToExpression["del"<>ToString[h]],2])//
ReplaceAll[#,Join[SUNSimplify[knownResults1L,SUNNToCACF->False],gluonSE$RenConstants2L]]&]]//SelectNotFree[#,ZA,ZAmxt]&;


finalResults//TableForm


knownResult = {rc[delZA, 2] -> (SUNN*(3 + 2*GaugeXi["G"])*(4*Nf - 13*SUNN + 3*SUNN*GaugeXi["G"]))/(24*ep^2) - 
   (-8*Nf + 28*Nf*SUNN^2 - 59*SUNN^3 + 11*SUNN^3*GaugeXi["G"] + 2*SUNN^3*GaugeXi["G"]^2)/(16*ep*SUNN), 
 rc[delZAmxt, 2] -> -1/128*(64*Nf^2*SUNN - 112*Nf*SUNN^2 - 57*SUNN^3 - 32*Nf*GaugeXi["G"] - 50*SUNN^3*GaugeXi["G"] - 21*SUNN^3*GaugeXi["G"]^2)/(ep^2*SUNN) - 
   (40*Nf*SUNN^2 + 29*SUNN^3 + 24*Nf*GaugeXi["G"] + 36*Nf*SUNN^2*GaugeXi["G"] + 87*SUNN^3*GaugeXi["G"] + 30*SUNN^3*GaugeXi["G"]^2)/(192*ep*SUNN)};


FCCompareResults[gluonSE$RenConstants2L,knownResult,
Text->{"\tCompare to Chetyrkin, Four-loop renormalization of QCD: full set of renormalization constants and anomalous dimensions, \
arXiv:hep-ph/0405193:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



