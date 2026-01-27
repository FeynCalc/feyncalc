(* ::Package:: *)

(* :Title: Renormalization-GaGa										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QED, MSbar, Photon self-energy, massless, 2-loop *)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*2-loop QED renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QED model created with FeynRules.*)


description="Renormalization, QED, MSbar, Photon self-energy, massless, 2-loop";
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


modelDir=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","Examples","Models","QED"}]


FAPatch[PatchModelsOnly->True,FAModelsDirectory->modelDir];


renConstants=Zm|Zpsi|ZA|ZAmxt|Ze|Zxi


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[mu,"\[Mu]"];
FCAttachTypesettingRule[nu,"\[Nu]"];
FCAttachTypesettingRule[rho,"\[Rho]"];
FCAttachTypesettingRule[si,"\[Sigma]"];


diagPhotonSE=InsertFields[CreateTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[1]} -> {V[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


diagPhotonSECT=InsertFields[CreateCTTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[1]} -> {V[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


diagPhotonTreeSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[1]} -> {V[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagPhotonSE, ColumnsXRows -> {3, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{3, 1}];


(* ::Text:: *)
(*1-loop counter-term diagrams*)


Paint[diagPhotonSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Tree-level counter-term diagrams*)


Paint[diagPhotonTreeSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
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


{photonSE$RawAmp,photonSECT$RawAmp,diagPhotonTreeSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k1,k2}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_e"]->0,SMP["e"]->4 Pi Sqrt[a4]}]&/@{
	diagPhotonSE,diagPhotonSECT,diagPhotonTreeSECT};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Photon self-energy at 2 loops*)


(* ::Text:: *)
(*The 2-loop photon self-energy has superficial degree of divergence equal to 2*)


FCClearScalarProducts[];
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[Nf photonSE$RawAmp,
{k1,k2},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1,k2},-mxt^2,0,Head->denHead]


AbsoluteTiming[photonSE$PreAmp1=Contract[(aux1[[1]]/.aux2),FCParallelize->True];]


AbsoluteTiming[photonSE$Amp=photonSE$PreAmp1//
SUNSimplify[#,FCI->True,FCParallelize->True]&//DiracSimplify[#,FCI->True,FCParallelize->True]&;]


isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]


AbsoluteTiming[photonSE$Amp1=Collect2[photonSE$Amp,p,IsolateNames->isoSymbols,FCParallelize->True];]


AbsoluteTiming[photonSE$Amp2=FourSeries[photonSE$Amp1,{p,0,divDegree},FCParallelize->True];]


AbsoluteTiming[photonSE$Amp3=Collect2[FRH2[photonSE$Amp2,isoSymbols],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[]
SPD[p]=pp;


AbsoluteTiming[{photonSE$Amp4,photonSE$Topos}=FCLoopFindTopologies[photonSE$Amp3,{k1,k2},FCI->True,FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp}];]


AbsoluteTiming[photonSE$Amp5=FCLoopTensorReduce[photonSE$Amp4,photonSE$Topos,FCParallelize->True];]


AbsoluteTiming[photonSE$Amp6=DiracSimplify[photonSE$Amp5,FCParallelize->True];]


AbsoluteTiming[{photonSE$Amp7,photonSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[photonSE$Amp6,photonSE$Topos,FCParallelize->True];]


AbsoluteTiming[{photonSE$Amp8,photonSE$Topos3}=FCLoopRewriteIncompleteTopologies[photonSE$Amp7,photonSE$Topos2,FCParallelize->True];]


AbsoluteTiming[photonSE$SubTopos=FCLoopFindSubtopologies[photonSE$Topos3,Flatten->True,Remove->True,FCParallelize->True];]


{photonSE$TopoMappings,
photonSE$FinalTopos}=FCLoopFindTopologyMappings[photonSE$Topos3,PreferredTopologies->photonSE$SubTopos,FCParallelize->True];


AbsoluteTiming[photonSE$AmpGLI=FCLoopApplyTopologyMappings[photonSE$Amp8,{photonSE$TopoMappings,
photonSE$FinalTopos},FCParallelize->True];]


photonSE$GLIs=Cases2[photonSE$AmpGLI,GLI];


photonSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-photonSE-2L-massless"}];
Quiet[CreateDirectory[photonSE$dir]];


KiraCreateJobFile[photonSE$FinalTopos, photonSE$GLIs, photonSE$dir]


KiraCreateIntegralFile[photonSE$GLIs, photonSE$FinalTopos, photonSE$dir]
KiraCreateConfigFiles[photonSE$FinalTopos, photonSE$GLIs, photonSE$dir, 
 KiraMassDimensions -> {pp -> 2,mxt->1}]


KiraRunReduction[photonSE$dir, photonSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


photonSE$ReductionTables=KiraImportResults[photonSE$FinalTopos, photonSE$dir]//Flatten;


AbsoluteTiming[photonSE$resPreFinal1=(photonSE$AmpGLI/.Dispatch[photonSE$ReductionTables]);]


AbsoluteTiming[photonSE$resPreFinal2=Map[Collect2[#,GLI,DiracGamma,FCParallelize->True]&,photonSE$resPreFinal1];]


photonSE$masters=Cases2[photonSE$resPreFinal1,GLI];


photonSE$MIMappings=FCLoopFindIntegralMappings[photonSE$masters,Join[tadpoleMaster1[[2]],{tadpoleMaster2[[2]]},
photonSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


isoSymbols1=FCMakeSymbols[LL,Range[1,$KernelCount],List];
isoSymbols2=FCMakeSymbols[LM,Range[1,$KernelCount],List];


AbsoluteTiming[photonSE$resPreFinal2=Collect2[photonSE$resPreFinal1,D,GLI,IsolateNames->isoSymbols1,FCParallelize->True]//FCReplaceD[#,D->4-2ep]&//ReplaceAll[#,photonSE$MIMappings[[1]]]&//
ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//Collect2[#,ep,IsolateNames->isoSymbols2,FCParallelize->True]&;]


AbsoluteTiming[photonSE$resPreFinal3=photonSE$resPreFinal2//Series[#,{ep,0,-1}]&//Normal//Series[(I*(4*Pi)^(-2 + ep))^2 #,{ep,0,-1}]&//Normal;]


AbsoluteTiming[photonSE$resPreFinal4=Collect2[FRH2[FRH2[photonSE$resPreFinal3,isoSymbols2],isoSymbols1],DiracGamma,pp,mxt,ep,FCParallelize->True];]


isoSymbols3=FCMakeSymbols[LH,Range[1,$KernelCount],List];


AbsoluteTiming[photonSE$resPreFinal5=Series[Total[Collect2[photonSE$resPreFinal4,mxt,IsolateNames->isoSymbols3,FCParallelize->True]],{mxt,0,2}]//Normal;]


AbsoluteTiming[photonSE$resPreFinal6=Collect2[FRH2[photonSE$resPreFinal5,isoSymbols3]//ReplaceAll[#,Log[m_Symbol^2]:>2Log[m]]&,DiracGamma,pp,mxt,ep,FCParallelize->True];]


photonSE$resFinal=Collect2[Collect2[photonSE$resPreFinal6,ep,CA,CF,ml,Nf,SUNFDelta,a4,DiracGamma,GaugeXi,Factoring->FullSimplify,FCParallelize->True],ep,ml,mxt]


(* ::Subsection:: *)
(*Photon self-energy 1-loop CT*)


FCClearScalarProducts[];
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[Nf photonSECT$RawAmp,{k1},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1},-mxt^2,0,Head->denHead]


photonSECT$StrName=StringReplace[ToString[Hold[photonSECT$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[photonSECT$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[photonSECT$Amp1=Collect2[photonSECT$Amp,p,IsolateNames->KK];]
AbsoluteTiming[photonSECT$Amp2=FourSeries[photonSECT$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[photonSECT$Amp3=Collect2[FRH[photonSECT$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{photonSECT$Amp4,photonSECT$Topos}=FCLoopFindTopologies[photonSECT$Amp3,{k1},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->quarkSEtopo];


AbsoluteTiming[photonSECT$Amp5=FCLoopTensorReduce[photonSECT$Amp4,photonSECT$Topos,FCParallelize->True];]


AbsoluteTiming[photonSECT$Amp6=DiracSimplify[photonSECT$Amp5,FCParallelize->True];]


{photonSECT$Amp7,photonSECT$Topos2}=FCLoopRewriteOverdeterminedTopologies[photonSECT$Amp6,photonSECT$Topos,FCParallelize->True];


{photonSECT$Amp8,photonSECT$Topos3}=FCLoopRewriteIncompleteTopologies[photonSECT$Amp7,photonSECT$Topos2,FCParallelize->True];


AbsoluteTiming[photonSECT$SubTopos=FCLoopFindSubtopologies[photonSECT$Topos2,Flatten->True,Remove->True,FCParallelize->True];]


AbsoluteTiming[{photonSECT$TopoMappings,photonSECT$FinalTopos}=FCLoopFindTopologyMappings[photonSECT$Topos2,PreferredTopologies->photonSECT$SubTopos,FCParallelize->True];]


AbsoluteTiming[photonSECT$AmpGLI=FCLoopApplyTopologyMappings[photonSECT$Amp8,{photonSECT$TopoMappings,photonSECT$FinalTopos},FCParallelize->True];]


photonSECT$GLIs=Cases2[photonSECT$AmpGLI,GLI];


photonSECT$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>photonSECT$StrName<>"-1L-massive"}];
Quiet[CreateDirectory[photonSECT$dir]];


KiraCreateJobFile[photonSECT$FinalTopos, photonSECT$GLIs, photonSECT$dir]


KiraCreateIntegralFile[photonSECT$GLIs, photonSECT$FinalTopos, photonSECT$dir]
KiraCreateConfigFiles[photonSECT$FinalTopos, photonSECT$GLIs, photonSECT$dir, 
 KiraMassDimensions -> {pp -> 2,mxt->1}]


KiraRunReduction[photonSECT$dir, photonSECT$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


photonSECT$ReductionTables=KiraImportResults[photonSECT$FinalTopos, photonSECT$dir]//Flatten;


photonSECT$resPreFinal1=Collect2[Total[photonSECT$AmpGLI/.Dispatch[photonSECT$ReductionTables]],GLI,
GaugeXi,D,DiracGamma,FCParallelize->True];


photonSECT$masters=Cases2[photonSECT$resPreFinal1,GLI];


photonSECT$MIMappings=FCLoopFindIntegralMappings[photonSECT$masters,Join[tadpoleMaster1[[2]],
photonSECT$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


(* ::Text:: *)
(*At this point we need to insert the 1-loop renormalization constants*)


knownResults1L = {
rc[delZA, 1] -> (-4*Nf)/(3*ep), 
rc[delZAmxt, 1] -> -2 Nf/ep, 
rc[delZxi, 1] -> (-4*Nf)/(3*ep), 
 rc[delZpsi, 1] -> -(GaugeXi[V[1]]/ep), 
 rc[delZe, 1] -> (2*Nf)/(3*ep)};


AbsoluteTiming[photonSECT$resPreFinal2=Collect2[photonSECT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,photonSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,1}]&//Normal//FCLoopAddMissingHigherOrdersWarning[#,ep,epHelp]&//FRH//
ReplaceAll[#,{Log[mxt^2]->2Log[mxt]}]&;]


AbsoluteTiming[photonSECT$resPreFinal3=Collect2[photonSECT$resPreFinal2,Join[{a4},List@@renConstants],IsolateNames->KK]//ReplaceAll[#,Zxi->ZA]&//
ReplaceAll[#,{(h:renConstants):>1+(a4 rc[ToExpression["del"<>ToString[h]],1]+a4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//Series[#,{a4,0,2}]&//Normal;]


photonSECT$resPreFinal4=Collect2[photonSECT$resPreFinal3//FRH,{rc,D,GLI},IsolateNames->KK]//FCReplaceD[#,{D->4-2ep}]&//
ReplaceRepeated[#,knownResults1L]&//FRH//Series[#,{ep,0,-1}]&//Normal//Collect2[#,ep,mxt,Factoring->Simplify,FCParallelize->True,Pair]&;


photonSECT$resFinal=Collect2[photonSECT$resPreFinal4,mxt,IsolateNames->KK]//Series[#,{mxt,0,2}]&//Normal//FRH//Collect2[#,ep,mxt,Pair,Factoring->Simplify,FCParallelize->True]&


(* ::Subsection:: *)
(*Determination of renormalization constants*)


diagPhotonTreeSECT$Amp=(Total[diagPhotonTreeSECT$RawAmp])//ReplaceAll[#,Zxi->ZA]&//ReplaceRepeated[#,{
	(h:renConstants):>1+(a4 rc[ToExpression["del"<>ToString[h]],1]+a4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//
	Series[#,{a4,0,2}]&//Normal//ReplaceRepeated[#,knownResults1L]&


photonSE$RenConstants2L=Collect2[Coefficient[photonSE$resFinal+photonSECT$resFinal+ diagPhotonTreeSECT$Amp,a4,2],
a4,mxt,DiracGamma,Factoring->Simplify]//FCMatchSolve[#,{ep,CF,DiracGamma,ml,mxt,SUNDelta,SUNTF,SUNFDelta,CA,GaugeXi,a4,Pair,pp,Nf,SUNN}]&//Collect2[#,ep]&


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final QED 2-loop wave-function renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ a4 rc[ToExpression["del"<>ToString[h]],1]+ a4^2 rc[ToExpression["del"<>ToString[h]],2])//
ReplaceAll[#,Join[SUNSimplify[knownResults1L,SUNNToCACF->False],photonSE$RenConstants2L]]&]]//SelectNotFree[#,ZA,ZAmxt]&;


finalResults//TableForm


knownResult = {rc[delZA, 2] -> (-2*Nf)/ep, rc[delZAmxt, 2] -> (Nf*GaugeXi[V[1]])/(2*ep) - (Nf*(2*Nf + GaugeXi[V[1]]))/ep^2};


FCCompareResults[photonSE$RenConstants2L,knownResult,
Text->{"\tCompare to Grozing, Lectures on QED and QCD, Eq 5.25 \
hep-ph/0508242:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



