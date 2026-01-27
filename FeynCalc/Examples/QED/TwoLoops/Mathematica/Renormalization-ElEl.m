(* ::Package:: *)

(* :Title: Renormalization-LeAle										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QED, MSbar, Lepton self-energy, massive, 2-loop *)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*2-loop QED renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QED model created with FeynRules.*)


description="Renormalization, QED, MSbar, 2-loop";
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


diagLeptonSE=InsertFields[CreateTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[2,{1}]} -> {F[2,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


diagLeptonSECT=InsertFields[CreateCTTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[2,{1}]} -> {F[2,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


diagLeptonTreeSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[2,{1}]} -> {F[2,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


(* ::Text:: *)
(*Self-energy diagrams*)


Paint[diagLeptonSE, ColumnsXRows -> {3, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{3, 1}];


(* ::Text:: *)
(*1-loop counter-term diagrams*)


Paint[diagLeptonSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Tree-level counter-term diagrams*)


Paint[diagLeptonTreeSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Section:: *)
(*Master integrals*)


(* ::Text:: *)
(*The only required masters are 1- and 2-loop tadpoles*)


tadpoleMaster=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]];


tadpoleMaster1=tadpoleMaster/.m1->ml/.tad1LxFx1x1xxEp999x->"tad1Lv1";
tadpoleMaster2=tadpoleMaster/.m1->mxt/.tad1LxFx1x1xxEp999x->"tad1Lv2";


tadpoleMaster1


tadpoleMaster2


tadpoleMaster3=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxFx111x111xxEp1x.m"}]]/.m1->ml/.tad2LxFx111x111xxEp1x->"tad2Lv1";


tadpoleMaster4=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxFx111x111xxEp1x.m"}]]/.m1->mxt/.tad2LxFx111x111xxEp1x->"tad2Lv2";


tadpoleMaster5=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxAm2m1o4x111x122xxEp1x.m"}]]/.{m1->ml,m2->mxt}/.tad2LxAm2m1o4x111x122xxEp1x->"tad2Lv3";


tadpoleMaster6=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxAm2m1o4x111x112xxEp1x.m"}]]/.{m1->ml,m2->mxt}/.tad2LxAm2m1o4x111x112xxEp1x->"tad2Lv4";


(* ::Section:: *)
(*Obtain the amplitudes*)


{leptonSE$RawAmp,leptonSECT$RawAmp,diagLeptonTreeSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k1,k2}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_e"]->ml,SMP["e"]->4 Pi Sqrt[a4]}]&/@{
	diagLeptonSE,diagLeptonSECT,diagLeptonTreeSECT};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Lepton self-energy at 2 loops*)


(* ::Text:: *)
(*The 2-loop lepton self-energy has superficial degree of divergence equal to 1*)


FCClearScalarProducts[];
divDegree=1;
aux1=FCLoopGetFeynAmpDenominators[Join[leptonSE$RawAmp[[1;;1]],Nf leptonSE$RawAmp[[2;;2]],leptonSE$RawAmp[[3;;3]]],
{k1,k2},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1,k2},-mxt^2,0,Head->denHead]


AbsoluteTiming[leptonSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCI->True,FCParallelize->True]&//DiracSimplify[#,FCI->True,FCParallelize->True]&;]


isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]


AbsoluteTiming[leptonSE$Amp1=Collect2[leptonSE$Amp,p,IsolateNames->isoSymbols,FCParallelize->True];]


AbsoluteTiming[leptonSE$Amp2=FourSeries[leptonSE$Amp1,{p,0,1},FCParallelize->True];]


AbsoluteTiming[leptonSE$Amp3=Collect2[FRH2[leptonSE$Amp2,isoSymbols],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[]
SPD[p]=pp;


AbsoluteTiming[{leptonSE$Amp4,leptonSE$Topos}=FCLoopFindTopologies[leptonSE$Amp3,{k1,k2},FCI->True,FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp}];]


AbsoluteTiming[leptonSE$Amp5=FCLoopTensorReduce[leptonSE$Amp4,leptonSE$Topos,FCParallelize->True];]


AbsoluteTiming[leptonSE$Amp6=DiracSimplify[leptonSE$Amp5,FCParallelize->True];]


AbsoluteTiming[{leptonSE$Amp7,leptonSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[leptonSE$Amp6,leptonSE$Topos,FCParallelize->True];]


AbsoluteTiming[{leptonSE$Amp8,leptonSE$Topos3}=FCLoopRewriteIncompleteTopologies[leptonSE$Amp7,leptonSE$Topos2,FCParallelize->True];]


AbsoluteTiming[leptonSE$SubTopos=FCLoopFindSubtopologies[leptonSE$Topos3,Flatten->True,Remove->True,FCParallelize->True];]


{leptonSE$TopoMappings,
leptonSE$FinalTopos}=FCLoopFindTopologyMappings[leptonSE$Topos3,PreferredTopologies->leptonSE$SubTopos,FCParallelize->True];


AbsoluteTiming[leptonSE$AmpGLI=FCLoopApplyTopologyMappings[leptonSE$Amp8,{leptonSE$TopoMappings,
leptonSE$FinalTopos},FCParallelize->True];]


leptonSE$GLIs=Cases2[leptonSE$AmpGLI,GLI];


leptonSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-leptonSE-2L-massive"}];
Quiet[CreateDirectory[leptonSE$dir]];


KiraCreateJobFile[leptonSE$FinalTopos, leptonSE$GLIs, leptonSE$dir]


KiraCreateIntegralFile[leptonSE$GLIs, leptonSE$FinalTopos, leptonSE$dir]
KiraCreateConfigFiles[leptonSE$FinalTopos, leptonSE$GLIs, leptonSE$dir, 
 KiraMassDimensions -> {pp -> 2,ml->1,mxt->1}]


KiraRunReduction[leptonSE$dir, leptonSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


leptonSE$ReductionTables=KiraImportResults[leptonSE$FinalTopos, leptonSE$dir]//Flatten;


AbsoluteTiming[leptonSE$resPreFinal1=(leptonSE$AmpGLI/.Dispatch[leptonSE$ReductionTables]);]


AbsoluteTiming[leptonSE$resPreFinal2=Map[Collect2[#,GLI,DiracGamma,FCParallelize->True]&,leptonSE$resPreFinal1];]


leptonSE$masters=Cases2[leptonSE$resPreFinal1,GLI];


leptonSE$MIMappings=FCLoopFindIntegralMappings[leptonSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],{tadpoleMaster3[[2]]},{tadpoleMaster4[[2]]}
,{tadpoleMaster5[[2]]},{tadpoleMaster6[[2]]},leptonSE$FinalTopos],PreferredIntegrals->{tadpoleMaster2[[1]][[1]]tadpoleMaster2[[1]][[1]],
tadpoleMaster1[[1]][[1]]tadpoleMaster1[[1]][[1]],tadpoleMaster1[[1]][[1]]tadpoleMaster2[[1]][[1]],
tadpoleMaster3[[1]][[1]],
tadpoleMaster4[[1]][[1]],
tadpoleMaster5[[1]][[1]],
tadpoleMaster6[[1]][[1]]}]


isoSymbols1=FCMakeSymbols[LL,Range[1,$KernelCount],List];
isoSymbols2=FCMakeSymbols[LM,Range[1,$KernelCount],List];


AbsoluteTiming[leptonSE$resPreFinal2=Collect2[leptonSE$resPreFinal1,D,GLI,IsolateNames->isoSymbols1,FCParallelize->True]//FCReplaceD[#,D->4-2ep]&//ReplaceAll[#,leptonSE$MIMappings[[1]]]&//
ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]],tadpoleMaster3[[1]],tadpoleMaster4[[1]],tadpoleMaster5[[1]],tadpoleMaster6[[1]]}]&//Collect2[#,ep,IsolateNames->isoSymbols2,FCParallelize->True]&;]


AbsoluteTiming[leptonSE$resPreFinal3=leptonSE$resPreFinal2//Series[#,{ep,0,-1}]&//Normal//Series[(I*(4*Pi)^(-2 + ep))^2 #,{ep,0,-1}]&//Normal;]


AbsoluteTiming[leptonSE$resPreFinal4=Collect2[FRH2[FRH2[leptonSE$resPreFinal3,isoSymbols2],isoSymbols1],DiracGamma,pp,mxt,ep,FCParallelize->True];]


isoSymbols3=FCMakeSymbols[LH,Range[1,$KernelCount],List];


AbsoluteTiming[leptonSE$resPreFinal5=Series[Total[Collect2[leptonSE$resPreFinal4,mxt,IsolateNames->isoSymbols3,FCParallelize->True]],{mxt,0,0}]//Normal;]


AbsoluteTiming[leptonSE$resPreFinal6=Collect2[FRH2[leptonSE$resPreFinal5,isoSymbols3]//ReplaceAll[#,Log[m_Symbol^2]:>2Log[m]]&,DiracGamma,pp,mxt,ep,FCParallelize->True];]


leptonSE$resFinal=Collect2[Collect2[leptonSE$resPreFinal6,ep,CA,CF,ml,Nf,SUNFDelta,a4,DiracGamma,GaugeXi,Factoring->FullSimplify],
ep,ml,mxt]


(* ::Subsection:: *)
(*Lepton self-energy 1-loop CT*)


FCClearScalarProducts[];
divDegree=1;
aux1=FCLoopGetFeynAmpDenominators[leptonSECT$RawAmp,{k1},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1},-mxt^2,0,Head->denHead]


leptonSECT$StrName=StringReplace[ToString[Hold[leptonSECT$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[leptonSECT$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[leptonSECT$Amp1=Collect2[leptonSECT$Amp,p,IsolateNames->KK];]
AbsoluteTiming[leptonSECT$Amp2=FourSeries[leptonSECT$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[leptonSECT$Amp3=Collect2[FRH[leptonSECT$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{leptonSECT$Amp4,leptonSECT$Topos}=FCLoopFindTopologies[leptonSECT$Amp3,{k1},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->leptonSEtopo];


AbsoluteTiming[leptonSECT$Amp5=FCLoopTensorReduce[leptonSECT$Amp4,leptonSECT$Topos,FCParallelize->True];]


AbsoluteTiming[leptonSECT$Amp6=DiracSimplify[leptonSECT$Amp5,FCParallelize->True];]


{leptonSECT$Amp7,leptonSECT$Topos2}=FCLoopRewriteOverdeterminedTopologies[leptonSECT$Amp6,leptonSECT$Topos,FCParallelize->True];


{leptonSECT$Amp8,leptonSECT$Topos3}=FCLoopRewriteIncompleteTopologies[leptonSECT$Amp7,leptonSECT$Topos2,FCParallelize->True];


AbsoluteTiming[leptonSECT$SubTopos=FCLoopFindSubtopologies[leptonSECT$Topos2,Flatten->True,Remove->True,FCParallelize->True];]


AbsoluteTiming[{leptonSECT$TopoMappings,leptonSECT$FinalTopos}=FCLoopFindTopologyMappings[leptonSECT$Topos2,PreferredTopologies->leptonSECT$SubTopos,FCParallelize->True];]


AbsoluteTiming[leptonSECT$AmpGLI=FCLoopApplyTopologyMappings[leptonSECT$Amp8,{leptonSECT$TopoMappings,leptonSECT$FinalTopos},FCParallelize->True];]


leptonSECT$GLIs=Cases2[leptonSECT$AmpGLI,GLI];


leptonSECT$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>leptonSECT$StrName<>"-1L-massive"}];
Quiet[CreateDirectory[leptonSECT$dir]];


KiraCreateJobFile[leptonSECT$FinalTopos, leptonSECT$GLIs, leptonSECT$dir]


KiraCreateIntegralFile[leptonSECT$GLIs, leptonSECT$FinalTopos, leptonSECT$dir]
KiraCreateConfigFiles[leptonSECT$FinalTopos, leptonSECT$GLIs, leptonSECT$dir, 
 KiraMassDimensions -> {pp -> 2,ml->1,mxt->1}]


KiraRunReduction[leptonSECT$dir, leptonSECT$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


leptonSECT$ReductionTables=KiraImportResults[leptonSECT$FinalTopos, leptonSECT$dir]//Flatten;


leptonSECT$resPreFinal1=Collect2[Total[leptonSECT$AmpGLI/.Dispatch[leptonSECT$ReductionTables]],GLI,
GaugeXi,D,DiracGamma,FCParallelize->True];


leptonSECT$masters=Cases2[leptonSECT$resPreFinal1,GLI];


leptonSECT$MIMappings=FCLoopFindIntegralMappings[leptonSECT$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
leptonSECT$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


(* ::Text:: *)
(*At this point we need to insert the 1-loop renormalization constants*)


knownResults1L = {
rc[delZAmxt, 1] -> 0, 
rc[delZA, 1] -> (-4*Nf)/(3*ep), 
rc[delZxi, 1] -> (-4*Nf)/(3*ep), rc[delZm, 1] -> -3/ep, 
 rc[delZpsi, 1] -> -(GaugeXi[V[1]]/ep), 
 rc[delZe, 1] -> (2*Nf)/(3*ep)};


AbsoluteTiming[leptonSECT$resPreFinal2=Collect2[leptonSECT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,leptonSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,1}]&//Normal//FCLoopAddMissingHigherOrdersWarning[#,ep,epHelp]&//FRH//
ReplaceAll[#,{Log[mxt^2]->2Log[mxt]}]&;]


AbsoluteTiming[leptonSECT$resPreFinal2=Collect2[leptonSECT$resPreFinal1,Join[{a4},List@@renConstants],IsolateNames->KK]//ReplaceAll[#,Zxi->ZA]&//ReplaceAll[#,{
	(h:renConstants):>1+(a4 rc[ToExpression["del"<>ToString[h]],1]+a4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//Series[#,{a4,0,2}]&//Normal;]


AbsoluteTiming[leptonSECT$resPreFinal3=Collect2[leptonSECT$resPreFinal2//FRH,{rc,D,GLI},IsolateNames->KK]//FCReplaceD[#,{D->4-2ep}]&//ReplaceRepeated[#,knownResults1L]&//
ReplaceAll[#,leptonSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Abort[],#]&//Collect2[#,ep,IsolateNames->KK]&;]


leptonSECT$resFinal=leptonSECT$resPreFinal3//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//
Collect2[#,mxt,IsolateNames->KK]&//Series[#,{mxt,0,0}]&//Normal//FRH//ReplaceAll[#,Log[m_^2]:>2Log[m]]&//Collect2[#,ep,ml,mxt]&


(* ::Subsection:: *)
(*Determination of renormalization constants*)


diagLeptonTreeSECT$Amp=(Total[diagLeptonTreeSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+(a4 rc[ToExpression["del"<>ToString[h]],1]+a4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//
	Series[#,{a4,0,2}]&//Normal//ReplaceRepeated[#,knownResults1L]&


Collect2[Coefficient[SUNSimplify[leptonSE$resFinal+leptonSECT$resFinal+diagLeptonTreeSECT$Amp,SUNNToCACF->False],a4,2],a4,mxt,DiracGamma,Factoring->FullSimplify]


leptonSE$RenConstants2L=Collect2[Coefficient[SUNSimplify[leptonSE$resFinal+leptonSECT$resFinal+diagLeptonTreeSECT$Amp,SUNNToCACF->False],a4,2],a4,mxt,DiracGamma,Factoring->FullSimplify]//
	FCMatchSolve[#,{ep,CF,DiracGamma,ml,mxt,SUNDelta,SUNTF,SUNFDelta,CA,GaugeXi,a4,Pair,pp,Nf,SUNN}]&//Collect2[#,ep]&


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final QED 2-loop wave-function renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ a4 rc[ToExpression["del"<>ToString[h]],1]+ a4^2 rc[ToExpression["del"<>ToString[h]],2])//
ReplaceAll[#,Join[SUNSimplify[knownResults1L,SUNNToCACF->False],leptonSE$RenConstants2L]]&]]//SelectNotFree[#,Zpsi,Zm]&;


finalResults//TableForm


knownResult = {rc[delZm, 2] -> -1/2*(-9 + 4*Nf)/ep^2 + (-9 + 20*Nf)/(12*ep), rc[delZpsi, 2] -> (3 + 4*Nf)/(4*ep) + GaugeXi[V[1]]^2/(2*ep^2)}


FCCompareResults[leptonSE$RenConstants2L,knownResult,
Text->{"\tCompare to Grozing, Lectures on QED and QCD, Eq 5.52 \
hep-ph/0508242:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



