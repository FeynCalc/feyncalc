(* ::Package:: *)

(* :Title: RenormalizationMasssless													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QED, MSbar, massless, 1-loop							*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*1-loop QED renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QED model created with FeynRules.*)


description="Renormalization, QED, MSbar, massless,1-loop";
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


modelDir=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","Examples","Models","QED"}];


FAPatch[PatchModelsOnly->True,FAModelsDirectory->modelDir];


(* ::Text:: *)
(*Here we define all Z-factors for renormalization constants present in the Lagrangian*)


renConstants=Zpsi|ZA|ZAmxt|Ze|Zxi


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[mu,"\[Mu]"];
FCAttachTypesettingRule[nu,"\[Nu]"];
FCAttachTypesettingRule[rho,"\[Rho]"];
FCAttachTypesettingRule[si,"\[Sigma]"];


diagLeptonSE=InsertFields[CreateTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[2,{1}]} -> {F[2,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


diagPhotonSE=InsertFields[CreateTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[1]} -> {V[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


diagLeptonPhotonVTX=InsertFields[CreateTopologies[1, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[2,{1}],V[1]}->{F[2,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


diagLeptonSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[2,{1}]} -> {F[2,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


diagPhotonSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[1]} -> {V[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


diagLeptonPhotonVTXCT=InsertFields[CreateCTTopologies[1, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[2,{1}],V[1]}->{F[2,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QED"}],
GenericModel -> FileNameJoin[{modelDir,"QED"}],ExcludeParticles->{F[2,{2|3}]}];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagLeptonSE, ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{2, 1}];
Paint[diagPhotonSE, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagLeptonPhotonVTX, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Counter-term diagrams*)


Paint[diagLeptonSECT, ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{2, 1}];
Paint[diagPhotonSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagLeptonPhotonVTXCT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Section:: *)
(*Master integrals*)


(* ::Text:: *)
(*The only required masters are 1-loop tadpoles*)
(**)


tadpoleMaster=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]];


tadpoleMaster1=tadpoleMaster/.m1->ml/.tad1LxFx1x1xxEp999x->"tad1Lv1";
tadpoleMaster2=tadpoleMaster/.m1->mxt/.tad1LxFx1x1xxEp999x->"tad1Lv2";


tadpoleMaster1


tadpoleMaster2


(* ::Section:: *)
(*Obtain the amplitudes*)


{leptonSE$RawAmp,photonSE$RawAmp,leptonSECT$RawAmp,photonSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_e"]->0,SMP["e"]->4 Pi Sqrt[a4]}]&/@{
	diagLeptonSE,diagPhotonSE,diagLeptonSECT,diagPhotonSECT};


{leptonPhotonVTX$RawAmp,leptonPhotonVTXCT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{q},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_e"]->0,SMP["e"]->4 Pi Sqrt[a4]}]&/@{
	diagLeptonPhotonVTX,diagLeptonPhotonVTXCT
	};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Text:: *)
(*Infrared rearrangement works both for massive and massless leptons. However, in both cases we get different renormalization constants for the "photon mass". This is why both calculations are needed if we want to reuse these results at higher loops orders.*)


(* ::Subsection:: *)
(*Lepton self-energy*)


(* ::Text:: *)
(*The 1-loop lepton self-energy has superficial degree of divergence equal to 1*)


FCClearScalarProducts[];
divDegree=1;
aux1=FCLoopGetFeynAmpDenominators[leptonSE$RawAmp,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


leptonSE$StrName=StringReplace[ToString[Hold[leptonSE$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[leptonSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


(* ::Text:: *)
(*flagCheck is a safety flag to ensure that higher order terms in p (higher than the divergence degree) do not  contribute to the poles*)


AbsoluteTiming[leptonSE$Amp1=Collect2[leptonSE$Amp,p,IsolateNames->KK];]
AbsoluteTiming[leptonSE$Amp2=FourSeries[leptonSE$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[leptonSE$Amp3=Collect2[FRH[leptonSE$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{leptonSE$Amp4,leptonSE$Topos}=FCLoopFindTopologies[leptonSE$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->leptonSEtopo];


AbsoluteTiming[leptonSE$Amp5=FCLoopTensorReduce[leptonSE$Amp4,leptonSE$Topos,FCParallelize->True];]


{leptonSE$Amp6,leptonSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[leptonSE$Amp5,leptonSE$Topos];


leptonSE$SubTopos=FCLoopFindSubtopologies[leptonSE$Topos2,Flatten->True,Remove->True]


{leptonSE$TopoMappings,leptonSE$FinalTopos}=FCLoopFindTopologyMappings[leptonSE$Topos2,PreferredTopologies->leptonSE$SubTopos];


leptonSE$AmpGLI=FCLoopApplyTopologyMappings[leptonSE$Amp6,{leptonSE$TopoMappings,leptonSE$FinalTopos},FCParallelize->True];


leptonSE$GLIs=Cases2[leptonSE$AmpGLI,GLI];


leptonSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>leptonSE$StrName<>"-1L-massless"}];
Quiet[CreateDirectory[leptonSE$dir]];


KiraCreateJobFile[leptonSE$FinalTopos, leptonSE$GLIs, leptonSE$dir]


KiraCreateIntegralFile[leptonSE$GLIs, leptonSE$FinalTopos, leptonSE$dir]
KiraCreateConfigFiles[leptonSE$FinalTopos, leptonSE$GLIs, leptonSE$dir, 
 KiraMassDimensions -> {pp -> 2,ml->1,mxt->1}]


KiraRunReduction[leptonSE$dir, leptonSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


leptonSE$ReductionTables=KiraImportResults[leptonSE$FinalTopos, leptonSE$dir]//Flatten;


leptonSE$resPreFinal=Collect2[Total[leptonSE$AmpGLI/.Dispatch[leptonSE$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


leptonSE$masters=Cases2[leptonSE$resPreFinal,GLI];


leptonSE$MIMappings=FCLoopFindIntegralMappings[leptonSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
leptonSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


leptonSE$resFinal=Collect2[leptonSE$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,leptonSE$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


leptonSE$RenConstants=(leptonSE$resFinal+Total[leptonSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha a4 rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//
	ReplaceAll[#,alpha->1]&//Collect2[#,DiracGamma]&//
	FCMatchSolve[#,{ep,CF,DiracGamma,ml,mxt,SUNDelta,SUNFDelta,GaugeXi,a4}]&


(* ::Subsection:: *)
(*Photon self-energy*)


(* ::Text:: *)
(*The 1-loop photon self-energy has superficial degree of divergence equal to 2. We also add the number of flavors by hand by multiplying the corresponding diagrams with Nf.*)


FCClearScalarProducts[];
photonSE$RawAmp2={Nf photonSE$RawAmp[[1]]};
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[photonSE$RawAmp2,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


photonSE$StrName=StringReplace[ToString[Hold[photonSE$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[photonSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


(* ::Text:: *)
(*flagCheck is a safety flag to ensure that higher order terms in p (higher than the divergence degree) do not  contribute to the poles*)


AbsoluteTiming[photonSE$Amp1=Collect2[photonSE$Amp,p,IsolateNames->KK];]
AbsoluteTiming[photonSE$Amp2=FourSeries[photonSE$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[photonSE$Amp3=Collect2[FRH[photonSE$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{photonSE$Amp4,photonSE$Topos}=FCLoopFindTopologies[photonSE$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->photonSEtopo];


AbsoluteTiming[photonSE$Amp5=FCLoopTensorReduce[photonSE$Amp4,photonSE$Topos,FCParallelize->True];]


{photonSE$Amp6,photonSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[photonSE$Amp5,photonSE$Topos];


photonSE$SubTopos=FCLoopFindSubtopologies[photonSE$Topos2,Flatten->True,Remove->True]


{photonSE$TopoMappings,photonSE$FinalTopos}=FCLoopFindTopologyMappings[photonSE$Topos2,PreferredTopologies->photonSE$SubTopos];


photonSE$AmpGLI=FCLoopApplyTopologyMappings[photonSE$Amp6,{photonSE$TopoMappings,photonSE$FinalTopos},FCParallelize->True];


photonSE$GLIs=Cases2[photonSE$AmpGLI,GLI];


photonSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>photonSE$StrName<>"-1L-massless"}];
Quiet[CreateDirectory[photonSE$dir]];


KiraCreateJobFile[photonSE$FinalTopos, photonSE$GLIs, photonSE$dir]


KiraCreateIntegralFile[photonSE$GLIs, photonSE$FinalTopos, photonSE$dir]
KiraCreateConfigFiles[photonSE$FinalTopos, photonSE$GLIs, photonSE$dir, 
 KiraMassDimensions -> {pp -> 2,ml->1,mxt->1}]


KiraRunReduction[photonSE$dir, photonSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


photonSE$ReductionTables=KiraImportResults[photonSE$FinalTopos, photonSE$dir]//Flatten;


photonSE$resPreFinal=Collect2[Total[photonSE$AmpGLI/.Dispatch[photonSE$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,D,DiracGamma,FCParallelize->True];


photonSE$masters=Cases2[photonSE$resPreFinal,GLI];


photonSE$MIMappings=FCLoopFindIntegralMappings[photonSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
photonSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


photonSE$resFinal=Collect2[photonSE$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,photonSE$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


photonSE$RenConstants=(photonSE$resFinal+Total[photonSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha a4 rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//
	ReplaceAll[#,alpha->1]&//Collect2[#,DiracGamma,pp,Pair,mxt]&//
	FCMatchSolve[#,{ep,CF,DiracGamma,ml,mxt,SUNDelta,SUNFDelta,CA,GaugeXi,a4,Pair,pp,Nf}]&


(* ::Subsection:: *)
(*Lepton-photon vertex*)


(* ::Text:: *)
(*The 1-loop lepton-photon-vertex has superficial degree of divergence equal to 0. We set q=0, so that p+p2=q yields p=-p2*)


FCClearScalarProducts[];
divDegree=0;
aux1=FCLoopGetFeynAmpDenominators[leptonPhotonVTX$RawAmp/.q->0/.p2->-p,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


leptonPhotonVTX$StrName=StringReplace[ToString[Hold[leptonPhotonVTX$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[leptonPhotonVTX$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[leptonPhotonVTX$Amp1=Collect2[leptonPhotonVTX$Amp,p,IsolateNames->KK];]
AbsoluteTiming[leptonPhotonVTX$Amp2=FourSeries[leptonPhotonVTX$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[leptonPhotonVTX$Amp3=Collect2[FRH[leptonPhotonVTX$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{leptonPhotonVTX$Amp4,leptonPhotonVTX$Topos}=FCLoopFindTopologies[leptonPhotonVTX$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->leptonPhotonVTXtopo];


AbsoluteTiming[leptonPhotonVTX$Amp5=FCLoopTensorReduce[leptonPhotonVTX$Amp4,leptonPhotonVTX$Topos,FCParallelize->True];]


{leptonPhotonVTX$Amp6,leptonPhotonVTX$Topos2}=FCLoopRewriteOverdeterminedTopologies[leptonPhotonVTX$Amp5,leptonPhotonVTX$Topos];


leptonPhotonVTX$SubTopos=FCLoopFindSubtopologies[leptonPhotonVTX$Topos2,Flatten->True,Remove->True]


{leptonPhotonVTX$TopoMappings,leptonPhotonVTX$FinalTopos}=FCLoopFindTopologyMappings[leptonPhotonVTX$Topos2,PreferredTopologies->leptonPhotonVTX$SubTopos];


leptonPhotonVTX$AmpGLI=FCLoopApplyTopologyMappings[leptonPhotonVTX$Amp6,{leptonPhotonVTX$TopoMappings,leptonPhotonVTX$FinalTopos},FCParallelize->True];


leptonPhotonVTX$GLIs=Cases2[leptonPhotonVTX$AmpGLI,GLI];


leptonPhotonVTX$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>leptonPhotonVTX$StrName<>"-1L-massless"}];
Quiet[CreateDirectory[leptonPhotonVTX$dir]];


KiraCreateJobFile[leptonPhotonVTX$FinalTopos, leptonPhotonVTX$GLIs, leptonPhotonVTX$dir]


KiraCreateIntegralFile[leptonPhotonVTX$GLIs, leptonPhotonVTX$FinalTopos, leptonPhotonVTX$dir]
KiraCreateConfigFiles[leptonPhotonVTX$FinalTopos, leptonPhotonVTX$GLIs, leptonPhotonVTX$dir, 
 KiraMassDimensions -> {pp -> 2,ml->1,mxt->1}]


KiraRunReduction[leptonPhotonVTX$dir, leptonPhotonVTX$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


leptonPhotonVTX$ReductionTables=KiraImportResults[leptonPhotonVTX$FinalTopos, leptonPhotonVTX$dir]//Flatten;


leptonPhotonVTX$resPreFinal=Collect2[Total[leptonPhotonVTX$AmpGLI/.Dispatch[leptonPhotonVTX$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,D,DiracGamma,FCParallelize->True];


leptonPhotonVTX$masters=Cases2[leptonPhotonVTX$resPreFinal,GLI];


leptonPhotonVTX$MIMappings=FCLoopFindIntegralMappings[leptonPhotonVTX$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
leptonPhotonVTX$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


leptonPhotonVTX$resFinal=Collect2[leptonPhotonVTX$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,leptonPhotonVTX$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


leptonPhotonVTX$resFinal+Total[leptonPhotonVTXCT$RawAmp]


leptonPhotonVTX$RenConstants=(leptonPhotonVTX$resFinal+Total[leptonPhotonVTXCT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha a4 rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,Join[photonSE$RenConstants,leptonSE$RenConstants]]&//
	ReplaceAll[#,alpha->1]&//Collect2[#,DiracGamma,pp,Pair,mxt]&//
	FCMatchSolve[#,{ep,CF,DiracGamma,ml,mxt,SUNDelta,SUNTF,SUNFDelta,CA,GaugeXi,a4,Pair,pp,Nf}]&


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final QED 1-loop renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ a4 rc[ToExpression["del"<>ToString[h]],1])//ReplaceAll[#,Join[photonSE$RenConstants,leptonSE$RenConstants,
leptonPhotonVTX$RenConstants]]&]]


knownResult = {
rc[delZA, 1] -> (-4*Nf)/(3*ep), 
rc[delZAmxt, 1] -> -2 Nf/ep, 
rc[delZxi, 1] -> (-4*Nf)/(3*ep), 
 rc[delZpsi, 1] -> -(GaugeXi[V[1]]/ep), 
 rc[delZe, 1] -> (2*Nf)/(3*ep)};


FCCompareResults[Join[photonSE$RenConstants,leptonSE$RenConstants,
leptonPhotonVTX$RenConstants]/.Rule->Equal,knownResult/.Rule->Equal,
Text->{"\tCheck the final result:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



