(* ::Package:: *)

(* :Title: Renormalization													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QCD, MSbar, 1-loop							*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*1-loop QCD renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QCD model created with FeynRules.*)


description="Renormalization, QCD, MSbar, 1-loop";
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


modelDir=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","Examples","Models","QCD"}];


FAPatch[PatchModelsOnly->True,FAModelsDirectory->modelDir];


(* ::Text:: *)
(*Here we define all Z-factors for renormalization constants present in the Lagrangian*)


renConstants=Zm|Zpsi|ZA|ZAmxt|Zu|Zumxt|Zg|Zxi


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[mu,"\[Mu]"];
FCAttachTypesettingRule[nu,"\[Nu]"];
FCAttachTypesettingRule[rho,"\[Rho]"];
FCAttachTypesettingRule[si,"\[Sigma]"];


diagQuarkSE=InsertFields[CreateTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}]} -> {F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagGluonSE=InsertFields[CreateTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[5]} -> {V[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagGhostSE=InsertFields[CreateTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {U[5]} -> {U[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagquarkGluonVTX=InsertFields[CreateTopologies[1, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}],V[5]}->{F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagQuarkSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}]} -> {F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagGluonSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {V[5]} -> {V[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagGhostSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {U[5]} -> {U[5]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagquarkGluonVTXCT=InsertFields[CreateCTTopologies[1, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}],V[5]}->{F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagQuarkSE, ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{2, 1}];
Paint[diagGluonSE, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagGhostSE, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagquarkGluonVTX, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Counter-term diagrams*)


Paint[diagQuarkSECT, ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{2, 1}];
Paint[diagGluonSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagGhostSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagquarkGluonVTXCT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Section:: *)
(*Master integrals*)


(* ::Text:: *)
(*The only required masters are 1-loop tadpoles*)
(**)


tadpoleMaster=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]];


tadpoleMaster1=tadpoleMaster/.m1->mq/.tad1LxFx1x1xxEp999x->"tad1Lv1";
tadpoleMaster2=tadpoleMaster/.m1->mxt/.tad1LxFx1x1xxEp999x->"tad1Lv2";


tadpoleMaster1


tadpoleMaster2


(* ::Section:: *)
(*Obtain the amplitudes*)


{quarkSE$RawAmp,gluonSE$RawAmp,ghostSE$RawAmp,quarkSECT$RawAmp,gluonSECT$RawAmp,ghostSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->mq,SMP["g_s"]->4 Pi Sqrt[as4]}]&/@{
	diagQuarkSE,diagGluonSE,diagGhostSE,diagQuarkSECT,diagGluonSECT,diagGhostSECT};


{quarkGluonVTX$RawAmp,quarkGluonVTXCT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{q},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->mq,SMP["g_s"]->4 Pi Sqrt[as4]}]&/@{
	diagquarkGluonVTX,diagquarkGluonVTXCT
	};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Text:: *)
(*Infrared rearrangement works both for massive and massless quarks. However, in both cases we get different renormalization constants for the "gluon mass". This is why both calculations are needed if we want to reuse these results at higher loops orders.*)


(* ::Subsection:: *)
(*Quark self-energy*)


(* ::Text:: *)
(*The 1-loop quark self-energy has superficial degree of divergence equal to 1*)


FCClearScalarProducts[];
divDegree=1;
aux1=FCLoopGetFeynAmpDenominators[quarkSE$RawAmp,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


quarkSE$StrName=StringReplace[ToString[Hold[quarkSE$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[quarkSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


(* ::Text:: *)
(*flagCheck is a safety flag to ensure that higher order terms in p (higher than the divergence degree) do not  contribute to the poles*)


AbsoluteTiming[quarkSE$Amp1=Collect2[quarkSE$Amp,p,IsolateNames->KK];]
AbsoluteTiming[quarkSE$Amp2=FourSeries[quarkSE$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[quarkSE$Amp3=Collect2[FRH[quarkSE$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{quarkSE$Amp4,quarkSE$Topos}=FCLoopFindTopologies[quarkSE$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->quarkSEtopo];


AbsoluteTiming[quarkSE$Amp5=FCLoopTensorReduce[quarkSE$Amp4,quarkSE$Topos,FCParallelize->True];]


{quarkSE$Amp6,quarkSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[quarkSE$Amp5,quarkSE$Topos];


quarkSE$SubTopos=FCLoopFindSubtopologies[quarkSE$Topos2,Flatten->True,Remove->True]


{quarkSE$TopoMappings,quarkSE$FinalTopos}=FCLoopFindTopologyMappings[quarkSE$Topos2,PreferredTopologies->quarkSE$SubTopos];


quarkSE$AmpGLI=FCLoopApplyTopologyMappings[quarkSE$Amp6,{quarkSE$TopoMappings,quarkSE$FinalTopos},FCParallelize->True];


quarkSE$GLIs=Cases2[quarkSE$AmpGLI,GLI];


quarkSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>quarkSE$StrName<>"-1L"}];
Quiet[CreateDirectory[quarkSE$dir]];


KiraCreateJobFile[quarkSE$FinalTopos, quarkSE$GLIs, quarkSE$dir]


KiraCreateIntegralFile[quarkSE$GLIs, quarkSE$FinalTopos, quarkSE$dir]
KiraCreateConfigFiles[quarkSE$FinalTopos, quarkSE$GLIs, quarkSE$dir, 
 KiraMassDimensions -> {pp -> 2,mq->1,mxt->1}]


KiraRunReduction[quarkSE$dir, quarkSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


quarkSE$ReductionTables=KiraImportResults[quarkSE$FinalTopos, quarkSE$dir]//Flatten;


quarkSE$resPreFinal=Collect2[Total[quarkSE$AmpGLI/.Dispatch[quarkSE$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


quarkSE$masters=Cases2[quarkSE$resPreFinal,GLI];


quarkSE$MIMappings=FCLoopFindIntegralMappings[quarkSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
quarkSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


quarkSE$resFinal=Collect2[quarkSE$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,quarkSE$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


quarkSE$RenConstants=(quarkSE$resFinal+Total[quarkSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha as4 rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//
	ReplaceAll[#,alpha->1]&//Collect2[#,DiracGamma]&//
	FCMatchSolve[#,{ep,CF,DiracGamma,mq,mxt,SUNDelta,SUNFDelta,GaugeXi,as4}]&


(* ::Subsection:: *)
(*Gluon self-energy*)


(* ::Text:: *)
(*The 1-loop gluon self-energy has superficial degree of divergence equal to 2. We also add the number of flavors by hand by multiplying the corresponding diagrams with Nf.*)


FCClearScalarProducts[];
gluonSE$RawAmp2={gluonSE$RawAmp[[1]],Nf gluonSE$RawAmp[[2]],gluonSE$RawAmp[[3]],gluonSE$RawAmp[[4]]};
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[gluonSE$RawAmp2,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


gluonSE$StrName=StringReplace[ToString[Hold[gluonSE$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[gluonSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


(* ::Text:: *)
(*flagCheck is a safety flag to ensure that higher order terms in p (higher than the divergence degree) do not  contribute to the poles*)


AbsoluteTiming[gluonSE$Amp1=Collect2[gluonSE$Amp,p,IsolateNames->KK];]
AbsoluteTiming[gluonSE$Amp2=FourSeries[gluonSE$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[gluonSE$Amp3=Collect2[FRH[gluonSE$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{gluonSE$Amp4,gluonSE$Topos}=FCLoopFindTopologies[gluonSE$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->gluonSEtopo];


AbsoluteTiming[gluonSE$Amp5=FCLoopTensorReduce[gluonSE$Amp4,gluonSE$Topos,FCParallelize->True];]


{gluonSE$Amp6,gluonSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[gluonSE$Amp5,gluonSE$Topos];


gluonSE$SubTopos=FCLoopFindSubtopologies[gluonSE$Topos2,Flatten->True,Remove->True]


{gluonSE$TopoMappings,gluonSE$FinalTopos}=FCLoopFindTopologyMappings[gluonSE$Topos2,PreferredTopologies->gluonSE$SubTopos];


gluonSE$AmpGLI=FCLoopApplyTopologyMappings[gluonSE$Amp6,{gluonSE$TopoMappings,gluonSE$FinalTopos},FCParallelize->True];


gluonSE$GLIs=Cases2[gluonSE$AmpGLI,GLI];


gluonSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>gluonSE$StrName<>"-1L"}];
Quiet[CreateDirectory[gluonSE$dir]];


KiraCreateJobFile[gluonSE$FinalTopos, gluonSE$GLIs, gluonSE$dir]


KiraCreateIntegralFile[gluonSE$GLIs, gluonSE$FinalTopos, gluonSE$dir]
KiraCreateConfigFiles[gluonSE$FinalTopos, gluonSE$GLIs, gluonSE$dir, 
 KiraMassDimensions -> {pp -> 2,mq->1,mxt->1}]


KiraRunReduction[gluonSE$dir, gluonSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


gluonSE$ReductionTables=KiraImportResults[gluonSE$FinalTopos, gluonSE$dir]//Flatten;


gluonSE$resPreFinal=Collect2[Total[gluonSE$AmpGLI/.Dispatch[gluonSE$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


gluonSE$masters=Cases2[gluonSE$resPreFinal,GLI];


gluonSE$MIMappings=FCLoopFindIntegralMappings[gluonSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
gluonSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


gluonSE$resFinal=Collect2[gluonSE$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,gluonSE$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


gluonSE$RenConstants=(gluonSE$resFinal+Total[gluonSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha as4 rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//
	ReplaceAll[#,alpha->1]&//Collect2[#,DiracGamma,pp,Pair,mxt]&//
	FCMatchSolve[#,{ep,CF,DiracGamma,mq,mxt,SUNDelta,SUNFDelta,CA,GaugeXi,as4,Pair,pp,Nf}]&


(* ::Subsection:: *)
(*Ghost self-energy*)


(* ::Text:: *)
(*The 1-loop ghost self-energy has superficial degree of divergence equal to 2*)


FCClearScalarProducts[];
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[ghostSE$RawAmp,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


ghostSE$StrName=StringReplace[ToString[Hold[ghostSE$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[ghostSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[ghostSE$Amp1=Collect2[ghostSE$Amp,p,IsolateNames->KK];]
AbsoluteTiming[ghostSE$Amp2=FourSeries[ghostSE$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[ghostSE$Amp3=Collect2[FRH[ghostSE$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{ghostSE$Amp4,ghostSE$Topos}=FCLoopFindTopologies[ghostSE$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->ghostSEtopo];


AbsoluteTiming[ghostSE$Amp5=FCLoopTensorReduce[ghostSE$Amp4,ghostSE$Topos,FCParallelize->True];]


{ghostSE$Amp6,ghostSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[ghostSE$Amp5,ghostSE$Topos];


ghostSE$SubTopos=FCLoopFindSubtopologies[ghostSE$Topos2,Flatten->True,Remove->True]


{ghostSE$TopoMappings,ghostSE$FinalTopos}=FCLoopFindTopologyMappings[ghostSE$Topos2,PreferredTopologies->ghostSE$SubTopos];


ghostSE$AmpGLI=FCLoopApplyTopologyMappings[ghostSE$Amp6,{ghostSE$TopoMappings,ghostSE$FinalTopos},FCParallelize->True];


ghostSE$GLIs=Cases2[ghostSE$AmpGLI,GLI];


ghostSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>ghostSE$StrName<>"-1L"}];
Quiet[CreateDirectory[ghostSE$dir]];


KiraCreateJobFile[ghostSE$FinalTopos, ghostSE$GLIs, ghostSE$dir]


KiraCreateIntegralFile[ghostSE$GLIs, ghostSE$FinalTopos, ghostSE$dir]
KiraCreateConfigFiles[ghostSE$FinalTopos, ghostSE$GLIs, ghostSE$dir, 
 KiraMassDimensions -> {pp -> 2,mq->1,mxt->1}]


KiraRunReduction[ghostSE$dir, ghostSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


ghostSE$ReductionTables=KiraImportResults[ghostSE$FinalTopos, ghostSE$dir]//Flatten;


ghostSE$resPreFinal=Collect2[Total[ghostSE$AmpGLI/.Dispatch[ghostSE$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


ghostSE$masters=Cases2[ghostSE$resPreFinal,GLI];


ghostSE$MIMappings=FCLoopFindIntegralMappings[ghostSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
ghostSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


ghostSE$resFinal=Collect2[ghostSE$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,ghostSE$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


(* ::Text:: *)
(*It is not surprising that the ghost mass renormalization constant is zero, as the tree-level counter-term is not proportional to p^2*)


ghostSE$RenConstants=(ghostSE$resFinal+Total[ghostSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha as4 rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//
	ReplaceAll[#,alpha->1]&//Collect2[#,DiracGamma,pp,Pair,mxt]&//
	FCMatchSolve[#,{ep,CF,DiracGamma,mq,mxt,SUNDelta,SUNFDelta,CA,GaugeXi,as4,Pair,pp,Nf}]&


(* ::Subsection:: *)
(*Quark-gluon vertex*)


(* ::Text:: *)
(*The 1-loop quark-gluon-vertex has superficial degree of divergence equal to 0. We set q=0, so that p+p2=q yields p=-p2*)


FCClearScalarProducts[];
divDegree=0;
aux1=FCLoopGetFeynAmpDenominators[quarkGluonVTX$RawAmp/.q->0/.p2->-p,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


quarkGluonVTX$StrName=StringReplace[ToString[Hold[quarkGluonVTX$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[quarkGluonVTX$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[quarkGluonVTX$Amp1=Collect2[quarkGluonVTX$Amp,p,IsolateNames->KK];]
AbsoluteTiming[quarkGluonVTX$Amp2=FourSeries[quarkGluonVTX$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[quarkGluonVTX$Amp3=Collect2[FRH[quarkGluonVTX$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{quarkGluonVTX$Amp4,quarkGluonVTX$Topos}=FCLoopFindTopologies[quarkGluonVTX$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->quarkGluonVTXtopo];


AbsoluteTiming[quarkGluonVTX$Amp5=FCLoopTensorReduce[quarkGluonVTX$Amp4,quarkGluonVTX$Topos,FCParallelize->True];]


{quarkGluonVTX$Amp6,quarkGluonVTX$Topos2}=FCLoopRewriteOverdeterminedTopologies[quarkGluonVTX$Amp5,quarkGluonVTX$Topos];


quarkGluonVTX$SubTopos=FCLoopFindSubtopologies[quarkGluonVTX$Topos2,Flatten->True,Remove->True]


{quarkGluonVTX$TopoMappings,quarkGluonVTX$FinalTopos}=FCLoopFindTopologyMappings[quarkGluonVTX$Topos2,PreferredTopologies->quarkGluonVTX$SubTopos];


quarkGluonVTX$AmpGLI=FCLoopApplyTopologyMappings[quarkGluonVTX$Amp6,{quarkGluonVTX$TopoMappings,quarkGluonVTX$FinalTopos},FCParallelize->True];


quarkGluonVTX$GLIs=Cases2[quarkGluonVTX$AmpGLI,GLI];


quarkGluonVTX$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>quarkGluonVTX$StrName<>"-1L"}];
Quiet[CreateDirectory[quarkGluonVTX$dir]];


KiraCreateJobFile[quarkGluonVTX$FinalTopos, quarkGluonVTX$GLIs, quarkGluonVTX$dir]


KiraCreateIntegralFile[quarkGluonVTX$GLIs, quarkGluonVTX$FinalTopos, quarkGluonVTX$dir]
KiraCreateConfigFiles[quarkGluonVTX$FinalTopos, quarkGluonVTX$GLIs, quarkGluonVTX$dir, 
 KiraMassDimensions -> {pp -> 2,mq->1,mxt->1}]


KiraRunReduction[quarkGluonVTX$dir, quarkGluonVTX$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


quarkGluonVTX$ReductionTables=KiraImportResults[quarkGluonVTX$FinalTopos, quarkGluonVTX$dir]//Flatten;


quarkGluonVTX$resPreFinal=Collect2[Total[quarkGluonVTX$AmpGLI/.Dispatch[quarkGluonVTX$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


quarkGluonVTX$masters=Cases2[quarkGluonVTX$resPreFinal,GLI];


quarkGluonVTX$MIMappings=FCLoopFindIntegralMappings[quarkGluonVTX$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
quarkGluonVTX$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


quarkGluonVTX$resFinal=Collect2[quarkGluonVTX$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,quarkGluonVTX$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


quarkGluonVTX$RenConstants=(quarkGluonVTX$resFinal+Total[quarkGluonVTXCT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha as4 rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,Join[gluonSE$RenConstants,quarkSE$RenConstants]]&//
	ReplaceAll[#,alpha->1]&//Collect2[#,DiracGamma,pp,Pair,mxt]&//
	FCMatchSolve[#,{ep,CF,DiracGamma,mq,mxt,SUNDelta,SUNTF,SUNFDelta,CA,GaugeXi,as4,Pair,pp,Nf}]&


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final QCD 1-loop renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ as4 rc[ToExpression["del"<>ToString[h]],1])//ReplaceAll[#,Join[gluonSE$RenConstants,quarkSE$RenConstants,
ghostSE$RenConstants,quarkGluonVTX$RenConstants]]&]]


knownResult = {
rc[delZA, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
 rc[delZAmxt, 1] -> - (CA*(1 + 3*GaugeXi["G"]))/(8*ep), 
 rc[delZxi, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
 rc[delZm, 1] -> - (3*CF)/ep, 
 rc[delZpsi, 1] -> -((CF*GaugeXi["G"])/ep), 
 rc[delZumxt, 1] -> 0, 
 rc[delZu, 1] -> (CA*(3 - GaugeXi["G"]))/(4*ep), 
 rc[delZg, 1] -> -1/6*(11*CA - 2*Nf)/ep};


FCCompareResults[Join[gluonSE$RenConstants,quarkSE$RenConstants,
ghostSE$RenConstants,quarkGluonVTX$RenConstants]/.Rule->Equal,knownResult/.Rule->Equal,
Text->{"\tCompare to Muta, Foundations of QCD, Eqs 2.5.131-2.5.147:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];




