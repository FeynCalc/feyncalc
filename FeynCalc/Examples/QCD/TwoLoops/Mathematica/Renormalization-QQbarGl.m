(* ::Package:: *)

(* :Title: Renormalization-QQbarGl										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QCD, MSbar, Quark-gluon vertex, massless, 2-loop *)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*2-loop QCD renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QCD model created with FeynRules.*)


description="Renormalization, QCD, MSbar, Quark-gluon vertex, massless, 2-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
LaunchKernels[$ProcessorCount];
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


diagQuarkGluonVTX=InsertFields[CreateTopologies[2, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}],V[5]}->{F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagQuarkGluonVTXCT=InsertFields[CreateCTTopologies[2, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}],V[5]}->{F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagQuarkGluonTreeVTXCT=InsertFields[CreateCTTopologies[1, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}],V[5]}->{F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


(* ::Text:: *)
(*2-loop diagrams*)


Paint[diagQuarkGluonVTX, ColumnsXRows -> {6, 6},SheetHeader->None,
Numbering -> Simple, ImageSize->128{6, 6}];


(* ::Text:: *)
(*1-loop counter-term diagrams*)


Paint[diagQuarkGluonVTXCT, ColumnsXRows -> {6, 2},SheetHeader->None,
Numbering -> Simple, ImageSize->128{6, 2}];


(* ::Text:: *)
(*Tree-level counter-term diagrams*)


Paint[diagQuarkGluonTreeVTXCT, ColumnsXRows -> {4, 1},SheetHeader->None,
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


{quarkGluonVTX$RawAmp,quarkGluonVTXCT$RawAmp,diagQuarkGluonTreeSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{q},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k1,k2}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->0,SMP["g_s"]->4 Pi Sqrt[as4]}]&/@{
	diagQuarkGluonVTX,diagQuarkGluonVTXCT,diagQuarkGluonTreeVTXCT};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Quark-gluon vertex at 2 loops*)


(* ::Text:: *)
(*The 2-loop Quark-gluon vertex has superficial degree of divergence equal to 0. We set q=0, so that p+p2=q yields p=-p2*)


FCClearScalarProducts[];
divDegree=0;
quarkGluonVTX$RawAmp2=Join[quarkGluonVTX$RawAmp[[1;;9]],Nf quarkGluonVTX$RawAmp[[10;;11]],
quarkGluonVTX$RawAmp[[12;;24]],Nf quarkGluonVTX$RawAmp[[25;;25]],
quarkGluonVTX$RawAmp[[26;;29]],Nf quarkGluonVTX$RawAmp[[30;;30]],quarkGluonVTX$RawAmp[[31;;33]],Nf quarkGluonVTX$RawAmp[[34;;34]],quarkGluonVTX$RawAmp[[35;;36]]];
aux1=FCLoopGetFeynAmpDenominators[quarkGluonVTX$RawAmp2/.q->0/.p2->-p/.p1->p,
{k1,k2},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1,k2},-mxt^2,0,Head->denHead]


AbsoluteTiming[quarkGluonVTX$PreAmp1=Contract[(aux1[[1]]/.aux2),FCParallelize->True];]


AbsoluteTiming[quarkGluonVTX$PreAmp2=SUNSimplify[quarkGluonVTX$PreAmp1,FCParallelize->True];]


AbsoluteTiming[quarkGluonVTX$PreAmp3=DiracSimplify[quarkGluonVTX$PreAmp2,FCParallelize->True];]


quarkGluonVTX$Amp=quarkGluonVTX$PreAmp3;


isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]


AbsoluteTiming[quarkGluonVTX$Amp1=Collect2[quarkGluonVTX$Amp,p,IsolateNames->isoSymbols,FCParallelize->True];]


AbsoluteTiming[quarkGluonVTX$Amp2=FourSeries[quarkGluonVTX$Amp1,{p,0,divDegree},FCParallelize->True];]


AbsoluteTiming[quarkGluonVTX$Amp3=Collect2[FRH2[quarkGluonVTX$Amp2,isoSymbols],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[]
SPD[p]=pp;


AbsoluteTiming[{quarkGluonVTX$Amp4,quarkGluonVTX$Topos}=FCLoopFindTopologies[quarkGluonVTX$Amp3,{k1,k2},FCI->True,FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp}];]


AbsoluteTiming[quarkGluonVTX$Amp5=FCLoopTensorReduce[quarkGluonVTX$Amp4,quarkGluonVTX$Topos,FCParallelize->True];]


AbsoluteTiming[quarkGluonVTX$Amp6=DiracSimplify[quarkGluonVTX$Amp5,FCParallelize->True];]


AbsoluteTiming[{quarkGluonVTX$Amp7,quarkGluonVTX$Topos2}=FCLoopRewriteOverdeterminedTopologies[quarkGluonVTX$Amp6,quarkGluonVTX$Topos,FCParallelize->True];]


AbsoluteTiming[{quarkGluonVTX$Amp8,quarkGluonVTX$Topos3}=FCLoopRewriteIncompleteTopologies[quarkGluonVTX$Amp7,quarkGluonVTX$Topos2,FCParallelize->True];]


AbsoluteTiming[quarkGluonVTX$SubTopos=FCLoopFindSubtopologies[quarkGluonVTX$Topos3,Flatten->True,Remove->True,FCParallelize->True];]


{quarkGluonVTX$TopoMappings,
quarkGluonVTX$FinalTopos}=FCLoopFindTopologyMappings[quarkGluonVTX$Topos3,PreferredTopologies->quarkGluonVTX$SubTopos,FCParallelize->True];


AbsoluteTiming[quarkGluonVTX$AmpGLI=FCLoopApplyTopologyMappings[quarkGluonVTX$Amp8,{quarkGluonVTX$TopoMappings,
quarkGluonVTX$FinalTopos},FCParallelize->True];]


quarkGluonVTX$GLIs=Cases2[quarkGluonVTX$AmpGLI,GLI];


quarkGluonVTX$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-quarkGluonVTX-2L-massless"}];
Quiet[CreateDirectory[quarkGluonVTX$dir]];


KiraCreateJobFile[quarkGluonVTX$FinalTopos, quarkGluonVTX$GLIs, quarkGluonVTX$dir]


KiraCreateIntegralFile[quarkGluonVTX$GLIs, quarkGluonVTX$FinalTopos, quarkGluonVTX$dir]
KiraCreateConfigFiles[quarkGluonVTX$FinalTopos, quarkGluonVTX$GLIs, quarkGluonVTX$dir, 
 KiraMassDimensions -> {pp -> 2,mxt->1}]


KiraRunReduction[quarkGluonVTX$dir, quarkGluonVTX$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


quarkGluonVTX$ReductionTables=KiraImportResults[quarkGluonVTX$FinalTopos, quarkGluonVTX$dir]//Flatten;


AbsoluteTiming[quarkGluonVTX$resPreFinal1=(quarkGluonVTX$AmpGLI/.Dispatch[quarkGluonVTX$ReductionTables]);]


AbsoluteTiming[quarkGluonVTX$resPreFinal2=Map[Collect2[#,GLI,DiracGamma,FCParallelize->True]&,quarkGluonVTX$resPreFinal1];]


quarkGluonVTX$masters=Cases2[quarkGluonVTX$resPreFinal1,GLI];


quarkGluonVTX$MIMappings=FCLoopFindIntegralMappings[quarkGluonVTX$masters,Join[tadpoleMaster1[[2]],{tadpoleMaster2[[2]]},
quarkGluonVTX$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


isoSymbols1=FCMakeSymbols[LL,Range[1,$KernelCount],List];
isoSymbols2=FCMakeSymbols[LM,Range[1,$KernelCount],List];


AbsoluteTiming[quarkGluonVTX$resPreFinal2=Collect2[quarkGluonVTX$resPreFinal1,D,GLI,IsolateNames->isoSymbols1,FCParallelize->True]//FCReplaceD[#,D->4-2ep]&//ReplaceAll[#,quarkGluonVTX$MIMappings[[1]]]&//
ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//Collect2[#,ep,IsolateNames->isoSymbols2,FCParallelize->True]&;]


AbsoluteTiming[quarkGluonVTX$resPreFinal3=quarkGluonVTX$resPreFinal2//Series[#,{ep,0,-1}]&//Normal//Series[(I*(4*Pi)^(-2 + ep))^2 #,{ep,0,-1}]&//Normal;]


AbsoluteTiming[quarkGluonVTX$resPreFinal4=Collect2[FRH2[FRH2[quarkGluonVTX$resPreFinal3,isoSymbols2],isoSymbols1],DiracGamma,pp,mxt,ep,FCParallelize->True];]


isoSymbols3=FCMakeSymbols[LH,Range[1,$KernelCount],List];


AbsoluteTiming[quarkGluonVTX$resPreFinal5=Series[Total[Collect2[quarkGluonVTX$resPreFinal4,mxt,IsolateNames->isoSymbols3,FCParallelize->True]],{mxt,0,2}]//Normal;]


AbsoluteTiming[quarkGluonVTX$resPreFinal6=Collect2[FRH2[quarkGluonVTX$resPreFinal5,isoSymbols3]//ReplaceAll[#,Log[m_Symbol^2]:>2Log[m]]&,DiracGamma,pp,mxt,ep,FCParallelize->True];]


quarkGluonVTX$resFinal=Collect2[Collect2[quarkGluonVTX$resPreFinal6,ep,CA,CF,mq,Nf,SUNFDelta,as4,DiracGamma,GaugeXi,Factoring->FullSimplify],ep,mq,mxt]


(* ::Subsection:: *)
(*Quark-gluon vertex 1-loop CT*)


FCClearScalarProducts[];
divDegree=0;
aux1=FCLoopGetFeynAmpDenominators[quarkGluonVTXCT$RawAmp/.q->0/.p2->-p/.p1->p,{k1},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1},-mxt^2,0,Head->denHead]


quarkGluonVTXCT$StrName=StringReplace[ToString[Hold[quarkGluonVTXCT$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[quarkGluonVTXCT$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[quarkGluonVTXCT$Amp1=Collect2[quarkGluonVTXCT$Amp,p,IsolateNames->KK];]
AbsoluteTiming[quarkGluonVTXCT$Amp2=FourSeries[quarkGluonVTXCT$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[quarkGluonVTXCT$Amp3=Collect2[FRH[quarkGluonVTXCT$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{quarkGluonVTXCT$Amp4,quarkGluonVTXCT$Topos}=FCLoopFindTopologies[quarkGluonVTXCT$Amp3,{k1},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->quarkSEtopo];


AbsoluteTiming[quarkGluonVTXCT$Amp5=FCLoopTensorReduce[quarkGluonVTXCT$Amp4,quarkGluonVTXCT$Topos,FCParallelize->True];]


AbsoluteTiming[quarkGluonVTXCT$Amp6=DiracSimplify[quarkGluonVTXCT$Amp5,FCParallelize->True];]


{quarkGluonVTXCT$Amp7,quarkGluonVTXCT$Topos2}=FCLoopRewriteOverdeterminedTopologies[quarkGluonVTXCT$Amp6,quarkGluonVTXCT$Topos,FCParallelize->True];


{quarkGluonVTXCT$Amp8,quarkGluonVTXCT$Topos3}=FCLoopRewriteIncompleteTopologies[quarkGluonVTXCT$Amp7,quarkGluonVTXCT$Topos2,FCParallelize->True];


AbsoluteTiming[quarkGluonVTXCT$SubTopos=FCLoopFindSubtopologies[quarkGluonVTXCT$Topos2,Flatten->True,Remove->True,FCParallelize->True];]


AbsoluteTiming[{quarkGluonVTXCT$TopoMappings,quarkGluonVTXCT$FinalTopos}=FCLoopFindTopologyMappings[quarkGluonVTXCT$Topos2,PreferredTopologies->quarkGluonVTXCT$SubTopos,FCParallelize->True];]


AbsoluteTiming[quarkGluonVTXCT$AmpGLI=FCLoopApplyTopologyMappings[quarkGluonVTXCT$Amp8,{quarkGluonVTXCT$TopoMappings,quarkGluonVTXCT$FinalTopos},FCParallelize->True];]


quarkGluonVTXCT$GLIs=Cases2[quarkGluonVTXCT$AmpGLI,GLI];


quarkGluonVTXCT$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>quarkGluonVTXCT$StrName<>"-1L-massive"}];
Quiet[CreateDirectory[quarkGluonVTXCT$dir]];


KiraCreateJobFile[quarkGluonVTXCT$FinalTopos, quarkGluonVTXCT$GLIs, quarkGluonVTXCT$dir]


KiraCreateIntegralFile[quarkGluonVTXCT$GLIs, quarkGluonVTXCT$FinalTopos, quarkGluonVTXCT$dir]
KiraCreateConfigFiles[quarkGluonVTXCT$FinalTopos, quarkGluonVTXCT$GLIs, quarkGluonVTXCT$dir, 
 KiraMassDimensions -> {pp -> 2,mxt->1}]


KiraRunReduction[quarkGluonVTXCT$dir, quarkGluonVTXCT$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


quarkGluonVTXCT$ReductionTables=KiraImportResults[quarkGluonVTXCT$FinalTopos, quarkGluonVTXCT$dir]//Flatten;


quarkGluonVTXCT$resPreFinal1=Collect2[Total[quarkGluonVTXCT$AmpGLI/.Dispatch[quarkGluonVTXCT$ReductionTables]],GLI,
GaugeXi,D,DiracGamma,FCParallelize->True];


quarkGluonVTXCT$masters=Cases2[quarkGluonVTXCT$resPreFinal1,GLI];


quarkGluonVTXCT$MIMappings=FCLoopFindIntegralMappings[quarkGluonVTXCT$masters,Join[tadpoleMaster1[[2]],
quarkGluonVTXCT$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


(* ::Text:: *)
(*At this point we need to insert the 1-loop renormalization constants*)


knownResults1L = {
rc[delZA, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
 rc[delZAmxt, 1] -> - 1/8*(CA + 8*Nf + 3*CA*GaugeXi["G"])/ep, 
 rc[delZxi, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
 rc[delZpsi, 1] -> -((CF*GaugeXi["G"])/ep), 
 rc[delZumxt, 1] -> 0, rc[delZu, 1] -> (CA*(3 - GaugeXi["G"]))/(4*ep), 
 rc[delZg, 1] -> -1/6*(11*CA - 2*Nf)/ep};


knownResults2L = {rc[delZA, 2] -> (SUNN*(3 + 2*GaugeXi["G"])*(4*Nf - 13*SUNN + 3*SUNN*GaugeXi["G"]))/(24*ep^2) - 
   (-8*Nf + 28*Nf*SUNN^2 - 59*SUNN^3 + 11*SUNN^3*GaugeXi["G"] + 2*SUNN^3*GaugeXi["G"]^2)/(16*ep*SUNN),
    rc[delZpsi, 2] -> ((-1 + SUNN)*(1 + SUNN)*GaugeXi["G"]*(3*SUNN^2 - GaugeXi["G"] + 2*SUNN^2*GaugeXi["G"]))/(8*ep^2*SUNN^2) - 
   ((-1 + SUNN)*(1 + SUNN)*(3 - 4*Nf*SUNN + 22*SUNN^2 + 8*SUNN^2*GaugeXi["G"] + SUNN^2*GaugeXi["G"]^2))/(16*ep*SUNN^2) };


AbsoluteTiming[quarkGluonVTXCT$resPreFinal2=Collect2[quarkGluonVTXCT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,quarkGluonVTXCT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,1}]&//Normal//FCLoopAddMissingHigherOrdersWarning[#,ep,epHelp]&//FRH//
ReplaceAll[#,{Log[mxt^2]->2Log[mxt]}]&;]


AbsoluteTiming[quarkGluonVTXCT$resPreFinal3=Collect2[quarkGluonVTXCT$resPreFinal2,Join[{as4},List@@renConstants],IsolateNames->KK]//ReplaceAll[#,Zxi->ZA]&//
ReplaceAll[#,{(h:renConstants):>1+(as4 rc[ToExpression["del"<>ToString[h]],1]+as4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//Series[#,{as4,0,3}]&//Normal;]


quarkGluonVTXCT$resFinal=Collect2[quarkGluonVTXCT$resPreFinal3//FRH,{rc,D,GLI},IsolateNames->KK]//FCReplaceD[#,{D->4-2ep}]&//
ReplaceRepeated[#,knownResults1L]&//FRH//Series[#,{ep,0,-1}]&//Normal//Collect2[#,ep,mq,mxt]&


(* ::Subsection:: *)
(*Determination of renormalization constants*)


diagQuarkGluonTreeVTXCTGluonTreeSECT$Amp=(Total[diagQuarkGluonTreeSECT$RawAmp])//ReplaceAll[#,Zxi->ZA]&//ReplaceRepeated[#,{
	(h:renConstants):>1+(as4 rc[ToExpression["del"<>ToString[h]],1]+as4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//
	Series[#,{as4,0,3}]&//Normal//ReplaceRepeated[#,Join[knownResults1L,knownResults2L]]&


quarkGluonVTX$RenConstants2L=Collect2[Coefficient[SUNSimplify[ quarkGluonVTX$resFinal+ quarkGluonVTXCT$resFinal+ diagQuarkGluonTreeVTXCTGluonTreeSECT$Amp,SUNNToCACF->False],as4,5/2],
as4,mxt,DiracGamma,Factoring->Simplify]//FCMatchSolve[#,{ep,CF,DiracGamma,mq,mxt,SUNDelta,SUNTF,SUNFDelta,CA,GaugeXi,as4,Pair,pp,Nf,SUNN}]&//Collect2[#,ep]&


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final QCD 2-loop wave-function renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ as4 rc[ToExpression["del"<>ToString[h]],1]+ as4^2 rc[ToExpression["del"<>ToString[h]],2])//
ReplaceAll[#,Join[SUNSimplify[knownResults1L,SUNNToCACF->False],quarkGluonVTX$RenConstants2L]]&]]//SelectNotFree[#,Zg]&;


finalResults//TableForm


knownResult = {rc[delZg, 2] -> (2*Nf - 11*SUNN)^2/(24*ep^2) + (-3*Nf + 13*Nf*SUNN^2 - 34*SUNN^3)/(12*ep*SUNN)};


(* ::Text:: *)
(*Notice that Chetyrkin's results is for Zal=Zg^2*)


FCCompareResults[quarkGluonVTX$RenConstants2L,knownResult,
Text->{"\tCompare to Chetyrkin, Four-loop renormalization of QCD: full set of renormalization constants and anomalous dimensions, \
arXiv:hep-ph/0405193:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



