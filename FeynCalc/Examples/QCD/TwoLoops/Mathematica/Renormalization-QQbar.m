(* ::Package:: *)

(* :Title: Renormalization-QQbar										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QCD, MSbar, Quark self-energy, massive, 2-loop *)

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


diagQuarkSE=InsertFields[CreateTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}]} -> {F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diagQuarkSECT=InsertFields[CreateCTTopologies[2, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}]} -> {F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}]];


diagQuarkTreeSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[3,{1}]} -> {F[3,{1}]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,"QCD"}],
GenericModel -> FileNameJoin[{modelDir,"QCD"}]];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagQuarkSE, ColumnsXRows -> {4, 2},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 2}];


(* ::Text:: *)
(*1-loop counter-term diagrams*)


Paint[diagQuarkSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Text:: *)
(*Tree-level counter-term diagrams*)


Paint[diagQuarkTreeSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Section:: *)
(*Master integrals*)


(* ::Text:: *)
(*The only required masters are 1- and 2-loop tadpoles*)


tadpoleMaster=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]];


tadpoleMaster1=tadpoleMaster/.m1->mq/.tad1LxFx1x1xxEp999x->"tad1Lv1";
tadpoleMaster2=tadpoleMaster/.m1->mxt/.tad1LxFx1x1xxEp999x->"tad1Lv2";


tadpoleMaster1


tadpoleMaster2


tadpoleMaster3=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxFx111x111xxEp1x.m"}]]/.m1->mq/.tad2LxFx111x111xxEp1x->"tad2Lv1";


tadpoleMaster4=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxFx111x111xxEp1x.m"}]]/.m1->mxt/.tad2LxFx111x111xxEp1x->"tad2Lv2";


tadpoleMaster5=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxAm2m1o4x111x122xxEp1x.m"}]]/.{m1->mq,m2->mxt}/.tad2LxAm2m1o4x111x122xxEp1x->"tad2Lv3";


tadpoleMaster6=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",
"tad2LxAm2m1o4x111x112xxEp1x.m"}]]/.{m1->mq,m2->mxt}/.tad2LxAm2m1o4x111x112xxEp1x->"tad2Lv4";


(* ::Section:: *)
(*Obtain the amplitudes*)


{quarkSE$RawAmp,quarkSECT$RawAmp,diagQuarkTreeSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{k1,k2}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->mq,SMP["g_s"]->4 Pi Sqrt[as4]}]&/@{
	diagQuarkSE,diagQuarkSECT,diagQuarkTreeSECT};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Quark self-energy at 2 loops*)


(* ::Text:: *)
(*The 2-loop quark self-energy has superficial degree of divergence equal to 1*)


FCClearScalarProducts[];
divDegree=1;
aux1=FCLoopGetFeynAmpDenominators[Join[quarkSE$RawAmp[[1;;3]],{Nf quarkSE$RawAmp[[4]]},quarkSE$RawAmp[[5;;]]],
{k1,k2},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1,k2},-mxt^2,0,Head->denHead]


AbsoluteTiming[quarkSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCI->True,FCParallelize->True]&//DiracSimplify[#,FCI->True,FCParallelize->True]&;]


isoSymbols=FCMakeSymbols[KK,Range[1,$KernelCount],List]


AbsoluteTiming[quarkSE$Amp1=Collect2[quarkSE$Amp,p,IsolateNames->isoSymbols,FCParallelize->True];]


AbsoluteTiming[quarkSE$Amp2=FourSeries[quarkSE$Amp1,{p,0,1},FCParallelize->True];]


AbsoluteTiming[quarkSE$Amp3=Collect2[FRH2[quarkSE$Amp2,isoSymbols],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[]
SPD[p]=pp;


AbsoluteTiming[{quarkSE$Amp4,quarkSE$Topos}=FCLoopFindTopologies[quarkSE$Amp3,{k1,k2},FCI->True,FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp}];]


AbsoluteTiming[quarkSE$Amp5=FCLoopTensorReduce[quarkSE$Amp4,quarkSE$Topos,FCParallelize->True];]


AbsoluteTiming[quarkSE$Amp6=DiracSimplify[quarkSE$Amp5,FCParallelize->True];]


AbsoluteTiming[{quarkSE$Amp7,quarkSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[quarkSE$Amp6,quarkSE$Topos,FCParallelize->True];]


AbsoluteTiming[{quarkSE$Amp8,quarkSE$Topos3}=FCLoopRewriteIncompleteTopologies[quarkSE$Amp7,quarkSE$Topos2,FCParallelize->True];]


AbsoluteTiming[quarkSE$SubTopos=FCLoopFindSubtopologies[quarkSE$Topos3,Flatten->True,Remove->True,FCParallelize->True];]


{quarkSE$TopoMappings,
quarkSE$FinalTopos}=FCLoopFindTopologyMappings[quarkSE$Topos3,PreferredTopologies->quarkSE$SubTopos,FCParallelize->True];


AbsoluteTiming[quarkSE$AmpGLI=FCLoopApplyTopologyMappings[quarkSE$Amp8,{quarkSE$TopoMappings,
quarkSE$FinalTopos},FCParallelize->True];]


quarkSE$GLIs=Cases2[quarkSE$AmpGLI,GLI];


quarkSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-quarkSE-2L-massive"}];
Quiet[CreateDirectory[quarkSE$dir]];


KiraCreateJobFile[quarkSE$FinalTopos, quarkSE$GLIs, quarkSE$dir]


KiraCreateIntegralFile[quarkSE$GLIs, quarkSE$FinalTopos, quarkSE$dir]
KiraCreateConfigFiles[quarkSE$FinalTopos, quarkSE$GLIs, quarkSE$dir, 
 KiraMassDimensions -> {pp -> 2,mq->1,mxt->1}]


KiraRunReduction[quarkSE$dir, quarkSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


quarkSE$ReductionTables=KiraImportResults[quarkSE$FinalTopos, quarkSE$dir]//Flatten;


AbsoluteTiming[quarkSE$resPreFinal1=(quarkSE$AmpGLI/.Dispatch[quarkSE$ReductionTables]);]


AbsoluteTiming[quarkSE$resPreFinal2=Map[Collect2[#,GLI,DiracGamma,FCParallelize->True]&,quarkSE$resPreFinal1];]


quarkSE$masters=Cases2[quarkSE$resPreFinal1,GLI];


quarkSE$MIMappings=FCLoopFindIntegralMappings[quarkSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],{tadpoleMaster3[[2]]},{tadpoleMaster4[[2]]}
,{tadpoleMaster5[[2]]},{tadpoleMaster6[[2]]},quarkSE$FinalTopos],PreferredIntegrals->{tadpoleMaster2[[1]][[1]]tadpoleMaster2[[1]][[1]],
tadpoleMaster1[[1]][[1]]tadpoleMaster1[[1]][[1]],tadpoleMaster1[[1]][[1]]tadpoleMaster2[[1]][[1]],
tadpoleMaster3[[1]][[1]],
tadpoleMaster4[[1]][[1]],
tadpoleMaster5[[1]][[1]],
tadpoleMaster6[[1]][[1]]}]


isoSymbols1=FCMakeSymbols[LL,Range[1,$KernelCount],List];
isoSymbols2=FCMakeSymbols[LM,Range[1,$KernelCount],List];


AbsoluteTiming[quarkSE$resPreFinal2=Collect2[quarkSE$resPreFinal1,D,GLI,IsolateNames->isoSymbols1,FCParallelize->True]//FCReplaceD[#,D->4-2ep]&//ReplaceAll[#,quarkSE$MIMappings[[1]]]&//
ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]],tadpoleMaster3[[1]],tadpoleMaster4[[1]],tadpoleMaster5[[1]],tadpoleMaster6[[1]]}]&//Collect2[#,ep,IsolateNames->isoSymbols2,FCParallelize->True]&;]


AbsoluteTiming[quarkSE$resPreFinal3=quarkSE$resPreFinal2//Series[#,{ep,0,-1}]&//Normal//Series[(I*(4*Pi)^(-2 + ep))^2 #,{ep,0,-1}]&//Normal;]


AbsoluteTiming[quarkSE$resPreFinal4=Collect2[FRH2[FRH2[quarkSE$resPreFinal3,isoSymbols2],isoSymbols1],DiracGamma,pp,mxt,ep,FCParallelize->True];]


isoSymbols3=FCMakeSymbols[LH,Range[1,$KernelCount],List];


AbsoluteTiming[quarkSE$resPreFinal5=Series[Total[Collect2[quarkSE$resPreFinal4,mxt,IsolateNames->isoSymbols3,FCParallelize->True]],{mxt,0,0}]//Normal;]


AbsoluteTiming[quarkSE$resPreFinal6=Collect2[FRH2[quarkSE$resPreFinal5,isoSymbols3]//ReplaceAll[#,Log[m_Symbol^2]:>2Log[m]]&,DiracGamma,pp,mxt,ep,FCParallelize->True];]


quarkSE$resFinal=Collect2[Collect2[quarkSE$resPreFinal6,ep,CA,CF,mq,Nf,SUNFDelta,as4,DiracGamma,GaugeXi,Factoring->FullSimplify],ep,mq,mxt]


(* ::Subsection:: *)
(*Quark self-energy 1-loop CT*)


FCClearScalarProducts[];
divDegree=1;
aux1=FCLoopGetFeynAmpDenominators[quarkSECT$RawAmp,{k1},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k1},-mxt^2,0,Head->denHead]


quarkSECT$StrName=StringReplace[ToString[Hold[quarkSECT$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[quarkSECT$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
SUNSimplify[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[quarkSECT$Amp1=Collect2[quarkSECT$Amp,p,IsolateNames->KK];]
AbsoluteTiming[quarkSECT$Amp2=FourSeries[quarkSECT$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[quarkSECT$Amp3=Collect2[FRH[quarkSECT$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{quarkSECT$Amp4,quarkSECT$Topos}=FCLoopFindTopologies[quarkSECT$Amp3,{k1},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->quarkSEtopo];


AbsoluteTiming[quarkSECT$Amp5=FCLoopTensorReduce[quarkSECT$Amp4,quarkSECT$Topos,FCParallelize->True];]


AbsoluteTiming[quarkSECT$Amp6=DiracSimplify[quarkSECT$Amp5,FCParallelize->True];]


{quarkSECT$Amp7,quarkSECT$Topos2}=FCLoopRewriteOverdeterminedTopologies[quarkSECT$Amp6,quarkSECT$Topos,FCParallelize->True];


{quarkSECT$Amp8,quarkSECT$Topos3}=FCLoopRewriteIncompleteTopologies[quarkSECT$Amp7,quarkSECT$Topos2,FCParallelize->True];


AbsoluteTiming[quarkSECT$SubTopos=FCLoopFindSubtopologies[quarkSECT$Topos2,Flatten->True,Remove->True,FCParallelize->True];]


AbsoluteTiming[{quarkSECT$TopoMappings,quarkSECT$FinalTopos}=FCLoopFindTopologyMappings[quarkSECT$Topos2,PreferredTopologies->quarkSECT$SubTopos,FCParallelize->True];]


AbsoluteTiming[quarkSECT$AmpGLI=FCLoopApplyTopologyMappings[quarkSECT$Amp8,{quarkSECT$TopoMappings,quarkSECT$FinalTopos},FCParallelize->True];]


quarkSECT$GLIs=Cases2[quarkSECT$AmpGLI,GLI];


quarkSECT$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>quarkSECT$StrName<>"-1L-massive"}];
Quiet[CreateDirectory[quarkSECT$dir]];


KiraCreateJobFile[quarkSECT$FinalTopos, quarkSECT$GLIs, quarkSECT$dir]


KiraCreateIntegralFile[quarkSECT$GLIs, quarkSECT$FinalTopos, quarkSECT$dir]
KiraCreateConfigFiles[quarkSECT$FinalTopos, quarkSECT$GLIs, quarkSECT$dir, 
 KiraMassDimensions -> {pp -> 2,mq->1,mxt->1}]


KiraRunReduction[quarkSECT$dir, quarkSECT$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


quarkSECT$ReductionTables=KiraImportResults[quarkSECT$FinalTopos, quarkSECT$dir]//Flatten;


quarkSECT$resPreFinal1=Collect2[Total[quarkSECT$AmpGLI/.Dispatch[quarkSECT$ReductionTables]],GLI,
GaugeXi,D,DiracGamma,FCParallelize->True];


quarkSECT$masters=Cases2[quarkSECT$resPreFinal1,GLI];


quarkSECT$MIMappings=FCLoopFindIntegralMappings[quarkSECT$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
quarkSECT$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


(* ::Text:: *)
(*At this point we need to insert the 1-loop renormalization constants*)


knownResults1L = {
rc[delZA, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
 rc[delZAmxt, 1] -> - (CA*( 1 +  3*GaugeXi["G"]))/(8*ep), 
 rc[delZxi, 1] -> (13*CA - 4*Nf - 3*CA*GaugeXi["G"])/(6*ep), 
 rc[delZm, 1] -> - (3*CF)/ep, 
 rc[delZpsi, 1] -> -((CF*GaugeXi["G"])/ep), 
 rc[delZumxt, 1] -> 0, 
 rc[delZu, 1] -> (CA*(3 - GaugeXi["G"]))/(4*ep), 
 rc[delZg, 1] -> -1/6*(11*CA - 2*Nf)/ep};


AbsoluteTiming[quarkSECT$resPreFinal2=Collect2[quarkSECT$resPreFinal1,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,quarkSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,1}]&//Normal//FCLoopAddMissingHigherOrdersWarning[#,ep,epHelp]&//FRH//
ReplaceAll[#,{Log[mxt^2]->2Log[mxt]}]&;]


AbsoluteTiming[quarkSECT$resPreFinal2=Collect2[quarkSECT$resPreFinal1,Join[{as4},List@@renConstants],IsolateNames->KK]//ReplaceAll[#,Zxi->ZA]&//ReplaceAll[#,{
	(h:renConstants):>1+(as4 rc[ToExpression["del"<>ToString[h]],1]+as4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//Series[#,{as4,0,2}]&//Normal;]


AbsoluteTiming[quarkSECT$resPreFinal3=Collect2[quarkSECT$resPreFinal2//FRH,{rc,D,GLI},IsolateNames->KK]//FCReplaceD[#,{D->4-2ep}]&//ReplaceRepeated[#,knownResults1L]&//
ReplaceAll[#,quarkSECT$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Abort[],#]&//Collect2[#,ep,IsolateNames->KK]&;]


quarkSECT$resFinal=quarkSECT$resPreFinal3//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//
Collect2[#,mxt,IsolateNames->KK]&//Series[#,{mxt,0,0}]&//Normal//FRH//ReplaceAll[#,Log[m_^2]:>2Log[m]]&//Collect2[#,ep,mq,mxt]&


(* ::Subsection:: *)
(*Determination of renormalization constants*)


diagQuarkTreeSECT$Amp=(Total[diagQuarkTreeSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+(as4 rc[ToExpression["del"<>ToString[h]],1]+as4^2 rc[ToExpression["del"<>ToString[h]],2])}]&//
	Series[#,{as4,0,2}]&//Normal//ReplaceRepeated[#,knownResults1L]&


quarkSE$RenConstants2L=Collect2[Coefficient[SUNSimplify[quarkSE$resFinal+quarkSECT$resFinal+diagQuarkTreeSECT$Amp,SUNNToCACF->False],as4,2],as4,mxt,DiracGamma,Factoring->FullSimplify]//
	FCMatchSolve[#,{ep,CF,DiracGamma,mq,mxt,SUNDelta,SUNTF,SUNFDelta,CA,GaugeXi,as4,Pair,pp,Nf,SUNN}]&//Collect2[#,ep]&


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final QCD 2-loop wave-function renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ as4 rc[ToExpression["del"<>ToString[h]],1]+ as4^2 rc[ToExpression["del"<>ToString[h]],2])//
ReplaceAll[#,Join[SUNSimplify[knownResults1L,SUNNToCACF->False],quarkSE$RenConstants2L]]&]]//SelectNotFree[#,Zpsi,Zm]&;


finalResults//TableForm


knownResult = {rc[delZm, 2] -> ((-1 + SUNN)*(1 + SUNN)*(-9 - 4*Nf*SUNN + 31*SUNN^2))/(8*ep^2*SUNN^2) - ((-1 + SUNN)*(1 + SUNN)*(-9 - 20*Nf*SUNN + 203*SUNN^2))/(48*ep*SUNN^2), 
 rc[delZpsi, 2] -> ((-1 + SUNN)*(1 + SUNN)*GaugeXi["G"]*(3*SUNN^2 - GaugeXi["G"] + 2*SUNN^2*GaugeXi["G"]))/(8*ep^2*SUNN^2) - 
   ((-1 + SUNN)*(1 + SUNN)*(3 - 4*Nf*SUNN + 22*SUNN^2 + 8*SUNN^2*GaugeXi["G"] + SUNN^2*GaugeXi["G"]^2))/(16*ep*SUNN^2)};


FCCompareResults[quarkSE$RenConstants2L,knownResult,
Text->{"\tCompare to Chetyrkin, Four-loop renormalization of QCD: full set of renormalization constants and anomalous dimensions, \
arXiv:hep-ph/0405193:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



