(* ::Package:: *)

(* :Title: Renormalization													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, pseudoscalar Yukawa, MSbar, 1-loop							*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*1-loop pseudoscalar Yukawa renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom Yukawa model created with FeynRules.*)


description="Renormalization, pseudoscalar Yukawa, MSbar, 1-loop";
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


modelName="YukawaPS";


modelDir=FileNameJoin[{$UserBaseDirectory,"Applications","FeynCalc","Examples","Models",modelName}];


FAPatch[PatchModelsOnly->True,FAModelsDirectory->modelDir];


(* ::Text:: *)
(*Here we define all Z-factors for renormalization constants present in the Lagrangian*)


renConstants=Zg|Zla|Zx|Zphi|Zmphi|Zmx|Zx


(* ::Section:: *)
(*Generate Feynman diagrams*)


diagFermionSE=InsertFields[CreateTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[10]} -> {F[10]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"YukawaPS"}]];


diagScalarSE=InsertFields[CreateTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"YukawaPS"}]];


diagFermionScalarVTX=InsertFields[CreateTopologies[1, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[10],S[1]}->{F[10]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"YukawaPS"}]];


diagScalar4VTX=InsertFields[CreateTopologies[1, 2 -> 2,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1],S[1]} -> {S[1],S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"YukawaPS"}]];


diagFermionSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[10]} -> {F[10]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"YukawaPS"}]];


diagScalarSECT=InsertFields[CreateCTTopologies[1, 1 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1]} -> {S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"YukawaPS"}]];


diagFermionScalarVTXCT=InsertFields[CreateCTTopologies[1, 2 -> 1,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {F[10],S[1]}->{F[10]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"YukawaPS"}]];


diagScalar4VTXCT=InsertFields[CreateCTTopologies[1, 2 -> 2,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}], {S[1],S[1]} -> {S[1],S[1]},
InsertionLevel->{Particles},Model -> FileNameJoin[{modelDir,modelName}],
GenericModel -> FileNameJoin[{modelDir,"YukawaPS"}]];


(* ::Text:: *)
(*Self-energy and vertex diagrams*)


Paint[diagFermionSE, ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{2, 1}];
Paint[diagScalarSE, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagFermionScalarVTX, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagScalar4VTX, ColumnsXRows -> {4,3},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 3}];


(* ::Text:: *)
(*Counter-term diagrams*)


Paint[diagFermionSECT, ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{2, 1}];
Paint[diagScalarSECT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagFermionScalarVTXCT, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];
Paint[diagScalar4VTXCT, ColumnsXRows -> {4,1},SheetHeader->None,
Numbering -> Simple, ImageSize->128{4, 1}];


(* ::Section:: *)
(*Master integrals*)


(* ::Text:: *)
(*The only required masters are 1-loop tadpoles*)
(**)


tadpoleMaster=Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]];


tadpoleMaster1=tadpoleMaster/.m1->mx/.tad1LxFx1x1xxEp999x->"tad1Lv1";
tadpoleMaster2=tadpoleMaster/.m1-> mphi/.tad1LxFx1x1xxEp999x->"tad1Lv2";


tadpoleMaster1


tadpoleMaster2


(* ::Section:: *)
(*Obtain the amplitudes*)


{fermionSE$RawAmp,scalarSE$RawAmp,fermionSECT$RawAmp,scalarSECT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	DropSumOver->True,
	LoopMomenta->{k}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{GaugeXi[S[1]]->gxi,Mphi->mphi,Mx->mx}]&/@{
	diagFermionSE,diagScalarSE,diagFermionSECT,diagScalarSECT};


{fermionScalarVTX$RawAmp,scalar4VTX$RawAmp,fermionScalarVTXCT$RawAmp,scalar4VTXCT$RawAmp} = 
FCFAConvert[CreateFeynAmp[#,Truncated->True,PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{q1,q2},
	DropSumOver->True,
	LoopMomenta->{k}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True,
	FinalSubstitutions->{GaugeXi[S[1]]->gxi,Mphi->mphi,Mx->mx}]&/@{
	diagFermionScalarVTX,diagScalar4VTX,diagFermionScalarVTXCT,diagScalar4VTXCT
	};


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Fermion self-energy*)


(* ::Text:: *)
(*The 1-loop fermion self-energy has superficial degree of divergence equal to 1*)


FCClearScalarProducts[];
divDegree=1;
aux1=FCLoopGetFeynAmpDenominators[fermionSE$RawAmp,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


fermionSE$StrName=StringReplace[ToString[Hold[fermionSE$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[fermionSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[fermionSE$Amp1=Collect2[fermionSE$Amp,p,IsolateNames->KK];]
AbsoluteTiming[fermionSE$Amp2=FourSeries[fermionSE$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[fermionSE$Amp3=Collect2[FRH[fermionSE$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{fermionSE$Amp4,fermionSE$Topos}=FCLoopFindTopologies[fermionSE$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->fermionSEtopo];


AbsoluteTiming[fermionSE$Amp5=FCLoopTensorReduce[fermionSE$Amp4,fermionSE$Topos,FCParallelize->True];]


{fermionSE$Amp6,fermionSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[fermionSE$Amp5,fermionSE$Topos];


fermionSE$SubTopos=FCLoopFindSubtopologies[fermionSE$Topos2,Flatten->True,Remove->True]


{fermionSE$TopoMappings,fermionSE$FinalTopos}=FCLoopFindTopologyMappings[fermionSE$Topos2,PreferredTopologies->fermionSE$SubTopos];


fermionSE$AmpGLI=FCLoopApplyTopologyMappings[fermionSE$Amp6,{fermionSE$TopoMappings,fermionSE$FinalTopos},FCParallelize->True];


fermionSE$GLIs=Cases2[fermionSE$AmpGLI,GLI];


fermionSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>modelName<>"-"<>fermionSE$StrName<>"-1L"}];
Quiet[CreateDirectory[fermionSE$dir]];


KiraCreateJobFile[fermionSE$FinalTopos, fermionSE$GLIs, fermionSE$dir]


KiraCreateIntegralFile[fermionSE$GLIs, fermionSE$FinalTopos, fermionSE$dir]
KiraCreateConfigFiles[fermionSE$FinalTopos, fermionSE$GLIs, fermionSE$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1,mx->1,gxi->0}]


KiraRunReduction[fermionSE$dir, fermionSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


fermionSE$ReductionTables=KiraImportResults[fermionSE$FinalTopos, fermionSE$dir]//Flatten;


fermionSE$resPreFinal=Collect2[Total[fermionSE$AmpGLI/.Dispatch[fermionSE$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


fermionSE$masters=Cases2[fermionSE$resPreFinal,GLI];


fermionSE$MIMappings=FCLoopFindIntegralMappings[fermionSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
fermionSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


fermionSE$resFinal=Collect2[fermionSE$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,fermionSE$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Print["Unsubstituted GLIs!"];Abort[],#]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


fermionSE$RenConstants=(fermionSE$resFinal+Total[fermionSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//
	ReplaceAll[#,alpha->1]&//Collect2[#,DiracGamma]&//
	FCMatchSolve[#,{ep,CF,DiracGamma,mx,mxt,SUNDelta,SUNFDelta,gxi,g}]&


(* ::Subsection:: *)
(*Scalar self-energy*)


(* ::Text:: *)
(*The 1-loop scalar self-energy has superficial degree of divergence equal to 2.*)


FCClearScalarProducts[];
divDegree=2;
aux1=FCLoopGetFeynAmpDenominators[scalarSE$RawAmp,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


scalarSE$StrName=StringReplace[ToString[Hold[scalarSE$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[scalarSE$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
DiracSimplify[#,FCParallelize->True]&;]


(* ::Text:: *)
(*flagCheck is a safety flag to ensure that higher order terms in p (higher than the divergence degree) do not  contribute to the poles*)


AbsoluteTiming[scalarSE$Amp1=Collect2[scalarSE$Amp,p,IsolateNames->KK];]
AbsoluteTiming[scalarSE$Amp2=FourSeries[scalarSE$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[scalarSE$Amp3=Collect2[FRH[scalarSE$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{scalarSE$Amp4,scalarSE$Topos}=FCLoopFindTopologies[scalarSE$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->scalarSEtopo];


AbsoluteTiming[scalarSE$Amp5=FCLoopTensorReduce[scalarSE$Amp4,scalarSE$Topos,FCParallelize->True];]


{scalarSE$Amp6,scalarSE$Topos2}=FCLoopRewriteOverdeterminedTopologies[scalarSE$Amp5,scalarSE$Topos];


scalarSE$SubTopos=FCLoopFindSubtopologies[scalarSE$Topos2,Flatten->True,Remove->True]


{scalarSE$TopoMappings,scalarSE$FinalTopos}=FCLoopFindTopologyMappings[scalarSE$Topos2,PreferredTopologies->scalarSE$SubTopos];


scalarSE$AmpGLI=FCLoopApplyTopologyMappings[scalarSE$Amp6,{scalarSE$TopoMappings,scalarSE$FinalTopos},FCParallelize->True];


scalarSE$GLIs=Cases2[scalarSE$AmpGLI,GLI];


scalarSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>modelName<>"-"<>scalarSE$StrName<>"-1L"}];
Quiet[CreateDirectory[scalarSE$dir]];


KiraCreateJobFile[scalarSE$FinalTopos, scalarSE$GLIs, scalarSE$dir]


KiraCreateIntegralFile[scalarSE$GLIs, scalarSE$FinalTopos, scalarSE$dir]
KiraCreateConfigFiles[scalarSE$FinalTopos, scalarSE$GLIs, scalarSE$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1,mx->1,gxi->0}]


KiraRunReduction[scalarSE$dir, scalarSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


scalarSE$ReductionTables=KiraImportResults[scalarSE$FinalTopos, scalarSE$dir]//Flatten;


scalarSE$resPreFinal=Collect2[Total[scalarSE$AmpGLI/.Dispatch[scalarSE$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


scalarSE$masters=Cases2[scalarSE$resPreFinal,GLI];


scalarSE$MIMappings=FCLoopFindIntegralMappings[scalarSE$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
scalarSE$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


scalarSE$resFinal=Collect2[scalarSE$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,scalarSE$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Print["Unsubstituted GLIs!"];Abort[],#]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


scalarSE$RenConstants=(scalarSE$resFinal+Total[scalarSECT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//
	ReplaceAll[#,alpha->1]&//Collect2[#,pp,Pair]&//
	FCMatchSolve[#,{ep,g,gxi,la,DiracGamma,mphi,mx,pp}]&//ExpandAll


(* ::Subsection:: *)
(*Fermion-scalar vertex*)


(* ::Text:: *)
(*The 1-loop fermion-scalar-vertex has superficial degree of divergence equal to 0. We set q1=0, so that p1+p2=q yields p1=-p2*)


FCClearScalarProducts[];
divDegree=0;
aux1=FCLoopGetFeynAmpDenominators[fermionScalarVTX$RawAmp/.q1->0/.p2->-p1/.p1->p,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


fermionScalarVTX$StrName=StringReplace[ToString[Hold[fermionScalarVTX$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[fermionScalarVTX$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[fermionScalarVTX$Amp1=Collect2[fermionScalarVTX$Amp,p,IsolateNames->KK];]
AbsoluteTiming[fermionScalarVTX$Amp2=FourSeries[fermionScalarVTX$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[fermionScalarVTX$Amp3=Collect2[FRH[fermionScalarVTX$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{fermionScalarVTX$Amp4,fermionScalarVTX$Topos}=FCLoopFindTopologies[fermionScalarVTX$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->fermionScalarVTXtopo];


AbsoluteTiming[fermionScalarVTX$Amp5=FCLoopTensorReduce[fermionScalarVTX$Amp4,fermionScalarVTX$Topos,FCParallelize->True];]


{fermionScalarVTX$Amp6,fermionScalarVTX$Topos2}=FCLoopRewriteOverdeterminedTopologies[fermionScalarVTX$Amp5,fermionScalarVTX$Topos];


fermionScalarVTX$SubTopos=FCLoopFindSubtopologies[fermionScalarVTX$Topos2,Flatten->True,Remove->True]


{fermionScalarVTX$TopoMappings,fermionScalarVTX$FinalTopos}=FCLoopFindTopologyMappings[fermionScalarVTX$Topos2,PreferredTopologies->fermionScalarVTX$SubTopos];


fermionScalarVTX$AmpGLI=FCLoopApplyTopologyMappings[fermionScalarVTX$Amp6,{fermionScalarVTX$TopoMappings,fermionScalarVTX$FinalTopos},FCParallelize->True];


fermionScalarVTX$GLIs=Cases2[fermionScalarVTX$AmpGLI,GLI];


fermionScalarVTX$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>modelName<>"-"<>fermionScalarVTX$StrName<>"-1L"}];
Quiet[CreateDirectory[fermionScalarVTX$dir]];


KiraCreateJobFile[fermionScalarVTX$FinalTopos, fermionScalarVTX$GLIs, fermionScalarVTX$dir]


KiraCreateIntegralFile[fermionScalarVTX$GLIs, fermionScalarVTX$FinalTopos, fermionScalarVTX$dir]
KiraCreateConfigFiles[fermionScalarVTX$FinalTopos, fermionScalarVTX$GLIs, fermionScalarVTX$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1,mx->1,gxi->0}]


KiraRunReduction[fermionScalarVTX$dir, fermionScalarVTX$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


fermionScalarVTX$ReductionTables=KiraImportResults[fermionScalarVTX$FinalTopos, fermionScalarVTX$dir]//Flatten;


fermionScalarVTX$resPreFinal=Collect2[Total[fermionScalarVTX$AmpGLI/.Dispatch[fermionScalarVTX$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


fermionScalarVTX$masters=Cases2[fermionScalarVTX$resPreFinal,GLI];


fermionScalarVTX$MIMappings=FCLoopFindIntegralMappings[fermionScalarVTX$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
fermionScalarVTX$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


fermionScalarVTX$resFinal=Collect2[fermionScalarVTX$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,fermionScalarVTX$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Print["Unsubstituted GLIs!"];Abort[],#]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//DiracSubstitute67//Collect2[#,DiracGamma]&


fermionScalarVTX$RenConstants=(fermionScalarVTX$resFinal+Total[fermionScalarVTXCT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,Join[fermionSE$RenConstants,scalarSE$RenConstants]]&//
	ReplaceAll[#,alpha->1]&//Collect2[#,pp,Pair]&//
	FCMatchSolve[#,{ep,g,gxi,la,DiracGamma,mphi,mx,pp}]&//ExpandAll


(* ::Subsection:: *)
(*Four-scalar vertex*)


(* ::Text:: *)
(*The 1-loop four-scalar-vertex has superficial degree of divergence equal to 0. We set q1=q2=0, so that p1+p2=0 yields p1=-p2*)


FCClearScalarProducts[];
divDegree=0;
aux1=FCLoopGetFeynAmpDenominators[scalar4VTX$RawAmp/.q1|q2->0/.p2->-p1/.p1->p,{k},denHead,Momentum->{p},"Massless"->True];
aux2=FCLoopAddAuxiliaryMass[aux1[[2]],{k},-mxt^2,0,Head->denHead]


scalar4VTX$StrName=StringReplace[ToString[Hold[scalar4VTX$Amp]],{"Hold["->"","]"->""}]


AbsoluteTiming[scalar4VTX$Amp=(aux1[[1]]/.aux2)//Contract[#,FCParallelize->True]&//
DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[scalar4VTX$Amp1=Collect2[scalar4VTX$Amp,p,IsolateNames->KK];]
AbsoluteTiming[scalar4VTX$Amp2=FourSeries[scalar4VTX$Amp1,{p,0,divDegree},FCParallelize->True];]
AbsoluteTiming[scalar4VTX$Amp3=Collect2[FRH[scalar4VTX$Amp2],FeynAmpDenominator,FCParallelize->True];]


(* ::Text:: *)
(*The rest of the calculation follows the standard multiloop template*)


FCClearScalarProducts[];
SPD[p]=pp;


{scalar4VTX$Amp4,scalar4VTX$Topos}=FCLoopFindTopologies[scalar4VTX$Amp3,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][p]->pp},Names->scalar4VTXtopo];


AbsoluteTiming[scalar4VTX$Amp5=FCLoopTensorReduce[scalar4VTX$Amp4,scalar4VTX$Topos,FCParallelize->True];]


{scalar4VTX$Amp6,scalar4VTX$Topos2}=FCLoopRewriteOverdeterminedTopologies[scalar4VTX$Amp5,scalar4VTX$Topos];


scalar4VTX$SubTopos=FCLoopFindSubtopologies[scalar4VTX$Topos2,Flatten->True,Remove->True]


{scalar4VTX$TopoMappings,scalar4VTX$FinalTopos}=FCLoopFindTopologyMappings[scalar4VTX$Topos2,PreferredTopologies->scalar4VTX$SubTopos];


scalar4VTX$AmpGLI=FCLoopApplyTopologyMappings[scalar4VTX$Amp6,{scalar4VTX$TopoMappings,scalar4VTX$FinalTopos},FCParallelize->True];


scalar4VTX$GLIs=Cases2[scalar4VTX$AmpGLI,GLI];


scalar4VTX$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>modelName<>"-"<>scalar4VTX$StrName<>"-1L"}];
Quiet[CreateDirectory[scalar4VTX$dir]];


KiraCreateJobFile[scalar4VTX$FinalTopos, scalar4VTX$GLIs, scalar4VTX$dir]


KiraCreateIntegralFile[scalar4VTX$GLIs, scalar4VTX$FinalTopos, scalar4VTX$dir]
KiraCreateConfigFiles[scalar4VTX$FinalTopos, scalar4VTX$GLIs, scalar4VTX$dir, 
 KiraMassDimensions -> {pp -> 2,mphi->1,mx->1,gxi->0}]


KiraRunReduction[scalar4VTX$dir, scalar4VTX$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


scalar4VTX$ReductionTables=KiraImportResults[scalar4VTX$FinalTopos, scalar4VTX$dir]//Flatten;


scalar4VTX$resPreFinal=Collect2[Total[scalar4VTX$AmpGLI/.Dispatch[scalar4VTX$ReductionTables]]//FeynAmpDenominatorExplicit,GLI,
GaugeXi,flagCheck,D,DiracGamma,FCParallelize->True];


scalar4VTX$masters=Cases2[scalar4VTX$resPreFinal,GLI];


scalar4VTX$MIMappings=FCLoopFindIntegralMappings[scalar4VTX$masters,Join[tadpoleMaster1[[2]],tadpoleMaster2[[2]],
scalar4VTX$FinalTopos],PreferredIntegrals->{tadpoleMaster1[[1]][[1]],tadpoleMaster2[[1]][[1]]}]


(* ::Text:: *)
(*Our master integrals are calculated using the standard multiloop normalization. To convert it back to the textbook normalization*)
(*we need to multiply by I*(4 Pi)^(ep-2)*)


scalar4VTX$resFinal=Collect2[scalar4VTX$resPreFinal,D,GLI,IsolateNames->KK]//FCReplaceD[#,D->4-2ep]&//
ReplaceAll[#,scalar4VTX$MIMappings[[1]]]&//ReplaceAll[#,{tadpoleMaster1[[1]],tadpoleMaster2[[1]]}]&//If[!FreeQ[#,GLI],Print["Unsubstituted GLIs!"];Abort[],#]&//
Collect2[#,ep,IsolateNames->KK2]&//Series[(I*(4*Pi)^(-2 + ep)) #,{ep,0,-1}]&//Normal//FRH//Collect2[#,DiracGamma]&


scalar4VTX$RenConstants=(scalar4VTX$resFinal+Total[scalar4VTXCT$RawAmp])//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,Join[fermionSE$RenConstants,scalarSE$RenConstants]]&//
	ReplaceAll[#,alpha->1]&//Collect2[#,pp,Pair]&//
	FCMatchSolve[#,{ep,g,gxi,la,DiracGamma,mphi,mx,pp}]&//ExpandAll


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Our final pseudoscalar Yukawa 1-loop renormalization constants*)


finalResults=Thread[Rule[List@@renConstants,
(List@@renConstants/.(h:renConstants):>1+ rc[ToExpression["del"<>ToString[h]],1])//ReplaceAll[#,Join[fermionSE$RenConstants,scalarSE$RenConstants,
fermionScalarVTX$RenConstants,scalar4VTX$RenConstants]]&]]


(* ::Text:: *)
(*We can compare our results to the calculation in Chapter 51 in Srednicki's Quantum Field Theory book.*)
(*However, we need to keep in mind that there the mass terms get a single renormalization constant instead*)
(*of a combination of those and that the author uses D=4-ep*)


Join[fermionSE$RenConstants,scalarSE$RenConstants,
fermionScalarVTX$RenConstants,scalar4VTX$RenConstants]//InputForm


knownResult ={rc[delZmx, 1] -> -1/32*g^2/(ep*Pi^2), rc[delZx, 1] -> -1/32*g^2/(ep*Pi^2), rc[delZmphi, 1] -> g^2/(8*ep*Pi^2) + la/(32*ep*Pi^2) - (g^2*mx^2)/(4*ep*mphi^2*Pi^2), rc[delZphi, 1] -> -1/8*g^2/(ep*Pi^2), 
 rc[delZg, 1] -> (5*g^2)/(32*ep*Pi^2), rc[delZla, 1] -> g^2/(4*ep*Pi^2) - (3*g^4)/(2*ep*la*Pi^2) + (3*la)/(32*ep*Pi^2)};


renConstants


{rc[delZx$SRQFT, 1],rc[delZphi$SRQFT, 1],rc[delZmx$SRQFT, 1],rc[delZmphi$SRQFT, 1],rc[delZg$SRQFT, 1],rc[delZla$SRQFT, 1]}={(Zx - 1),(Zphi - 1),(Zmx Zx - 1),(Zmphi Zphi - 1),(Zg Zx Sqrt[Zphi]-1 ),(Zla Zphi^2 - 1)}//ReplaceRepeated[#,{
	(h:renConstants):>1+alpha rc[ToExpression["del"<>ToString[h]],1]}]&//
	Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,Join[fermionSE$RenConstants,scalarSE$RenConstants,fermionScalarVTX$RenConstants,scalar4VTX$RenConstants]]&//
	ReplaceAll[#,{alpha->1,ep->1/2ep}]&//Simplify


(* ::Text:: *)
(*Here are the literature results*)


resLit={
(*delZx$SRQFT*)- g^2/(16 Pi^2)1/ep,
(*delZphi$SRQFT*)- g^2/(4 Pi^2)1/ep,
(*delZmx$SRQFT*)- g^2/(8 Pi^2)1/ep,
(*delZmphi$SRQFT*)1/ep (la/(16 Pi^2)-g^2/(2Pi^2) mx^2/mphi^2),
(*delZg$SRQFT*) g^2/(8 Pi^2)1/ep,
(*delZla$SRQFT*)1/ep (3 la/(16 Pi^2)-3 g^4/(Pi^2 la))
}


FCCompareResults[{rc[delZx$SRQFT, 1],rc[delZphi$SRQFT, 1],rc[delZmx$SRQFT, 1],
rc[delZmphi$SRQFT, 1],rc[delZg$SRQFT, 1],rc[delZla$SRQFT, 1]}/.Rule->Equal,resLit/.Rule->Equal,
Text->{"\tCheck the final result:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}]
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



