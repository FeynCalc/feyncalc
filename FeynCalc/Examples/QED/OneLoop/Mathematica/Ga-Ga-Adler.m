(* ::Package:: *)

(* :Title: Ga-Ga															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Ga -> Ga, QED, only UV divergences, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*QED vacuum polarization*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Ga -> Ga, QED, only UV divergences, 1-loop";
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
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[mu,"\[Mu]"];
FCAttachTypesettingRule[nu,"\[Nu]"];


diags = InsertFields[CreateTopologies[1, 1 -> 1], {V[1]} ->
		{V[1]}, InsertionLevel -> {Particles},
		ExcludeParticles->{S[_],V[2|3],(S|U)[_],F[4],F[3,{2|3}],F[2]}];
diagsCT = InsertFields[CreateCTTopologies[2, 1 -> 1], {V[1]} ->
		{V[1]}, InsertionLevel -> {Particles},
		ExcludeParticles->{S[_],V[2|3],(S|U)[_],V[2|3|4],F[4],F[3,{2|3}],F[2]}];


Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->256{2, 1}];


Paint[diagsCT, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->256{2, 1}];


(* ::Section:: *)
(*Obtain the amplitude*)


photonSE$RawAmp = FCFAConvert[CreateFeynAmp[diags, Truncated -> True, PreFactor->1],
	IncomingMomenta->{q}, OutgoingMomenta->{q},LoopMomenta->{k},
	LorentzIndexNames->{mu,nu}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True]/.{SumOver[SUNFIndex[Col3], 3]->SUNN}/.SMP["m_e"]|SMP["m_u"]->0


photonSE$Amp1={(3/2)^2 eQ^2 Nf photonSE$RawAmp[[1]]}


photonSECT$AmpRaw = FCFAConvert[CreateFeynAmp[diagsCT, Truncated -> True, PreFactor->1],
	IncomingMomenta->{q}, OutgoingMomenta->{q},LoopMomenta->{k},
	LorentzIndexNames->{mu,nu}, UndoChiralSplittings->True,
	ChangeDimension->D, SMP->True]/.{SumOver[SUNFIndex[Col3], 3]->SUNN}


(* ::Section:: *)
(*Calculate the amplitude*)


FCClearScalarProducts[];
SPD[q]=qq;


projector=MTD[mu,nu]/((1-D)qq)


photonSE$Amp2=(photonSE$Amp1 projector)//Contract[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&


{photonSE$Amp3,photonSE$Topos}=FCLoopFindTopologies[photonSE$Amp2,{k},FCParallelize->True,
FCLoopBasisOverdeterminedQ->True,FinalSubstitutions->{Hold[SPD][q]->qq},Names->photonSEtopo];


AbsoluteTiming[photonSE$Amp4=FCLoopTensorReduce[photonSE$Amp3,photonSE$Topos,FCParallelize->True];]


{photonSE$TopoMappings,photonSE$FinalTopos}=FCLoopFindTopologyMappings[photonSE$Topos]


photonSE$AmpGLI=FCLoopApplyTopologyMappings[photonSE$Amp4,{photonSE$TopoMappings,photonSE$FinalTopos},FCParallelize->True];


photonSE$GLIs=Cases2[photonSE$AmpGLI,GLI];


photonSE$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-photonSE-1L"}];
Quiet[CreateDirectory[photonSE$dir]];


KiraCreateJobFile[photonSE$FinalTopos, photonSE$GLIs, photonSE$dir]


KiraCreateIntegralFile[photonSE$GLIs, photonSE$FinalTopos, photonSE$dir]
KiraCreateConfigFiles[photonSE$FinalTopos, photonSE$GLIs, photonSE$dir, 
 KiraMassDimensions -> {qq -> 2}]


KiraRunReduction[photonSE$dir, photonSE$FinalTopos, 
 KiraBinaryPath -> FileNameJoin[{$HomeDirectory, ".local", "bin", "kira"}],
 KiraFermatPath -> FileNameJoin[{$HomeDirectory, "bin", "ferl64", "fer64"}]]


photonSE$ReductionTables=KiraImportResults[photonSE$FinalTopos, photonSE$dir]//Flatten;


photonSE$resPreFinal=Collect2[Total[photonSE$AmpGLI/.Dispatch[photonSE$ReductionTables]],GLI,
GaugeXi,FCParallelize->True]


miRes=Get["/media/Data/Projects/VS/FeynCalc/FeynCalc/Examples/MasterIntegrals/Mincer/prop1L00.m"]


aux=Series[photonSE$resPreFinal/.GLI[photonSEtopo1,{1,1}]->miRes[[1]][[2]]/.D->4-2ep,{ep,0,0}]//Normal


qq D[aux/.Log[-qq-I eta]->Log[qq]- I Pi ,qq]
12 Pi^2 %/(4 Pi)^2


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*Keep in mind that Peskin and Schroeder use D = 4-Epsilon,*)
(*while we did the calculation with D = 4-2Epsilon.*)


knownResult = -SMP["e"]^2/(4Pi)^(D/2) Gamma[2-D/2]/
	(SMP["m_e"]^2- x(1-x)SPD[p,p])^(2-D/2)*(8x(1-x))//
	FCReplaceD[#,D->4-Epsilon]&//Series[#,{Epsilon,0,0}]&//
	Normal//SelectNotFree2[#,Epsilon]&//Integrate[#,{x,0,1}]&//
	ReplaceAll[#,1/Epsilon->1/(2Epsilon)]&;
FCCompareResults[pi[0],knownResult,
Text->{"\tCompare to Peskin and Schroeder, An Introduction to QFT, \
Eq 10.44:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];

