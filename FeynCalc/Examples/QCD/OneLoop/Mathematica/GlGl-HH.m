(* ::Package:: *)

(* :Title: GlGl-HH															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  GlGl -> HH, QCD, amplitude, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Higgs production in gluon fusion*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="GlGl -> HH, QCD, amplitude, 1-loop";
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

FCCheckVersion[10,2,1];
If[ToExpression[StringSplit[$FeynHelpersVersion,"."]][[1]]<2,
	Print["You need at least FeynHelpers 2.1 to run this example."];
	Abort[];
]


(* ::Section:: *)
(*Generate Feynman diagrams*)


diags=InsertFields[CreateTopologies[1, 2 -> 2,ExcludeTopologies -> {Tadpoles}], 
{V[5],V[5]}->{S[1],S[1]}, InsertionLevel -> {Particles}, 
ExcludeParticles->{F[4],F[3,{1|2,_}]},Model->SMQCD];


Paint[diags,ColumnsXRows->{6,2},SheetHeader -> False,
Numbering -> Simple,ImageSize->128{6,2}];


(* ::Section:: *)
(*Obtain the amplitudes*)


ampRaw = FCFAConvert[CreateFeynAmp[diags, PreFactor->1], 
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{-q1,-q2},LoopMomenta->{k},
	UndoChiralSplittings->True, ChangeDimension->D, List->True, SMP->True,
	TransversePolarizationVectors->{p1,p2},
	DropSumOver->False]//SMPToSymbol//ReplaceAll[#,mH->mh]&;


(* ::Text:: *)
(*Each of the remaining diagrams has a color matrix so that it is safe to remove SumOver symbols*)


glglToHH$Amp=ampRaw/.SumOver[__]->1;


processID="glglToHHat1L";


(* ::Section:: *)
(*Fix the kinematics*)


(* ::Text:: *)
(*Notice that all external momenta are incoming*)


FCClearScalarProducts[];
SetMandelstam[s,t,u,p1,p2,q1,q2,0,0,mh,mh];


(* ::Section:: *)
(*Calculate the amplitude*)


AbsoluteTiming[glglToHH$Amp1=glglToHH$Amp//Contract[#,FCParallelize->True]&//
DiracSimplify[#,FCParallelize->True]&//SUNSimplify[#,FCParallelize->True]&;]


glglToHH$StrName=StringReplace[ToString[Hold[glglToHH$Amp]],{"Hold["->"","]"->""}]


(* ::Section:: *)
(*Identify and minimize the topologies*)


{glglToHH$Amp2,glglToHH$Topos}=FCLoopFindTopologies[glglToHH$Amp1,{k},FCParallelize->True,
Names->glglToHH1Ltopo,FinalSubstitutions->{s->2 mh^2-t-u}];


AbsoluteTiming[glglToHH$Amp3=FCLoopTensorReduce[glglToHH$Amp2,glglToHH$Topos,FCParallelize->True];]


AbsoluteTiming[{glglToHH$Amp4,glglToHH$Topos2}=FCLoopRewriteOverdeterminedTopologies[glglToHH$Amp3,
glglToHH$Topos,FCParallelize->True];]


AbsoluteTiming[{glglToHH$Amp5,glglToHH$Topos3}=FCLoopRewriteIncompleteTopologies[glglToHH$Amp4,
glglToHH$Topos2,FCParallelize->True];]


glglToHH$Subtopos=FCLoopFindSubtopologies[glglToHH$Topos3,FCParallelize->False,Names->R];


glglToHH$TopoMappings=FCLoopFindTopologyMappings[glglToHH$Topos3,PreferredTopologies->glglToHH$Subtopos,
FCParallelize->True];


AbsoluteTiming[glglToHH$AmpGLI=FCLoopApplyTopologyMappings[glglToHH$Amp5,glglToHH$TopoMappings,FCParallelize->True];]


glglToHH$GLIs=Cases2[glglToHH$AmpGLI,GLI];


glglToHH$dir=FileNameJoin[{$TemporaryDirectory,"Reduction-"<>glglToHH$StrName<>"-1L"}];
Quiet[CreateDirectory[glglToHH$dir]];


tables=FIREReduceLoopIntegrals[glglToHH$GLIs,glglToHH$TopoMappings,glglToHH$dir];


masters=Cases2[Last/@tables,GLI];


AbsoluteTiming[intMappings=FCLoopFindIntegralMappings[masters,glglToHH$TopoMappings,FCParallelize->True,FinalSubstitutions->{s->2 mh^2-t-u}];]


AbsoluteTiming[glglToHH$resPreFinal1=(glglToHH$AmpGLI/.Dispatch[tables]/.Dispatch[intMappings[[1]]])//FeynAmpDenominatorExplicit[#,FCParallelize->True]&;]


AbsoluteTiming[glglToHH$resPreFinal2=Collect2[glglToHH$resPreFinal1,GLI,FCParallelize->True];]


AbsoluteTiming[glglToHH$resPreFinal3=Collect2[glglToHH$resPreFinal2,GLI,Factoring->Function[x,Factor2[TrickMandelstam[x,{s,t,u,2 mh^2}]]],FCParallelize->True];]


AbsoluteTiming[glglToHH$resFinal=Collect2[Total[glglToHH$resPreFinal3],GLI,Factoring->Function[x,Factor2[TrickMandelstam[x,{s,t,u,2 mh^2}]]],FCParallelize->True];]


(* ::Text:: *)
(*Check gauge invariance of the amplitude*)


polVec1=Pair[LorentzIndex[mu,D],Momentum[Polarization[p1,I,Transversality->True],D]];
rulePolVec1=Contract[(FVD[p2,mu]==-FVD[q1+q2+p1,mu])polVec1]/.Equal->Rule//ExpandScalarProduct;


polVec2=Pair[LorentzIndex[mu,D],Momentum[Polarization[p2,I,Transversality->True],D]];
rulePolVec2=Contract[(FVD[p1,mu]==-FVD[q1+q2+p2,mu])polVec2]/.Equal->Rule//ExpandScalarProduct;


rulePolVec={rulePolVec1,rulePolVec2}


AbsoluteTiming[check1=TrickMandelstam[glglToHH$resPreFinal3/.Momentum[Polarization[p2,___],D]:>Momentum[p2,D]/.rulePolVec,{s,t,u,2 mh^2},FCParallelize->True];]


AbsoluteTiming[check2=Collect2[Total[check1],GLI,FCParallelize->True]//TrickMandelstam[#,{s,t,u,2 mh^2}]&;]


check2


(* ::Section:: *)
(*Check the final results*)


(* ::Text:: *)
(*We take the literature result from  Eqs. 2 and 3 of the Glover and van der Bij paper, CERN-TH-4934/87,  https://cds.cern.ch/record/183945/files/198802013.pdf*)


pT2=(u t-mh^4)/s;
Aten[mu_,nu_]:=MT[mu,nu]-FV[p1,nu]FV[p2,mu]/SPD[p1,p2];
Bten[mu_,nu_]:=MT[mu,nu]+mh^2FV[p1,nu]FV[p2,mu]/(pT2 SPD[p1,p2])-2 SPD[p1,q1]FV[p2,mu]FV[q1,nu]/(pT2 SPD[p1,p2])-
2 SPD[p2,q1]FV[p1,nu]FV[q1,mu]/(pT2 SPD[p1,p2])+2 FV[q1,mu] FV[q1,nu]/pT2;


Dfun[q1_,q2_,p3_]:=TrickMandelstam[1/(I Pi^2)ToPaVe[FAD[{k,mt},{k+q1,mt},{k+q1+q2,mt},
{k+q1+q2+p3,mt}],k]//PaVeOrder,{s,t,u,2 mh^2}];


Cfun[q1_,q2_]:=TrickMandelstam[1/(I Pi^2)ToPaVe[FAD[{k,mt},{k+q1,mt},
{k+q1+q2,mt}],k]//PaVeOrder,{s,t,u,2 mh^2}];


litResGauge1Tri=12 mh^2mt^2/(s-mh^2)(2+(4mt^2-s)Cfun[p1,p2]);


litResGauge1Box=4 mt^2(
mt^2(8mt^2-s-2 mh^2)(Dfun[p1,p2,q1]+Dfun[p2,p1,q1]+Dfun[p1,q1,p2])
+( u t - mh^4)/s(4 mt^2-mh^2)Dfun[p1,q1,p2]+2+ 4 mt^2 Cfun[p1,p2]+
2/s(mh^2-4 mt^2)(   (t-mh^2) Cfun[p1,q1] + (u-mh^2) Cfun[p2,q1]  )
);


litResGauge2Box=2 mt^2(
2(8mt^2+s-2 mh^2)*( mt^2(Dfun[p1,p2,q1]+Dfun[p2,p1,q1]+Dfun[p1,q1,p2])- Cfun[q1,q2])
-2(s Cfun[p1,p2]+ (t-mh^2) Cfun[p1,q1] + (u-mh^2)Cfun[p2,q1]  )+
1/(u t - mh^4)(
s u (8 u mt^2-u^2- mh^4)Dfun[p1,p2,q1]+ 
s t (8 t mt^2-t^2-mh^4)Dfun[p2,p1,q1]+ 
(8mt^2+s-2 mh^2)(
s(s-2 mh^2)Cfun[p1,p2]+s (s-4 mh^2) Cfun[q1,q2]+
2t (mh^2-t) Cfun[p1,q1]+2 u (mh^2-u) Cfun[p2,q1])
)
);


litAmpRaw=Contract[((litResGauge1Box +litResGauge1Tri)Aten[mu,nu]+  litResGauge2Box Bten[mu,nu])Pair[LorentzIndex[mu,D],
Momentum[Polarization[p1,I,Transversality->True],D]]Pair[LorentzIndex[nu,D],Momentum[Polarization[p2,I,Transversality->True],D]]]//ChangeDimension[#,4]&;


litAmp=Collect2[ToPaVe2[litAmpRaw],PaVe,Factoring->Function[x,TrickMandelstam[x,{s,t,u,2 mh^2}]]];


mastersNew=Cases2[glglToHH$resFinal,GLI];


mastersNewPaVe=ToPaVe2[ToPaVe[FDS[FCLoopFromGLI[mastersNew,glglToHH$TopoMappings[[2]]],k],k]//TrickMandelstam[#,{s,t,u,2 mh^2}]&]


prefLit=((1/2*I)*mW^2)/(alphas*alphaW*Pi^4*SUNDelta[SUNIndex[Glu1], SUNIndex[Glu2]]);


ruleCouplings={gs^2 -> 4*alphas*Pi, e -> gW*sinW, gW^2 -> 4*alphaW*Pi};


res$Check1=Collect2[PaVeLimitTo4[prefLit glglToHH$resFinal//.ruleCouplings/.Thread[Rule[mastersNew,mastersNewPaVe]]]//ToPaVe2,PaVe];


diff=Collect2[ToPaVe2[res$Check1 - litAmp ]/.ChangeDimension[rulePolVec,4],PaVe,Factoring->Function[x,TrickMandelstam[x,{s,t,u,2 mh^2}]]]


knownResult =0;
FCCompareResults[diff,knownResult,
Text->{"\tCompare to Glover and van Der Bij, CERN-TH-4934/87, \
Eqs. 2-3:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



