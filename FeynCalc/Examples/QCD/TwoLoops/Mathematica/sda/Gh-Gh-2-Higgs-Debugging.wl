(* ::Package:: *)

(* :Title: Gh-Gh															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Gh -> Gh, massless QCD, 2-loops								*)

(* ------------------------------------------------------------------------ *)


(* ::Title:: *)
(*QCD ghost self-energy*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gh -> Gh, massless QCD, 2-loops";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[10,0,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


??$ExcludeTopologies


$ExcludeTopologies[SelfEnergies]=FreeQ[ToTree[#1],Centre[2]]&


diags2 = InsertFields[CreateTopologies[2,1 -> 2,ExcludeTopologies->{Tadpoles}], 
	{S[1]} -> {V[5],V[5]}, InsertionLevel ->{Particles},Model->"SMQCD",
	ExcludeParticles->{F[3|4,{1|2}],F[4,{3}],V[1|2|3|4],S[_]}];

Paint[diags2, ColumnsXRows -> {10, 4}, Numbering -> Simple,
	SheetHeader->None,ImageSize->128{10,4}];


ampTest=FCFAConvert[CreateFeynAmp[DiagramExtract[diags2,52..55],PreFactor->-1,GaugeRules->{}],IncomingMomenta->{pH},
	OutgoingMomenta->{q1,q2},LoopMomenta->{k1,k2},List->True,Contract->True,
	TransversePolarizationVectors->{q1,q2}, ChangeDimension->D,
	DropSumOver->True,SMP->True,UndoChiralSplittings->True]//SMPToSymbol;


ampTest//SUNSimplify


diags2 = InsertFields[CreateTopologies[2,1 -> 2,ExcludeTopologies->{Tadpoles,WFCorrections}], 
	{S[1]} -> {V[5],V[5]}, InsertionLevel ->{Particles},Model->"SMQCD",
	ExcludeParticles->{F[3|4,{1|2}],F[4,{3}],V[1|2|3|4],S[_]}];


(* ::Section:: *)
(*Obtain the amplitude*)


ampRaw=FCFAConvert[CreateFeynAmp[diags,PreFactor->-1,GaugeRules->{}],IncomingMomenta->{pH},
	OutgoingMomenta->{q1,q2},LoopMomenta->{k1,k2},List->True,Contract->True,
	TransversePolarizationVectors->{q1,q2}, ChangeDimension->D,
	DropSumOver->True,SMP->True,UndoChiralSplittings->True]//SMPToSymbol;


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
ScalarProduct[q1,q1]=0;
ScalarProduct[q2,q2]=0;
ScalarProduct[pH,pH]=mh^2;
ScalarProduct[q1,q2]=(mh^2)/2;


(* ::Section:: *)
(*Calculate the amplitude*)


AbsoluteTiming[ampSimp=(ampRaw)//Contract//DiracSimplify//SUNSimplify;]


(*FCClearScalarProducts[];
fcTopologies2L=Get["/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Topologies/FCTopologies.m"]/.FCTopology[s_String,rest___]:>FCTopology[ToExpression[s],rest]/.Pair->Hold[Pair];
FCClearScalarProducts[];
ScalarProduct[q1,q1]=0;
ScalarProduct[q2,q2]=0;
ScalarProduct[pH,pH]=mh^2;
ScalarProduct[q1,q2]=(mh^2)/2;
*)


(*{amp,topos}=FCLoopFindTopologies[ampSimp/.{k1->-k1,k2->-k2},{k1,k2},PreferredTopologies->SelectNotFree2[fcTopologies2L,topology1,topology2]/.mqt->mt,FinalSubstitutions->{Hold[SPD][q1]->0,Hold[SPD][q2]->0,Hold[SPD][q1,q2]->1/2*mh^2}];
*)(*FCClearScalarProducts[];
ScalarProduct[q1,q1]=0;
ScalarProduct[q2,q2]=0;
ScalarProduct[pH,pH]=mh^2;
ScalarProduct[q1,q2]=(mh^2)/2;*)


{amp,topos}=FCLoopFindTopologies[ampSimp,{k1,k2}];


(*subtopos=FCLoopFindSubtopologies[topos];*)
mappings=FCLoopFindTopologyMappings[topos(*,PreferredTopologies->subtopos*)];
mappings;


completedTopos=FCLoopBasisFindCompletion[topos];


basisCompletionRules = FCLoopCreateRuleGLIToGLI[completedTopos, List /@ topos] //Flatten;


mappings2={mappings[[1]]/.basisCompletionRules,completedTopos};
(*mappings2=mappings;*)


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amp,topos,FCVerbose->1];]


AbsoluteTiming[ampPreFinal=FCLoopApplyTopologyMappings[ampReduced/.basisCompletionRules,mappings2];]


AbsoluteTiming[ampFinal=(ampPreFinal(*/.GaugeXi[g]->- gxi+ 1*))//DiracSimplify//SUNSimplify;]


(*tablesFjj1=FIREImportResults[topology2,"/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Reductions/topology2/topology2.tables"];
tablesFjj2=FIREImportResults[topology1,"/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Reductions/topology1/topology1.tables"];*)


(*nn1=ampFinal/.Flatten[{tablesFjj1,tablesFjj2}]/.mqt->mt;*)


(*nmappings=FCLoopFindIntegralMappings[Cases2[nn1,GLI],topos,PreferredIntegrals->SelectNotFree[Cases2[nn1,GLI],topology1]];*)


(*nn2=Collect2[nn1/.nmappings[[1]],GLI];*)


(*Collect2[cc1 Collect2[Coefficient[nn2[[1]],gxi],GLI]+ cc2 Collect2[Coefficient[nn2[[2]],gxi],GLI],GLI,Pair]/.cc1+cc2->0*)


FCReloadAddOns[{"FeynHelpers"}];


(*topos*)


FIRECreateConfigFile[completedTopos,FCGetNotebookDirectory[],FIREUseLiteRed->False];


FIREPrepareStartFile[completedTopos,FCGetNotebookDirectory[],FIREUseLiteRed->False];


(*FIRECreateLiteRedFiles[FCGetNotebookDirectory[],completedTopos]*)


FIRECreateStartFile[FCGetNotebookDirectory[],completedTopos]


FIRECreateIntegralFile[Cases2[ampFinal,GLI],completedTopos,FCGetNotebookDirectory[]];


FIRERunReduction[FCGetNotebookDirectory[],completedTopos]


tables=FIREImportResults[completedTopos,FCGetNotebookDirectory[]]//Flatten;


preMasters=Cases2[Last/@tables,GLI];


integralMappings=FCLoopFindIntegralMappings[preMasters,completedTopos];


integralMappings//Last//Length


integralMappings[[1]]


resPreFinal=Collect2[ampFinal//.Dispatch[tables]//.Dispatch[integralMappings[[1]]],GLI,GaugeXi];


gaugeDep=SelectNotFree2[#,GaugeXi]&/@resPreFinal;


yyy1=Collect2[Total[gaugeDep],GaugeXi];


Coefficient[yyy1,GaugeXi[g],2]


Coefficient[yyy1,GaugeXi[g],1]


resPreFinal//Length


Coefficient[yyy1,GaugeXi[g],2]


Collect2[Total[gaugeDep],GaugeXi]//Variables2//InputForm


Collect2[Total[gaugeDep],GaugeXi]
gg=SelectNotFree[%,GaugeXi[g]^2]


Coefficient[gg,GaugeXi[g],2]


resFinal=Collect2[resPreFinal/.integralMappings[[1]],GLI];


ggg1=SelectNotFree2[resFinal[[1]]/.GaugeXi[g]->-gxi+1,gxi];
ggg2=SelectNotFree2[resFinal[[2]]/.GaugeXi[g]->-gxi+1,gxi];


hh1=Collect2[ggg1,GLI];
hh2=Collect2[ggg2,GLI];


completedTopos//FCE//InputForm


Cases2[hh1,GLI]//InputForm


Collect2[cc1 hh2+cc2 hh1,GLI]
%/.(cc1+cc2)->0


jj1=(Collect2[Coefficient[Get["/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Results/debug-s1dia15L2.m"]/.{(lsclSkipDen|lsclDen)[x_]->1/x,
(lsclSkipNum|lsclNum)[x_]->x},GaugeXi,1]/.topology2[inds___]:>GLI[topology2,{inds}],GLI]/.mt->mqt/. {mW->mz cW ,sinW->sW,e->el,SUNDelta[__]->1,lsclFermionLoop[_]->1,
Polarization[x_, -I]->Polarization[x, -I,Transversality->True]})//FeynAmpDenominatorCombine//FeynAmpDenominatorSimplify;


jj1a1=Collect2[jj1/.Flatten[FCLoopCreateRulesToGLI[SelectNotFree[fcTopologies2L,topology2]]]/.GLI->GLIMultiply,GLIMultiply]/.GLIMultiply->GLI;


jj2=(Collect2[Coefficient[Get["/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Results/debug-s1dia16L2.m"]/.{(lsclSkipDen|lsclDen)[x_]->1/x,
(lsclSkipNum|lsclNum)[x_]->x},GaugeXi,1]/.topology1[inds___]:>GLI[topology1,{inds}],GLI]/.mt->mqt/. {mW->mz cW ,sinW->sW,e->el,SUNDelta[__]->1,lsclFermionLoop[_]->1,
Polarization[x_, -I]->Polarization[x, -I,Transversality->True]})//FeynAmpDenominatorCombine//FeynAmpDenominatorSimplify;


jj2a1=Collect2[jj2/.Flatten[FCLoopCreateRulesToGLI[SelectNotFree[fcTopologies2L,topology1]]]/.GLI->GLIMultiply,GLIMultiply]/.GLIMultiply->GLI;


Collect2[jj2old-jj2a1,GLI]











jj1v2=jj1/.Dispatch[Flatten[tablesFjj1]];


jj1av2=jj1a1/.Dispatch[Flatten[tablesFjj1]];


jj2v2=jj2/.Dispatch[Flatten[tablesFjj2]];


jj2av2=jj2a1/.Dispatch[Flatten[tablesFjj2]];


imappings1=FCLoopFindIntegralMappings[Cases2[jj1v2,GLI],fcTopologies2L];


imappings2=FCLoopFindIntegralMappings[Cases2[jj2v2,GLI],fcTopologies2L];


imappings3=FCLoopFindIntegralMappings[Join[imappings1[[2]],imappings2[[2]]],fcTopologies2L,PreferredIntegrals->imappings2[[2]]]


jj1v3=Collect2[jj1v2/.Dispatch[imappings1[[1]]]//.imappings3[[1]],GLI];


jj1av3=Collect2[jj1av2/.Dispatch[imappings1[[1]]]//.imappings3[[1]],GLI];


jj1v3-jj1av3


jj2v3=Collect2[jj2v2/.Dispatch[imappings2[[1]]]//.imappings3[[1]],GLI];


jj2av3=Collect2[jj2av2/.Dispatch[imappings1[[1]]]//.imappings3[[1]],GLI];


Cases2[jj2v3,GLI]
Cases2[jj2av3,GLI]


Collect2[jj2v3-jj2av3,GLI]


Collect2[cc1 jj1v3+cc2 jj2v3,GLI]/.(cc1+cc2)->0


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


vv2=(Collect2[Coefficient[Get["/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Results/s0dia16L2.m"]/.{(lsclSkipDen|lsclDen)[x_]->1/x,
(lsclSkipNum|lsclNum)[x_]->x},GaugeXi,1],topology4]/.mt->mqt/. {mW->mz cW ,sinW->sW,e->el,SUNDelta[__]->1,lsclFermionLoop[_]->1,
Polarization[x_, -I]->Polarization[x, -I,Transversality->True]})//FeynAmpDenominatorCombine//FeynAmpDenominatorSimplify;


vv1=(Coefficient[ampSimp[[1]]/.{k1->-k1,k2->-k2(*,k2->-k2*)}/.GaugeXi[g]->- gxi+1,gxi,1]/.mt->mqt/. {mW->mz cW ,sinW->sW,e->el,SUNDelta[__]->1,lsclFermionLoop[_]->1,
Polarization[x_, -I]->Polarization[x, -I,Transversality->True]})//FeynAmpDenominatorCombine//FeynAmpDenominatorSimplify;


Cases2[vv1,FeynAmpDenominator]
Cases2[vv2,FeynAmpDenominator](*/.k2->k2-k1/.k2->k2+q2/.k1->k1+q1+q2/.k2->k2-q2+q1//FDS*)


Collect2[cc1 vv1-cc2 vv2,FeynAmpDenominator]/.cc1+cc2->0


test1= GaugeXi vv1/.{(*FCI@SPD[k1,q2]->0,*)(*FCI@SPD[k1,q1]->0*)};
test1[[2]]//Length


tr1=FCMultiLoopTID[test1,{k1,k2},ApartFF->False,FDS->False]//Contract;


tr2=(Collect2[Identity[Get["/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Results/s0dia16L2-tr.m"]/.{(lsclSkipDen|lsclDen)[x_]->1/x,
(lsclSkipNum|lsclNum)[x_]->x}],topology4]/. {mW->mz cW ,sinW->sW,e->el,SUNDelta[__]->1,lsclFermionLoop[_]->1,
Polarization[x_, -I]->Polarization[x, -I,Transversality->True]})//FeynAmpDenominatorCombine//FeynAmpDenominatorSimplify;


tr3=(Collect2[Identity[Get["/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Results/s0dia15L2-tr.m"]/.{(lsclSkipDen|lsclDen)[x_]->1/x,
(lsclSkipNum|lsclNum)[x_]->x}],topology4]/. {mW->mz cW ,sinW->sW,e->el,SUNDelta[__]->1,lsclFermionLoop[_]->1,
Polarization[x_, -I]->Polarization[x, -I,Transversality->True]})//FeynAmpDenominatorCombine//FeynAmpDenominatorSimplify;


tr2


tr3


tr2//Length


Collect2[cc1 tr1-cc2 tr2,FeynAmpDenominator]/.(cc1+cc2)->0


vv1=FCMultiLoopTID[FAD[{k1, mqt}]*FAD[{k2, mqt}]*FAD[{k1 - q1 - q2, mqt}]*FAD[k1 - k2, k1 - k2]*FAD[k1 - k2 - q1 - q2, k1 - k2 - q1 - q2]SPD[k1,q1]SPD[k2, q2],{k1,k2},FDS->False,ApartFF->False,FCVerbose->0,
Uncontract->{q1,q2}]//Contract//Simplify


ff=Collect2[FeynAmpDenominatorSimplify[FeynAmpDenominatorCombine[cc1 vv1- cc2 vv2]],Pair,FeynAmpDenominator]/.cc1|cc2->1


.


vv1a=Coefficient[ampSimp[[32]]/.{k1->-k1,k2->-k2}/.GaugeXi[g]->- gxi+1,gxi,1]//Simplify;




vv1T=mt^2 Coefficient[vv1a,mt,2]//.{FCI@SPD[k1]->0,FCI@SPD[k2]->0,FCI@SPD[k1,q1]->0,(*FCI@SPD[k1,q2]->0,*)FCI@SPD[k2,q1|q2]->0,
FCI@SPD[Polarization[q1, -I, Transversality -> True], Polarization[q2, -I, Transversality -> True]]->0,
FCI@SPD[k1|k2, Polarization[q2, -I, Transversality -> True]]->0,
FCI@SPD[k1, Polarization[q1, -I, Transversality -> True]]->0,mh->0
}(*/.FCI@SPD[k1,q2]->FVD[k1,mu]*)


vv2TR=Collect2[Coefficient[Get["/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Results/s0dia5L2-tr.m"]/.{(lsclSkipDen|lsclDen)[x_]->1/x,
(lsclSkipNum|lsclNum)[x_]->x},GaugeXi,2],topology4]//FeynAmpDenominatorSimplify;


vv1a=Coefficient[ampSimp[[3]]/.{k1->-k1,k2->-k2}/.GaugeXi[g]->- gxi+1,gxi,2]//Simplify;
vv1TR=(FCMultiLoopTID[vv1a,{k1,k2},FDS->False,ApartFF->False,FCVerbose->0])//Contract;


diffTR=Collect2[(( cc1 (vv1TR)-cc2(vv2TR/.{(*k1->-k1,k2->-k2*)}))/.mt->mqt/. {mW->mz cW ,sinW->sW,e->el,SUNDelta[__]->1,lsclFermionLoop[_]->1,
Polarization[x_, -I]->Polarization[x, -I,Transversality->True]})//FeynAmpDenominatorCombine//FeynAmpDenominatorSimplify,FeynAmpDenominator];


diffTR


Coefficient[Coefficient[diffTR,cc1,1],mqt,4]-Coefficient[Coefficient[diffTR,cc2,1],mqt,4]//Simplify


Collect2[diffTR,FeynAmpDenominator,mh,Pair,Polarization](*/.cc1+cc2->0*)


Collect2[diffTR,FeynAmpDenominator,Pair,mh,mqt]


vv1TR//Simplify


(*vv2TR=Collect2[Coefficient[Get["/media/Data/Projects/VS/LoopScalla/Projects/MyHiggsProject/Diagrams/HToGlGl/SM/2/Results/s0dia5L2-tr.m"]/.{(lsclSkipDen|lsclDen)[x_]->1/x,
(lsclSkipNum|lsclNum)[x_]->x},GaugeXi,1],topology4]//FeynAmpDenominatorSimplify;*)


vv2TR//Simplify


vv1TR=(FCMultiLoopTID[vv1,{k1,k2},FDS->False,ApartFF->False,FCVerbose->0]FVD[q2,mu])//Contract;


vv1TR





Collect2[diffTR/.qq2->q2,FeynAmpDenominator,Polarization,mh,mqt,D]/.(cc1+cc2)->0


diff=Collect2[(( vv1-cc(vv2))/.mt->mqt/. {mW->mz cW ,sinW->sW,e->el,SUNDelta[__]->1,lsclFermionLoop[_]->1,
Polarization[x_, -I]->Polarization[x, -I,Transversality->True]})//FeynAmpDenominatorCombine//FeynAmpDenominatorSimplify,FeynAmpDenominator]
%/.cc->-1/.dd->-1


diff2=Collect2[diff,FeynAmpDenominator]


Numerator[diff2]


Collect2[diff,mqt]


Collect2[FDS[FCI[(vv2//FeynAmpDenominatorCombine)-cc vv1],k1,k2],Pair]


Cases2[rr,topology4]


Cases2[Coefficient[ampFinal[[1]],gxi,2],GLI]


ruleMasters={
GLI["fctopology1", {0, 1, 1, 0, 1}] -> (-pp)^(1 - 2*ep)*(13/8 + 1/(4*ep) + (115*ep)/16 + (49*ep^2)/2 - (ep*Zeta2)/4 - (13*ep^2*Zeta2)/8 + (9*ep^2*(9/4 - 2*Zeta[3]))/8 - (5*ep^2*Zeta[3])/12), 
 GLI["fctopology1", {1, 1, 0, 1, 1}] -> (2 + ep^(-1) + 4*ep + (16*ep^2)/3 - (ep*Zeta2)/2 - ep^2*Zeta2 + (4*ep^2*(2 - 2*Zeta[3]))/3 + (ep^2*Zeta[3])/3)^2/(-pp)^(2*ep)
}


resEpPre=FCReplaceD[resFinal/.ruleMasters,D->4-2ep]


(* ::Text:: *)
(*To bring our result into the suitable form comparable with the literature, we  must divide it  (1- Zeta2/2 ep^2)^2  and again expand it in ep. This yields a prefactor called eta^2. *)
(*We also factor out the prefactor (-pp)^(-2ep)*)


resEp=Collect2[Series[eta^2/(1- Zeta2/2 ep^2)^2 FCReplaceD[Cancel[resEpPre/(-pp)^(-2ep)],D->4-2ep],{ep,0,0}]//Normal//SUNSimplify,ep,CA,ep]


(* ::Section:: *)
(*Check the final results*)


G2xPaper=(((CA^2*eta^2*gs^4)/(-pp)^(2*ep)) * ((83/16 + 7/32*GaugeXi)/ep + (5/4 + 7/16*GaugeXi - 1/32 GaugeXi^2)/ep^2 + 
599/32 - 3/4 Zeta[3] - 9/64 GaugeXi + 3/8 GaugeXi^2 - 3/16 GaugeXi^2 Zeta[3] )/(4*Pi)^D);


G2qPaper=(((CA*eta^2*gs^4*Tf)/(-pp)^(2*ep))*(-53/8 - 1/(2*ep^2) - 7/(4*ep))/(4*Pi)^D);


G2xPaperFinal=pp FCI@SUNDelta[Glu1,Glu2]Collect2[Series[FCReplaceD[I (4Pi)^D Cancel[(G2xPaper)/(-pp)^(-2ep)],{D->4-2ep}],{ep,0,0}]//Normal//PowerExpand,ep]
G2qPaperFinal=pp FCI@SUNDelta[Glu1,Glu2]Collect2[Series[FCReplaceD[I (4Pi)^D Cancel[(G2qPaper)/(-pp)^(-2ep)],{D->4-2ep}],{ep,0,0}]//Normal//PowerExpand,ep]/.Tf->1/2


resLit=G2xPaperFinal+G2qPaperFinal


FCCompareResults[resLit, resEp, 
     Text -> {"\tCompare to Davydychev, Osland and Tarasov, \
    hep-ph/9801380, Eqs. 6.14-6.15:", "CORRECT.", "WRONG!"}, 
     Interrupt -> {Hold[Quit[1]], Automatic}, 
     Factoring -> Simplify]; 
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], 
     " s."];

