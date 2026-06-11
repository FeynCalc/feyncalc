(* ::Package:: *)

(* :Title: AsyExp-tad2LxFx111x112xxEp999x					               *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary: Asymptotic expansion of a 2L tadpole integral 
			tad2LxFx111x112xxEp999x in m2^2/m1^2.                            *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynHelpers*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Asymptotic expansion of a 2L tadpole integral tad2LxFx111x112xxEp999x in m2^2/m1^2."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadAddOns={"FeynHelpers"};
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Load asy*)


Get["asy21.m"]
SetOptions[QHull,Executable->"/usr/bin/qhull"];


(* ::Section:: *)
(*Define the integral*)


kinematics={}


{int,topo}={GLI[tad2LxFx111x112xxEp999x,{1,1,1}],FCTopology[tad2LxFx111x112xxEp999x,{FAD[{k1,m1}],FAD[{k2,m1}],FAD[{k1-k2,m2}]},{k1,k2},{},{},{}]}
id=topo[[1]]


lmoms=topo[[3]]


(* ::Text:: *)
(*We will use the variable la for our asymptotic expansion*)


DataType[la,FCVariable]=True;


(* ::Text:: *)
(*The expansion will be done up to 4th order in m1/m2*)


laPower=4;


(* ::Text:: *)
(*We will put the reductions to this directory*)


fireFCDir=FileNameJoin[{NotebookDirectory[],"FIRE"}];


Quiet[CreateDirectory[FileNameJoin[{fireFCDir,"FinalMasters"}]]];


Quiet[CreateDirectory[FileNameJoin[{ParentDirectory[fireFCDir],"FinalExpressions",ToString@FCLoopGLIToSymbol[int]}]]];


(* ::Section:: *)
(*Visualize the integral (if possible, optional)*)


SetOptions[FCLoopGraphPlot, GraphPlot-> {MultiedgeStyle->0.35,Frame->True},Style->{
{"InternalLine",_,_,mm_/;!FreeQ[mm,m1]}->{Black,Thick},
{"InternalLine",_,_,mm_/;!FreeQ[mm,m2]}->{Blue,Thick}
}];


FCLoopGraphPlot[FCLoopIntegralToGraph[int,topo]]


(* ::Section:: *)
(*Use asy to reveal the contributing regions*)


propsFC=FCLoopIntegralToPropagators[FCLoopFromGLI[int,topo],topo[[3]],Negative->True]
propsAsy=FCE[FeynAmpDenominatorExplicit[%,Head->Function[x,1/x]]]/.SPD->Times


asyOutput=AlphaRepExpand[lmoms,propsAsy,{},{m1->x^0,m2->x^1},Verbose->False]
asyRescaling=Abs[(Min/@asyOutput)];
asyFinal=MapThread[(#1+#2)&,{asyOutput,asyRescaling}]


(* ::Text:: *)
(*To be able to expand in 4-momenta and masses we need to shift the loop momentum accordingly in each region*)


(* ::Text:: *)
(*k1~m1,k2~m2*)


regN=1;
scaling[regN]={m1->la^0 m1, m2->la^1 m2, k1->la^0 k1,k2->la^1 k2}
momShifts[regN]={k2->k1+k2};
propsR[regN]=ReplaceAll[{propsFC,propsAsy},momShifts[regN]];
topoR[regN]=FCReplaceMomenta[topo,momShifts[regN]];
{ExpandAll[(1/propsR[regN][[2]])],asyFinal}


(* ::Text:: *)
(*k1,k2~m1*)


regN=2;
scaling[regN]={m1->la^0 m1, m2->la^1 m2, k1->la^0 k1,k2->la^0 k2}
momShifts[regN]={};
propsR[regN]=ReplaceAll[{propsFC,propsAsy},momShifts[regN]];
topoR[regN]=FCReplaceMomenta[topo,momShifts[regN]];
{ExpandAll[(1/propsR[regN][[2]])],asyFinal}


(* ::Section:: *)
(*Define the topologies for each region*)


ClearAll[topoWithScaling];
Table[topoWithScaling[id,i]=FCLoopAddScalingParameter[topoR[i],la,scaling[i]],{i,1,2}]


(* ::Section:: *)
(*Rules for eliminating eikonal propagators*)


fromGFADRules={}


(* ::Section:: *)
(*Region 1*)


nRegion=1;


intExpanded[id,nRegion,1]=FCLoopGLIExpand[ int,{topoWithScaling[id,nRegion]},{la,0,laPower}]


intExpanded[id,nRegion,2]=FromGFAD[FCLoopFromGLI[Sequence@@intExpanded[id,nRegion,1]]/.x_FeynAmpDenominator/;FreeQ2[x,lmoms]:>FeynAmpDenominatorExplicit[x],
InitialSubstitutions->fromGFADRules]


intExpanded[id,nRegion,3]=FCLoopFindTopologies[intExpanded[id,nRegion,2],lmoms,
Names->"asyR"<>ToString[nRegion]<>ToString[FCLoopGLIToSymbol[int]]<>"NAux",
FCVerbose->-1,FinalSubstitutions->kinematics,FCLoopBasisOverdeterminedQ->True]


intExpanded[id,nRegion,4]=FCLoopRewriteOverdeterminedTopologies[intExpanded[id,nRegion,3],
Names->"asyR"<>ToString[nRegion]<>ToString[FCLoopGLIToSymbol[int]]<>"Pfr"]


intExpanded[id,nRegion,5]=FCLoopRewriteIncompleteTopologies[intExpanded[id,nRegion,4],Method->ScalarProduct,
Names->"asyR"<>ToString[nRegion]<>ToString[FCLoopGLIToSymbol[int]]<>"C"]


intExpanded[id,nRegion,6]=FCLoopTensorReduce[intExpanded[id,nRegion,5]]


topoMappings[id,nRegion]=FCLoopFindTopologyMappings[intExpanded[id,nRegion,5][[2]]]


intExpanded[id,nRegion,7]=FCLoopApplyTopologyMappings[intExpanded[id,nRegion,6],topoMappings[id,nRegion]]


FIREPrepareStartFile[#,fireFCDir]&/@Last[topoMappings[id,nRegion]]
FIRECreateConfigFile[#,fireFCDir]&/@Last[topoMappings[id,nRegion]]
FIRECreateIntegralFile[intExpanded[id,nRegion,7],Last[topoMappings[id,nRegion]],fireFCDir]


FIRECreateLiteRedFiles[fireFCDir,Last[topoMappings[id,nRegion]]]


FIRECreateStartFile[fireFCDir,Last[topoMappings[id,nRegion]]]


FIRERunReduction[fireFCDir,Last[topoMappings[id,nRegion]]]


ibpRules[id,nRegion]=FIREImportResults[Last[topoMappings[id,nRegion]],fireFCDir]//Flatten;


Cases2[Last/@ibpRules[id,nRegion],GLI]
mappings[id,nRegion]=FCLoopFindIntegralMappings[%,Last[topoMappings[id,nRegion]]]


Put[{mappings[id,nRegion][[2]],Last[topoMappings[id,nRegion]]},FileNameJoin[{fireFCDir,"FinalMasters",ToString[FCLoopGLIToSymbol[int]]<>"R"<>ToString[nRegion]<>".m"}]]


res[id,nRegion]=Collect2[intExpanded[id,nRegion,7]/.ibpRules[id,nRegion]/.mappings[id,nRegion][[1]],GLI,la]


Put[{res[id,nRegion],Last[topoMappings[id,nRegion]]},
FileNameJoin[{ParentDirectory[fireFCDir],"FinalExpressions",ToString@FCLoopGLIToSymbol[int],ToString[FCLoopGLIToSymbol[int]]<>"R"<>ToString[nRegion]<>".m"}]]


(* ::Section:: *)
(*Region 2*)


nRegion=2;


intExpanded[id,nRegion,1]=FCLoopGLIExpand[ int,{topoWithScaling[id,nRegion]},{la,0,laPower}]


intExpanded[id,nRegion,2]=FromGFAD[FCLoopFromGLI[Sequence@@intExpanded[id,nRegion,1]]/.x_FeynAmpDenominator/;FreeQ2[x,lmoms]:>FeynAmpDenominatorExplicit[x],
InitialSubstitutions->fromGFADRules]


intExpanded[id,nRegion,3]=FCLoopFindTopologies[intExpanded[id,nRegion,2],lmoms,
Names->"asyR"<>ToString[nRegion]<>ToString[FCLoopGLIToSymbol[int]]<>"NAux",
FCVerbose->-1,FinalSubstitutions->kinematics,FCLoopBasisOverdeterminedQ->True]


intExpanded[id,nRegion,4]=FCLoopRewriteOverdeterminedTopologies[intExpanded[id,nRegion,3],
Names->"asyR"<>ToString[nRegion]<>ToString[FCLoopGLIToSymbol[int]]<>"Pfr"]


intExpanded[id,nRegion,5]=FCLoopRewriteIncompleteTopologies[intExpanded[id,nRegion,4],Method->ScalarProduct,
Names->"asyR"<>ToString[nRegion]<>ToString[FCLoopGLIToSymbol[int]]<>"C"]


intExpanded[id,nRegion,6]=FCLoopTensorReduce[intExpanded[id,nRegion,5]]


topoMappings[id,nRegion]=FCLoopFindTopologyMappings[intExpanded[id,nRegion,5][[2]]]


intExpanded[id,nRegion,7]=FCLoopApplyTopologyMappings[intExpanded[id,nRegion,6],topoMappings[id,nRegion]]


FIREPrepareStartFile[#,fireFCDir]&/@Last[topoMappings[id,nRegion]]
FIRECreateConfigFile[#,fireFCDir]&/@Last[topoMappings[id,nRegion]]
FIRECreateIntegralFile[intExpanded[id,nRegion,7],Last[topoMappings[id,nRegion]],fireFCDir]


FIRECreateLiteRedFiles[fireFCDir,Last[topoMappings[id,nRegion]]]


FIRECreateStartFile[fireFCDir,Last[topoMappings[id,nRegion]]]


FIRERunReduction[fireFCDir,Last[topoMappings[id,nRegion]]]


ibpRules[id,nRegion]=FIREImportResults[Last[topoMappings[id,nRegion]],fireFCDir]//Flatten;


Cases2[Last/@ibpRules[id,nRegion],GLI]
mappings[id,nRegion]=FCLoopFindIntegralMappings[%,Last[topoMappings[id,nRegion]]]


Put[{mappings[id,nRegion][[2]],Last[topoMappings[id,nRegion]]},FileNameJoin[{fireFCDir,"FinalMasters",ToString[FCLoopGLIToSymbol[int]]<>"R"<>ToString[nRegion]<>".m"}]]


res[id,nRegion]=Collect2[intExpanded[id,nRegion,7]/.ibpRules[id,nRegion]/.mappings[id,nRegion][[1]],GLI,la]


Put[{res[id,nRegion],Last[topoMappings[id,nRegion]]},
FileNameJoin[{ParentDirectory[fireFCDir],"FinalExpressions",ToString@FCLoopGLIToSymbol[int],ToString[FCLoopGLIToSymbol[int]]<>"R"<>ToString[nRegion]<>".m"}]]


(* ::Section:: *)
(*Putting everything together*)


ClearAll[regions,finalExpr,asyTopos,mappingRules,presentGLIs]


regions=Get/@FileNames["*.m",FileNameJoin[{ParentDirectory[fireFCDir],"FinalExpressions",ToString@FCLoopGLIToSymbol[int]}]];


{finalExpr,asyTopos}=Transpose[regions];
asyTopos=Flatten[asyTopos];
finalExpr=Total[finalExpr];


mappingRules=FCLoopFindIntegralMappings[Cases2[finalExpr,GLI],asyTopos]


presentGLIs=Cases2[finalExpr/.mappingRules[[1]],GLI]


intsToGraphs=FCLoopIntegralToGraph[presentGLIs,asyTopos]
intPlots=MapThread[FCLoopGraphPlot[#1,GraphPlot->{MultiedgeStyle->0.35,Frame->True,FrameLabel->Style[#2,FontSize->7.5,FontWeight->Bold]}]&,{intsToGraphs,presentGLIs}];
Magnify[Grid[Partition[intPlots,UpTo[4]]],1.5]


{knownMasters,miTopos}=Transpose[{
Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]],
Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad1LxFx1x1xxEp999x.m"}]]/.m1->m2/.tad1LxFx1x1xxEp999x->tad1LxFx1x2xxEp999x,
Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad2LxFx111x111xxEp1x.m"}]],
Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad2LxFx111x110xxEp999x.m"}]],
Get[FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles","tad2LxFx111x100xxEp999x.m"}]]

}];
miTopos=Flatten[miTopos];
knownMasters=Flatten[knownMasters];


finalMappingRules=FCLoopFindIntegralMappings[Join[presentGLIs,Cases2[knownMasters,GLI]],Join[asyTopos,miTopos],PreferredIntegrals->
Join[Cases2[knownMasters,GLI],{GLI[tad1LxFx1x1xxEp999x, {1}]GLI[tad1LxFx1x1xxEp999x, {1}],
GLI[tad1LxFx1x1xxEp999x, {1}]GLI[tad1LxFx1x2xxEp999x, {1}],GLI[tad1LxFx1x2xxEp999x, {1}]GLI[tad1LxFx1x2xxEp999x, {1}]}]]


rawAnalyticResult=FCReplaceD[finalExpr/.mappingRules[[1]]/.finalMappingRules[[1]]//.knownMasters,D->4-2ep]


resultExpanded1=Series[rawAnalyticResult/.la->1,{ep,0,1}]//Normal;


resultExpanded2=resultExpanded1//ReplaceAll[#,{Log[m_^2]->2Log[m]}]&//FunctionExpand;


resultExpanded3=Series[resultExpanded2/.{m2->la m2},{la,0,laPower}]//Normal//ReplaceAll[#,la->1]&;


resultFinal=Collect2[resultExpanded3,ep,m1,m2,Factoring->FullSimplify]//FCLoopAddMissingHigherOrdersWarning[#,ep,epHelp]&//FCLoopAddMissingHigherOrdersWarning[#,m2,asyHelp]&


newId=ToExpression[StringReplace[ToString[id],{"F"->"Am2m1o4","Ep999"->"Ep1"}]]


Put[{int->resultFinal,{topo}}/.id->newId,FileNameJoin[{$FeynCalcDirectory,"Examples","MasterIntegrals","Tadpoles",ToString[newId]<>".m"}]]


files=FSACreateMathematicaScripts[int,FCLoopSwitchEtaSign[topo,-1],FileNameJoin[{NotebookDirectory[], "FIESTA"}],
FSAParameterRules -> {m1 -> 7.},OverwriteTarget->True,FSASDExpandAsy->True,FSASDExpandAsyOrder->4,FSAOrderInEps->1,
FSAExpandVar->m2,FSAIntegratorOptions -> {{"maxeval", "100000"}, {"epsrel", "1.000000E-05"}, {"epsabs", "1.000000E-12"}, {"integralTransform", "korobov"}}]


FSARunIntegration[files[[1]]]


v1=Collect2[FSALoadNumericalResults[files],ep,m1]
v2=Collect2[Collect2[ExpandAll[resultFinal/.la->1/. { m1 -> 7.}],ep]//ExpandAll//Chop[#,10^-5]&,ep,m2]/.asyHelp|epHelp->0


(*v1=(0.5*(98. + 1.*m2^2))/ep^2 - (2.*(117.199194605 - 0.75*m2^2 + 1.*m2^2*Log[m2]))/ep - 
 0.00680272*(-112269.88535468167 + 2210.043360596938*m2^2 - 3.2792441846790696*m2^4 - 850.1953042312487*m2^2*Log[m2] + 1.*m2^4*Log[m2] - 
   294.00004704000753*m2^2*Log[m2]^2) + 0.00680272*ep*(-258396.3389026742 + 9634.826237152198*m2^2 - 21.347534515605524*m2^4 - 
   1859.9167362466778*m2^2*Log[m2] + 2.891819742691159*m2^4*Log[m2] - 850.1953042312487*m2^2*Log[m2]^2 + 1.*m2^4*Log[m2]^2 - 
   196.00003087000496*m2^2*Log[m2]^3 - 1.4700002352000376*^-6*m2^4*pm[79])*)


FCCompareNumbers[v1,v2,DigitCount->2,Chop->10^-6]



