(* ::Package:: *)

(* :Title: Gl-Gl															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Gl -> Gl, massless QCD, 2-loops								*)

(* ------------------------------------------------------------------------ *)


(* ::Title:: *)
(*QCD gluon self-energy*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gl -> Gl, massless QCD, 2-loops";
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


diags=InsertFields[CreateTopologies[2, 1 -> 1,ExcludeTopologies -> {Tadpoles}], {V[5]} -> {V[5]},
InsertionLevel -> {Particles}, ExcludeParticles->{V[1|2|3],S[_],F[1|4],F[3,{2|3}]},Model->SMQCD];


Paint[DiagramExtract[diags,{2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,39}],ColumnsXRows->{4,1},SheetHeader -> False,   
Numbering -> True,ImageSize->{1024,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


ampRaw = FCFAConvert[CreateFeynAmp[DiagramExtract[diags,{2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20,39}], Truncated -> True,
	PreFactor->1], IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{q1,q2},
	UndoChiralSplittings->True, ChangeDimension->D, List->True, SMP->True,
	DropSumOver->True]//SMPToSymbol;


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
ScalarProduct[p,p]=pp;


(* ::Section:: *)
(*Calculate the amplitude*)


projector=MTD[Lor1,Lor2]1/((D-1)SPD[p])1/(2CA CF)SUNDelta[Glu1,Glu2]


AbsoluteTiming[ampSimp=(projector ampRaw/.mu->0)//Contract//DiracSimplify//SUNSimplify;]


(* ::Section:: *)
(*Identify and minimize the topologies*)


{amp,topos}=FCLoopFindTopologies[ampSimp,{q1,q2}];


subtopos=FCLoopFindSubtopologies[topos];


mappings=FCLoopFindTopologyMappings[topos,PreferredTopologies->subtopos];


(* ::Section:: *)
(*Rewrite the amplitude in terms of GLIs*)


AbsoluteTiming[ampReduced=FCLoopTensorReduce[amp,topos];]


AbsoluteTiming[ampPreFinal=FCLoopApplyTopologyMappings[ampReduced,mappings];]


AbsoluteTiming[ampFinal=ampPreFinal//DiracSimplify//SUNSimplify;]


(*FCReloadAddOns[{"FeynHelpers"}];
FIREPrepareStartFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateStartFile[FCGetNotebookDirectory[],mappings[[2]]]
FIRECreateConfigFile[mappings[[2]],FCGetNotebookDirectory[]]
FIRECreateIntegralFile[Cases2[ampPreFinal,GLI],mappings[[2]],FCGetNotebookDirectory[]]
FIRERunReduction[FCGetNotebookDirectory[],mappings[[2]]]
tables=FIREImportResults[mappings[[2]],FCGetNotebookDirectory[]]//Flatten;
Put[tables,FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-Gl-Gl.m"}]];*)


reductionTable=Get[FileNameJoin[{FCGetNotebookDirectory[],"ReductionTable-Gl-Gl.m"}]];


resPreFinal=Collect2[Total[ampFinal/.reductionTable],GLI]


integralMappings=FCLoopFindIntegralMappings[Cases2[resPreFinal,GLI],mappings[[2]]]


resFinal=Collect2[resPreFinal/.integralMappings[[1]],GLI]


(*
aux1=FCFeynmanParametrize[integralMappings[[2]][[1]],mappings[[2]],Names->x,FCReplaceD->{D->4-2ep}]
preInt1=Integrate[aux1[[1]]/.x[1]->1,{x[2],0,Infinity},Assumptions->{ep>0,x[3]>=0,pp>0,ep<1}]
preInt2=Integrate[preInt1,{x[3],0,Infinity},Assumptions->{ep>0,pp>0,ep<1}]
int2L=SelectNotFree[preInt2,pp](Series[E^(2ep*EulerGamma)SelectFree[preInt2,pp]aux1[[2]],{ep,0,2}]//Normal//SimplifyPolyLog)
*)


(*
aux1=FCFeynmanParametrize[SFAD[q1,q1+p],{q1},Names->x,FCReplaceD->{D->4-2ep}]
preInt1=Integrate[aux1[[1]]/.x[1]->1-x[2],{x[2],0,1},Assumptions->{ep>0,pp>0,ep<1}]
int1L=SelectNotFree[preInt1,pp](Series[E^(ep*EulerGamma)SelectFree[preInt1,pp]aux1[[2]],{ep,0,2}]//Normal//SimplifyPolyLog)
*)


(*ruleMasters=Thread[Rule[integralMappings[[2]],{ int2L, int1L^2}]]*)


ruleMasters={
GLI["fctopology1", {0, 1, 1, 0, 1}] -> (-pp)^(1 - 2*ep)*(13/8 + 1/(4*ep) + (115*ep)/16 + (49*ep^2)/2 - (ep*Zeta2)/4 - (13*ep^2*Zeta2)/8 + (9*ep^2*(9/4 - 2*Zeta[3]))/8 - (5*ep^2*Zeta[3])/12), 
 GLI["fctopology1", {1, 1, 0, 1, 1}] -> (2 + ep^(-1) + 4*ep + (16*ep^2)/3 - (ep*Zeta2)/2 - ep^2*Zeta2 + (4*ep^2*(2 - 2*Zeta[3]))/3 + (ep^2*Zeta[3])/3)^2/(-pp)^(2*ep)
}


resEpPre=FCReplaceD[resFinal/.ruleMasters,D->4-2ep]


resEp=Collect2[Series[FCReplaceD[Cancel[resEpPre/(-pp)^(-2ep)],D->4-2ep],{ep,0,0}]//Normal//SUNSimplify[#,SUNNToCACF->False]&,ep]


(* ::Section:: *)
(*Check the final results*)


funJ[2,xi]=(1-1/12 Pi^2 ep^2)^2(CA^2 gs^4 (1/ep^2 (-25/12+5/24 xi + 1/4 xi^2)+1/ep (-583/72+113/144xi-19/24 xi^2 +3/8 xi^3)-14311/432+Zeta[3]+425/864 xi+ 2 xi Zeta[3]-71/72 xi^2+9/16 xi^3+1/16xi^4));
funJ[2,q]=(1-1/12 Pi^2 ep^2)^2(CA Tf gs^4 (1/ep^2 (5/3-2/3xi) + 1/ep (101/18+8/9xi - 2/3 xi^2) + 1961/108 + 8 Zeta[3]+142/27 xi-22/9 xi^2 )+CF Tf gs^4 (2/ep+55/3-16 Zeta[3]));


resLit=Collect2[Series[ I(funJ[2,xi]+funJ[2,q])/.{xi->0,Tf->1/2},{ep,0,0}]//Normal//SimplifyPolyLog//SUNSimplify[#,SUNNToCACF->False]&,ep]


FCCompareResults[resLit, resEp, 
     Text -> {"\tCompare to Davydychev, Osland and Tarasov, \
    hep-ph/9801380, Eqs. 6.10-6.11:", "CORRECT.", "WRONG!"}, 
     Interrupt -> {Hold[Quit[1]], Automatic}, 
     Factoring -> Simplify]; 
Print["\tCPU Time used: ", Round[N[TimeUsed[], 4], 0.001], 
     " s."];



