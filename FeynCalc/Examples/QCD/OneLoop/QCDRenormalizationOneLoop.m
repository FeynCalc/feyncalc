(* ::Package:: *)

(* :Title: QCDRenormalizationOneLoop                                       *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the QCD renormalization constants at 1-loop  *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*QCD Renormalization at  1-loop*)


(* ::Text:: *)
(*This example uses custom QCD model created with FeynRules. Please evaluate the file*)
(*FeynCalc/Examples/FeynRules/QCD/GenerateModelQCD.m before running it for the first time*)


(* ::Subsection:: *)
(*Load FeynCalc, FeynArts and FeynHelpers*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the QCD renormalization constants at 1-loop"];
];
$LoadFeynArts= True;
<< FeynCalc`
$FAVerbose = 0;


(* ::Subsection::Closed:: *)
(*Important options*)


FAPatch[PatchModelsOnly->True];
$KeepLogDivergentScalelessIntegrals=True;


(* ::Subsection:: *)
(*Generate the diagrams*)


params={InsertionLevel->{Particles},Model -> FileNameJoin[{"QCD","QCD"}],
GenericModel -> FileNameJoin[{"QCD","QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}};
top[i_,j_]:=CreateTopologies[1, i -> j,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}];
topCT[i_,j_]:=CreateCTTopologies[1, i ->j,
ExcludeTopologies ->{Tadpoles,WFCorrections,WFCorrectionCTs}];

{diagQuarkSE,diagQuarkSECT} = InsertFields[#, {F[3,{1}]} -> {F[3,{1}]},
Sequence@@params]&/@{top[1,1],topCT[1,1]};
{diagGluonSE,diagGluonSECT} = InsertFields[#, {V[5]} -> {V[5]},
Sequence@@params]&/@{top[1,1],topCT[1,1]};
{diagGhostSE,diagGhostSECT} = InsertFields[#, {U[5]} -> {U[5]},
Sequence@@params]&/@{top[1,1],topCT[1,1]};
{diagVertex,diagVertexCT} = InsertFields[#,  {F[3,{1}],V[5]}->{F[3,{1}]},
Sequence@@params]&/@{top[2,1],topCT[2,1]};


Paint[diagQuarkSE, ColumnsXRows -> {1, 1},SheetHeader->None,
Numbering -> None,ImageSize->{256,256}];
Paint[diagQuarkSECT, ColumnsXRows -> {1, 1},SheetHeader->None,
Numbering -> None,ImageSize->{256,256}];
Paint[diagGluonSE, ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> None,ImageSize->{1024,256}];
Paint[diagGluonSECT, ColumnsXRows -> {1, 1},SheetHeader->None,
Numbering -> None,ImageSize->{256,256}];
Paint[diagGhostSE, ColumnsXRows -> {1, 1},SheetHeader->None,
Numbering -> None,ImageSize->{256,256}];
Paint[diagGhostSECT, ColumnsXRows -> {1, 1},SheetHeader->None,
Numbering -> None,ImageSize->{256,256}];
Paint[diagVertex, ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> None,ImageSize->{512,256}];
Paint[diagVertexCT, ColumnsXRows -> {1, 1},SheetHeader->None,
Numbering -> None,ImageSize->{256,256}];


(* ::Subsection:: *)
(*Quark self-energy*)


(* ::Text:: *)
(*First of all we need to generate the amplitudes and convert them into FeynCalc notation. We choose l to be the loop momentum and p the external momentum.*)


{ampQuarkSE,ampQuarkSECT}=Contract[FCFAConvert[CreateFeynAmp[#, Truncated -> True,PreFactor->1,
GaugeRules->{}],IncomingMomenta->{p}, OutgoingMomenta->{p},LoopMomenta->{l},DropSumOver->True,
UndoChiralSplittings->True,ChangeDimension->D,List->False,SMP->True, FinalSubstitutions->{Zm->SMP["Z_m"],
Zpsi->SMP["Z_psi"],SMP["g_s"]->Sqrt[4Pi SMP["alpha_s"]]}]]&/@{diagQuarkSE,diagQuarkSECT}


(* ::Text:: *)
(*Tensor reduction allows us to express the electron self-energy in tems of the Passarino-Veltman coefficient functions.*)


ampQuarkSE1=ampQuarkSE//SUNSimplify//TID[#,l,UsePaVeBasis->True,ToPaVe->True]&


(* ::Text:: *)
(*With PaVeUVPart we obtain the full analytic result for the UV part of the self-energy diagram*)


ampQuarkSE2=PaVeUVPart[ampQuarkSE1,Prefactor->1/(2Pi)^D]//FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,SMP["Delta"]]&


(* ::Text:: *)
(*This is the amplitude for the self-energy counter-term*)


ampQuarkSECT2=ampQuarkSECT//ReplaceAll[#,{SMP["Z_psi"]->1+alpha SMP["d_psi"],
SMP["Z_m"]->1+alpha SMP["d_m"]}]&//Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*Now we add the 1-loop SE diagram and the SE counter-term and discard all the finite pieces*)


tmp[1]=SelectNotFree2[(ampQuarkSECT2+ampQuarkSE2),{SMP["Delta"],SMP["d_m"],SMP["d_psi"]}]


(* ::Text:: *)
(*Equating our result to zero and solving for d_psi and d_m we obtain  the renormalization constants in the minimal subtraction schemes.*)


sol[1]=Solve[SelectNotFree2[tmp[1], DiracGamma]==0,SMP["d_psi"]]//Flatten//Simplify;
sol[2]=Solve[(SelectFree2[tmp[1], DiracGamma]==0)/.sol[1],SMP["d_m"]]//Flatten//Simplify;
solMS=Join[sol[1],sol[2]]/.{SMP["d_psi"]->SMP["d_psi^MS"],SMP["d_m"]->SMP["d_m^MS"],SMP["Delta"]->1/Epsilon}
solMSbar=Join[sol[1],sol[2]]/.{SMP["d_psi"]->SMP["d_psi^MSbar"],SMP["d_m"]->SMP["d_m^MSbar"]}


(* ::Subsection:: *)
(*Gluon self-energy*)


(* ::Text:: *)
(*Again, first we obtain the amplitudes*)


{ampGluonSE0,ampGluonSECT0}=FCFAConvert[CreateFeynAmp[#, Truncated -> True,
PreFactor->1,GaugeRules->{}],IncomingMomenta->{q},OutgoingMomenta->{q},LoopMomenta->{l},
DropSumOver->True,UndoChiralSplittings->True,ChangeDimension->D,List->True,SMP->True,
FinalSubstitutions->{ZA->SMP["Z_A"],Zpsi->SMP["Z_psi"],Zxi->SMP["Z_xi"],
SMP["g_s"]->Sqrt[4Pi SMP["alpha_s"]]}]&/@{diagGluonSE,diagGluonSECT};


(* ::Text:: *)
(*Here we introduce the number of flavors in the quark loop*)


ampGluonSE=Total[{ampGluonSE0[[1]],SMP["N_F"]ampGluonSE0[[2]],ampGluonSE0[[3]],ampGluonSE0[[4]]}]//Contract//FCTraceFactor;
ampGluonSECT=Total[ampGluonSECT0]//Contract//FCTraceFactor;


(* ::Text:: *)
(*then we do the tensor decomposition*)


ampGluonSE1=ampGluonSE//SUNSimplify//TID[#,l,UsePaVeBasis->True,ToPaVe->True]&


(* ::Text:: *)
(*and finally obtain the explicit results.*)


ampGluonSE2=PaVeUVPart[ampGluonSE1,Prefactor->1/(2Pi)^D]//FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,SMP["Delta"]]&


(* ::Text:: *)
(*This is the counter-term amplitude*)


ampGluonSECT2=ampGluonSECT//ReplaceRepeated[#,{SMP["Z_xi"]->SMP["Z_A"],
SMP["Z_A"]->1+alpha SMP["d_A"]}]&//Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*Now we add the 1-loop SE diagram and the SE counter-term and discard all the finite pieces*)


tmp[4]=(ampGluonSECT2+ampGluonSE2)//SelectNotFree2[#,{SMP["Delta"],SMP["d_A"]}]&//Simplify


(* ::Text:: *)
(*Equating this to zero and solving for d_A we obtain the wave-function renormalization constant for the gluon in the minimal subtraction schemes.*)


sol[3]=Solve[tmp[4]==0,SMP["d_A"]]//Flatten;
tmp[5]=sol[3]/.{SMP["d_A"]->SMP["d_A^MS"],SMP["Delta"]->1/Epsilon}
solMS=Union[Join[solMS,tmp[5]]];
tmp[6]=sol[3]/.{SMP["d_A"]->SMP["d_A^MSbar"]}
solMSbar=Union[Join[solMSbar,tmp[6]]];


((SMP["d_A^MS"]/(SMP["alpha_s"]/(4Pi Epsilon)))/. solMS)//Expand


CA(5/3+1/2(1-GaugeXi["G"]))//Expand


sol[3]


(* ::Subsection:: *)
(*Ghost self-energy*)


(* ::Text:: *)
(*Again, first we obtain the amplitudes*)


{ampGhostSE,ampGhostSECT}=Contract[FCFAConvert[CreateFeynAmp[#, Truncated -> True,
PreFactor->1,GaugeRules->{}],IncomingMomenta->{q},OutgoingMomenta->{q},LoopMomenta->{l},
DropSumOver->True,UndoChiralSplittings->True,ChangeDimension->D,List->False,SMP->True,
FinalSubstitutions->{Zu->SMP["Z_u"],SMP["g_s"]->Sqrt[4Pi SMP["alpha_s"]]}]&/@{diagGhostSE,diagGhostSECT}]


(* ::Text:: *)
(*then we do the tensor decomposition*)


ampGhostSE1=ampGhostSE//SUNSimplify//TID[#,l,UsePaVeBasis->True,ToPaVe->True]&


(* ::Text:: *)
(*and finally obtain the explicit results.*)


ampGhostSE2=PaVeUVPart[ampGhostSE1,Prefactor->1/(2Pi)^D]//FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,SMP["Delta"]]&


(* ::Text:: *)
(*This is the counter-term amplitude*)


ampGhostSECT2=ampGhostSECT//ReplaceRepeated[#,{SMP["Z_u"]->1+alpha SMP["d_u"]}]&//
Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*Now we add the 1-loop SE diagram and the SE counter-term and discard all the finite pieces*)


tmp[4]=(ampGhostSECT2+ampGhostSE2)//SelectNotFree2[#,{SMP["Delta"],SMP["d_u"]}]&//Simplify


(* ::Text:: *)
(*Equating this to zero and solving for d_u we obtain the wave-function renormalization constant for the ghost in the minimal subtraction schemes.*)


sol[3]=Solve[tmp[4]==0,SMP["d_u"]]//Flatten;
tmp[5]=sol[3]/.{SMP["d_u"]->SMP["d_u^MS"],SMP["Delta"]->1/Epsilon}
solMS=Union[Join[solMS,tmp[5]]];
tmp[6]=sol[3]/.{SMP["d_u"]->SMP["d_u^MSbar"]}
solMSbar=Union[Join[solMSbar,tmp[6]]];


(* ::Subsection::Closed:: *)
(*Quark-gluon vertex*)


(* ::Text:: *)
(*The last pieces are the two vertex diagrams*)


{ampVertex,ampVertexCT}=Contract[FCFAConvert[CreateFeynAmp[#, Truncated -> True,PreFactor->1,
GaugeRules->{}],IncomingMomenta->{p1,k},OutgoingMomenta->{p2},LoopMomenta->{l},DropSumOver->True,
UndoChiralSplittings->True,ChangeDimension->D,List->False,SMP->True,
FinalSubstitutions->{k->p2-p1,ZA->SMP["Z_A"],Zpsi->SMP["Z_psi"],
Zg->SMP["Z_g"]}]&/@{diagVertex,diagVertexCT}]//ReplaceAll[#,SMP["g_s"]^3->4Pi SMP["alpha_s"]SMP["g_s"]]&


(* ::Text:: *)
(*The result of the tensor reduction is quite large, since we keep the full gauge dependence and do not specify the kinematics*)


ampVertex1=ampVertex//TID[#,l,ToPaVe->True,UsePaVeBasis->True]&;//AbsoluteTiming


(* ::Text:: *)
(*As we are interested in the UV piece only, there is no need to obtain the full analytic result*)


tmp[10]=PaVeUVPart[ampVertex1,Prefactor->1/(2Pi)^D]//FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
SelectNotFree2[#,Epsilon]&//SUNFSimplify//
ReplaceAll[#,SUNTF[li_List,__SUNFIndex]:>SUNT[Sequence@@li]]&//SUNSimplify


(* ::Text:: *)
(*This is the amplitude from the counter-term vertex*)


ampVertexCT


ampVertexCT2=(Normal[Series[ampVertexCT/.{SMP["Z_A"]->1+alpha SMP["d_A"],
SMP["Z_g"]->1+alpha SMP["d_g"],SMP["Z_psi"]->1+alpha SMP["d_psi"]},{alpha,0,1}]]/.alpha->1)//
ReplaceAll[#,SUNTF[li_List,__SUNFIndex]:>SUNT[Sequence@@li]]&


(* ::Text:: *)
(*Adding both amplitudes and equating them to zero gives*)


tmp[11]=(Cancel[(tmp[10]+ampVertexCT2)/(-FCI [I SMP["g_s"] GAD[Lor1]])]//Simplify)==0


(* ::Text:: *)
(*After plugging in d_Psi and d_m that were determined previously, we can confirm Ward's identity which fixes the relation between d_A and d_e*)


(SUNSimplify[Simplify[tmp[10]]]/.GaugeXi["G"]->1)//SUNSimplify


SUNSimplify[Simplify[tmp[11]/.{SMP["d_psi"]->SMP["d_psi^MS"],SMP["d_A"]->SMP["d_A^MS"]}/.solMS]]
Solve[%,SMP["d_g"]]


SUNSimplify[Simplify[tmp[11]/.Epsilon->1/SMP["Delta"]/.{SMP["d_psi"]->SMP["d_psi^MSbar"],SMP["d_A"]->SMP["d_A^MSbar"]}/.solMSbar]]
Solve[%,SMP["d_g"]]


(* ::Text:: *)
(*At the end we summarize our results*)


solMS//TableForm


solMSbar//TableForm


(* ::Subsection:: *)
(*Check with the literature*)


solMSLit={SMP["d_A^MS"] -> -SMP["alpha_fs"]/(3*Epsilon*Pi), SMP["d_m^MS"] -> (-3*SMP["alpha_fs"])/(4*Epsilon*Pi), 
 SMP["d_psi^MS"] -> -(GaugeXi[V[1]]*SMP["alpha_fs"])/(4*Epsilon*Pi)};
solMSbarLit={SMP["d_A^MSbar"] -> -(SMP["alpha_fs"]*SMP["Delta"])/(3*Pi), SMP["d_m^MSbar"] -> (-3*SMP["alpha_fs"]*SMP["Delta"])/(4*Pi), 
 SMP["d_psi^MSbar"] -> -(GaugeXi[V[1]]*SMP["alpha_fs"]*SMP["Delta"])/(4*Pi)};


Print["Check with the literature: ", If[Union[Flatten[{solMS-solMSLit,solMSbar-solMSbarLit}]]==={0},
		"CORRECT.", "!!! WRONG !!!"]];
