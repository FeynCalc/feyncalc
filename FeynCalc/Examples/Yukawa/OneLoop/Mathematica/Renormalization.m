(* ::Package:: *)

(* :Title: Renormalization													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, Yukawa, MS and MSbar, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*1-loop Yukawa renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QED model created with FeynRules. Please evaluate the file*)
(*FeynCalc/Examples/FeynRules/QED/GenerateModelYukawa.m before running it for the first time.*)


description="Renormalization, Yukawa, MS and MSbar, 1-loop";
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
(*Configure some options*)


(* ::Text:: *)
(*We keep scaleless B0 functions, since otherwise the UV part would not come out right.*)


$KeepLogDivergentScalelessIntegrals=True;


FAPatch[PatchModelsOnly->True];


(* ::Section:: *)
(*Generate Feynman diagrams*)


params={InsertionLevel->{Particles},Model -> FileNameJoin[{"LY","LY"}],
GenericModel -> FileNameJoin[{"LY","LY"}],ExcludeParticles->{}};
top[i_,j_]:=CreateTopologies[1, i -> j,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}];
topCT[i_,j_]:=CreateCTTopologies[1, i ->j,
ExcludeTopologies ->{Tadpoles,WFCorrections,WFCorrectionCTs}];

{diagFermionSE,diagFermionSECT} = InsertFields[#, {F[10]} -> {F[10]},
Sequence@@params]&/@{top[1,1],topCT[1,1]};
{diagScalarSE,diagScalarSECT} = InsertFields[#, {S[1]} -> {S[1]},
Sequence@@params]&/@{top[1,1],topCT[1,1]};
{diagVertexFFS,diagVertexFFSCT} = InsertFields[#,  {F[10],S[1]}->{F[10]},
Sequence@@params]&/@{top[2,1],topCT[2,1]};
{diagVertexSSSS,diagVertexSSSSCT} = InsertFields[#,  {S[1],S[1]}->{S[1],S[1]},
Sequence@@params]&/@{top[2,2],topCT[2,2]};


diag1[0]=diagFermionSE[[0]][diagFermionSE[[1]],diagFermionSECT[[1]]];
diag2[0]=diagScalarSE[[0]][diagScalarSE[[1]],diagScalarSECT[[1]]];
diag3[0]=diagVertexFFS[[0]][diagVertexFFS[[1]],diagVertexFFSCT[[1]]];
diag4[0]=diagVertexSSSS[[0]][diagVertexSSSS[[1]],diagVertexSSSSCT[[1]]];


Paint[diag1[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->256{2,1}];


Paint[diag2[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->256{2,1}];


Paint[diag3[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->256{2,1}];


Paint[diag4[0], ColumnsXRows -> {3, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->256{3,1}];


(* ::Section:: *)
(*Obtain the amplitudes*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


(* ::Text:: *)
(*Fermion self-energy including the counter-term*)


amp1[0] = FCFAConvert[CreateFeynAmp[diag1[0],Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},	
	LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{},Contract->True]


(* ::Text:: *)
(*Scalar self-energy including the counter-term*)


amp2[0] = FCFAConvert[CreateFeynAmp[diag2[0],Truncated->True,
	GaugeRules->{}, PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},	
	LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True, Contract->True]


(* ::Text:: *)
(*Fermion-scalar vertex including the counter-term*)


amp3[0] = FCFAConvert[CreateFeynAmp[diag3[0],Truncated->True,
	GaugeRules->{}, PreFactor->1],
	IncomingMomenta->{p1,k}, OutgoingMomenta->{p2},
	LoopMomenta->{l}, UndoChiralSplittings->True, ChangeDimension->D,
	List->False, SMP->True, Contract->True]


(* ::Text:: *)
(*Scalar self-interaction vertex including the counter-term*)


amp4[0] = FCFAConvert[CreateFeynAmp[diag4[0],Truncated->True,
	GaugeRules->{}, PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{p3,p4},
	LoopMomenta->{l}, UndoChiralSplittings->True, ChangeDimension->D,
	List->False, SMP->True, Contract->True]


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Fermion self-energy*)


amp1[1] = amp1[0]//ReplaceAll[#,{Zx->1+alpha dZx,
Zmx->1+alpha dZmx}]&//Series[#,{alpha,0,1}]&//
Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*Tensor reduction allows us to express the electron self-energy in tems of the Passarino-Veltman coefficient functions.*)


amp1[2]=TID[amp1[1],l,ToPaVe->True]


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


amp1Div[0]=PaVeUVPart[amp1[2],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,{SMP["Delta"],dZx,
dZmx}]&//Simplify//Collect2[#,DiracGamma]&


(* ::Text:: *)
(*Equating the result to zero and solving for dZx and dZmx we obtain  the renormalization constants in *)
(*the minimal subtraction schemes.*)


solMSbar1=FCMatchSolve[amp1Div[0],{g,la,Mx,DiracGamma,SMP}];
solMS1=solMSbar1/.SMP["Delta"]->1/Epsilon


(* ::Subsection:: *)
(*Scalar self-energy*)


amp2[0]


amp2[1] = amp2[0]//ReplaceRepeated[#,{Zphi->1+alpha dZphi,
Zmphi->1+alpha dZmphi}]&//Series[#,{alpha,0,1}]&//
Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*Tensor reduction allows us to express the scalar self-energy in tems of the Passarino-Veltman coefficient functions.*)


amp2[2]=TID[amp2[1],l,ToPaVe->True]


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


amp2Div[0]=PaVeUVPart[amp2[2],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,{SMP["Delta"],dZphi,dZmphi}]&//Simplify//
Collect2[#,p,Mphi]&


(* ::Text:: *)
(*Equating this to zero and solving for dZphi and dZmphi  obtain  the renormalization constants in the minimal subtraction schemes.*)


solMSbar2=FCMatchSolve[amp2Div[0],{g,la,Mphi,p,SMP,GaugeXi}]
solMS2=solMSbar2/.SMP["Delta"]->1/Epsilon;


(* ::Subsection:: *)
(*Fermion-scalar vertex*)


amp3[1] = amp3[0]//ReplaceRepeated[#,{Zphi->1+alpha dZphi,
Zx->1+alpha dZx,Zg->1+alpha dZg}]&//
Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*The result of the tensor reduction is quite large, since we keep the full gauge dependence and do not specify the kinematics*)


amp3[2]=TID[amp3[1],l,ToPaVe->True,UsePaVeBasis->True]


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


amp3Div[0]=PaVeUVPart[amp3[2],Prefactor->1/(2Pi)^D]//DiracSimplify//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,{SMP["Delta"],dZphi,
dZx,dZg}]&//ReplaceAll[#,Join[solMSbar1,solMSbar2]]&//Simplify//FCFactorOut[#,g]&


(* ::Text:: *)
(*Equating this to zero and solving for dZg we  obtain  the renormalization constant in the minimal subtraction schemes.*)


solMSbar3=FCMatchSolve[amp3Div[0],{g,SMP}]
solMS3=solMSbar3/.SMP["Delta"]->1/Epsilon;


(* ::Subsection:: *)
(*Scalar self-interaction vertex*)


amp4[1] = amp4[0]//ReplaceRepeated[#,{Zphi->1+alpha dZphi,
Zla->1+alpha dZla}]&//
Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*The result of the tensor reduction is quite large, since we keep the full gauge dependence and do not specify the kinematics*)


amp4[2]=TID[amp4[1],l,ToPaVe->True,UsePaVeBasis->True]


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


amp4Div[0]=PaVeUVPart[amp4[2],Prefactor->1/(2Pi)^D]//DiracSimplify//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,{SMP["Delta"],dZphi,
dZla}]&//ReplaceAll[#,Join[solMSbar1,solMSbar2]]&//Simplify


(* ::Text:: *)
(*Equating this to zero and solving for dZg we  obtain  the renormalization constant in the minimal subtraction schemes.*)


solMSbar4=FCMatchSolve[amp4Div[0],{g,SMP,la}]
solMS4=solMSbar4/.SMP["Delta"]->1/Epsilon;


Join[solMSbar1,solMSbar2,solMSbar3,solMSbar4]//TableForm
