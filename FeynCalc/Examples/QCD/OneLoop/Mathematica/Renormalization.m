(* ::Package:: *)

(* :Title: Renormalization													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QCD, MS and MSbar, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*1-loop QCD renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QCD model created with FeynRules. Please evaluate the file*)
(*FeynCalc/Examples/FeynRules/QCD/GenerateModelQCD.m before running it for the first time.*)


description="Renormalization, QCD, MS and MSbar, 1-loop";
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

FCCheckVersion[9,3,1];


(* ::Section:: *)
(*Configure some options*)


(* ::Text:: *)
(*We keep scaleless B0 functions, since otherwise the UV part would not come out right.*)


$KeepLogDivergentScalelessIntegrals=True;


FAPatch[PatchModelsOnly->True];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[mu,TraditionalForm]:="\[Mu]";
MakeBoxes[nu,TraditionalForm]:="\[Nu]";
MakeBoxes[rho,TraditionalForm]:="\[Rho]";
MakeBoxes[si,TraditionalForm]:="\[Sigma]";


params={InsertionLevel->{Particles},Model -> FileNameJoin[{"QCD","QCD"}],
GenericModel -> FileNameJoin[{"QCD","QCD"}],ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}};
top[i_,j_]:=CreateTopologies[1, i -> j,
	ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}];
topTriangle[i_,j_]:=CreateTopologies[1, i -> j,
	ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs,SelfEnergies}];

topCT[i_,j_]:=CreateCTTopologies[1, i ->j,
	ExcludeTopologies ->{Tadpoles,WFCorrections,WFCorrectionCTs}];
topTriangleCT[i_,j_]:=CreateCTTopologies[1, i -> j,
	ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs,SelfEnergyCTs}];

{diagQuarkSE,diagQuarkSECT} = InsertFields[#, {F[3,{1}]} -> {F[3,{1}]},
	Sequence@@params]&/@{top[1,1],topCT[1,1]};

{diagGluonSE,diagGluonSECT} = InsertFields[#, {V[5]} -> {V[5]},
	Sequence@@params]&/@{top[1,1],topCT[1,1]};

{diagGhostSE,diagGhostSECT} = InsertFields[#, {U[5]} -> {U[5]},
	Sequence@@params]&/@{top[1,1],topCT[1,1]};

{diagQuarkGluonVertex,diagQuarkGluonVertexCT} = InsertFields[#,
	{F[3,{1}],V[5]}->{F[3,{1}]}, Sequence@@params]&/@{topTriangle[2,1],topTriangleCT[2,1]};


diag1[0]=diagQuarkSE[[0]][Sequence@@diagQuarkSE,
	Sequence@@diagQuarkSECT];
diag2[0]=diagGluonSE[[0]][Sequence@@diagGluonSE,
	Sequence@@diagGluonSECT];
diag3[0]=diagGhostSE[[0]][Sequence@@diagGhostSE,
	Sequence@@diagGhostSECT];
diag4[0]=diagQuarkGluonVertex[[0]][Sequence@@diagQuarkGluonVertex,
	Sequence@@diagQuarkGluonVertexCT];


Paint[diag1[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


Paint[diag2[0], ColumnsXRows -> {4, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,128}];


Paint[diag3[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


Paint[diag4[0], ColumnsXRows -> {3, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


(* ::Text:: *)
(*Quark self-energy including the counter-term*)


ampQuarkSE[0] = FCFAConvert[CreateFeynAmp[diag1[0],Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{Zm->SMP["Z_m"], Zpsi->SMP["Z_psi"],
	SMP["m_u"]->SMP["m_q"]}]


(* ::Text:: *)
(*Gluon self-energy including the counter-term*)


ampGluonSE[0] = FCFAConvert[CreateFeynAmp[diag2[0],Truncated->True,
	GaugeRules->{}, PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu,rho,si}, DropSumOver->True,
	LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->True, SMP->True,
	FinalSubstitutions->{ZA->SMP["Z_A"], Zxi->SMP["Z_xi"],
	SMP["m_u"]->SMP["m_q"]}]


(* ::Text:: *)
(*Ghost self-energy including the counter-term*)


ampGhostSE[0] = FCFAConvert[CreateFeynAmp[diag3[0],Truncated->True,
	GaugeRules->{}, PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu}, DropSumOver->True,
	LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{Zu->SMP["Z_u"]}]


(* ::Text:: *)
(*Quark-gluon vertex including the counter-term*)


ampQGlVertex[0] = FCFAConvert[CreateFeynAmp[diag4[0],Truncated->True,
	GaugeRules->{}, PreFactor->1],
	IncomingMomenta->{p1,k}, OutgoingMomenta->{p2},
	LorentzIndexNames->{mu,nu,rho}, DropSumOver->True, LoopMomenta->{l},
	UndoChiralSplittings->True, ChangeDimension->D,
	List->False, SMP->True, FinalSubstitutions->
	{ZA->SMP["Z_A"], Zg->SMP["Z_g"], Zpsi->SMP["Z_psi"],
	SMP["m_u"]->SMP["m_q"]}]


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Quark self-energy*)


(* ::Text:: *)
(*Tensor reduction allows us to express the quark self-energy in tems of the Passarino-Veltman coefficient functions.*)


ampQuarkSE[1]=ampQuarkSE[0]//SUNSimplify//DiracSimplify//
	TID[#,l,UsePaVeBasis->True,ToPaVe->True]&;


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude.*)


ampQuarkSEDiv[0]=ampQuarkSE[1]//PaVeUVPart[#,Prefactor->1/(2Pi)^D]&;


ampQuarkSEDiv[1]=FCReplaceD[ampQuarkSEDiv[0],D->4-2Epsilon]//
	Series[#,{Epsilon,0,0}]&//Normal//FCHideEpsilon//Simplify


ampQuarkSEDiv[2]=ampQuarkSEDiv[1]//ReplaceRepeated[#,{
	SMP["Z_m"]->1+alpha SMP["d_m"],
	SMP["Z_psi"]->1+alpha SMP["d_psi"]}]&//Series[#,{alpha,0,1}]&//
	Normal//ReplaceAll[#,alpha->1]&//SelectNotFree2[#,SMP["Delta"],SMP["d_m"],
	SMP["d_psi"]]&


ampQuarkSEDiv[3]=ampQuarkSEDiv[2]//SUNSimplify//
	Collect2[#,DiracGamma,Factoring->Simplify]&


sol[1]=Solve[SelectNotFree2[ampQuarkSEDiv[3], DiracGamma]==0,SMP["d_psi"]]//
	Flatten//ReplaceAll[#,Rule[a_,b_]:>Rule[a,SUNSimplify[b]]]&//
	ReplaceAll[#,SMP["g_s"]^2->4Pi SMP["alpha_s"]]&;

sol[2]=Solve[(SelectFree2[ampQuarkSEDiv[3], DiracGamma]==0)/.sol[1],SMP["d_m"]]//
	Flatten//ReplaceAll[#,Rule[a_,b_]:>Rule[a,SUNSimplify[b]]]&//
	ReplaceAll[#,SMP["g_s"]^2->4Pi SMP["alpha_s"]]&;

solMS1=Join[sol[1],sol[2]]/.{
	SMP["d_psi"]->SMP["d_psi^MS"],
	SMP["d_m"]->SMP["d_m^MS"],SMP["Delta"]->1/Epsilon
}
solMSbar1=Join[sol[1],sol[2]]/.{
	SMP["d_psi"]->SMP["d_psi^MSbar"],
	SMP["d_m"]->SMP["d_m^MSbar"]
}


(* ::Subsection:: *)
(*Gluon self-energy*)


(* ::Text:: *)
(*Tensor reduction allows us to express the gluon self-energy in tems of the Passarino-Veltman coefficient functions.*)


ampGluonSE[1]=(ampGluonSE[0][[1]]+Nf ampGluonSE[0][[2]]+
	Total[ampGluonSE[0][[3;;]]])//SUNSimplify//DiracSimplify;


ampGluonSE[2]=TID[ampGluonSE[1],l,UsePaVeBasis->True,ToPaVe->True];


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


ampGluonSEDiv[0]=ampGluonSE[2]//PaVeUVPart[#,Prefactor->1/(2Pi)^D]&


ampGluonSEDiv[1]=FCReplaceD[ampGluonSEDiv[0],D->4-2Epsilon]//
	Series[#,{Epsilon,0,0}]&//
	Normal//FCHideEpsilon//SUNSimplify;


ampGluonSEDiv[2]=ampGluonSEDiv[1]//ReplaceRepeated[#,{
	SMP["Z_A"]->1+alpha SMP["d_A"],
	SMP["Z_xi"]->1+alpha SMP["d_A"]}]&//Series[#,{alpha,0,1}]&//
	Normal//ReplaceAll[#,alpha->1]&//SelectNotFree2[#,SMP["Delta"],SMP["d_A"],
	SMP["d_xi"]]&


sol[3]=Solve[ampGluonSEDiv[2]==0,SMP["d_A"]]//Flatten//
	ReplaceAll[#,SMP["g_s"]^2->4Pi SMP["alpha_s"]]&//Simplify
solMS2=sol[3]/.{SMP["d_A"]->SMP["d_A^MS"],SMP["Delta"]->1/Epsilon}
solMSbar2=sol[3]/.{SMP["d_A"]->SMP["d_A^MSbar"]}


(* ::Subsection:: *)
(*Ghost self-energy*)


(* ::Text:: *)
(*Tensor reduction allows us to express the ghost self-energy in tems of the Passarino-Veltman coefficient functions.*)


ampGhostSE[1]=ampGhostSE[0]//SUNSimplify//DiracSimplify;


ampGhostSE[2]=TID[ampGhostSE[1],l,UsePaVeBasis->True,ToPaVe->True];


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


ampGhostSEDiv[0]=ampGhostSE[2]//PaVeUVPart[#,Prefactor->1/(2Pi)^D]&


ampGhostSEDiv[1]=FCReplaceD[ampGhostSEDiv[0],D->4-2Epsilon]//
	Series[#,{Epsilon,0,0}]&//
	Normal//FCHideEpsilon//SUNSimplify


ampGhostSEDiv[2]=ampGhostSEDiv[1]//ReplaceRepeated[#,{
	SMP["Z_u"]->1+alpha SMP["d_u"]}]&//Series[#,{alpha,0,1}]&//
	Normal//ReplaceAll[#,alpha->1]&//
	SelectNotFree2[#,SMP["Delta"],SMP["d_u"]]&//Simplify


sol[4]=Solve[ampGhostSEDiv[2]==0,SMP["d_u"]]//Flatten//
	ReplaceAll[#,SMP["g_s"]^2->4Pi SMP["alpha_s"]]&//Simplify
solMS3=sol[4]/.{SMP["d_u"]->SMP["d_u^MS"],SMP["Delta"]->1/Epsilon}
solMSbar3=sol[4]/.{SMP["d_u"]->SMP["d_u^MSbar"]}


(* ::Subsection:: *)
(*Quark-gluon vertex*)


(* ::Text:: *)
(*Tensor reduction allows us to express the quark-gluon vertex in tems of the Passarino-Veltman coefficient functions.*)


ampQGlVertex[1]=ampQGlVertex[0]//SUNSimplify//DiracSimplify;


ampQGlVertex[2]=TID[ampQGlVertex[1],l,UsePaVeBasis->True,ToPaVe->True];


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


ampQGlVertexDiv[0]=ampQGlVertex[2]//PaVeUVPart[#,Prefactor->1/(2Pi)^D]&


ampQGlVertexDiv[1]=FCReplaceD[ampQGlVertexDiv[0],D->4-2Epsilon]//
	Series[#,{Epsilon,0,0}]&//
	Normal//FCHideEpsilon//SUNSimplify;


ampQGlVertexDiv[2]=ampQGlVertexDiv[1]//ReplaceRepeated[#,{
	SMP["Z_g"]->1+alpha SMP["d_g"],
	SMP["Z_A"]->1+alpha SMP["d_A"],
	SMP["Z_psi"]->1+alpha SMP["d_psi"]
	}]&//Series[#,{alpha,0,1}]&//
	Normal//ReplaceAll[#,alpha->1]&//
	SelectNotFree2[#,SMP["Delta"],SMP["d_g"],SMP["d_A"],SMP["d_psi"]]&//Simplify


ampQGlVertexDiv[3]=ampQGlVertexDiv[2]//SUNSimplify[#,Explicit->True]&//
	ReplaceAll[#,SUNTrace[x__]:>SUNTrace[x,Explicit->True]]&//
	Collect2[#,Epsilon,SUNIndex]&


ampQGlVertexDiv[4]=ampQGlVertexDiv[3]//
	ReplaceAll[#,suntf[xx_,_SUNFIndex,_SUNFIndex]:>SUNT@@xx]&//
	ReplaceAll[#,SUNTF->suntf]&//ReplaceAll[#,
	suntf[xx_,_SUNFIndex,_SUNFIndex]:>SUNT@@xx]&//
	SUNSimplify//Collect2[#,SMP]&


ampQGlVertexDiv[5]=(ampQGlVertexDiv[4]/.{
	SMP["d_A"]->SMP["d_A^MS"],
	SMP["d_psi"]->SMP["d_psi^MS"],
	SMP["d_g"]->SMP["d_g"],
	SMP["Delta"]->1/Epsilon
}/.solMS1/.solMS2)//ReplaceAll[#,SMP["g_s"]^3->4Pi SMP["alpha_s"]SMP["g_s"]]&//
Collect2[#,Epsilon]&//SUNSimplify


sol[5]=Solve[ampQGlVertexDiv[5]==0,SMP["d_g"]]//Flatten//Simplify
solMS4=sol[5]/.{SMP["d_g"]->SMP["d_g^MS"]}
solMSbar4=sol[5]/.{SMP["d_g"]->SMP["d_g^MSbar"],1/Epsilon->SMP["Delta"]}


(* ::Section:: *)
(*Check the final results*)


knownResult = {
	SMP["d_psi^MS"]->-SMP["alpha_s"]/(4Pi) 1/Epsilon CF GaugeXi["G"],
	SMP["d_m^MS"]->-SMP["alpha_s"]/(4Pi) 1/Epsilon 3 CF,
	SMP["d_psi^MSbar"]->-SMP["alpha_s"]/(4Pi) SMP["Delta"] CF GaugeXi["G"],
	SMP["d_m^MSbar"]->-SMP["alpha_s"]/(4Pi) SMP["Delta"] 3 CF,
	SMP["d_A^MS"]->SMP["alpha_s"]/(4Pi) 1/Epsilon (1/2 CA(13/3-GaugeXi["G"])-2/3 Nf),
	SMP["d_A^MSbar"]->SMP["alpha_s"]/(4Pi) SMP["Delta"] (1/2 CA(13/3-GaugeXi["G"])-2/3 Nf),
	SMP["d_u^MS"]->SMP["alpha_s"]/(4Pi) CA/Epsilon (3-GaugeXi["G"])/4,
	SMP["d_u^MSbar"]->SMP["alpha_s"]/(4Pi) CA SMP["Delta"] (3-GaugeXi["G"])/4,
	SMP["d_g^MS"]->((-11*CA*SMP["alpha_s"])/(24*Epsilon*Pi) + (Nf*SMP["alpha_s"])/(12*Epsilon*Pi)),
	SMP["d_g^MSbar"]->((-11*CA*SMP["alpha_s"])/(24 Pi)SMP["Delta"] + (Nf*SMP["alpha_s"])/(12*Pi)SMP["Delta"])
}//Factor2;
FCCompareResults[Join[solMS1,solMSbar1,solMS2,solMSbar2,solMS3,solMSbar3,solMS4,solMSbar4]//Factor2,knownResult,
Text->{"\tCompare to Muta, Foundations of QCD, \
Eqs 2.5.131-2.5.147:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
