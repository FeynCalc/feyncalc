(* ::Package:: *)

(* :Title: Gl-GlGl															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Gl - Gl Gl, QCD, only UV divergences, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*QCD 3-gluon vertex at 1-loop*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QCD model created with FeynRules. Please evaluate the file*)
(*FeynCalc/Examples/FeynRules/QCD/GenerateModelQCD.m before running it for the first time.*)


description="Gl - Gl Gl, QCD, only UV divergences, 1-loop";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArtsLoader"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];


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


template = insertFields[createTopologies[1, 1 -> 2,
		ExcludeTopologies -> {Tadpoles,WFCorrections,
		WFCorrectionCTs,SelfEnergies}], {V[5]} ->
		{V[5],V[5]}, InsertionLevel -> {Particles},
		Model -> FileNameJoin[{"QCD","QCD"}],
		GenericModel -> FileNameJoin[{"QCD","QCD"}],
		ExcludeParticles->{F[3|4,{2|3}],F[4,{1}]}];


diags=template/.createTopologies->CreateTopologies/.
	insertFields->InsertFields;
diagsCT=template/.createTopologies->CreateCTTopologies/.
	insertFields->InsertFields;


Paint[diags, ColumnsXRows -> {3,1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


Paint[diagsCT, ColumnsXRows -> {3,1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit. We keep the full gauge dependence. To simplify comparisons*)
(*to the literature, we make all momenta incoming.*)


(* ::Text:: *)
(*Quark contribution. Notice that we multiply the amplitude by Nf to account for the number*)
(*of quark flavours in the loop.*)


amp1[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diags,{1,2}],
	Truncated->True, GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p1}, OutgoingMomenta->{p2,p3},
	LorentzIndexNames->{mu,nu,rho}, DropSumOver->True,
	SUNIndexNames->{a,b,c}, LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,Prefactor->Nf,
	FinalSubstitutions->{SMP["m_u"]->SMP["m_q"],p2->-p2,p3->-p3}]


(* ::Text:: *)
(*Ghost contribution*)


amp2[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diags,{3,4}],
	Truncated->True, GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p1}, OutgoingMomenta->{p2,p3},
	LorentzIndexNames->{mu,nu,rho}, SUNIndexNames->{a,b,c},
	DropSumOver->True, LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->SMP["m_q"],p2->-p2,p3->-p3}]


(* ::Text:: *)
(*Gluon contribution*)


amp3[0] = FCFAConvert[CreateFeynAmp[DiagramExtract[diags,{5,6,7,8}],
	Truncated->True, GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p1}, OutgoingMomenta->{p2,p3},
	LorentzIndexNames->{mu,nu,rho}, SUNIndexNames->{a,b,c},
	DropSumOver->True, LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->SMP["m_q"],p2->-p2,p3->-p3}];


(* ::Text:: *)
(*Counter-term*)


amp4[0] = FCFAConvert[CreateFeynAmp[diagsCT,
	Truncated->True, GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p1}, OutgoingMomenta->{p2,p3},
	LorentzIndexNames->{mu,nu,rho}, SUNIndexNames->{a,b,c},
	DropSumOver->True, LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{SMP["m_u"]->SMP["m_q"],p2->-p2,p3->-p3,
	ZA->SMP["Z_A"],Zg->SMP["Z_g"]}]


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Quark contribution*)


AbsoluteTiming[amp1[1]=TID[FCE[amp1[0]]/.{p2+p3->-p1,-p2-p3->p1},l,
	UsePaVeBasis->True,ToPaVe->True];]


amp1Div[0]=amp1[1]//PaVeUVPart[#,Prefactor->1/(2Pi)^D,FCLoopExtract->False]&;


amp1Div[1]=amp1Div[0]//SUNSimplify[#,Explicit->True]&//ReplaceAll[#,
	SUNTrace[x__]:>SUNTrace[x,Explicit->True]]&//
	FCReplaceD[#,D->4-2 Epsilon]&//Series[#,{Epsilon,0,0}]&//
	Normal//FCHideEpsilon//SelectNotFree2[#,SMP["Delta"]]&//FCE//
	Collect2[#,MTD,Factoring->Function[x,MomentumCombine[Factor[x]]]]&


(* ::Text:: *)
(*In the calculation p3 was eliminated via the 4-momentum conservation p1+p2+p3=0. *)
(*Now we need to reintroduce it*)


amp1Div[2]=amp1Div[1]/. {2p1+p2->p1-p3,p1+2p2->p2-p3}


(* ::Subsection:: *)
(*Ghost contribution*)


AbsoluteTiming[amp2[1]=TID[FCE[amp2[0]]/.{p2+p3->-p1,-p2-p3->p1},l,
	UsePaVeBasis->True,ToPaVe->True];]


amp2Div[0]=amp2[1]//PaVeUVPart[#,Prefactor->1/(2Pi)^D,FCLoopExtract->False]&;


amp2Div[1]=amp2Div[0]//SUNSimplify[#,Explicit->True]&//ReplaceAll[#,
	SUNTrace[x__]:>SUNTrace[x,Explicit->True]]&//
	FCReplaceD[#,D->4-2 Epsilon]&//Series[#,{Epsilon,0,0}]&//
	Normal//FCHideEpsilon//SelectNotFree2[#,SMP["Delta"]]&//FCE//
	Collect2[#,MTD,Factoring->Function[x,MomentumCombine[Factor[x]]]]&


(* ::Text:: *)
(*In the calculation p3 was eliminated via the 4-momentum conservation p1+p2+p3=0. *)
(*Now we need to reintroduce it*)


amp2Div[2]=amp2Div[1]/. {2p1+p2->p1-p3,p1+2p2->p2-p3}


(* ::Subsection:: *)
(*Gluon contribution*)


(* ::Text:: *)
(*This calculation requires about 70 seconds on a modern laptop*)


AbsoluteTiming[amp3[1]=TID[FCE[amp3[0]]/.{p2+p3->-p1,-p2-p3->p1},l,
	UsePaVeBasis->True,ExpandScalarProduct->False,ToPaVe->True];]


amp3Div[0]=amp3[1]//PaVeUVPart[#,Prefactor->1/(2Pi)^D,FCLoopExtract->False]&;


amp3Div[1]=amp3Div[0]//SUNSimplify//FCReplaceD[#,D->4-2 Epsilon]&//
	Series[#,{Epsilon,0,0}]&//Normal//FCHideEpsilon//
	SelectNotFree2[#,SMP["Delta"]]&//FCE//
	Collect2[#,MTD,Factoring->Function[x,MomentumCombine[Factor[x]]]]&


amp3Div[2]=(((amp3Div[1]/.{p3->-p1-p2})//ExpandScalarProduct//FCE//
	Collect2[#,MTD,GaugeXi,Factoring->Function[x,MomentumCombine[Factor[x]]]]&)/.
	2p1+p2->p1-p3/.p1+2p2->p2-p3/. 22p1+11p2 -> 11p1-11p3/.
	-22p2-11p1 -> -11p2+11p3)//ExpandScalarProduct//FCE//
	Collect2[#,MTD,Factoring->Function[x,MomentumCombine[Factor[x]]]]&


(* ::Subsection:: *)
(*Counter-term*)


amp4[1]=amp4[0]//ReplaceAll[#,{SMP["Z_A"]->1+alpha SMP["d_A"],
	SMP["Z_g"]->1+alpha SMP["d_g"]}]&//Series[#,{alpha,0,1}]&//
	Normal//ReplaceAll[#,alpha->1]&//ExpandScalarProduct//FCE//
	Collect2[#,MTD,GaugeXi,Factoring->Function[x,MomentumCombine[Factor[x]]]]&


(* ::Text:: *)
(*Check the cancellation of the UV divergences in the MSbar scheme. The renormalization constants*)
(*are obtained from another example calculation, "Renormalization.m"*)


renormalizationConstants = {
	SMP["d_A"]->SMP["alpha_s"]/(4Pi) SMP["Delta"] (1/2 CA(13/3-GaugeXi["G"])-2/3 Nf),
	SMP["d_g"]->((-11*CA*SMP["alpha_s"])/(24 Pi)SMP["Delta"] + (Nf*SMP["alpha_s"])/(12*Pi)SMP["Delta"])
}/.SMP["alpha_s"]->SMP["g_s"]^2/(4Pi);


uvDiv[0]=ExpandScalarProduct[amp1Div[2]+amp2Div[2]+amp3Div[2]+amp4[1]]//FCE//
Collect2[#,MTD,Factoring->Function[x,MomentumCombine[Factor[x]]]]&


uvDiv[1]=(uvDiv[0]/.renormalizationConstants)//Simplify


FCCompareResults[uvDiv[1],0,
Text->{"\tThe UV divergence of the 3-gluon vertex at 1-loop is cancelled by \
the counter-term :",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];


(* ::Section:: *)
(*Check the final results*)


VertexLorentzStruct[{p_,q_,k_},{mu_,nu_,si_},{a_,b_,c_}]:=
-I SUNF[a,b,c](MTD[mu,nu]FVD[p-q,si]+MTD[nu,si]FVD[q-k,mu]+MTD[si,mu]FVD[k-p,nu]);


knownResult = {
	(I SMP["g_s"]) Nf SMP["g_s"]^2/(4Pi)^2*(-2/3)SMP["Delta"]*
		VertexLorentzStruct[{p1,p2,p3},{mu,nu,rho},{a,b,c}],


	(I SMP["g_s"]) SMP["g_s"]^2/(4Pi)^2  CA/8 (1/3)SMP["Delta"]*
		VertexLorentzStruct[{p1,p2,p3},{mu,nu,rho},{a,b,c}],


	(I SMP["g_s"]) SMP["g_s"]^2/(4Pi)^2  CA/8 SMP["Delta"]*(
		-4-9GaugeXi["G"]+15+3GaugeXi["G"])*
	VertexLorentzStruct[{p1,p2,p3},{mu,nu,rho},{a,b,c}]
}//FCI;
FCCompareResults[{amp1Div[2],amp2Div[2],amp3Div[2]}, knownResult,
	Text->{"\tCompare to Pascual and Tarrach, QCD: Renormalization \
for the Practitioner, Eq III.46:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
