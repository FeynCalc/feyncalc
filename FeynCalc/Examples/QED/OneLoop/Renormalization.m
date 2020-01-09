(* ::Package:: *)

(* :Title: Renormalization													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, QED, MS and MSbar, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*1-loop QED renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom QED model created with FeynRules. Please evaluate the file*)
(*FeynCalc/Examples/FeynRules/QED/GenerateModelQED.m before running it for the first time.*)


description="Renormalization, QED, MS and MSbar, 1-loop";
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


params={InsertionLevel->{Particles},Model -> FileNameJoin[{"QED","QED"}],
GenericModel -> FileNameJoin[{"QED","QED"}],ExcludeParticles->{F[2,{2|3}]}};
top[i_,j_]:=CreateTopologies[1, i -> j,
ExcludeTopologies -> {Tadpoles,WFCorrections,WFCorrectionCTs}];
topCT[i_,j_]:=CreateCTTopologies[1, i ->j,
ExcludeTopologies ->{Tadpoles,WFCorrections,WFCorrectionCTs}];

{diagElectronSE,diagElectronSECT} = InsertFields[#, {F[2,{1}]} -> {F[2,{1}]},
Sequence@@params]&/@{top[1,1],topCT[1,1]};
{diagPhotonSE,diagPhotonSECT} = InsertFields[#, {V[1]} -> {V[1]},
Sequence@@params]&/@{top[1,1],topCT[1,1]};
{diagVertex,diagVertexCT} = InsertFields[#,  {F[2,{1}],V[1]}->{F[2,{1}]},
Sequence@@params]&/@{top[2,1],topCT[2,1]};


diag1[0]=diagElectronSE[[0]][diagElectronSE[[1]],diagElectronSECT[[1]]];
diag2[0]=diagPhotonSE[[0]][diagPhotonSE[[1]],diagPhotonSECT[[1]]];
diag3[0]=diagVertex[[0]][diagVertex[[1]],diagVertexCT[[1]]];


Paint[diag1[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


Paint[diag2[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


Paint[diag3[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit. We need to replace e with -e to be compatible*)
(*with the convention D^mu = d^mu + ie A^mu*)


(* ::Text:: *)
(*Electron self-energy including the counter-term*)


amp1[0] = FCFAConvert[CreateFeynAmp[diag1[0],Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu},
	LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{Zm->SMP["Z_m"], Zpsi->SMP["Z_psi"],
	SMP["e"]->Sqrt[4Pi SMP["alpha_fs"]],GaugeXi[V[1]]->GaugeXi},
	Contract->True]


(* ::Text:: *)
(*Photon self-energy including the counter-term*)


amp2[0] = FCFAConvert[CreateFeynAmp[diag2[0],Truncated->True,
	GaugeRules->{}, PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu,nu},
	LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{ZA->SMP["Z_A"], Zxi->SMP["Z_xi"],
	SMP["e"]->Sqrt[4Pi SMP["alpha_fs"]],GaugeXi[V[1]]->GaugeXi},
	Contract->True]//FCTraceFactor


(* ::Text:: *)
(*Electron-photon vertex including the counter-term*)


amp3[0] = FCFAConvert[CreateFeynAmp[diag3[0],Truncated->True,
	GaugeRules->{}, PreFactor->1],
	IncomingMomenta->{p1,k}, OutgoingMomenta->{p2},
	LorentzIndexNames->{mu}, LoopMomenta->{l},
	UndoChiralSplittings->True, ChangeDimension->D,
	List->False, SMP->True, FinalSubstitutions->
	{ZA->SMP["Z_A"], Ze->SMP["Z_e"], Zpsi->SMP["Z_psi"],
	SMP["e"]^3->4Pi SMP["alpha_fs"] SMP["e"],GaugeXi[V[1]]->GaugeXi},
	Contract->True]/.SMP["e"]->-SMP["e"]


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Electron self-energy*)


amp1[1] = amp1[0]//ReplaceAll[#,{SMP["Z_psi"]->1+alpha SMP["d_psi"],
SMP["Z_m"]->1+alpha SMP["d_m"]}]&//Series[#,{alpha,0,1}]&//
Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*Tensor reduction allows us to express the electron self-energy in tems of the Passarino-Veltman coefficient functions.*)


amp1[2]=TID[amp1[1],l,ToPaVe->True]


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


amp1Div[0]=PaVeUVPart[amp1[2],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,{SMP["Delta"],SMP["d_m"],
SMP["d_psi"]}]&//Simplify


(* ::Text:: *)
(*Equating the result to zero and solving for d_psi and d_m we obtain  the renormalization constants in *)
(*the minimal subtraction schemes.*)


sol[1]=Solve[SelectNotFree2[amp1Div[0], DiracGamma]==0,
	SMP["d_psi"]]//Flatten//Simplify;
sol[2]=Solve[(SelectFree2[amp1Div[0], DiracGamma]==0)/.sol[1],
	SMP["d_m"]]//Flatten//Simplify;
solMS1=Join[sol[1],sol[2]]/.{
	SMP["d_psi"]->SMP["d_psi^MS"],
	SMP["d_m"]->SMP["d_m^MS"],SMP["Delta"]->1/Epsilon
}
solMSbar1=Join[sol[1],sol[2]]/.{
	SMP["d_psi"]->SMP["d_psi^MSbar"],
	SMP["d_m"]->SMP["d_m^MSbar"]
}


(* ::Subsection:: *)
(*Photon self-energy*)


amp2[1] = amp2[0]//ReplaceRepeated[#,{SMP["Z_xi"]->SMP["Z_A"],
SMP["Z_A"]->1+alpha SMP["d_A"]}]&//Series[#,{alpha,0,1}]&//
Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*Tensor reduction allows us to express the electron self-energy in tems of the Passarino-Veltman coefficient functions.*)


amp2[2]=TID[amp2[1],l,ToPaVe->True]


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


amp2Div[0]=PaVeUVPart[amp2[2],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,{SMP["Delta"],SMP["d_A"]}]&//Simplify


(* ::Text:: *)
(*Equating this to zero and solving for d_A we obtain the wave-function renormalization constant for the photon in the minimal subtraction schemes.*)


sol[3]=Solve[amp2Div[0]==0,SMP["d_A"]]//Flatten;
solMS2=sol[3]/.{SMP["d_A"]->SMP["d_A^MS"],SMP["Delta"]->1/Epsilon}
solMSbar2=sol[3]/.{SMP["d_A"]->SMP["d_A^MSbar"]}


(* ::Subsection:: *)
(*Electron-photon vertex*)


amp3[1] = amp3[0]//ReplaceRepeated[#,{SMP["Z_psi"]->1+alpha SMP["d_psi"],
SMP["Z_A"]->1+alpha SMP["d_A"],SMP["Z_e"]->1+alpha SMP["d_e"]}]&//
Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*The result of the tensor reduction is quite large, since we keep the full gauge dependence and do not specify the kinematics*)


amp3[2]=TID[amp3[1],l,ToPaVe->True,UsePaVeBasis->True]


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


amp3Div[0]=PaVeUVPart[amp3[2],Prefactor->1/(2Pi)^D]//DiracSimplify//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,{SMP["Delta"],SMP["d_A"],
SMP["d_e"],SMP["d_psi"]}]&//Simplify


ward[0]=Simplify[amp3Div[0]/(-FCI [I SMP["e"] GAD[mu]])==0]


wardMS[0]=Simplify[ward[0]/.Epsilon->1/SMP["Delta"]/.
	{SMP["d_psi"]->SMP["d_psi^MSbar"]}/.solMSbar1]
wardMSbar[0]=Simplify[ward[0]/.{SMP["d_psi"]->SMP["d_psi^MSbar"]}/.
	solMSbar1]


knownResults={SMP["d_A"] + 2*SMP["d_e"] == 0,
	SMP["d_A"] + 2*SMP["d_e"] == 0};
FCCompareResults[{wardMS[0],wardMSbar[0]},knownResults,
Text->{"\tVerify Ward's identity:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];


(* ::Section:: *)
(*Check the final results*)


knownResult = {
	SMP["d_psi^MS"] -> -(GaugeXi*SMP["alpha_fs"])/(4*Epsilon*Pi),
	SMP["d_m^MS"] -> (-3*SMP["alpha_fs"])/(4*Epsilon*Pi),
	SMP["d_A^MS"] -> -SMP["alpha_fs"]/(3*Epsilon*Pi),
	SMP["d_psi^MSbar"] -> -(GaugeXi*SMP["alpha_fs"]*SMP["Delta"])/(4*Pi),
	SMP["d_m^MSbar"] -> (-3*SMP["alpha_fs"]*SMP["Delta"])/(4*Pi),
	SMP["d_A^MSbar"] -> -(SMP["alpha_fs"]*SMP["Delta"])/(3*Pi)
	};
FCCompareResults[Join[solMS1,solMS2,solMSbar1,solMSbar2],knownResult,
Text->{"\tCheck the final result:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];



