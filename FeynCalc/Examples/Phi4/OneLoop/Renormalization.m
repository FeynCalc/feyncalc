(* ::Package:: *)

(* :Title: Renormalization													*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Renormalization, phi^4, MS and MSbar, 1-loop					*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*1-loop phi^4 renormalization in the minimal subtraction schemes*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


(* ::Text:: *)
(*This example uses a custom phi^4 model created with FeynRules. Please evaluate the file*)
(*FeynCalc/Examples/FeynRules/Phi4/GenerateModelPhi4.m before running it for the first time.*)


description="Renormalization, phi^4, MS and MSbar, 1-loop";
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


params={InsertionLevel->{Particles},Model -> FileNameJoin[{"Phi4","Phi4"}],
GenericModel -> FileNameJoin[{"Phi4","Phi4"}]};
top[i_,j_]:=CreateTopologies[1, i -> j];
topCT[i_,j_]:=CreateCTTopologies[1, i ->j];
topVertex[i_,j_]:=CreateTopologies[1, i ->j,
	ExcludeTopologies->{WFCorrections}];
topVertexCT[i_,j_]:=CreateCTTopologies[1, i ->j,
	ExcludeTopologies->{WFCorrectionCTs}];

{diagPhi4SE,diagPhi4SECT} = InsertFields[#, {S[1]} -> {S[1]},
Sequence@@params]&/@{top[1,1],topCT[1,1]};
{diagVertex,diagVertexCT} = InsertFields[#,  {S[1],S[1]}->{S[1],S[1]},
Sequence@@params]&/@{topVertex[2,2],topVertexCT[2,2]};


diag1[0]=diagPhi4SE[[0]][Sequence@@diagPhi4SE,
	Sequence@@diagPhi4SECT];
diag2[0]=diagVertex[[0]][Sequence@@diagVertex,
	Sequence@@diagVertexCT];


Paint[diag1[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


Paint[diag2[0], ColumnsXRows -> {2, 1},SheetHeader->None,
Numbering -> Simple, ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


(* ::Text:: *)
(*The 1/(2Pi)^D prefactor is implicit.*)


(* ::Text:: *)
(*Self-energy including the counter-term*)


amp1[0] = FCFAConvert[CreateFeynAmp[diag1[0],Truncated->True,
	GaugeRules->{},PreFactor->1],
	IncomingMomenta->{p}, OutgoingMomenta->{p},
	LorentzIndexNames->{mu},
	LoopMomenta->{l}, UndoChiralSplittings->True,
	ChangeDimension->D, List->False, SMP->True,
	FinalSubstitutions->{Zm->SMP["Z_m"], Zphi->SMP["Z_phi"],
	GaugeXi[S[1]]->1,Mphi->m}]


(* ::Text:: *)
(*Quartic vertex including the counter-term*)


amp2[0] = FCFAConvert[CreateFeynAmp[diag2[0],Truncated->True,
	GaugeRules->{}, PreFactor->1],
	IncomingMomenta->{p1,p2}, OutgoingMomenta->{p3,p4},
	LorentzIndexNames->{mu}, LoopMomenta->{l},
	UndoChiralSplittings->True, ChangeDimension->D,
	List->False, SMP->True,FinalSubstitutions->{Zg->SMP["Z_g"],
	Zphi->SMP["Z_phi"], GaugeXi[S[1]]->1,Mphi->m}]


(* ::Section:: *)
(*Calculate the amplitudes*)


(* ::Subsection:: *)
(*Self-energy*)


amp1[1] = amp1[0]//ReplaceAll[#,{SMP["Z_phi"]->1+alpha SMP["d_phi"],
SMP["Z_m"]->1+alpha SMP["d_m"]}]&//Series[#,{alpha,0,1}]&//
Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*Express the self-energy in tems of the Passarino-Veltman coefficient functions.*)


amp1[2]=ToPaVe[amp1[1],l]


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


amp1Div[0]=PaVeUVPart[amp1[2],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,{SMP["Delta"],SMP["d_m"],
SMP["d_phi"]}]&//Simplify


(* ::Text:: *)
(*Equating the result to zero and solving for d_phi and d_m we obtain  the renormalization constants in  the minimal subtraction schemes.*)


sol[1]=Solve[SelectNotFree2[amp1Div[0], p]==0,
	SMP["d_phi"]]//Flatten//Simplify;
sol[2]=Solve[(SelectFree2[amp1Div[0], p]==0)/.sol[1],
	SMP["d_m"]]//Flatten//Simplify;
solMS1=Join[sol[1],sol[2]]/.{
	SMP["d_phi"]->SMP["d_phi^MS"],
	SMP["d_m"]->SMP["d_m^MS"],SMP["Delta"]->1/Epsilon
}
solMSbar1=Join[sol[1],sol[2]]/.{
	SMP["d_phi"]->SMP["d_phi^MSbar"],
	SMP["d_m"]->SMP["d_m^MSbar"]
}


(* ::Subsection:: *)
(*Quartic vertex*)


amp2[1] = amp2[0]//ReplaceRepeated[#,{SMP["Z_g"]->1+alpha SMP["d_g"],
SMP["Z_phi"]->1+alpha SMP["d_phi"]}]&//
Series[#,{alpha,0,1}]&//Normal//ReplaceAll[#,alpha->1]&


(* ::Text:: *)
(*Express the quartic vertex in tems of the Passarino-Veltman coefficient functions.*)


amp2[2]=ToPaVe[amp2[1],l]


(* ::Text:: *)
(*Discard all the finite pieces of the 1-loop amplitude*)


amp2Div[0]=PaVeUVPart[amp2[2],Prefactor->1/(2Pi)^D]//
FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//
FCHideEpsilon//SelectNotFree2[#,{SMP["Delta"],SMP["d_g"],SMP["d_phi"]}]&//Simplify


sol[3]=Solve[(amp2Div[0]==0)/.sol[1],
	SMP["d_g"]]//Flatten//Simplify;
solMS2=sol[3]/.{
	SMP["d_g"]->SMP["d_g^MS"],
	SMP["Delta"]->1/Epsilon
}
solMSbar2=sol[3]/.{
	SMP["d_g"]->SMP["d_g^MSbar"]
}


(* ::Section:: *)
(*Check the final results*)


knownResult = {
	SMP["d_phi^MS"] -> 0,
	SMP["d_m^MS"] -> (g*1/Epsilon)/(32*Pi^2),
	SMP["d_g^MS"] -> (3*g*1/Epsilon)/(32*Pi^2),

	SMP["d_phi^MSbar"] -> 0,
	SMP["d_m^MSbar"] -> (g*SMP["Delta"])/(32*Pi^2),
	SMP["d_g^MSbar"] -> (3*g*SMP["Delta"])/(32*Pi^2)
	};
FCCompareResults[Join[solMS1,solMS2,solMSbar1,solMSbar2],knownResult,
Text->{"\tCompare to Bailin and Love, Introduction to Gauge Field Theory, \
Eqs. 7.73-7.74 and Eqs. 7.76-7.77:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],4],0.001], " s."];
