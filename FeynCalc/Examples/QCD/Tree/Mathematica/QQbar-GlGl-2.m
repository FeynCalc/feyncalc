(* ::Package:: *)

(* :Title: QQbar-GlGl-2														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Q Qbar -> Gl Gl with ghosts, QCD, matrix element squared,
	tree																	*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Quark-antiquark pair annihilation into gluons*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Q Qbar -> Gl Gl with ghosts, QCD, matrix element squared, tree";
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
LaunchKernels[8];

$ParallelizeFeynCalc = True; 
FCCheckVersion[10,2,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


FCAttachTypesettingRule[p1,{SubscriptBox,p,1}]
FCAttachTypesettingRule[p2,{SubscriptBox,p,2}]
FCAttachTypesettingRule[q1,{SubscriptBox,q,1}]
FCAttachTypesettingRule[q2,{SubscriptBox,q,2}]


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], -F[3, {1}]} ->
		{V[5], V[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];

Paint[diags, ColumnsXRows -> {4, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->128 {4, 1}];


diagsGh1 = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], -F[3, {1}]} ->
		{-U[5], U[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];
Paint[diagsGh1, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->128{1,1}];


diagsGh2 = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}], -F[3, {1}]} ->
		{U[5], -U[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];
Paint[diagsGh2, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->128{1,1}];


(* ::Section:: *)
(*Obtain the amplitudes*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{q1,q2},UndoChiralSplittings->True,ChangeDimension->4,
	List->True, SMP->True, Contract->True,DropSumOver->True];


ampGh1[0] = FCFAConvert[CreateFeynAmp[diagsGh1], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{q1,q2},UndoChiralSplittings->True,ChangeDimension->4,
	List->True, SMP->True, Contract->True,DropSumOver->True]


ampGh2[0] = FCFAConvert[CreateFeynAmp[diagsGh2], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{q1,q2},UndoChiralSplittings->True,ChangeDimension->4,
	List->True, SMP->True, Contract->True,DropSumOver->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -q1, -q2, SMP["m_u"], SMP["m_u"], 0, 0];


(* ::Section:: *)
(*Square the amplitudes*)


ampSquaredUnphys[0]=SquareAmplitude[amp[0],ComplexConjugate[amp[0]],Real->True];


AbsoluteTiming[ampSquaredUnphys[1]=FeynAmpDenominatorExplicit[ampSquaredUnphys[0],FCParallelize->True]//
SUNSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[ampSquaredUnphys[2]=ampSquaredUnphys[1]//DoPolarizationSums[#,q1,0,
FCParallelize->True]&//DoPolarizationSums[#,q2,0,FCParallelize->True]&;]


AbsoluteTiming[ampSquaredUnphys[3]=ampSquaredUnphys[2]//FermionSpinSum[#,ExtraFactor -> 1/2^2,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[ampSquaredUnphys[4]=1/(SUNN^2)ampSquaredUnphys[3]//TrickMandelstam[#,{s,t,u,2  SMP["m_u"]^2},FCParallelize->True]&;]


ampSquaredUnphys[5]=Collect2[ampSquaredUnphys[4]//Total,CA,CF,D,
Factoring->Function[{x},TrickMandelstam[x,{s,t,u,2  SMP["m_u"]^2}]]]


ampSquaredGh1[0] = 1/(SUNN^2)(ampGh1[0] (ComplexConjugate[ampGh1[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify//TrickMandelstam[#,{s,t,u,2  SMP["m_u"]^2}]&//Total


ampSquaredGh2[0] = 1/(SUNN^2)(ampGh1[0] (ComplexConjugate[ampGh1[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
	DiracSimplify//TrickMandelstam[#,{s,t,u,2  SMP["m_u"]^2}]&//Total


(* ::Text:: *)
(*Subtract unphysical degrees of freedom using ghost contributions*)


ampSquared[0]=ampSquaredUnphys[5]-ampSquaredGh1[0]-ampSquaredGh2[0]//Together


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_u"] -> 0}]&//
	TrickMandelstam[#,{s,t,u,0}]&


ampSquaredMasslessSUNN3[0] = SUNSimplify[ampSquaredMassless[0],SUNNToCACF->False]/.SUNN->3


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(32/27)SMP["g_s"]^4 (t^2+u^2)/(t u)-(8/3)SMP["g_s"]^4 (t^2+u^2)/(s^2)
};
FCCompareResults[{ampSquaredMasslessSUNN3[0]},{knownResults},
Text->{"\tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, \
Table 7.1:","CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic},Factoring->
Function[x,Simplify[TrickMandelstam[x,{s,t,u,0}]]]]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];



