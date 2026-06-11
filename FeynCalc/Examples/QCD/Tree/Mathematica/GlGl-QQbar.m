(* ::Package:: *)

(* :Title: GlGl-QQbar                                                     	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2026 Rolf Mertig
	Copyright (C) 1997-2026 Frederik Orellana
	Copyright (C) 2014-2026 Vladyslav Shtabovenko
*)

(* :Summary:  Gl Gl -> Q Qbar, QCD, matrix element squared, tree           	*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Quark-antiquark pair production from gluon-gluon annihilation*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Gl Gl -> Q Qbar, QCD, matrix element squared, tree";
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


diags = InsertFields[CreateTopologies[0, 2 -> 2], {V[5], V[5]}->
		{F[3, {1}], -F[3, {1}]}, InsertionLevel -> {Classes},
		Model -> "SMQCD"];

Paint[diags, ColumnsXRows -> {4, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->128 {4, 1}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,p2},
	OutgoingMomenta->{q1,q2},UndoChiralSplittings->True,ChangeDimension->D,
	TransversePolarizationVectors->{p1,p2}, List->True, SMP->True,
	Contract->True,DropSumOver->True];


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, p2, -q1, -q2, 0, 0, SMP["m_u"], SMP["m_u"]];


(* ::Section:: *)
(*Square the amplitude*)


ampSquared[0]=SquareAmplitude[amp[0],ComplexConjugate[amp[0]],Real->True];


AbsoluteTiming[ampSquared[1]=FeynAmpDenominatorExplicit[ampSquared[0],FCParallelize->True]//
SUNSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[ampSquared[2]=ampSquared[1]//DoPolarizationSums[#,p1,p2,
FCParallelize->True,ExtraFactor->1/2]&//DoPolarizationSums[#,p2,p1,
FCParallelize->True,ExtraFactor->1/2]&;]


AbsoluteTiming[ampSquared[3]=ampSquared[2]//FermionSpinSum[#,FCParallelize->True]&//DiracSimplify[#,FCParallelize->True]&;]


AbsoluteTiming[ampSquared[4]=1/((SUNN^2-1)^2)ampSquared[3]//TrickMandelstam[#,{s,t,u,2  SMP["m_u"]^2},FCParallelize->True]&;]


ampSquared[5]=Collect2[ampSquared[4]//Total,CA,CF,D,Factoring->Function[{x},TrickMandelstam[x,{s,t,u,2  SMP["m_u"]^2}]]]


ampSquaredMassless[0] = Collect2[ampSquared[5]//ReplaceAll[#,{SMP["m_u"] -> 0}]&,D,CA,CF,
Factoring->Function[{x},TrickMandelstam[x,{s,t,u,2  SMP["m_u"]^2}]]]


ampSquaredMasslessSUNN3[0] = TrickMandelstam[SUNSimplify[ampSquaredMassless[0]/.D->4,SUNNToCACF->False]/.SUNN->3,{s,t,u,0}]


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(1/6)SMP["g_s"]^4 (t^2+u^2)/(t u)-(3/8)SMP["g_s"]^4 (t^2+u^2)/(s^2)
};
FCCompareResults[{ampSquaredMasslessSUNN3[0]},{knownResults},
Text->{"\tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, \
Table 7.1:","CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic},Factoring->
Function[x,Simplify[TrickMandelstam[x,{s,t,u,0}]]]]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];



