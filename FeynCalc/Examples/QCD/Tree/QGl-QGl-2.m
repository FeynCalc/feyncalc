(* ::Package:: *)

(* :Title: QGl-QGl-2														*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2023 Rolf Mertig
	Copyright (C) 1997-2023 Frederik Orellana
	Copyright (C) 2014-2023 Vladyslav Shtabovenko
*)

(* :Summary:  Q Gl -> Q Gl with ghosts, QCD, matrix element squared, tree	*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Quark-gluon scattering*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Q Gl -> Q Gl with ghosts, QCD, matrix element squared, tree";
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
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Nicer typesetting*)


MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";
MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";


diags = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}],V[5]}->
		{F[3, {1}],V[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];

Paint[diags, ColumnsXRows -> {2, 2}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,512}];


diagsGh1 = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}],U[5]}->
		{F[3, {1}],U[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];

Paint[diagsGh1, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


diagsGh2 = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}],-U[5]}->
		{F[3, {1}],-U[5]}, InsertionLevel -> {Classes}, Model -> "SMQCD"];

Paint[diagsGh2, ColumnsXRows -> {1, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags], IncomingMomenta->{p1,k1},
	OutgoingMomenta->{p2,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True,DropSumOver->True]


ampGh1[0] = FCFAConvert[CreateFeynAmp[diagsGh1], IncomingMomenta->{p1,k1},
	OutgoingMomenta->{p2,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True,DropSumOver->True]


ampGh2[0] = FCFAConvert[CreateFeynAmp[diagsGh2], IncomingMomenta->{p1,k1},
	OutgoingMomenta->{p2,k2},UndoChiralSplittings->True,ChangeDimension->4,
	List->False, SMP->True, Contract->True,DropSumOver->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, p1, k1, -p2, -k2, SMP["m_u"], 0, SMP["m_u"], 0];


(* ::Section:: *)
(*Square the amplitude*)


ampSquaredUnphys[0] = 1/(SUNN (SUNN^2-1))(amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2]&//
	DiracSimplify//DoPolarizationSums[#,k1,0]&//DoPolarizationSums[#,k2,0]&//
	TrickMandelstam[#,{s,t,u,2SMP["m_u"]^2}]&//Simplify


ampSquaredGh1[0] = 1/(SUNN (SUNN^2-1))(ampGh1[0] (ComplexConjugate[ampGh1[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2]&//
	DiracSimplify//TrickMandelstam[#,{s,t,u,2SMP["m_u"]^2}]&//
	Simplify


ampSquaredGh2[0] = 1/(SUNN (SUNN^2-1))(ampGh2[0] (ComplexConjugate[ampGh2[0]]))//
	FeynAmpDenominatorExplicit//SUNSimplify[#,Explicit->True,
	SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2]&//
	DiracSimplify//TrickMandelstam[#,{s,t,u,2SMP["m_u"]^2}]&//
	Simplify


(* ::Text:: *)
(*Subtract unphysical degrees of freedom using ghost contributions. Notice that here the averaging over the polarizations of*)
(*the initial gluons is done only after the subtraction of the unphysical contributions.*)


ampSquared[0]=1/2(ampSquaredUnphys[0]-ampSquaredGh1[0]-ampSquaredGh2[0])//
	Together


ampSquaredMassless[0] = ampSquared[0]//ReplaceAll[#,{SMP["m_u"] -> 0}]&//
	Simplify


ampSquaredMasslessSUNN3[0] = ampSquaredMassless[0]/.SUNN->3


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	-(4/9)SMP["g_s"]^4 (s^2+u^2)/(s u)+SMP["g_s"]^4 (u^2+s^2)/(t^2)
};
FCCompareResults[{ampSquaredMasslessSUNN3[0]},{knownResults},
Text->{"\tCompare to Ellis, Stirling and Weber, QCD and Collider Physics, \
Table 7.1:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic},Factoring->
Function[x,Simplify[TrickMandelstam[x,{s,t,u,0}]]]]
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
