(* ::Package:: *)

(* :Title: Z-FFbar															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  Z -> F Fbar, EW, total decay rate, tree              			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Z boson decaying into a fermion-antifermion pair*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Z -> F Fbar, EW, total decay rate, tree";
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


MakeBoxes[k1,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(1\)]\)";
MakeBoxes[k2,TraditionalForm]:="\!\(\*SubscriptBox[\(k\), \(2\)]\)";
MakeBoxes[l1,TraditionalForm]:="\!\(\*SubscriptBox[\(l\), \(1\)]\)";
MakeBoxes[l2,TraditionalForm]:="\!\(\*SubscriptBox[\(l\), \(2\)]\)";
MakeBoxes[p1,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(1\)]\)";
MakeBoxes[p2,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(2\)]\)";


diagsLeptonsMassless = InsertFields[CreateTopologies[0,1 -> 2],
	{V[2]} -> {F[1,{l}],-F[1,{l}]}, InsertionLevel -> {Classes}];

Paint[diagsLeptonsMassless, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


diagsLeptonsMassive = InsertFields[CreateTopologies[0,1 -> 2],
	{V[2]} -> {F[2,{l}],-F[2,{l}]}, InsertionLevel -> {Classes}];

Paint[diagsLeptonsMassive, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


diagsUpQuarks = InsertFields[CreateTopologies[0,1 -> 2],
	{V[2]} -> {F[3,{l}],-F[3,{l}]}, InsertionLevel -> {Classes}];

Paint[diagsUpQuarks, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


diagsDownQuarks = InsertFields[CreateTopologies[0,1 -> 2],
	{V[2]} -> {F[4,{l}],-F[4,{l}]}, InsertionLevel -> {Classes}];

Paint[diagsDownQuarks, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


ampLeptonsMassless[0]=FCFAConvert[CreateFeynAmp[diagsLeptonsMassless],IncomingMomenta->{p},
	OutgoingMomenta->{l1,l2},List->False,ChangeDimension->4,
	DropSumOver->True,SMP->True,Contract->True,
	FinalSubstitutions->{MLE[l]->SMP["m_l"], SMP["e"]->Sqrt[8/Sqrt[2]*
	SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]


ampLeptonsMassive[0]=FCFAConvert[CreateFeynAmp[diagsLeptonsMassive],IncomingMomenta->{p},
	OutgoingMomenta->{p1,p2},List->False,ChangeDimension->4,
	DropSumOver->True,SMP->True,Contract->True,
	FinalSubstitutions->{MLE[l]->SMP["m_l"], SMP["e"]->Sqrt[8/Sqrt[2]*
	SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]


ampUpQuarks[0]=FCFAConvert[CreateFeynAmp[diagsUpQuarks],IncomingMomenta->{p},
	OutgoingMomenta->{k1,k2},List->False,ChangeDimension->4,
	DropSumOver->True,SMP->True,Contract->True,
	FinalSubstitutions->{MQU[l]->SMP["m_q"], SMP["e"]->Sqrt[8/Sqrt[2]*
	SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]


ampDownQuarks[0]=FCFAConvert[CreateFeynAmp[diagsDownQuarks],IncomingMomenta->{p},
	OutgoingMomenta->{k1,k2},List->False,ChangeDimension->4,
	DropSumOver->True,SMP->True,Contract->True,
	FinalSubstitutions->{MQD[l]->SMP["m_q"], SMP["e"]->Sqrt[8/Sqrt[2]*
	SMP["G_F"]  SMP["m_Z"]^2 SMP["cos_W"]^2 SMP["sin_W"]^2]}]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[]
SP[p]=SMP["m_Z"]^2;
SP[k1]=SMP["m_q"]^2;
SP[k2]=SMP["m_q"]^2;

SP[p1]=SMP["m_l"]^2;
SP[p2]=SMP["m_l"]^2;

SP[l1]=0;
SP[l2]=0;

SP[k1,k2]=Simplify[(SP[p]-SP[k1]-SP[k2])/2];
SP[p,k1]=Simplify[ExpandScalarProduct[SP[k1+k2,k1]]];
SP[p,k2]=Simplify[ExpandScalarProduct[SP[k1+k2,k2]]];

SP[p1,p2]=Simplify[(SP[p]-SP[p1]-SP[p2])/2];
SP[p,p1]=Simplify[ExpandScalarProduct[SP[p1+p2,p1]]];
SP[p,p2]=Simplify[ExpandScalarProduct[SP[p1+p2,p2]]];

SP[l1,l2]=Simplify[(SP[p]-SP[l1]-SP[l2])/2];
SP[p,l1]=Simplify[ExpandScalarProduct[SP[l1+l2,l1]]];
SP[p,l2]=Simplify[ExpandScalarProduct[SP[l1+l2,l2]]];


(* ::Section:: *)
(*Square the amplitudes*)


(* ::Text:: *)
(*We average over the polarizations of the W-boson, hence the additional factor 1/3*)


ampSquaredLeptonsMassless[0] =
	(ampLeptonsMassless[0] (ComplexConjugate[ampLeptonsMassless[0]]))//
	FermionSpinSum//DiracSimplify//
	DoPolarizationSums[#,p,ExtraFactor -> 1/3]&//Simplify


ampSquaredLeptonsMassive[0] =
	(ampLeptonsMassive[0] (ComplexConjugate[ampLeptonsMassive[0]]))//
	FermionSpinSum//DiracSimplify//
	DoPolarizationSums[#,p,ExtraFactor -> 1/3]&//Simplify


ampSquaredUpQuarks[0] =
	(ampUpQuarks[0] (ComplexConjugate[ampUpQuarks[0]]))//
	FermionSpinSum//DiracSimplify//SUNSimplify//
	DoPolarizationSums[#,p,ExtraFactor -> 1/3]&//Simplify


ampSquaredDownQuarks[0] =
	(ampDownQuarks[0] (ComplexConjugate[ampDownQuarks[0]]))//
	FermionSpinSum//DiracSimplify//SUNSimplify//
	DoPolarizationSums[#,p,ExtraFactor -> 1/3]&//Simplify


(* ::Section:: *)
(*Total decay rates*)


phaseSpacePrefactor[m1_,m2_,M_]:=1/(16 Pi M)Sqrt[1-(m1+m2)^2/M^2]*
	Sqrt[1-(m1-m2)^2/M^2];


totalDecayRateLeptonsMassless=phaseSpacePrefactor[0,0,SMP["m_Z"]]*
	ampSquaredLeptonsMassless[0]//Simplify


totalDecayRateLeptonsMassive=phaseSpacePrefactor[SMP["m_l"],SMP["m_l"],SMP["m_Z"]]*
	ampSquaredLeptonsMassive[0]//Simplify


totalDecayRateUpQuarks=phaseSpacePrefactor[SMP["m_q"],SMP["m_q"],SMP["m_Z"]]*
	ampSquaredUpQuarks[0]//Simplify


totalDecayRateDownQuarks=phaseSpacePrefactor[SMP["m_q"],SMP["m_q"],SMP["m_Z"]]*
	ampSquaredDownQuarks[0]//Simplify


(* ::Section:: *)
(*Check the final results*)


tmp=(SMP["G_F"]*Sqrt[1 - (4*mf^2)/SMP["m_Z"]^2]*SMP["m_Z"]^3*
	(cv^2(1+2mf^2/SMP["m_Z"]^2)+ca^2(1-4mf^2/SMP["m_Z"]^2)))/(6*Pi Sqrt[2]);
knownResults = {
	tmp/.{cv|ca->1/2,mf->0},
	tmp/.{cv->-1/2+2SMP["sin_W"]^2,ca->-1/2,mf->SMP["m_l"]},
	CA tmp/.{cv->1/2-4/3SMP["sin_W"]^2,ca->1/2,mf->SMP["m_q"]},
	CA tmp/.{cv->-1/2+2/3SMP["sin_W"]^2,ca->-1/2,mf->SMP["m_q"]}
};
FCCompareResults[{
	totalDecayRateLeptonsMassless,
	totalDecayRateLeptonsMassive,
	totalDecayRateUpQuarks,
	totalDecayRateDownQuarks},
knownResults,
Text->{"\tCompare to Grozin, \
Using REDUCE in High Energy Physics, Chapter 5.2:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
