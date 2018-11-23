(* ::Package:: *)

(* :Title: AnelEl-QubarQd            										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Anel El -> Qubar Qd, EW, total cross section, tree            *)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Electron electron-antineutrino annihilation into an antiup and a down*)
(*quark*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Anel El -> Qubar Qd, EW, total cross section, tree";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Enable CKM mixing*)


$CKM=True;


(* ::Text:: *)
(*To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge*)


InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];


diags = InsertFields[CreateTopologies[0, 2 -> 2], {-F[1,
		{1}],F[2, {1}]} -> {-F[3,{1}],F[4,{1}]}, InsertionLevel -> {Classes},
		Model -> {SM, UnitarySM},GenericModel->{Lorentz, UnitaryLorentz}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags],DropSumOver->True, IncomingMomenta->{q,pe},
	OutgoingMomenta->{l1,l2},ChangeDimension->4,List->False, SMP->True,
	Contract->True, FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2]*
	SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, q, pe, -l1, -l2, 0, SMP["m_e"], SMP["m_u"], SMP["m_d"]];


(* ::Section:: *)
(*Square the amplitude*)


(* ::Text:: *)
(*There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	FermionSpinSum[#, ExtraFactor -> 1/2]&//DiracSimplify


(* ::Text:: *)
(*In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (pm-q2)^2  <= m_mu^2 << m_W^2.*)


ampSquared[1]=ampSquared[0]//FCE//ReplaceAll[#,{l1+l2->0}]&//
	FeynAmpDenominatorExplicit//Factor2


(* ::Section:: *)
(*Total cross-section*)


(* ::Text:: *)
(*We need to carry out the angular integration, so let us specify the values of the temporal and spatial components of the 4-vectors*)


TC[pe]=(s+SMP["m_e"]^2)/(2Sqrt[s]);
TC[l1]=(s+(SMP["m_u"]^2-SMP["m_d"]^2))/(2Sqrt[s]);
CSP[pe]=(s-SMP["m_e"]^2)^2/(4s);
CSP[l1]=((s-SMP["m_u"]^2-SMP["m_d"]^2)^2-4SMP["m_u"]^2 SMP["m_d"]^2)/(4s);


prefac=2Pi/(64 Pi^2 s) Sqrt[CSP[l1]]/Sqrt[CSP[pe]];


integral=Collect2[ampSquared[1] /.u-> SMP["m_e"]^2+SMP["m_u"]^2-2(TC[l1]TC[pe]-Sqrt[CSP[l1]]Sqrt[CSP[pe]]x),x,IsolateNames->KK]//
Integrate[#,{x,-1,1}]&//FRH//SUNSimplify//Simplify


(* ::Text:: *)
(*The total cross-section *)


crossSectionTotal=integral*prefac//PowerExpand//Factor2


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(CA*SMP["G_F"]^2*Sqrt[(s - SMP["m_d"]^2 - 2*SMP["m_d"]*SMP["m_u"] -
	SMP["m_u"]^2)*(s - SMP["m_d"]^2 + 2*SMP["m_d"]*SMP["m_u"] - SMP["m_u"]^2)]*
(2*s^3 - s^2*SMP["m_d"]^2 - s*SMP["m_d"]^4 + s^2*SMP["m_e"]^2 +
s*SMP["m_d"]^2*SMP["m_e"]^2 - 2*SMP["m_d"]^4*SMP["m_e"]^2 -
s^2*SMP["m_u"]^2 + 2*s*SMP["m_d"]^2*SMP["m_u"]^2 +
s*SMP["m_e"]^2*SMP["m_u"]^2 + 4*SMP["m_d"]^2*SMP["m_e"]^2*SMP["m_u"]^2 -
s*SMP["m_u"]^4 - 2*SMP["m_e"]^2*SMP["m_u"]^4)*SMP["V_ud", -I]*
SMP["V_ud", I])/(6*Pi*s^3)
};
FCCompareResults[{crossSectionTotal},
knownResults,
Text->{"\tCompare to the known result:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
