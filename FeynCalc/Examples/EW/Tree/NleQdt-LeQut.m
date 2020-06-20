(* ::Package:: *)

(* :Title: NleQdt-LeQut                                                  	*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  Nle Qdt -> Le Qut, EW, total cross section, tree              *)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Neutrino down-type quark annihilation into a lepton and an up-type quark*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="Nle Qdt -> Le Qut, EW, total cross section, tree";
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


MakeBoxes[pu,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(u\)]\)";
MakeBoxes[pd,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(d\)]\)";
MakeBoxes[pn,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(n\)]\)";
MakeBoxes[pl,TraditionalForm]:="\!\(\*SubscriptBox[\(p\), \(l\)]\)";


(* ::Text:: *)
(*Enable CKM mixing*)


$CKM=True;


(* ::Text:: *)
(*To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge*)


InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];


diags = InsertFields[CreateTopologies[0, 2 -> 2],
{F[4,{1,1}],F[1,{2}]} -> {F[3,{1,1}],F[2,{2}]}, InsertionLevel -> {Classes},
Model -> {SM, UnitarySM},GenericModel->{Lorentz, UnitaryLorentz}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitude*)


amp[0] = FCFAConvert[CreateFeynAmp[diags,GaugeRules->{FAGaugeXi[W|Z]->Infinity}], 
IncomingMomenta->{pd,pn}, OutgoingMomenta->{pu,pl},ChangeDimension->4,
List->False, SMP->True, Contract->True,DropSumOver->True,  
FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2]*SMP["G_F"]*
SMP["m_W"]^2SMP["sin_W"]^2]}]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s, t, u, pd, pn, -pu, -pl, SMP["m_d"], 0, SMP["m_u"], SMP["m_l"]];


(* ::Section:: *)
(*Square the amplitude*)


(* ::Text:: *)
(*There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact*)


ampSquared[0] = (amp[0] (ComplexConjugate[amp[0]]))//
	FermionSpinSum[#, ExtraFactor -> 1/2]&//DiracSimplify//Factor


(* ::Text:: *)
(*In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (pl-pn)^2  <= m_mu^2 << m_W^2.*)


ampSquared[1]=ampSquared[0]//FCE//ReplaceAll[#,{pl-pn->0}]&//
	FeynAmpDenominatorExplicit//Series[#,{SMP["m_W"],Infinity,0}]&//Normal


(* ::Section:: *)
(*Total cross section*)


(* ::Text:: *)
(*The total cross-section *)


prefac=4Pi/(64 Pi^2 s) Sqrt[(s-SMP["m_l"]^2-SMP["m_u"]^2)^2-4 SMP["m_l"]^2 *
	SMP["m_u"]^2]/Sqrt[(s-SMP["m_d"]^2)^2]


crossSectionTotal=prefac*ampSquared[1]//PowerExpand


(* ::Text:: *)
(*If the lepton is a muon, then the up quark mass can be neglected*)


crossSectionTotalMuon=PowerExpand[crossSectionTotal/.{SMP["m_u"]->0,
	SMP["m_l"]->SMP["m_mu"]}]


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(SMP["G_F"]^2*(s - SMP["m_mu"]^2)^2*SMP["V_ud", -I]*SMP["V_ud", I])/(Pi*s)
};
FCCompareResults[{crossSectionTotalMuon},
knownResults,
Text->{"\tCompare to Grozin, \
Using REDUCE in High Energy Physics, Chapter 5.3:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];



