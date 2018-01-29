(* ::Package:: *)

(* :Title: EWElectronMuonNeutrinoToMuonElectronNeutrinoTree                                                  *)

(*
	 This software is covered by the GNU General Public License 3.
	 Copyright (C) 1990-2016 Rolf Mertig
	 Copyright (C) 1997-2016 Frederik Orellana
	 Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the scattering cross section for an electron and
			  a muon neutrino going to a muon and an electro neutrino in 
			  Electroweak theory at tree level                              *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the scattering cross section for an electron and
a muon neutrino going to a muon and an electro neutrino in Electroweak theory at tree level."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge*)


InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];


topElNuScattering = CreateTopologies[0, 2 -> 2];
diagsElNuScatteringTree = InsertFields[topElNuScattering, {F[2,
		{1}],F[1, {2}]} -> {F[1,{1}],F[2,{2}]}, InsertionLevel -> {Classes},
		Model -> {SM, UnitarySM},GenericModel->{Lorentz, UnitaryLorentz}];
Paint[diagsElNuScatteringTree, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->False];


(* ::Section:: *)
(*Obtain the amplitude*)


ampElNuScatteringTree=FCFAConvert[CreateFeynAmp[diagsElNuScatteringTree],
List->False,SMP->True,ChangeDimension->4, IncomingMomenta->{p,q1},OutgoingMomenta->{q2,k},
FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s,t,u,p,q1,-q2,-k,SMP["m_e"],0,0,SMP["m_mu"]];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact*)


sqElNuScatteringTree=ampElNuScatteringTree FCRenameDummyIndices[ComplexConjugate[ampElNuScatteringTree]]//
FermionSpinSum[#,ExtraFactor->1/2]&//ReplaceAll[#, DiracTrace :> Tr]&//Contract//Factor2


(* ::Text:: *)
(*In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (k-q1)^2  <= m_mu^2 << m_W^2.*)


sqElNuScatteringTree2=(FCE[sqElNuScatteringTree]/.{k-q1->0})//PropagatorDenominatorExplicit//Factor2


(* ::Section:: *)
(*Total cross-section*)


(* ::Text:: *)
(*The total cross-section *)


prefac=4Pi/(64 Pi^2 s) Sqrt[(s-SMP["m_mu"]^2)^2]/Sqrt[(s-SMP["m_e"]^2)^2]  


crossSectionTotal=sqElNuScatteringTree2*prefac//PowerExpand


(* ::Section:: *)
(*Check with the literature*)


crossSectionTotalKnown=(SMP["G_F"]^2*(s - SMP["m_mu"]^2)^2)/(Pi*s);
Print["Check with Greiner and Mueller, Gauge Theory of Weak Interactions, Chapter 3:",
			If[Simplify[crossSectionTotal-crossSectionTotalKnown]===0, "CORRECT.", "!!! WRONG !!!"]];
