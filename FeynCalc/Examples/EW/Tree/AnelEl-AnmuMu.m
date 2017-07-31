(* ::Package:: *)

(* :Title: EWElectronAntineutrinoElectronToMuonAntineutrinoMuonTree             *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the scattering cross section for an electron
			  anineutrino and an electron going to a muon antineutrino and
			  a muon in Electroweak theory at tree level                     *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the scattering cross section for an electron anineutrino and an electron going to a muon antineutrino and a muon in Electroweak theory at tree level "];
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
diagsElNuScatteringTree = InsertFields[topElNuScattering, {-F[1,
		{1}],F[2, {1}]} -> {-F[1,{2}],F[2,{2}]}, InsertionLevel -> {Classes},
		Model -> {SM, UnitarySM},GenericModel->{Lorentz, UnitaryLorentz}];
Paint[diagsElNuScatteringTree, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->False];


(* ::Section:: *)
(*Obtain the amplitude*)


ampElNuScatteringTree=FCFAConvert[CreateFeynAmp[diagsElNuScatteringTree],
List->False,SMP->True,ChangeDimension->4, IncomingMomenta->{q1,pe},OutgoingMomenta->{q2,pm},
FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s,t,u,q1,pe,-q2,-pm,0,SMP["m_e"],0,SMP["m_mu"]];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact*)


sqElNuScatteringTree=ampElNuScatteringTree ComplexConjugate[ampElNuScatteringTree]//
FermionSpinSum[#,ExtraFactor->1/2]&//ReplaceAll[#, DiracTrace :> Tr]&//Contract//Factor2


(* ::Text:: *)
(*In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (pm-q2)^2  <= m_mu^2 << m_W^2.*)


sqElNuScatteringTree2=(FCE[sqElNuScatteringTree]/.{pm+q2->0})//PropagatorDenominatorExplicit//Factor2


(* ::Section:: *)
(*Total cross-section*)


(* ::Text:: *)
(*We need to carry out the angular integration, so let us specify the values of the temporal and spatial components of the 4-vectors*)


TC[q2]=(s-SMP["m_mu"]^2)/(2Sqrt[s]);
TC[pe]=(s+SMP["m_e"]^2)/(2Sqrt[s]);
CSP[pe]=(s-SMP["m_e"]^2)^2/(4s);
CSP[pm]=(s-SMP["m_mu"]^2)^2/(4s);
CSP[q2]=(s-SMP["m_mu"]^2)^2/(4s);


sqElNuScatteringTree3=Integrate[Simplify[sqElNuScatteringTree2 /.u-> SMP["m_e"]^2-2(TC[q2]TC[pe]-Sqrt[CSP[q2]]Sqrt[CSP[pe]]x)],{x,-1,1}]


(* ::Text:: *)
(*The total cross-section *)


prefac=2Pi/(64 Pi^2 s) Sqrt[(s-SMP["m_mu"]^2)^2]/Sqrt[(s-SMP["m_e"]^2)^2]


crossSectionTotal=sqElNuScatteringTree3*prefac//PowerExpand//Factor2


(* ::Section:: *)
(*Check with the literature*)


crossSectionTotalKnown=(SMP["G_F"]^2*(s - SMP["m_mu"]^2)^2)(s^2+(SMP["m_mu"]^2+SMP["m_e"]^2)/2 s+SMP["m_mu"]^2 SMP["m_e"]^2)/(3Pi s^3);
Print["Check with Grozin, Using REDUCE in High Energy Physics, Chapter 5.3: ",
			If[Simplify[crossSectionTotal-crossSectionTotalKnown]===0, "CORRECT.", "!!! WRONG !!!"]];
