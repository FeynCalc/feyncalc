(* ::Package:: *)

(* :Title: EWNeutrinoDownQuarkToLeptonUpQuarkTree                   *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:
		Computation of the scattering cross section for a 
		neutrino and a down-type quark going to the 
		corresponding lepton and up-type quark in the 
		Electroweak theory at tree level.                          *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the scattering cross section for a neutrino and
a down-type quark going to the corresponding lepton and up-type quark in Electroweak theory at tree level."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
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


topQuarkNuScattering = CreateTopologies[0, 2 -> 2];
diagsQuarkNuScatteringTree = InsertFields[topQuarkNuScattering,
{F[4,{1,1}],F[1,{2}]} -> {F[3,{1,1}],F[2,{2}]}, InsertionLevel -> {Classes},
Model -> {SM, UnitarySM},GenericModel->{Lorentz, UnitaryLorentz}];
Paint[diagsQuarkNuScatteringTree, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->False];


(* ::Section:: *)
(*Obtain the amplitude*)


ampQuarkNuScatteringTree=FCFAConvert[CreateFeynAmp[diagsQuarkNuScatteringTree],DropSumOver->True,
List->False,SMP->True,ChangeDimension->4, IncomingMomenta->{pd,pn},OutgoingMomenta->{pu,pl},
FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2],
CKM[1,1]->SMP[{"V_ud",I}]}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s,t,u,pd,pn,-pu,-pl,SMP["m_d"],0,SMP["m_u"],SMP["m_l"]];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact*)


sqAmpQuarkNuScatteringTree=ampQuarkNuScatteringTree ComplexConjugate[ampQuarkNuScatteringTree]//
FermionSpinSum[#,ExtraFactor->1/2]&//ReplaceAll[#, DiracTrace :> Tr]&//Contract//Factor2


(* ::Text:: *)
(*In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (pl-pn)^2  <= m_l^2 << m_W^2.*)


sqAmpQuarkNuScatteringTree2=(FCE[sqAmpQuarkNuScatteringTree]/.{pl-pn->0})//PropagatorDenominatorExplicit//Factor2


(* ::Section:: *)
(*Total cross-section*)


(* ::Text:: *)
(*The total cross-section *)


prefac=4Pi/(64 Pi^2 s) Sqrt[(s-SMP["m_l"]^2-SMP["m_u"]^2)^2-4 SMP["m_l"]^2 SMP["m_u"]^2]/Sqrt[(s-SMP["m_d"]^2)^2]


crossSectionTotal=sqAmpQuarkNuScatteringTree2*prefac//PowerExpand


(* ::Text:: *)
(*If the lepton is a muon, then the up quark mass can be neglected*)


crossSectionTotalMuon=PowerExpand[crossSectionTotal/.{SMP["m_u"]->0,SMP["m_l"]->SMP["m_mu"]}]


(* ::Section:: *)
(*Check with the literature*)


(* ::Text:: *)
(*The result for the muon and neglected up quark mass can be found in Grozin, "Using REDUCE in High Energy Physics", Chapter 5.3*)


crossSectionTotalMuonKnown=(SMP["G_F"]^2*(s - SMP["m_mu"]^2)^2*SMP["V_ud", -I]*SMP["V_ud", I])/(Pi*s);
Print["Check with Grozin, Chapter 5.3:",
			If[Simplify[crossSectionTotalMuon-crossSectionTotalMuonKnown]===0, "CORRECT.", "!!! WRONG !!!"]];
