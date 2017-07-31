(* ::Package:: *)

(* :Title: EWElectronAntineutrinoElectronToAntiupQuarkDownQuarkTree             *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the scattering cross section for an electron
			  anineutrino and an electron going to an antiup quark and
			  a down quark in Electroweak theory at tree level *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the scattering cross section for an electron anineutrino and an electron going to an antiup quark and a down antiquark in Electroweak theory at tree level"];
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


diagsAmp = InsertFields[CreateTopologies[0, 2 -> 2], {-F[1,
		{1}],F[2, {1}]} -> {-F[3,{1}],F[4,{1}]}, InsertionLevel -> {Classes},
		Model -> {SM, UnitarySM},GenericModel->{Lorentz, UnitaryLorentz}];
Paint[diagsAmp, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->False];


(* ::Section:: *)
(*Obtain the amplitude*)


ampTree=FCFAConvert[CreateFeynAmp[diagsAmp],DropSumOver->True,
List->False,SMP->True,ChangeDimension->4, IncomingMomenta->{q,pe},OutgoingMomenta->{l1,l2},
FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[];
SetMandelstam[s,t,u,q,pe,-l1,-l2,0,SMP["m_e"],SMP["m_u"],SMP["m_d"]];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*There is no polarization averaging for neutrinos here, as right handed neutrinos do not interact*)


ampTreeSquared=ampTree ComplexConjugate[ampTree]//
FermionSpinSum[#,ExtraFactor->1/2]&//ReplaceAll[#, DiracTrace :> Tr]&//Contract//Factor2


(* ::Text:: *)
(*In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a good approximation at low energies.*)


ampTreeSquared2=(FCE[ampTreeSquared]/.{l1+l2->0})//PropagatorDenominatorExplicit//Factor2


(* ::Section:: *)
(*Total cross-section*)


(* ::Text:: *)
(*We need to carry out the angular integration, so let us specify the values of the temporal and spatial components of the 4-vectors*)


TC[pe]=(s+SMP["m_e"]^2)/(2Sqrt[s]);
TC[l1]=(s+(SMP["m_u"]^2-SMP["m_d"]^2))/(2Sqrt[s]);
CSP[pe]=(s-SMP["m_e"]^2)^2/(4s);
CSP[l1]=((s-SMP["m_u"]^2-SMP["m_d"]^2)^2-4SMP["m_u"]^2 SMP["m_d"]^2)/(4s);


ampTreeSquared3=Collect2[ampTreeSquared2 /.u-> SMP["m_e"]^2+SMP["m_u"]^2-2(TC[l1]TC[pe]-Sqrt[CSP[l1]]Sqrt[CSP[pe]]x),x,IsolateNames->KK]//
Integrate[#,{x,-1,1}]&//FRH//SUNSimplify//Simplify


(* ::Text:: *)
(*The total cross-section *)


prefac=2Pi/(64 Pi^2 s) Sqrt[CSP[l1]]/Sqrt[CSP[pe]]


crossSectionTotal=ampTreeSquared3*prefac//PowerExpand//Simplify//Factor2


(* ::Section:: *)
(*Check with the literature*)


crossSectionTotalKnown=(CA*SMP["G_F"]^2*Sqrt[(s - SMP["m_d"]^2 - 2*SMP["m_d"]*SMP["m_u"] - SMP["m_u"]^2)*(s - SMP["m_d"]^2 + 2*SMP["m_d"]*SMP["m_u"] - SMP["m_u"]^2)]*
  (2*s^3 - s^2*SMP["m_d"]^2 - s*SMP["m_d"]^4 + s^2*SMP["m_e"]^2 + s*SMP["m_d"]^2*SMP["m_e"]^2 - 2*SMP["m_d"]^4*SMP["m_e"]^2 - s^2*SMP["m_u"]^2 + 2*s*SMP["m_d"]^2*SMP["m_u"]^2 + 
   s*SMP["m_e"]^2*SMP["m_u"]^2 + 4*SMP["m_d"]^2*SMP["m_e"]^2*SMP["m_u"]^2 - s*SMP["m_u"]^4 - 2*SMP["m_e"]^2*SMP["m_u"]^4)*SMP["V_ud", -I]*SMP["V_ud", I])/(6*Pi*s^3);
Print["Check with the known result: ",
			If[Simplify[crossSectionTotal-crossSectionTotalKnown]===0, "CORRECT.", "!!! WRONG !!!"]];
