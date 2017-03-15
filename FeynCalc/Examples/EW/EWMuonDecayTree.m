(* ::Package:: *)

(* :Title: EWMuonDecayTree                                                  *)

(*
	 This software is covered by the GNU General Public License 3.
	 Copyright (C) 1990-2016 Rolf Mertig
	 Copyright (C) 1997-2016 Frederik Orellana
	 Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the total decay rate for muon going into an
			  electron, an electron anitnueutrino and a muon neutrino in 
			  Electroweak Theory at tree level.                            *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the total decay rate for muon going into an electron, 
an electron anitnueutrino and a muon neutrino in Electroweak Theory at tree level."];
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


topMuonDecayTree = CreateTopologies[0, 1 -> 3];
diagsMuonDecayTree = InsertFields[topMuonDecayTree, {F[2, {2}]} -> {F[2,
		{1}],-F[1,{1}],F[1,{2}]}, InsertionLevel -> {Classes},
		Model -> {SM, UnitarySM},GenericModel->{Lorentz, UnitaryLorentz}];
Paint[diagsMuonDecayTree, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->False];


(* ::Section:: *)
(*Obtain the amplitude*)


ampMuonDecayTree=FCFAConvert[CreateFeynAmp[diagsMuonDecayTree,Truncated -> False],List->False,SMP->True,ChangeDimension->4,
IncomingMomenta->{p},OutgoingMomenta->{k,q1,q2},
FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[]
SP[k,k]=SMP["m_e"]^2;
SP[q1,q1]=0;
SP[q2,q2]=0;


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*We average over the polarizations of the muon, hence the additional factor 1/2*)


sqAmpMuonDecayTree = ampMuonDecayTree ComplexConjugate[ampMuonDecayTree]//FermionSpinSum[#,ExtraFactor -> 1/2]&//ReplaceAll[#, DiracTrace :> Tr]&//Contract//
Factor2


(* ::Text:: *)
(*In the following we neglect the momentum in the W-propagator as compared to the W-mass. This is a very good approximation at low energies, as then (k+q1)^2 = (p-q2)^2 <= m_mu^2 << m_W^2.*)


sqAmpMuonDecayTree2=(FCE[sqAmpMuonDecayTree]/.{k+q1->0})//PropagatorDenominatorExplicit


(* ::Section:: *)
(*Total decay rate for the unpolarized process*)


(* ::Text:: *)
(*To compute the total decay rate, we follow the calculation done in Okun, "Leptons and Quarks", Chapter 3. The differential decay rate is given by*)
(**)
(*d \[CapitalGamma] = 1/(2M) d^3 k / ((2\[Pi])^3 2 k^0) d^3 q1 / ((2\[Pi])^3 2 q1^0) d^3 q2 / ((2\[Pi])^3 2 q2^0) (2\[Pi])^4 \[Delta]^4 (q-q1-q2) sqAmpMuonDecayTree2 with q = p-k*)


prefac=(2SMP["m_mu"] (2Pi)^5 8)^-1;
diffDecayRate = prefac d3q1/En[q1] d3q2/En[q2] d3k/En[k] delta4[q-q1-q2] sqAmpMuonDecayTree2


(* ::Text:: *)
(*First we reduce the tensor integrals in q1 to q2 to scalar ones by using tensor decomposition*)


q1q2[mu_,nu_]:=ReplaceAll[Tdec[{{q1x,mu},{q2x,nu}},{q},List->False,Dimension->4],
{SP[q1x,q2x]->SP[q,q]/2, SP[q,q1x|q2x]->SP[q,q]/2}];


q1q2[mu,nu]//Factor2


diffDecayRate1=Uncontract[diffDecayRate,q1,q2,Pair->All]


diffDecayRate2=((diffDecayRate1//FCE)/. FV[q1,mu_]FV[q2,nu_]:>q1q2[mu,nu])//Contract//FCE


(* ::Text:: *)
(*Integrating over q1 and q2 (in the rest frame of the decaying muon) we get rid of the Dirac delta and simplify the integral*)


diffDecayRate3=diffDecayRate2/.{d3q2 delta4[q-q1-q2]-> delta[En[q]-2En[q1]]}/.{En[q2]->En[q1]}/.
{d3q1-> 4Pi dq10 En[q1]^2}/.{dq10 delta[En[q]-2En[q1]]->1/2}


(* ::Text:: *)
(*Then we use the kinematics of the process, to simplify things even further. Here we also use the fact that the mass of the *)
(*electron is very small as compared to its energy*)


diffDecayRate4=(diffDecayRate3//.{SP[q,p]->SMP["m_mu"]^2-SMP["m_mu"] En[k],SP[k,q]|SP[p,k]-> SMP["m_mu"] En[k],
SP[q,q]-> SMP["m_mu"]^2-2 SMP["m_mu"]En[k]})//Simplify


(* ::Text:: *)
(*Next we trade d3k for dOmega d k^0  (k^0)^2  and introduce Eps  that is defined as 2 k^0/ m_mu*)


diffDecayRate5=(diffDecayRate4/.d3k-> dk0 En[k]^2 4Pi /. En[k]-> Eps SMP["m_mu"]/2/. 
dk0-> dEps SMP["m_mu"]/2)//Factor2


(* ::Text:: *)
(*Integrating over Eps we arrive to the final result*)


decayRateTotal=Integrate[diffDecayRate5/.dEps->1,{Eps,0,1}]


(* ::Section:: *)
(*Check with the literature*)


decayRateTotalKnown=(SMP["G_F"]^2*SMP["m_mu"]^5)/(192*Pi^3);
Print["Check with the Okun, Chapter 3.2:",
			If[Simplify[decayRateTotal-decayRateTotalKnown]===0, "CORRECT.", "!!! WRONG !!!"]];
