(* ::Package:: *)

(* :Title: EWMuonDecayTree                                                  *)

(*
	 This software is covered by the GNU General Public License 3.
	 Copyright (C) 1990-2016 Rolf Mertig
	 Copyright (C) 1997-2016 Frederik Orellana
	 Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the decay
							of a muon into an electron, an electron anitnueutrino
				and a muon neutrino in Electroweak Theory at tree level.      *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the decay of a muon into an electron, an electron anitnueutrino and a muon neutrino in Electroweak Theory at tree level."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadTARCER = False;
$LoadPhi =$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topMuonDecayTree = CreateTopologies[0, 1 -> 3];
diagsMuonDecayTree = InsertFields[topMuonDecayTree, {F[2, {2}]} -> {F[2,
		{1}],-F[1,{1}],F[1,{2}]}, InsertionLevel -> {Classes},
		Model -> "SM", ExcludeParticles -> {S[_], V[2]}];
Paint[diagsMuonDecayTree, ColumnsXRows -> {2, 1}, Numbering -> None];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampMuonDecayTree = (Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &,
		Apply[List, FCPrepareFAAmp[CreateFeynAmp[diagsMuonDecayTree,
		Truncated -> False],SMP->True]]]//ChangeDimension[#,4]&)/.{InMom1->p,
		OutMom1->p1,OutMom2->p2,OutMom3->p3}


(* ::Section:: *)
(*Unpolarized process  mu -> e^- nubar_e nu_mu*)


ScalarProduct[p1,p1]=SMP["m_e"];
ScalarProduct[p2,p2]=0;
ScalarProduct[p3,p3]=0;
sqAmpMuonDecayTree = Total[ampMuonDecayTree] (Total[ampMuonDecayTree]//ComplexConjugate//
		FCRenameDummyIndices)//PropagatorDenominatorExplicit//Contract//
		FermionSpinSum[#,ExtraFactor -> 1/2]&//ReplaceAll[#, DiracTrace :> Tr]&//Contract//Simplify


approxSqAmpMuonDecayTree = (sqAmpMuonDecayTree /. {SMP["m_e"] -> 0,ScalarProduct[p1,p2]:>0,
		SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]})//Simplify


approxSqAmpMuonDecayTreeKnown=64SMP["G_F"]^2ScalarProduct[p,p2]ScalarProduct[p1,p3];
Print["Check with the literature: ",
			If[Simplify[approxSqAmpMuonDecayTree-approxSqAmpMuonDecayTreeKnown]===0, "CORRECT.", "!!! WRONG !!!"]];

