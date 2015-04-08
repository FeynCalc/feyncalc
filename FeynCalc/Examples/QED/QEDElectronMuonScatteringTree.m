(* ::Package:: *)



(* :Title: QEDElectronMuonScatteringTree                                    *)

(*
	 This software is covered by the GNU Lesser General Public License 3.
	 Copyright (C) 1990-2015 Rolf Mertig
	 Copyright (C) 1997-2015 Frederik Orellana
	 Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for electron
							muon scattering in QED at tree level                          *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for electron muon scattering in QED at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadTARCER = False;
$LoadPhi =$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topElMuScat = CreateTopologies[0, 2 -> 2];
diagsElMuScat = InsertFields[topElMuScat, {F[2, {1}], F[2, {2}]} ->
		{F[2, {1}], F[2, {2}]}, InsertionLevel -> {Classes},
		Model -> "SM", ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsElMuScat, ColumnsXRows -> {1, 1}, Numbering -> None];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampElMuScat = Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &, Apply[List,
		FCPrepareFAAmp[CreateFeynAmp[diagsElMuScat,
		Truncated -> False]]]]/.{FAMass["Muon"]->MMu,InMom1->p1,
		InMom2->p2,OutMom1->k1,OutMom2->k2}


(* ::Section:: *)
(*Unpolarized process e^- mu^- -> e^- mu^-*)


SetMandelstam[s, t, u, p1, p2, -k1, -k2, ME, MMu, ME, MMu];
sqAmpElMuScat = (Total[ampElMuScat] Total[(ComplexConjugate[ampElMuScat]//FCRenameDummyIndices)])//
		PropagatorDenominatorExplicit//Contract//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
		ReplaceAll[#, DiracTrace :> Tr]&//Contract//Simplify



masslessElectronsSqAmpElMuScat = (sqAmpElMuScat /. {ME -> 0})//Simplify


masslessElectronsSqAmpElMuScatPeskin= (8EL^4 (SP[p1,k2]SP[p2,k1]+SP[p1,p2]SP[k1,k2]-
		MMu^2 SP[p1,k1]))/(SP[k1-p1])^2// ReplaceAll[#,ME->0]&//FCI//ExpandScalarProduct//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.61: ",
		If[(masslessElectronsSqAmpElMuScatPeskin-masslessElectronsSqAmpElMuScat)===0,
		"Correct.", "Mistake!"]];


masslessElectronsMuonsSqAmpElMuScat = (masslessElectronsSqAmpElMuScat /. {MMu -> 0})//Simplify


mmasslessElectronsMuonsSqAmpElMuScatPeskin=((8EL^4/t^2)((s/2)^2+(u/2)^2))//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.71: ",
		If[(mmasslessElectronsMuonsSqAmpElMuScatPeskin-masslessElectronsMuonsSqAmpElMuScat)===0,
		"Correct.", "Mistake!"]];
