(* ::Package:: *)

(* :Title: QEDElectronMuonScatteringTree                                    *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for electron
							muon scattering in QED at tree level                          *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for electron muon scattering in QED at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topElMuScat = CreateTopologies[0, 2 -> 2];
diagsElMuScat = InsertFields[topElMuScat, {F[2, {1}], F[2, {2}]} ->
		{F[2, {1}], F[2, {2}]}, InsertionLevel -> {Classes},
		Model -> "SM", ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsElMuScat, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampElMuScat = FCFAConvert[CreateFeynAmp[diagsElMuScat, Truncated -> False],
IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,
ChangeDimension->4,List->False,SMP->True]


(* ::Section:: *)
(*Unpolarized process e^- mu^- -> e^- mu^-*)


SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"], SMP["m_mu"], SMP["m_e"], SMP["m_mu"]];
sqAmpElMuScat = (ampElMuScat (ComplexConjugate[ampElMuScat]))//
		PropagatorDenominatorExplicit//Contract//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
		ReplaceAll[#, DiracTrace :> Tr]&//Contract//Simplify



masslessElectronsSqAmpElMuScat = (sqAmpElMuScat /. {SMP["m_e"] -> 0})//Simplify


masslessElectronsSqAmpElMuScatPeskin= (8SMP["e"]^4 (SP[p1,k2]SP[p2,k1]+SP[p1,p2]SP[k1,k2]-
		SMP["m_mu"]^2 SP[p1,k1]))/(SP[k1-p1])^2// ReplaceAll[#,SMP["m_e"]->0]&//FCI//ExpandScalarProduct//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.61: ",
		If[(masslessElectronsSqAmpElMuScatPeskin-masslessElectronsSqAmpElMuScat)===0,
		"CORRECT.", "!!! WRONG !!!"]];


masslessElectronsMuonsSqAmpElMuScat = (masslessElectronsSqAmpElMuScat /. {SMP["m_mu"] -> 0})//Simplify


mmasslessElectronsMuonsSqAmpElMuScatPeskin=((8SMP["e"]^4/t^2)((s/2)^2+(u/2)^2))//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.71: ",
		If[(mmasslessElectronsMuonsSqAmpElMuScatPeskin-masslessElectronsMuonsSqAmpElMuScat)===0,
		"CORRECT.", "!!! WRONG !!!"]];
