(* ::Package:: *)

(* :Title: QEDPairAnnihilationTree                                          *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for electron
							postiron annihilation into two photons in QED at tree level   *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for electron postiron annihilation into two photons in QED at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts=True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topPairAnnihilation = CreateTopologies[0, 2 -> 2];
diagsPairAnnihilation = InsertFields[topPairAnnihilation, {F[2, {1}],
		-F[2, {1}]} -> {V[1], V[1]}, InsertionLevel -> {Classes},
		Model -> "SM", ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsPairAnnihilation, ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampPairAnnihilation=FCFAConvert[CreateFeynAmp[diagsPairAnnihilation, Truncated -> False],
IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,List->False,SMP->True]


(* ::Section:: *)
(*Unpolarized process  e^+ e^- -> 2 gamma *)


SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"], SMP["m_e"], 0, 0];
sqAmpPairAnnihilation = (ampPairAnnihilation (ComplexConjugate[ampPairAnnihilation]//
		FCRenameDummyIndices))//PropagatorDenominatorExplicit//Expand//DoPolarizationSums[#,k1,
		0]&//DoPolarizationSums[#,k2,0]&//Contract//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
		ReplaceAll[#, DiracTrace -> Tr] & // Contract//Simplify//TrickMandelstam[#,
		{s,t,u,2SMP["m_e"]^2}]&//Simplify


sqAmpPairAnnihilationPeskin = (2SMP["e"]^4(SP[p1,k2]/SP[p1,k1]+SP[p1,k1]/SP[p1,k2]+
		2SMP["m_e"]^2 (1/SP[p1,k1]+1/SP[p1,k2])-SMP["m_e"]^4 (1/SP[p1,k1]+1/SP[p1,k2])^2))//FCI//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.105: ",
			If[(sqAmpPairAnnihilationPeskin-sqAmpPairAnnihilation)===0, "CORRECT.", "!!! WRONG !!!"]];


sqMasslessAmpPairAnnihilation=sqAmpPairAnnihilation//ReplaceAll[#,SMP["m_e"]->0]&
