(* ::Package:: *)

(* :Title: QEDMuonProductionTree                                            *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for muon
							production in QED at tree level                               *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for muon production in QED at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts=True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topMuonProd = CreateTopologies[0, 2 -> 2];
diagsMuonProd =
		InsertFields[topMuonProd, {F[2, {1}], -F[2, {1}]} -> {F[2,
		{2}], -F[2, {2}]}, InsertionLevel -> {Classes}, Model -> "SM",
		ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsMuonProd, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampMuonProd = FCFAConvert[CreateFeynAmp[diagsMuonProd, Truncated -> False],
IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,List->False,SMP->True];


(* ::Section:: *)
(*Unpolarized process  e^- e^+ -> mu^- mu^+ *)


$ProcessID


SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"], SMP["m_e"], SMP["m_mu"], SMP["m_mu"]];
sqAmpMuonProd = (ampMuonProd (ComplexConjugate[ampMuonProd]//FCRenameDummyIndices))//
		PropagatorDenominatorExplicit//Contract//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
		ReplaceAll[#, DiracTrace :> Tr] &//Contract//Simplify


masslessElectronsSqAmpMuonProd = (sqAmpMuonProd /. {SMP["m_e"] -> 0})//Simplify


masslessElectronsMuonsSqAmpMuonProdPeskin = (8SMP["e"]^4 (SP[p1,k1]SP[p2,k2]+SP[p1,k2]SP[p2,k1]+SMP["m_mu"]^2 SP[p1,p2]))/(SP[p1+p2])^2//
		ReplaceAll[#,SMP["m_e"]->0]&//ExpandScalarProduct//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.10: ",
		If[(masslessElectronsMuonsSqAmpMuonProdPeskin-masslessElectronsSqAmpMuonProd)===0,
		"CORRECT.", "!!! WRONG !!!"]];


masslessElectronsMuonsSqAmpMuonProd = (masslessElectronsSqAmpMuonProd /. {SMP["m_mu"] -> 0})//Simplify


masslessElectronsMuonsSqAmpMuonProdPeskinMandelstam=((8SMP["e"]^4/s^2)((t/2)^2+(u/2)^2))//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.70: ",
			If[(masslessElectronsMuonsSqAmpMuonProdPeskinMandelstam-masslessElectronsMuonsSqAmpMuonProd)===0, "CORRECT.", "!!! WRONG !!!"]];


(* ::Section:: *)
(*Polarized process e_R^- e_L^+ -> mu_R^- mu_L^+ *)


ampMuonProdElRPosLMuRAntiMuL=ampMuonProd/.{Spinor[-Momentum[k2],SMP["m_mu"],1]->GA[6].Spinor[-Momentum[k2],SMP["m_mu"],1],
		Spinor[Momentum[p1],SMP["m_e"],1]->GA[6].Spinor[Momentum[p1],SMP["m_e"],1]}


sqAmpMuonProdElRPosLMuRAntiMuL = (((ampMuonProdElRPosLMuRAntiMuL (ComplexConjugate[ampMuonProdElRPosLMuRAntiMuL]//
		FCRenameDummyIndices))//PropagatorDenominatorExplicit//Contract//FermionSpinSum[#,
		SpinorCollect -> True]&//ReplaceAll[#, DiracTrace :> Tr] &//Contract)/.{SMP["m_e"]->0,SMP["m_mu"]->0})//Simplify


sqAmpMuonProdElRPosLMuRAntiMuLPeskin=(16SMP["e"]^4 (SP[p1,k2]SP[p2,k1]))/(SP[p1+p2])^2//
		ReplaceAll[#,{SMP["m_e"]->0,SMP["m_mu"]->0}]&//FCI//ExpandScalarProduct//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.21: ",
			If[(sqAmpMuonProdElRPosLMuRAntiMuLPeskin-sqAmpMuonProdElRPosLMuRAntiMuL)===0, "CORRECT.", "!!! WRONG !!!"]];
