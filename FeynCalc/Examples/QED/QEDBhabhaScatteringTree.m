(* ::Package:: *)

(* :Title: QEDBhabhaScatteringTree                                          *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for Bhabha
							scattering in QED at tree level                               *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for Bhabha scattering in QED at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topBhabha = CreateTopologies[0, 2 -> 2];
diagsBhabha = InsertFields[topBhabha, {F[2, {1}], -F[2, {1}]} ->
		{F[ 2, {1}], -F[2, {1}]}, InsertionLevel -> {Classes},
		Model -> "SM", ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsBhabha, ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampBhabha=FCFAConvert[CreateFeynAmp[diagsBhabha, Truncated -> False],
IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,List->False, SMP->True]


(* ::Section:: *)
(*Unpolarized process  e^- e^+ -> e^- e^+ *)


SetMandelstam[s, t, u, p1, p2, -k1, -k2, SMP["m_e"], SMP["m_e"], SMP["m_e"], SMP["m_e"]];
sqAmpBhabha =
		(ampBhabha (ComplexConjugate[ampBhabha]))//PropagatorDenominatorExplicit//
		Contract//FermionSpinSum[#, ExtraFactor -> 1/2^2] & //ReplaceAll[#, DiracTrace :> Tr]&//Contract//Simplify


masslessSqAmpBhabha = (sqAmpBhabha /. {SMP["m_e"] -> 0})//Simplify


masslessSqAmpBhabhaLiterature =
(2 SMP["e"]^4 (s^2+u^2)/t^2 + 4  SMP["e"]^4 u^2/(s t) + 2 SMP["e"]^4 (t^2+u^2)/s^2);
Print["Check with the known result: ",
			If[Simplify[(masslessSqAmpBhabhaLiterature-masslessSqAmpBhabha)]===0, "CORRECT.", "!!! WRONG !!!"]];
