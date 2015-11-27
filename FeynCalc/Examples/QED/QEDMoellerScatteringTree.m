(* ::Package:: *)

(* :Title: QEDMoellerScatteringTree                                         *)

(*
	 This software is covered by the GNU Lesser General Public License 3.
	 Copyright (C) 1990-2015 Rolf Mertig
	 Copyright (C) 1997-2015 Frederik Orellana
	 Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for Moeller
							scattering in QED at tree level                               *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for Moeller scattering in QED at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topMoeller = CreateTopologies[0, 2 -> 2];
diagsMoeller = InsertFields[topMoeller, {F[2, {1}], F[2, {1}]} -> {F[
			2, {1}], F[2, {1}]}, InsertionLevel -> {Classes}, Model -> "SM",
			ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsMoeller, ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampMoeller=FCFAConvert[CreateFeynAmp[diagsMoeller, Truncated -> False],
IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},UndoChiralSplittings->True,ChangeDimension->4,List->False]


(* ::Section:: *)
(*Unpolarized process  e^- e^- -> e^- e^- *)


SetMandelstam[s, t, u, p1, p2, -k1, -k2, ME, ME, ME, ME];
sqAmpMoeller =
		(ampMoeller (ComplexConjugate[ampMoeller]//FCRenameDummyIndices))//
		PropagatorDenominatorExplicit//Contract//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
		ReplaceAll[#, DiracTrace :> Tr] & // Contract//Simplify


masslessSqAmpMoeller = (sqAmpMoeller /. {ME -> 0})//Simplify


masslessSqAmpMoellerLiterature = 
2 EL^4 (s^2/t^2+ u^2/t^2   + s^2/u^2 + t^2/u^2   ) + 4 EL^4 s^2/(t u);
Print["Check with the known result: ",
			If[Simplify[(masslessSqAmpMoellerLiterature-masslessSqAmpMoeller)]===0, "CORRECT.", "!!! WRONG !!!"]];
