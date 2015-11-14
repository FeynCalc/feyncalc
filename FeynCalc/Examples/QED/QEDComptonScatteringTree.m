(* ::Package:: *)

(* :Title: QEDComptonScatteringTree                                         *)

(*
	 This software is covered by the GNU Lesser General Public License 3.
	 Copyright (C) 1990-2015 Rolf Mertig
	 Copyright (C) 1997-2015 Frederik Orellana
	 Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for Compton
							scattering in QED at tree level                               *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for Compton scattering in QED at tree level"];
];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topCompton = CreateTopologies[0, 2 -> 2];
diagsCompton = InsertFields[topCompton, {F[2, {1}], V[1]} ->
		{F[2, {1}], V[1]}, InsertionLevel -> {Classes},
		Model -> "SM", ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsCompton, ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,
ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampCompton=FCFAConvert[CreateFeynAmp[diagsCompton, Truncated -> False],
IncomingMomenta->{p1,k1},OutgoingMomenta->{p2,k2},TransversePolarizationVectors->{k1,k2},
UndoChiralSplittings->True,ChangeDimension->4,List->False]


(* ::Section:: *)
(*Unpolarized process  e^- gamma -> e^- gamma *)


SetMandelstam[s, t, u, p1, k1, -p2, -k2, ME, 0, ME, 0];
sqAmpCompton = (ampCompton (ComplexConjugate[ampCompton]//FCRenameDummyIndices))//
		PropagatorDenominatorExplicit//Expand//DoPolarizationSums[#,k1,0,ExtraFactor ->
		1/2]&//DoPolarizationSums[#,k2,0]&//Contract//FermionSpinSum[#, ExtraFactor -> 1/2]&//
		ReplaceAll[#, DiracTrace :> Tr] & // Contract//Simplify//TrickMandelstam[#,{s,t,u,2ME^2}]&//Simplify


sqAmpComptonPeskin = (2EL^4(SP[p1,k2]/SP[p1,k1]+SP[p1,k1]/SP[p1,k2]+
		2ME^2 (1/SP[p1,k1]-1/SP[p1,k2])+ME^4 (1/SP[p1,k1]-1/SP[p1,k2])^2))//FCI//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.87: ", If[(sqAmpComptonPeskin-sqAmpCompton)===0,
		"Correct.", "Mistake!"]];


masslessSqAmpCompton = (sqAmpCompton /. {ME -> 0})//Simplify
