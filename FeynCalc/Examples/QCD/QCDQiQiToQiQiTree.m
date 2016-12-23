(* ::Package:: *)

(* :Title: QCDQiQiToQiQiTree                                                *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
							q_i q_j -> q_i q_j scattering in QCD at tree level            *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the q_i q_i -> q_i q_i scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts=True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topQiQiToQiQi = CreateTopologies[0, 2 -> 2];
diagsQiQiToQiQi = InsertFields[topQiQiToQiQi, {F[3, {1}], F[3, {1}]} -> {F[
		3, {1}], F[3, {1}]}, InsertionLevel -> {Classes},
		Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[1],V[2]}];
Paint[diagsQiQiToQiQi, ColumnsXRows -> {2, 1}, Numbering -> None,SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampQiQiToQiQi=FCFAConvert[CreateFeynAmp[diagsQiQiToQiQi,Truncated -> False],IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},
DropSumOver->True,ChangeDimension->4,UndoChiralSplittings->True,List->False,SMP->True];


(* ::Section:: *)
(*Unpolarized process  q_i q_i -> q_i q_i *)


SetMandelstam[s, t, u, p1, p2, -p3, -p4, SMP["m_u"], SMP["m_u"], SMP["m_u"], SMP["m_u"]];
sqAmpQiQiToQiQi =(1/3^2)*(ampQiQiToQiQi (ComplexConjugate[ampQiQiToQiQi]))//
		PropagatorDenominatorExplicit//Contract//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
		SUNSimplify[#,Explicit->True,SUNNToCACF->False]&//
		ReplaceAll[#,{DiracTrace->Tr,SUNN->3}]&//Contract//Simplify


masslesssqAmpQiQiToQiQi = (sqAmpQiQiToQiQi /. {SMP["m_u"] -> 0})//Simplify


masslesssqAmpQiQiToQiQiEllis=((4/9)SMP["g_s"]^4 ((s^2+u^2)/t^2+(s^2+t^2)/u^2)-(8/27)SMP["g_s"]^4 s^2/(t u));
Print["Check with Ellis, Stirling and Weber, Table 7.1: ",
			If[Simplify[(masslesssqAmpQiQiToQiQiEllis-masslesssqAmpQiQiToQiQi)]===0, "CORRECT.", "!!! WRONG !!!"]];
