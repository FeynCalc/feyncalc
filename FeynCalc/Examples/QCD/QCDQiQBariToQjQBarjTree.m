(* ::Package:: *)

(* :Title: QCDQiQBariToQjQBarjTree                                          *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
							q_i qbar_i -> q_j qbar_j scattering in QCD at tree level      *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the q_i qbar_i -> q_j qbar_j scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts=True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topQiQBariToQjQBarj = CreateTopologies[0, 2 -> 2];
diagsQiQBariToQjQBarj = InsertFields[topQiQBariToQjQBarj, {F[3, {1}], -F[3, {1}]} -> {F[3, {2}],
		-F[3, {2}]}, InsertionLevel -> {Classes}, Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[1],V[2]}];
Paint[diagsQiQBariToQjQBarj, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampQiQBariToQjQBarj=FCFAConvert[CreateFeynAmp[diagsQiQBariToQjQBarj,Truncated -> False],IncomingMomenta->{p1,p2},OutgoingMomenta->{p3,p4},
DropSumOver->True,ChangeDimension->4,UndoChiralSplittings->True,List->False,SMP->True];


(* ::Section:: *)
(*Unpolarized process  q_i qbar_i -> q_j qbar_j *)


SetMandelstam[s, t, u, p1, p2, -p3, -p4, SMP["m_u"], SMP["m_u"], SMP["m_c"], SMP["m_c"]];
sqAmpQiQBariToQjQBarj =(1/3^2)*(ampQiQBariToQjQBarj (ComplexConjugate[ampQiQBariToQjQBarj]//
		FCRenameDummyIndices))//PropagatorDenominatorExplicit//Contract//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
		ReplaceAll[#,{DiracTrace->Tr,SUNN->3}]&//Contract//Simplify//SUNSimplify[#,Explicit->True,
		SUNNToCACF->False]&//ReplaceAll[#,{SUNN->3}]&


masslesssqAmpQiQBariToQjQBarj = (sqAmpQiQBariToQjQBarj /. {SMP["m_u"] -> 0,SMP["m_c"]->0})//Simplify


masslesssqAmpQiQBariToQjQBarjEllis=((4/9)SMP["g_s"]^4 (t^2+u^2)/s^2);
Print["Check with Ellis, Stirling and Weber, Table 7.1: ",
			If[(masslesssqAmpQiQBariToQjQBarjEllis-masslesssqAmpQiQBariToQjQBarj)===0, "CORRECT.", "!!! WRONG !!!"]];
