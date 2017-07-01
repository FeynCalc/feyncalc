(* ::Package:: *)

(* :Title: QCDGGToQiQBariTree                                               *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
							g g -> q_i qbar_i scattering in QCD at tree level             *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the g g -> q_i qbar_i scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topGGToQiQBari = CreateTopologies[0, 2 -> 2];
diagsGGToQiQBari = InsertFields[topGGToQiQBari,  {V[5],V[5]}-> {F[3, {1}],
		-F[3, {1}]}, InsertionLevel -> {Classes},
		Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[1],V[2]}];
Paint[diagsGGToQiQBari, ColumnsXRows -> {3, 1}, Numbering -> None,SheetHeader->None,ImageSize->{768,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampGGToQiQBari = FCFAConvert[CreateFeynAmp[diagsGGToQiQBari,Truncated -> False],IncomingMomenta->{k1,k2},
OutgoingMomenta->{p1,p2},UndoChiralSplittings->True,TransversePolarizationVectors->{k1,k2},
DropSumOver->True,ChangeDimension->4,SMP->True]


(* ::Section:: *)
(*Unpolarized process  g g -> q_i qbar_i*)


SetMandelstam[s, t, u, k1, k2, -p1, -p2, 0, 0, SMP["m_u"], SMP["m_u"]];
sqAmpGGToQiQBari =(1/8^2)(Total[ampGGToQiQBari] Total[(ComplexConjugate[ampGGToQiQBari])])//
PropagatorDenominatorExplicit//SUNSimplify[#,Explicit->True,
		SUNNToCACF->False]&//FermionSpinSum//Contract//ReplaceAll[#,{DiracTrace->Tr,
		SUNN->3}]&//DoPolarizationSums[#,k1,k2,ExtraFactor->1/2]&//DoPolarizationSums[#,k2,
		k1,ExtraFactor->1/2]&//Simplify


masslesssqAmpGGToQiQBari = TrickMandelstam[(sqAmpGGToQiQBari /. {SMP["m_u"] -> 0})//Simplify,{s,t,u,0}]


masslesssqAmpGGToQiQBariEllis=(1/6)SMP["g_s"]^4 (t^2+u^2)/(t u)-(3/8)SMP["g_s"]^4 (t^2+u^2)/(s^2);
Print["Check with Ellis, Stirling and Weber, Table 7.1: ",
			If[TrickMandelstam[Simplify[masslesssqAmpGGToQiQBariEllis-masslesssqAmpGGToQiQBari],{s,t,u,0}]===0, "CORRECT.", "!!! WRONG !!!"]];
