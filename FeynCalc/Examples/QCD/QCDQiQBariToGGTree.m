(* ::Package:: *)

(* :Title: QCDQiQBariToGGTree                                          *)

(*
	 This software is covered by the GNU Lesser General Public License 3.
	 Copyright (C) 1990-2015 Rolf Mertig
	 Copyright (C) 1997-2015 Frederik Orellana
	 Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
							q_i qbar_i -> g g scattering in QCD at tree level      *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the q_i qbar_i -> g g scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topQiQBariToGG = CreateTopologies[0, 2 -> 2];
diagsQiQBariToGG = InsertFields[topQiQBariToGG, {F[3, {1}], -F[3, {1}]} -> {V[5],V[5]},
		InsertionLevel -> {Classes}, Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[1],V[2]}];
Paint[diagsQiQBariToGG, ColumnsXRows -> {3, 1}, Numbering -> None,SheetHeader->None,ImageSize->{768,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampQiQBariToGG =FCFAConvert[CreateFeynAmp[diagsQiQBariToGG,Truncated -> False],IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},
DropSumOver->True,ChangeDimension->4,UndoChiralSplittings->True,List->False,TransversePolarizationVectors->{k1,k2}];


(* ::Section:: *)
(*Unpolarized process  q_i qbar_i -> g g *)


SetMandelstam[s, t, u, p1, p2, -k1, -k2, MU, MU, 0, 0];
sqAmpQiQBariToGG =(1/3^2)(ampQiQBariToGG*
		(ComplexConjugate[ampQiQBariToGG]//FCRenameDummyIndices))//PropagatorDenominatorExplicit//
		SUNSimplify[#,Explicit->True,SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2^2]&//
		Contract//ReplaceAll[#,{DiracTrace->Tr,SUNN->3}]&//DoPolarizationSums[#,k1,k2]&//
		DoPolarizationSums[#,k2,k1]&//Simplify


masslesssqAmpQiQBariToGG = TrickMandelstam[(sqAmpQiQBariToGG /. {MU -> 0})//Simplify,{s,t,u,0}]


masslesssqAmpQiQBariToGGEllis=(32/27)Gstrong^4 (t^2+u^2)/(t u)-(8/3)Gstrong^4 (t^2+u^2)/(s^2);
Print["Check with Ellis, Stirling and Weber, Table 7.1: ",
			If[TrickMandelstam[Simplify[masslesssqAmpQiQBariToGGEllis-masslesssqAmpQiQBariToGG],{s,t,u,0}]===0, "Correct.", "Mistake!"]];
