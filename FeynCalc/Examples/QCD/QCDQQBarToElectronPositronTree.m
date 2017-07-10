(* ::Package:: *)

(* :Title: QCDQQBarToElectronPositronTree                                 *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
		      q qbar -> e^+ e^- scattering in QCD at tree level    *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for the 
				q qbar -> e^+ e^- scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


diagsAmp = InsertFields[CreateTopologies[0, 2 -> 2], {F[3, {1}],
		-F[3, {1}]} -> {F[2, {1}], -F[2, {1}]}, InsertionLevel -> {Particles},
		Model -> "SMQCD", ExcludeParticles -> {S[_],V[2]}];
Paint[diagsAmp, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampTree=3/2 SMP["e_Q"] FCFAConvert[CreateFeynAmp[diagsAmp,Truncated -> False],IncomingMomenta->{p1,p2},OutgoingMomenta->{k1,k2},
DropSumOver->True,ChangeDimension->4,UndoChiralSplittings->True,List->False,SMP->True,
FinalSubstitutions->{SMP["m_u"|"m_e"]->0}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[]
SetMandelstam[s,t,u,p1,p2,-k1,-k2,0,0,0,0];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*We average over the spins and the colors of the quarks, hence the additional factor 1/3^2 1/2^2*)


ampSquared=1/3^2 ampTree ComplexConjugate[ampTree]//FermionSpinSum[#,ExtraFactor -> 1/2^2]&//
DiracSimplify[#,DiracTraceEvaluate->True]&//PropagatorDenominatorExplicit//SUNSimplify[#,SUNNToCACF->False]&//
ReplaceAll[#,SUNN->3]&//Simplify


(* ::Section:: *)
(*Check with CompHEP*)


ampSquaredKnown=(2*(t^2 + u^2)*SMP["e"]^4*SMP["e_Q"]^2)/(3*s^2);
Print["Check with CompHEP: ",
			If[Simplify[FCI[ampSquared-ampSquaredKnown]]===0, "CORRECT.", "!!! WRONG !!!"]];
