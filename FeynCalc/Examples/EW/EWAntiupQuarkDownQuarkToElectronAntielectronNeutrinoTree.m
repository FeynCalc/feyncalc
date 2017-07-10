(* ::Package:: *)

(* :Title: EWUpAntiquarkDownQuarkToElectronAntielectronNeutrinoTree                                                  *)

(*
	 This software is covered by the GNU General Public License 3.
	 Copyright (C) 1990-2018 Rolf Mertig
	 Copyright (C) 1997-2018 Frederik Orellana
	 Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for an antiup quark
			  and a down quark annihilating into an electron and an 
			  antielectron neutrino in Electroweak Theory at tree level. *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the matrix element squared for an antiup quark
			  and a down quark annihilating into an electron and an 
			  antielectron neutrino in Electroweak Theory at tree level."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Enable CKM mixing*)


$CKM=True;


(* ::Text:: *)
(*To avoid dealing with Goldstone bosons we do  the computation in the unitary gauge*)


InitializeModel[{SM, UnitarySM}, GenericModel -> {Lorentz, UnitaryLorentz}];


diagsAmpTree = InsertFields[CreateTopologies[0, 2 -> 2], {-F[3, {1}],F[4, {1}]} -> {F[2,
		{1}],-F[1,{1}]}, InsertionLevel -> {Particles},
		Model -> {SM, UnitarySM},GenericModel->{Lorentz, UnitaryLorentz}];
Paint[diagsAmpTree, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->False];


(* ::Section:: *)
(*Obtain the amplitude*)


ampTree=FCFAConvert[CreateFeynAmp[diagsAmpTree,Truncated -> False],List->False,SMP->True,ChangeDimension->4,
IncomingMomenta->{p2,p1},OutgoingMomenta->{k1,k2},DropSumOver->True]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[]
SetMandelstam[s,t,u,p1,p2,-k1,-k2,0,0,0,0];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*We average over the spins and colors of the quarks hence the additional factor 1/3^2 1/2^2*)


ampSquared=1/3^2 ampTree ComplexConjugate[ampTree]//FermionSpinSum[#,ExtraFactor -> 1/2^2]&//
DiracSimplify[#,DiracTraceEvaluate->True]&//PropagatorDenominatorExplicit//SUNSimplify[#,SUNNToCACF->False]&//
ReplaceAll[#,SUNN->3]&


(* ::Section:: *)
(*Check with CompHEP*)


ampSquaredKnown=(u^2*SMP["e"]^4*SMP["V_ud", -I]*SMP["V_ud", I])/(12*(s - SMP["m_W"]^2)^2*SMP["sin_W"]^4);
Print["Check with CompHEP: ",
			If[Simplify[FCI[ampSquared-ampSquaredKnown]]===0, "CORRECT.", "!!! WRONG !!!"]];
