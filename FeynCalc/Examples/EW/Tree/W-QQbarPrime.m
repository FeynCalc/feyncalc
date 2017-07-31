(* ::Package:: *)

(* :Title: EWWBosonToQQbarp                                        *)

(*
	 This software is covered by the GNU General Public License 3.
	 Copyright (C) 1990-2018 Rolf Mertig
	 Copyright (C) 1997-2018 Frederik Orellana
	 Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  W \[Rule] q qbar', EW theory, total decay rate, 
              tree level                                            *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["W \[Rule] q qbar', EW theory, total decay rate, tree level"];
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


diagsAmpTree = InsertFields[CreateTopologies[0, 1 -> 2], 
{V[3]} -> {-F[3,{1}],F[4,{1}]}, InsertionLevel -> {Particles}];
Paint[diagsAmpTree, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->False];


(* ::Section:: *)
(*Obtain the amplitude*)


ampTree=FCFAConvert[CreateFeynAmp[diagsAmpTree,Truncated -> False],List->False,SMP->True,ChangeDimension->4,
IncomingMomenta->{p},OutgoingMomenta->{k1,k2},DropSumOver->True,FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[]
SP[p]=SMP["m_W"]^2;
SP[k1]=SMP["m_u"]^2;
SP[k2]=SMP["m_d"]^2;
SP[k1,k2]=(SMP["m_W"]^2-SMP["m_u"]^2-SMP["m_d"]^2)/2;
SP[p,k1]=Simplify[ExpandScalarProduct[SP[k1+k2,k1]]];
SP[p,k2]=Simplify[ExpandScalarProduct[SP[k1+k2,k2]]];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*We average over the polarizations of the W-boson*)


ampTreeSquared=ampTree ComplexConjugate[ampTree]//DoPolarizationSums[#,p, ExtraFactor->1/3]&//
FermionSpinSum//DiracSimplify[#,DiracTraceEvaluate->True]&//SUNSimplify


(* ::Section:: *)
(*Calculate the total decay rate*)


phaseSpacePrefactor[m1_,m2_,M_]:=1/(16 Pi M)Sqrt[1-(m1+m2)^2/M^2]Sqrt[1-(m1-m2)^2/M^2];


totalDecayRate=phaseSpacePrefactor[SMP["m_u"],SMP["m_d"],SMP["m_W"]]ampTreeSquared//Simplify//
ReplaceAll[#,Sqrt[x_]Sqrt[y_]:>Sqrt[ExpandAll[x y]]]&//Factor2


(* ::Section:: *)
(*Check with the literature*)


totalDecayRateKnown=SMP["m_W"]^3(CA*SMP["G_F"]*Sqrt[((SMP["m_d"] - SMP["m_u"] - SMP["m_W"])*(SMP["m_d"] + SMP["m_u"] - SMP["m_W"])*
      (SMP["m_d"] - SMP["m_u"] + SMP["m_W"])*(SMP["m_d"] + SMP["m_u"] + SMP["m_W"]))/SMP["m_W"]^4]*
   (1-(SMP["m_u"]^2+SMP["m_d"]^2)/(2SMP["m_W"]^2)-(SMP["m_u"]^2-SMP["m_d"]^2)^2/(2 SMP["m_W"]^4))*SMP["V_ud", -I]*SMP["V_ud", I])/
   (6*Sqrt[2]*Pi)
Print["Check with Grozin, Using REDUCE in High Energy Physics, Chapter 5.2: ",
			If[Simplify[FCI[totalDecayRate-totalDecayRateKnown]]===0, "CORRECT.", "!!! WRONG !!!"]];



