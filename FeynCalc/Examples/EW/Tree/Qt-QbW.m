(* ::Package:: *)

(* :Title: EWT-QW                                        *)

(*
	 This software is covered by the GNU General Public License 3.
	 Copyright (C) 1990-2018 Rolf Mertig
	 Copyright (C) 1997-2018 Frederik Orellana
	 Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  t \[Rule] q W, EW theory, total decay rate, 
              tree level                                            *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["t \[Rule] q W, EW theory, total decay rate, tree level"];
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
{F[3,{3}]} -> {F[4,{3}],-V[3]}, InsertionLevel -> {Particles}];
Paint[diagsAmpTree, ColumnsXRows -> {1, 1}, Numbering -> None,SheetHeader->False];


(* ::Section:: *)
(*Obtain the amplitude*)


ampTree=FCFAConvert[CreateFeynAmp[diagsAmpTree,Truncated -> False],List->False,SMP->True,ChangeDimension->4,
IncomingMomenta->{p},OutgoingMomenta->{k1,k2},DropSumOver->True,FinalSubstitutions->{SMP["e"]->Sqrt[8/Sqrt[2] SMP["G_F"] SMP["m_W"]^2SMP["sin_W"]^2]}]//Contract


(* ::Section:: *)
(*Setup the kinematics*)


FCClearScalarProducts[]
SP[p]=SMP["m_t"]^2;
SP[k1]=SMP["m_b"]^2;
SP[k2]=SMP["m_W"]^2;
SP[k1,k2]=Simplify[(SP[p]-SP[k1]-SP[k2])/2];
SP[p,k1]=Simplify[ExpandScalarProduct[SP[k1+k2,k1]]];
SP[p,k2]=Simplify[ExpandScalarProduct[SP[k1+k2,k2]]];


(* ::Section:: *)
(*Obtain squared amplitude for the unpolarized process*)


(* ::Text:: *)
(*We average over the polarizations of the top quark*)


ampTreeSquared=ampTree ComplexConjugate[ampTree]//DoPolarizationSums[#,k2]&//
FermionSpinSum[#,ExtraFactor->1/2]&//DiracSimplify[#,DiracTraceEvaluate->True]&//SUNSimplify


(* ::Section:: *)
(*Calculate the total decay rate*)


phaseSpacePrefactor[m1_,m2_,M_]:=1/(16 Pi M)Sqrt[1-(m1+m2)^2/M^2]Sqrt[1-(m1-m2)^2/M^2];


totalDecayRate=phaseSpacePrefactor[SMP["m_b"],SMP["m_W"],SMP["m_t"]]ampTreeSquared//Simplify//
ReplaceAll[#,Sqrt[x_]Sqrt[y_]:>Sqrt[ExpandAll[x y]]]&


(* ::Section:: *)
(*Check with the literature*)


totalDecayRateKnown=SMP["m_t"]^3(CA*SMP["G_F"]*Sqrt[((SMP["m_b"] - SMP["m_t"] - SMP["m_W"])*(SMP["m_b"] + SMP["m_t"] - SMP["m_W"])*(SMP["m_b"] - SMP["m_t"] + SMP["m_W"])*
     (SMP["m_b"] + SMP["m_t"] + SMP["m_W"]))/SMP["m_t"]^4]*(
     (1-SMP["m_b"]^2/SMP["m_t"]^2)^2+ SMP["m_W"]^2/SMP["m_t"]^2(1+SMP["m_b"]^2/SMP["m_t"]^2)-2SMP["m_W"]^4/SMP["m_t"]^4
     )*SMP["V_tb", -I]*SMP["V_tb", I])/(8*Sqrt[2]*Pi);
Print["Check with Grozin, Using REDUCE in High Energy Physics, Chapter 5.2: ",
			If[Simplify[FCI[totalDecayRate-totalDecayRateKnown]]===0, "CORRECT.", "!!! WRONG !!!"]];
