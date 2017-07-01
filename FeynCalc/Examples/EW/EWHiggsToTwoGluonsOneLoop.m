(* ::Package:: *)

(* :Title: EWHiggsToTwoGluonsOneLoop										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the total decay rate  for the decay
		of a Higgs into 2 gluons in Electroweak Theory at 1-loop level.	*)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the total decay rate  for the decay of a Higgs into 2 gluons in Electroweak Theory at 1-loop level."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadAddOns={"FeynHelpers"};
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*Here we consider only the dominant contribution from the top quark mass. However, it is trivial to include also loops from other quark flavors.*)


diagsHiggsToZZTree = InsertFields[CreateTopologies[1,1 -> 2,ExcludeTopologies->WFCorrections], {S[1]} -> {V[5],V[5]},
InsertionLevel -> {Particles},Model->"SMQCD"];
Paint[DiagramExtract[diagsHiggsToZZTree,{3,6}],ColumnsXRows->{2,1},Numbering -> None,SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


FCClearScalarProducts[];
ScalarProduct[k1,k1]=0;
ScalarProduct[k2,k2]=0;
ScalarProduct[pH,pH]=SMP["m_H"]^2;
ScalarProduct[k1,k2]=(SMP["m_H"]^2)/2;

ampHiggsToTwoGluons=FCFAConvert[CreateFeynAmp[DiagramExtract[diagsHiggsToZZTree,{3,6}],PreFactor->-1],IncomingMomenta->{pH},OutgoingMomenta->{k1,k2},LoopMomenta->{q},
List->False,TransversePolarizationVectors->{k1,k2},
ChangeDimension->D,DropSumOver->True,SMP->True,UndoChiralSplittings->True]//Contract//FCTraceFactor//SUNSimplify


(* ::Section:: *)
(*Evaluate the amplitudes*)


(* ::Text:: *)
(*Dirac trace and tensor decomposition*)


ampHiggsToTwoGluons2=(ampHiggsToTwoGluons/. DiracTrace->Tr)//TID[#,q]&//ToPaVe[#,q]&


(* ::Text:: *)
(*The explicit values for the PaVe functions B0 and  C0 can be obtained e.g. from H. Patel's Package-X. Here we just insert the known results.*)


loopInts={B0[SMP["m_H"]^2,SMP["m_t"]^2,SMP["m_t"]^2]->1/(16*Epsilon*Pi^4) - (-2*SMP["m_H"] + EulerGamma*SMP["m_H"] - Log[4*Pi]*SMP["m_H"] - 
Log[ScaleMu^2/SMP["m_t"]^2]*SMP["m_H"] - Log[(-SMP["m_H"]^2 + 2*SMP["m_t"]^2 + SMP["m_H"]*Sqrt[SMP["m_H"]^2 - 
4*SMP["m_t"]^2])/(2*SMP["m_t"]^2)]*Sqrt[SMP["m_H"]^2 - 4*SMP["m_t"]^2])/(16*Pi^4*SMP["m_H"]), 
C0[0,0,SMP["m_H"]^2,SMP["m_t"]^2,SMP["m_t"]^2,SMP["m_t"]^2]->Log[(-SMP["m_H"]^2 + 2*SMP["m_t"]^2 + 
SMP["m_H"]*Sqrt[SMP["m_H"]^2 - 4*SMP["m_t"]^2])/(2*SMP["m_t"]^2)]^2/(32*Pi^4*SMP["m_H"]^2)};


$Assumptions={SMP["m_H"]>0,SMP["m_t"]>0};
ampHiggsToTwoGluons3=(ampHiggsToTwoGluons2/.loopInts)//FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal


(* ::Text:: *)
(*As expected, the result is finite (i.e. contains no 1/Epsilon poles), so that it is safe to switch back to 4 dimensions*)


ampHiggsToTwoGluons4=ampHiggsToTwoGluons3//ChangeDimension[#,4]&


(* ::Section:: *)
(*Calculate the total decay rate*)


ampSq=ampHiggsToTwoGluons4*ComplexConjugate[ampHiggsToTwoGluons4]//DoPolarizationSums[#,k1,k2]&//DoPolarizationSums[#,k2,k1]&//Simplify//
SUNSimplify[#,SUNNToCACF->False]&


symmetryFactor=1/2;
phaseSpacePrefactor[m_]:=1/(16 Pi SMP["m_H"])Sqrt[1-4 m^2 / SMP["m_H"]^2];
decayRateTotalTree=symmetryFactor phaseSpacePrefactor[0] ampSq//
ReplaceAll[#,{SMP["e"]^2->4 Pi SMP["alpha_fs"],SMP["g_s"]^4->16 Pi^2 SMP["alpha_s"]^2}]&//Simplify


ISq=decayRateTotalTree/( SMP["alpha_s"]^2/(9 Pi^2) SMP["m_H"]^2/SMP["m_W"]^2  SMP["alpha_fs"]SMP["m_H"]/(8 SMP["sin_W"]^2))


(* ::Text:: *)
(*ISq corresponds to I(m_H^2/m_q^2) from Peskin and Schroeder, Final Project 3, part (c). It should go to 1 for m_q -> Infinity and to 0 for m_q -> 0*)


Limit[ISq,SMP["m_t"]->Infinity]/.SUNN->3
Limit[ISq,SMP["m_t"]->0]


Print["Check with Peskin and Schroeder, Final Project III, part (c): ",
			If[((Limit[ISq,SMP["m_t"]->Infinity]/.SUNN->3)===1) && (Limit[ISq,SMP["m_t"]->0]===0), 
			"CORRECT.", "!!! WRONG !!!"]];
