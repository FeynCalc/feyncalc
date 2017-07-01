(* ::Package:: *)

(* :Title: EWHiggsToTwoFermionsTree											*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the total decay rate  for the decay
		of a Higgs into 2 fermions in Electroweak Theory at tree level.		*)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the total decay rate  for the decay of a Higgs into 2 fermions in Electroweak Theory at tree level."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


diagsHiggsToLeptonsTree = InsertFields[CreateTopologies[0,1 -> 2], {S[1]} -> {F[2,{l}],-F[2,{l}]},
InsertionLevel -> {Classes}];
Paint[diagsHiggsToLeptonsTree,ColumnsXRows->{1,1},Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


diagsHiggsToQuarksTree = InsertFields[CreateTopologies[0,1 -> 2], {S[1]} -> {F[3,{l}],-F[3,{l}]},
InsertionLevel -> {Classes}];
Paint[diagsHiggsToQuarksTree,ColumnsXRows->{1,1},Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


FCClearScalarProducts[];
SP[p1,p1]=SMP["m_l"]^2;
SP[k1,k1]=SMP["m_q"]^2;
SP[p2,p2]=SMP["m_l"]^2;
SP[k2,k2]=SMP["m_q"]^2;
SP[pH,pH]=SMP["m_H"]^2;
SP[p1,p2]=(SMP["m_H"]^2-2 SMP["m_l"]^2)/2;
SP[k1,k2]=(SMP["m_H"]^2-2 SMP["m_q"]^2)/2;

ampdiagsHiggsToLeptonsTree=FCFAConvert[CreateFeynAmp[diagsHiggsToLeptonsTree],IncomingMomenta->{pH},OutgoingMomenta->{p1,p2},List->False,ChangeDimension->4,
DropSumOver->True,SMP->True,UndoChiralSplittings->True,FinalSubstitutions->{MLE[l]->SMP["m_l"]}]//Contract//DotSimplify

ampdiagsHiggsToQuarksTree=FCFAConvert[CreateFeynAmp[diagsHiggsToQuarksTree],IncomingMomenta->{pH},OutgoingMomenta->{k1,k2},List->False,ChangeDimension->4,
DropSumOver->True,SMP->True,UndoChiralSplittings->True,FinalSubstitutions->{MQU[l]->SMP["m_q"]}]//Contract//DotSimplify


(* ::Section:: *)
(*Calculate the total decay rates*)


ampSqLeptons=ampdiagsHiggsToLeptonsTree*ComplexConjugate[ampdiagsHiggsToLeptonsTree]//FermionSpinSum//ReplaceAll[#,DiracTrace->Tr]&//Simplify
ampSqQuarks=ampdiagsHiggsToQuarksTree*ComplexConjugate[ampdiagsHiggsToQuarksTree]//FermionSpinSum//ReplaceAll[#,DiracTrace->Tr]&//SUNSimplify[#,SUNNToCACF->False]&//Simplify


$Assumptions={SMP["m_H"]>0,SMP["m_l"]>0};
phaseSpacePrefactor[m_]:=1/(16 Pi SMP["m_H"])Sqrt[1-4 m^2 / SMP["m_H"]^2];
decayRateTotalLeptonsTree=phaseSpacePrefactor[SMP["m_l"]] ampSqLeptons//
ReplaceAll[#,SMP["e"]^2->4 Pi SMP["alpha_fs"]]&//FullSimplify//Factor2

decayRateTotalQuarksTree=phaseSpacePrefactor[SMP["m_q"]]*ampSqQuarks//
ReplaceAll[#,SMP["e"]^2->4 Pi SMP["alpha_fs"]]&//FullSimplify//Factor2


decayRateTotalLeptonsTreeKnown=(AlphaFS SMP["m_H"])/(8 SMP["sin_W"]^2)( SMP["m_l"]^2/SMP["m_W"]^2 (1- 4 SMP["m_l"]^2/SMP["m_H"]^2)^(3/2));
decayRateTotalQuarksTreeKnown=SUNN(decayRateTotalLeptonsTreeKnown/.SMP["m_l"]->SMP["m_q"]);

Print["Check with Peskin and Schroeder, Final Project III, part (a): ",
			If[(Simplify[decayRateTotalLeptonsTree-decayRateTotalLeptonsTreeKnown]===0) &&
			(Simplify[decayRateTotalQuarksTree-decayRateTotalQuarksTreeKnown]===0), "CORRECT.", "!!! WRONG !!!"]];

