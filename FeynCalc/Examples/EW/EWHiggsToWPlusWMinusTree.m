(* ::Package:: *)

(* :Title: EWHiggsToWPlusWMinusTree										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the total decay rate  for the decay
		of a Higgs into W^+ W^- in Electroweak Theory at tree level.		*)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the total decay rate  for the decay of a Higgs into W^+ W^- in Electroweak Theory at tree level."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


diagsHiggsToWPlusWMinusTree = InsertFields[CreateTopologies[0,1 -> 2], {S[1]} -> {-V[3],V[3]},
InsertionLevel -> {Classes}];
Paint[diagsHiggsToWPlusWMinusTree,ColumnsXRows->{1,1},Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


FCClearScalarProducts[];
SP[k1,k1]=SMP["m_W"]^2;
SP[k2,k2]=SMP["m_W"]^2;
SP[pH,pH]=SMP["m_H"]^2;
SP[k1,k2]=(SMP["m_H"]^2-2 SMP["m_W"]^2)/2;

ampHiggsToWPlusWMinusTree=FCFAConvert[CreateFeynAmp[diagsHiggsToWPlusWMinusTree],IncomingMomenta->{pH},OutgoingMomenta->{k1,k2},List->False,ChangeDimension->4,
DropSumOver->True,SMP->True,UndoChiralSplittings->True]//Contract//DotSimplify


(* ::Section:: *)
(*Calculate the total decay rate*)


ampSq=ampHiggsToWPlusWMinusTree*ComplexConjugate[ampHiggsToWPlusWMinusTree]//DoPolarizationSums[#,k1]&//DoPolarizationSums[#,k2]&//Simplify


$Assumptions={SMP["m_H"]>0,SMP["m_W"]>0};
phaseSpacePrefactor[m_]:=1/(16 Pi SMP["m_H"])Sqrt[1-4 m^2 / SMP["m_H"]^2];
decayRateTotalTree=phaseSpacePrefactor[SMP["m_W"]] ampSq//
ReplaceAll[#,SMP["e"]^2->4 Pi SMP["alpha_fs"]]&//FullSimplify


(decayRateTotalTree/.SMP["m_W"]^2-> h[SMP["m_W"]^2/SMP["m_H"]^2]SMP["m_H"]^2/.
SMP["m_W"]^4-> h[SMP["m_W"]^4/SMP["m_H"]^4]SMP["m_H"]^4)//FullSimplify//ReplaceAll[#,h->Identity]&


decayRateTotalTreeKnown= SMP["g_W"]^2/(64 Pi) SMP["m_H"]^3/SMP["m_W"]^2 Sqrt[1- 4 SMP["m_W"]^2/SMP["m_H"]^2](1-
4 SMP["m_W"]^2/SMP["m_H"]^2 + 12 SMP["m_W"]^4/SMP["m_H"]^4);
Print["Check with Gunion, Haber, Kane, Dawson, Higgs Hunter Guide, Eq 2.11: ",
			If[Simplify[decayRateTotalTree-
			(decayRateTotalTreeKnown//ReplaceAll[#,SMP["g_W"]-> SMP["e"]/SMP["sin_W"]]&//ReplaceAll[#,SMP["e"]^2->4 Pi SMP["alpha_fs"]]&)]===0, 
			"CORRECT.", "!!! WRONG !!!"]];
