(* ::Package:: *)

(* :Title: EWHiggsToZZTree										*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the total decay rate  for the decay
		of a Higgs into Z^0 Z^0 in Electroweak Theory at tree level.		*)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the total decay rate for the decay of a Higgs into Z^0 Z^0 in Electroweak Theory at tree level."];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadFeynArts= True;
<<FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


diagsHiggsToZZTree = InsertFields[CreateTopologies[0,1 -> 2], {S[1]} -> {V[2],V[2]},
InsertionLevel -> {Classes}];
Paint[diagsHiggsToZZTree,ColumnsXRows->{1,1},Numbering -> None,SheetHeader->None,ImageSize->{256,256}];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


FCClearScalarProducts[];
SP[k1,k1]=SMP["m_Z"]^2;
SP[k2,k2]=SMP["m_Z"]^2;
SP[pH,pH]=SMP["m_H"]^2;
SP[k1,k2]=(SMP["m_H"]^2-2 SMP["m_Z"]^2)/2;

ampHiggsToZZTree=FCFAConvert[CreateFeynAmp[diagsHiggsToZZTree],IncomingMomenta->{pH},OutgoingMomenta->{k1,k2},List->False,ChangeDimension->4,
DropSumOver->True,SMP->True,UndoChiralSplittings->True]//Contract//DotSimplify


(* ::Section:: *)
(*Calculate the total decay rate*)


ampSq=ampHiggsToZZTree*ComplexConjugate[ampHiggsToZZTree]//DoPolarizationSums[#,k1]&//DoPolarizationSums[#,k2]&//Simplify


$Assumptions={SMP["m_H"]>0,SMP["m_W"]>0};
symmetryFactor=1/2;
phaseSpacePrefactor[m_]:=1/(16 Pi SMP["m_H"])Sqrt[1-4 m^2 / SMP["m_H"]^2];
decayRateTotalTree=symmetryFactor phaseSpacePrefactor[SMP["m_Z"]] ampSq//
ReplaceAll[#,SMP["e"]^2->4 Pi SMP["alpha_fs"]]&//FullSimplify


(decayRateTotalTree/.SMP["m_Z"]^2-> h[SMP["m_Z"]^2/SMP["m_H"]^2]SMP["m_H"]^2/.
SMP["m_Z"]^4-> h[SMP["m_Z"]^4/SMP["m_H"]^4]SMP["m_H"]^4)//FullSimplify//ReplaceAll[#,h->Identity]&


decayRateTotalTreeKnown= SMP["g_W"]^2/(128 Pi) SMP["m_H"]^3/SMP["m_W"]^2 Sqrt[1- 4 SMP["m_Z"]^2/SMP["m_H"]^2](1-
4 SMP["m_Z"]^2/SMP["m_H"]^2 + 12 SMP["m_Z"]^4/SMP["m_H"]^4);
Print["Check with Gunion, Haber, Kane, Dawson, Higgs Hunter Guide, Eq 2.10: ",
			If[(Simplify[decayRateTotalTree-
			(decayRateTotalTreeKnown//ReplaceAll[#,SMP["g_W"]-> SMP["e"]/SMP["sin_W"]]&//ReplaceAll[#,SMP["e"]^2->4 Pi SMP["alpha_fs"]]&)]//
			ReplaceRepeated[#,SMP["cos_W"] -> SMP["m_W"]/SMP["m_Z"]]&)===0, 
			"CORRECT.", "!!! WRONG !!!"]];
