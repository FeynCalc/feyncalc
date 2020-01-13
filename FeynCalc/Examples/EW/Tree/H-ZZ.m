(* ::Package:: *)

(* :Title: H-ZZ																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2020 Rolf Mertig
	Copyright (C) 1997-2020 Frederik Orellana
	Copyright (C) 2014-2020 Vladyslav Shtabovenko
*)

(* :Summary:  H -> Z Z, EW, total decay rate, tree              			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Higgs decaying into a Z boson pair*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="H -> Z Z, EW, total decay rate, tree";
If[ $FrontEnd === Null,
	$FeynCalcStartupMessages = False;
	Print[description];
];
If[ $Notebooks === False,
	$FeynCalcStartupMessages = False
];
$LoadAddOns={"FeynArts"};
<<FeynCalc`
$FAVerbose = 0;

FCCheckVersion[9,3,0];


(* ::Section:: *)
(*Generate Feynman diagrams*)


diags = InsertFields[CreateTopologies[0,1 -> 2], {S[1]} -> {V[2],V[2]},
	InsertionLevel -> {Classes}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


amp[0]=FCFAConvert[CreateFeynAmp[diags],IncomingMomenta->{pH},
	OutgoingMomenta->{k1,k2},List->False,ChangeDimension->4,
	DropSumOver->True,SMP->True,Contract->True,UndoChiralSplittings->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SP[k1,k1]=SMP["m_Z"]^2;
SP[k2,k2]=SMP["m_Z"]^2;
SP[pH,pH]=SMP["m_H"]^2;
SP[k1,k2]=(SMP["m_H"]^2-2 SMP["m_Z"]^2)/2;


(* ::Section:: *)
(*Square the amplitudes*)


ampSquared[0]=1/2(amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//DoPolarizationSums[#,k1]&//
	DoPolarizationSums[#,k2]&//Simplify


(* ::Section:: *)
(*Total decay rates*)


$Assumptions={SMP["m_H"]>0,SMP["m_Z"]>0};
phaseSpacePrefactor[m_]:=1/(16 Pi SMP["m_H"])Sqrt[1-4 m^2 / SMP["m_H"]^2];


totalDecayRate=phaseSpacePrefactor[SMP["m_Z"]] ampSquared[0]//
ReplaceRepeated[#,{SMP["e"]^2->4 Pi SMP["alpha_fs"], 1/SMP["m_Z"]^4->
	SMP["cos_W"]^4/SMP["m_W"]^4}]&//Simplify


(* ::Text:: *)
(*Rewrite the result in a nicer way*)


(totalDecayRate/.SMP["m_Z"]^2-> h[SMP["m_Z"]^2/SMP["m_H"]^2]SMP["m_H"]^2/.
SMP["m_Z"]^4-> h[SMP["m_Z"]^4/SMP["m_H"]^4]SMP["m_H"]^4)//FullSimplify//ReplaceAll[#,h->Identity]&


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	(SMP["alpha_fs"]*SMP["m_H"]^3*Sqrt[1 - (4*SMP["m_Z"]^2)/SMP["m_H"]^2]*
(1 - (4*SMP["m_Z"]^2)/SMP["m_H"]^2 + (12*SMP["m_Z"]^4)/SMP["m_H"]^4))/
(32*SMP["m_W"]^2*SMP["sin_W"]^2)};
FCCompareResults[{totalDecayRate},
knownResults,Factoring->Simplify,
Text->{"\tCompare to Gunion, Haber, Kane and Dawson, \
Higgs Hunter Guide, Eq 2.10:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];
