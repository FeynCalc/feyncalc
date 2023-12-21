(* ::Package:: *)

(* :Title: H-WW																*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2024 Rolf Mertig
	Copyright (C) 1997-2024 Frederik Orellana
	Copyright (C) 2014-2024 Vladyslav Shtabovenko
*)

(* :Summary:  H -> W W, EW, total decay rate, tree              			*)

(* ------------------------------------------------------------------------ *)



(* ::Title:: *)
(*Higgs decaying into a W boson pair*)


(* ::Section:: *)
(*Load FeynCalc and the necessary add-ons or other packages*)


description="H -> W W, EW, total decay rate, tree";
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

FCCheckVersion[9,3,1];


(* ::Section:: *)
(*Generate Feynman diagrams*)


diags = InsertFields[CreateTopologies[0,1 -> 2], {S[1]} -> {-V[3],V[3]},
	InsertionLevel -> {Classes}];

Paint[diags, ColumnsXRows -> {2, 1}, Numbering -> Simple,
	SheetHeader->None,ImageSize->{512,256}];


(* ::Section:: *)
(*Obtain the amplitudes*)


amp[0]=FCFAConvert[CreateFeynAmp[diags],IncomingMomenta->{pH},
	OutgoingMomenta->{k1,k2},List->False,ChangeDimension->4,
	TransversePolarizationVectors->{k1,k2},
	DropSumOver->True,SMP->True,Contract->True,UndoChiralSplittings->True]


(* ::Section:: *)
(*Fix the kinematics*)


FCClearScalarProducts[];
SP[k1,k1]=SMP["m_W"]^2;
SP[k2,k2]=SMP["m_W"]^2;
SP[pH,pH]=SMP["m_H"]^2;
SP[k1,k2]=(SMP["m_H"]^2-2 SMP["m_W"]^2)/2;


(* ::Section:: *)
(*Square the amplitudes*)


ampSquared[0]=(amp[0] (ComplexConjugate[amp[0]]))//
	FeynAmpDenominatorExplicit//DoPolarizationSums[#,k1]&//
	DoPolarizationSums[#,k2]&//Simplify


(* ::Section:: *)
(*Total decay rate*)


$Assumptions={SMP["m_H"]>0,SMP["m_W"]>0};
phaseSpacePrefactor[m_]:=1/(16 Pi SMP["m_H"])Sqrt[1-4 m^2 / SMP["m_H"]^2];


totalDecayRate=phaseSpacePrefactor[SMP["m_W"]] ampSquared[0]//
ReplaceAll[#,SMP["e"]^2->4 Pi SMP["alpha_fs"]]&//Simplify


(* ::Text:: *)
(*Rewrite the result in a nicer way*)


(totalDecayRate/.SMP["m_W"]^2-> h[SMP["m_W"]^2/SMP["m_H"]^2]SMP["m_H"]^2/.
SMP["m_W"]^4-> h[SMP["m_W"]^4/SMP["m_H"]^4]SMP["m_H"]^4)//FullSimplify//
ReplaceAll[#,h->Identity]&


(* ::Section:: *)
(*Check the final results*)


knownResults = {
	SMP["g_W"]^2/(64 Pi) SMP["m_H"]^3/SMP["m_W"]^2 Sqrt[1- 4 SMP["m_W"]^2/
	SMP["m_H"]^2](1-4 SMP["m_W"]^2/SMP["m_H"]^2 + 12 SMP["m_W"]^4/SMP["m_H"]^4)}//
	ReplaceAll[#,SMP["g_W"]-> SMP["e"]/SMP["sin_W"]]&//
	ReplaceAll[#,SMP["e"]^2->4 Pi SMP["alpha_fs"]]&;
FCCompareResults[{totalDecayRate},
knownResults,Factoring->Simplify,
Text->{"\tCompare to Gunion, Haber, Kane and Dawson, \
Higgs Hunter Guide, Eq 2.11:",
"CORRECT.","WRONG!"}, Interrupt->{Hold[Quit[1]],Automatic}];
Print["\tCPU Time used: ", Round[N[TimeUsed[],3],0.001], " s."];



