(* ::Package:: *)

(* :Title: QCDQuarkSelfEnergyOneLoop                                        *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the quark self-energy in QCD at 1-loop         *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The quark self-energy in QCD at  1-loop *)


(* ::Subsection:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the quark self-energy in QCD at 1-loop"];
];
$LoadFeynArts=True;
<<FeynCalc`
$FAVerbose=0;


(* ::Subsection:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*We keep scaleless B0 functions, since otherwise the UV part would not come out right.*)


$KeepLogDivergentScalelessIntegrals=True;


Paint[diags = InsertFields[CreateTopologies[1, 1 -> 1,
		ExcludeTopologies -> {Tadpoles}], {F[3,{1}]} -> {F[3,{1}]},
		InsertionLevel -> {Classes}, GenericModel -> "Lorentz",
		Model -> "SMQCD",ExcludeParticles->{S[1],S[2],S[3],V[1],V[2],V[3]}], ColumnsXRows -> {1, 1},
		SheetHeader -> False,SheetHeader->None,Numbering -> None,ImageSize->{256,256}];


(* ::Text:: *)
(*We do not fix the gauge but let the gauge parameter GaugeXi take arbitrary values. The 1/(2Pi)^D prefactor is implicit.*)


amps=FCFAConvert[CreateFeynAmp[diags, Truncated -> True,GaugeRules->{},PreFactor->1],IncomingMomenta->{p},
OutgoingMomenta->{p},LoopMomenta->{q},DropSumOver->True,UndoChiralSplittings->True,ChangeDimension->D,List->False,SMP->True,
FinalSubstitutions->{SMP["m_u"]->M,GaugeXi[g]->GaugeXi}]


ampsEval=amps//Contract//SUNSimplify//TID[#,q,ToPaVe->True]&


(* ::Text:: *)
(*Since we are interested only in the divergent piece of the self-energy function, we do not have to compute full loop integrals. It is sufficient to substitute just the divergent pieces (using PaVeUVPart).*)
(*Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.*)


ampsSing=ampsEval//PaVeUVPart[#,Prefactor->1/(2Pi)^D]&//ReplaceAll[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//SelectNotFree[#,Epsilon]&//Collect2[#,M]&


(* ::Text:: *)
(*Finally, since self-energy function is usually defined as  I*Sigma = Amplitude, to obtain Sigma we need to multiply our result by I . This gives*)


quarSelfEnergy=I*ampsSing//Collect[#,M,Simplify]&


(* ::Text:: *)
(*We can compare this result to Eq. 2.5.138 in Foundations of QCD by T. Muto. Notice that the result in the book must be multiplied by (-1) due to the way how self-energy is defined there (c.f. Eq. 2.4.4 and Eq. 2.4.6).*)


quarSelfEnergyMuta=-(-SMP["g_s"]^2/(4Pi)^2 CF*(3+GaugeXi)(1/Epsilon)*M+GS[p]*SMP["g_s"]^2/(4Pi)^2*
		CF*GaugeXi*(1/Epsilon))SDF[Col1,Col2]//FCI;
Print["Check with Muta, Eq 2.5.138: ",
			If[Simplify[quarSelfEnergy-FCI[quarSelfEnergyMuta]]===0, "CORRECT.", "!!! WRONG !!!"]];


(* ::Text:: *)
(*Another cross-check is to compare to Eq. 16.76 in Peskin and Schroeder, where the authors compute the self-energy diagram for massless quarks in Feynman gauge. All we need to do is just set the quarks mass M to zero and the gauge parameter GaugeXi to 1.*)


ampsSingMassless=ampsSing/.M->0/. GaugeXi->1


ampsSingMasslessPeskin=I*SMP["g_s"]^2/(4Pi)^2*GS[p]*CF*(1/Epsilon)SDF[Col1,Col2]//FCI;
Print["Check with Peskin and Schroeder, Eq 16.76: ",
			If[Simplify[ampsSingMassless-FCI[ampsSingMasslessPeskin]]===0, "CORRECT.", "!!! WRONG !!!"]];
