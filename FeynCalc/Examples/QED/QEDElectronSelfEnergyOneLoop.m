(* ::Package:: *)

(* :Title: QEDElectronSelfEnergyOneLoop                                        *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the electron self-energy in QED at 1-loop         *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The electron self-energy in QED at  1-loop *)


(* ::Subsection:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the electron self-energy in QED at 1-loop"];
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
		ExcludeTopologies -> {Tadpoles}], {F[2,{1}]} -> {F[2,{1}]},
		InsertionLevel -> {Classes},ExcludeParticles->{S[_],V[2|3]}], ColumnsXRows -> {1, 1},
		SheetHeader -> False, SheetHeader->None, Numbering -> None,ImageSize->{256,256}];


(* ::Text:: *)
(*We do not fix the gauge but let the gauge parameter GaugeXi take arbitrary values. The 1/(2Pi)^D prefactor is implicit.*)


amps=FCFAConvert[CreateFeynAmp[diags, Truncated -> True,GaugeRules->{},PreFactor->1],IncomingMomenta->{p},
OutgoingMomenta->{p},LoopMomenta->{q},DropSumOver->True,UndoChiralSplittings->True,ChangeDimension->D,List->False,SMP->True,
FinalSubstitutions->{SMP["m_u"]->M,GaugeXi[A]->GaugeXi}]


ampsEval=amps//Contract//SUNSimplify//TID[#,q,ToPaVe->True]&


(* ::Text:: *)
(*Since we are interested only in the divergent piece of the self-energy function, we do not have to compute full loop integrals. It is sufficient to substitute just the divergent pieces (using PaVeUVPart).*)
(*Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.*)


ampsSing=ampsEval//PaVeUVPart[#,Prefactor->1/(2Pi)^D]&//FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//SelectNotFree[#,Epsilon]&//Collect2[#,SMP]&


(* ::Text:: *)
(*Finally, since self-energy function is usually defined as  -I*Sigma = Amplitude, to obtain Sigma we need to multiply our result by I . This gives*)


elSelfEnergy=I*ampsSing//Collect[#,M,Simplify]&


(* ::Text:: *)
(*We can compare this result to Eq. 10.41 in Peskin and Schroeder*)


ampsSingPeskin=SMP["e"]^2/(4Pi)^(D/2) Gamma[2-D/2]/((1-x) SMP["m_e"]^2+ x ScaleMu^2 - x(1-x) SPD[p,p])^(2-D/2)*
((4-Epsilon)SMP["m_e"]-(2-Epsilon)x GSD[p])


ampsSingPeskinExpanded=ampsSingPeskin//FCReplaceD[#,D->4-Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//SelectNotFree2[#,Epsilon]&//Integrate[#,{x,0,1}]&//Collect2[#,SMP]&


(* ::Text:: *)
(*Peskin and Schroeder use D=4-Epsilon, while we did the calculation with D=4-2Epsilon.*)


elSelfEnergyPeskin=ampsSingPeskinExpanded/. 1/Epsilon->1/(2Epsilon)


Print["Check with Peskin and Schroeder, Eq 10.41: ",
			If[Simplify[elSelfEnergyPeskin-(elSelfEnergy/.GaugeXi->1)]===0, "CORRECT.", "!!! WRONG !!!"]];
