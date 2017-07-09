(* ::Package:: *)

(* :Title: QEDPhotonSelfEnergyOneLoop                                        *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the photon self-energy in QED at one loop    *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The 1-loop photon self-energy in QED*)


(* ::Subsection:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the 1-loop photon self-energy in QED"];
];
$LoadFeynArts=True;
<<FeynCalc`
$FAVerbose=0;


(* ::Subsection:: *)
(*Generate Feynman diagrams*)


(* ::Text:: *)
(*We keep scaleless B0 functions, since otherwise the UV part would not come out right.*)


$KeepLogDivergentScalelessIntegrals=True;


Paint[diags = InsertFields[CreateTopologies[1, 1 -> 1 ,ExcludeTopologies->{Tadpoles}],
		{V[1]} -> {V[1]}, InsertionLevel -> {Particles}, 
		ExcludeParticles->{S[_],V[2|3],U[_],F[3|4],F[2,{2|3}]}], ColumnsXRows -> {4, 1},
		SheetHeader -> False,   Numbering -> None,SheetHeader->None,ImageSize->{1024,256}];


(* ::Text:: *)
(*We do not fix the gauge but let the gauge parameter GaugeXi take arbitrary values. The 1/(2Pi)^D prefactor is implicit.*)


amp =FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules->{},PreFactor->-1],List->False,
IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{q},DropSumOver->True,ChangeDimension->D,UndoChiralSplittings->True,
TransversePolarizationVectors->{k1,k2},SMP->True,FinalSubstitutions->{GaugeXi[_]->GaugeXi}]


(* ::Subsection:: *)
(*The quark loop*)


ampEval = TID[amp,q,ToPaVe->True]


(* ::Text:: *)
(*The photon self-energy is  gauge invariant.*)


Contract[FVD[p,Lor1]FVD[p,Lor2] ampEval]//Simplify


(* ::Text:: *)
(*Since we are interested only in the divergent piece of the self-energy function, we do not have to compute full loop integrals. It is sufficient to substitute just the divergent pieces (using PaVeUVPart).*)
(*Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.*)


ampsSing=ampEval//PaVeUVPart[#,Prefactor->1/(2Pi)^D]&//FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//SelectNotFree2[#,Epsilon]&//Simplify


(* ::Text:: *)
(*Finally, since self-energy function is usually defined as  I*Pi^{mu nu} = (p^2 g^{mu nu} - p^mu p^nu) i Pi(p^2),  to obtain Pi we need to multiply our result by -I *)
(*and divide by (p^2 g^{mu nu} - p^mu p^nu). This gives*)


photonSelfEnergy= -I*ampsSing/FCI[(FVD[p,Lor1]FVD[p,Lor2]-SPD[p,p]MTD[Lor1,Lor2])]//Simplify


(* ::Text:: *)
(*We can compare this result to Eq. 10.44 in Peskin and Schroeder*)


ampsSingPeskin=-SMP["e"]^2/(4Pi)^(D/2) Gamma[2-D/2]/(SMP["m_e"]^2- x(1-x)SPD[p,p])^(2-D/2)*
(8x(1-x))


ampsSingPeskinExpanded=ampsSingPeskin//FCReplaceD[#,D->4-Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//SelectNotFree2[#,Epsilon]&//Integrate[#,{x,0,1}]&//Collect2[#,SMP]&


(* ::Text:: *)
(*Peskin and Schroeder use D=4-Epsilon, while we did the calculation with D=4-2Epsilon.*)


elSelfEnergyPeskin=ampsSingPeskinExpanded/. 1/Epsilon->1/(2Epsilon)


Print["Check with Peskin and Schroeder, Eq 10.44: ",
			If[Simplify[elSelfEnergyPeskin-photonSelfEnergy]===0, "CORRECT.", "!!! WRONG !!!"]];
