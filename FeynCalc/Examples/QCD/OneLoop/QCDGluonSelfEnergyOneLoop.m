(* ::Package:: *)

(* :Title: QCDGluonSelfEnergyOneLoop                                        *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2018 Rolf Mertig
	Copyright (C) 1997-2018 Frederik Orellana
	Copyright (C) 2014-2018 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the gluon self-energy in QCD at one loop    *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The 1-loop gluon self-energy in QCD*)


(* ::Subsection:: *)
(*Load FeynCalc and FeynArts*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the 1-loop gluon self-energy in QCD"];
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
		{V[5]} -> {V[5]}, InsertionLevel -> {Particles}, Model->"SMQCD",
		ExcludeParticles->{S[_],V[1|2|3],U[1|2|3|4],F[4],F[3,{2|3}]}], ColumnsXRows -> {4, 1},
		SheetHeader -> False,   Numbering -> None,SheetHeader->None,ImageSize->{1024,256}];


(* ::Text:: *)
(*We do not fix the gauge but let the gauge parameter GaugeXi take arbitrary values. The 1/(2Pi)^D prefactor is implicit.*)


amps =FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules->{},PreFactor->1],
IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{q},DropSumOver->True,ChangeDimension->D,UndoChiralSplittings->True,
TransversePolarizationVectors->{k1,k2},SMP->True,FinalSubstitutions->{SMP["m_u"]->SMP["m_q"],GaugeXi[_]->GaugeXi}]


(* ::Subsection:: *)
(*The gluon tadpole loop*)


ampGluonLoop = amps[[1]]//SUNSimplify[#,Explicit->True]&//Contract//Simplify


(* ::Text:: *)
(*The above expression is zero in dimensional regularization, because the loop integrals have no scale (and they are not log divergent)*)


ampGluonLoopEval=ampGluonLoop//TID[#,q]&


(* ::Text:: *)
(*We can compare this result to Eq. A.9 in Foundations of QCD by T. Muto. Notice that the result in the book must be multiplied by (-1) due to the way how self-energy is defined there (c.f. Eq. 2.4.4 and Eq. 2.4.6).*)


Print["Check with Muta, Eq. A.9: ",
			If[ampGluonLoopEval===0, "CORRECT.", "!!! WRONG !!!"]];


(* ::Subsection:: *)
(*The quark loop*)


ampQuarkLoopEval = amps[[2]]//TID[#,q,ToPaVe->True]&//SUNSimplify


(* ::Text:: *)
(*The contribution of the quark loop alone is  gauge invariant.*)


Contract[FVD[p,Lor1]FVD[p,Lor2]ampQuarkLoopEval]


(* ::Subsection:: *)
(*The ghost loop*)


ampGhostLoopEval = amps[[3]]//TID[#,q,ToPaVe->True]&//SUNSimplify


(* ::Text:: *)
(*The contribution of the ghost loop alone is not gauge invariant.*)


Contract[FVD[p,Lor1]FVD[p,Lor2]ampGhostLoopEval]//Simplify


(* ::Subsection:: *)
(*The gluon loop*)


ampGluonLoop =  amps[[4]]//TID[#,q,ToPaVe->True]&//SUNSimplify


ampGluonLoopEval=((ampGluonLoop/.{GaugeXi->-OneMinusGaugeXi+1})//Expand//
		Collect[#,OneMinusGaugeXi,Simplify]&)/.{OneMinusGaugeXi->(1-GaugeXi)}


(* ::Text:: *)
(*The contribution of the gluon loop alone is not gauge invariant.*)


Contract[FVD[p,Lor1]FVD[p,Lor2]ampGluonLoopEval]//Simplify


(* ::Subsection:: *)
(*Putting everything together*)


ampGluonGhostEval=Simplify[ampGluonLoopEval+ampGhostLoopEval]


(* ::Text:: *)
(*The sum of the gluon and ghost loop contributions in clearly gauge invariant!*)


Contract[FVD[p,Lor1]FVD[p,Lor2]ampGluonGhostEval]//Simplify


(* ::Text:: *)
(*When adding all the contributions together, we multiply the quark contribution by N_f to account for the 6 quark flavors that actually run in that loop. We ignore the fact that different flavors have different masses, since the divergent piece of the gluon self-energy will not depend on the quark mass.*)


ampTotal=(ampGluonGhostEval+Nf*ampQuarkLoopEval)


(* ::Text:: *)
(*Since we are interested only in the divergent piece of the self-energy function, we do not have to compute full loop integrals. It is sufficient to substitute just the divergent pieces (using PaVeUVPart).*)
(*Here we also need to reintroduce the implicit 1/(2Pi)^D prefactor.*)


ampsSing=ampTotal//PaVeUVPart[#,Prefactor->1/(2Pi)^D]&//FCReplaceD[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//SelectNotFree2[#,Epsilon]&//Simplify


(* ::Text:: *)
(*Finally, since self-energy function is usually defined as  I*Pi = Amplitude, to obtain Sigma we need to multiply our result by -I . This gives*)


gluonSelfEnergy=-I*ampsSing


(* ::Text:: *)
(*We can compare this result to Eq. 2.5.131 and Eq. 2.5.132 in Foundations of QCD by T. Muto. *)


gaugePrefactor=(FVD[p,Lor1]FVD[p,Lor2]-SPD[p,p]MTD[Lor1,Lor2]);
gluonSelfEnergyMuta=(SMP["g_s"]^2/(4Pi)^2)*(4/3*(1/2)*Nf-(1/2)CA(13/3-GaugeXi))*1/Epsilon*
gaugePrefactor*SUNDelta[SUNIndex[Glu1], SUNIndex[Glu2]];
Print["Check with Muta, Eq 2.5.131 and Eq. 2.5.132: ",
			If[Simplify[FCI[gluonSelfEnergy-gluonSelfEnergyMuta]]===0, "CORRECT.", "!!! WRONG !!!"]];


(* ::Text:: *)
(*Another cross-check is to compare to Eq. 16.71 in Peskin and Schroeder, where the authors sum up the gluon and ghost contributions in Feynman gauge. All we need to do is just set the gauge parameter GaugeXi to 1 and remove the quark contribution. The latter is easy to do, since the quark contribution is the only piece proportional to N_f.*)


ampsSingGluonQuarkFeynmanGauge=Simplify[ampsSing/.{GaugeXi->1,Nf->0}]


ampsSingFeynmanGaugePeskin=I*(-gaugePrefactor)*(-SMP["g_s"]^2/(4Pi)^2*(-5/3)*CA*(1/Epsilon))*SUNDelta[SUNIndex[Glu1], SUNIndex[Glu2]];
Print["Check with Peskin and Schroeder, Eq 16.71: ",
			If[Simplify[FCI[ampsSingGluonQuarkFeynmanGauge-ampsSingFeynmanGaugePeskin]]===0, "CORRECT.", "!!! WRONG !!!"]];
