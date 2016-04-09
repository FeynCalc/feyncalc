(* ::Package:: *)

(* :Title: QCDGluonSelfEnergyOneLoop                                        *)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2016 Rolf Mertig
	Copyright (C) 1997-2016 Frederik Orellana
	Copyright (C) 2014-2016 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the gluon self-energy in QCD at one loop    *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The 1-loop gluon self-energy in QCD*)


(* ::Subsection:: *)
(*Load FeynCalc, FeynArts and Tarcer*)


If[ $FrontEnd === Null,
		$FeynCalcStartupMessages = False;
		Print["Computation of the 1-loop gluon self-energy in QCD"];
];
$LoadFeynArts = $LoadTARCER  = True;
<<FeynCalc`
$FAVerbose=0;


(* ::Subsection:: *)
(*Generate Feynman diagrams*)


Paint[diags = InsertFields[CreateTopologies[1, 1 -> 1 ,ExcludeTopologies->{Tadpoles}],
		{V[5]} -> {V[5]}, InsertionLevel -> {Classes}, GenericModel -> "Lorentz",Model->"SMQCD",
		ExcludeParticles->{S[1],S[2],S[3],V[2],V[3],U[1],U[2],U[3],F[4],U[4]}], ColumnsXRows -> {4, 1},
		SheetHeader -> False,   Numbering -> None,SheetHeader->None,ImageSize->{512,128}];


(* ::Text:: *)
(*Notice that we choose the prefactor to be 1/(2^D)*(Pi)^(D/2). This is because the 1/Pi^(D/2) piece of the general prefactor 1/(2Pi)^D goes into the definition of the loop integrals using Tarcer's notation.*)


amps =FCFAConvert[CreateFeynAmp[diags, Truncated -> True, GaugeRules->{},PreFactor->1/((2^D)*(Pi)^(D/2))],
IncomingMomenta->{p},OutgoingMomenta->{p},LoopMomenta->{q},DropSumOver->True,ChangeDimension->D,UndoChiralSplittings->True,
TransversePolarizationVectors->{k1,k2},SMP->True]/. {MQU[Index[Generation, 3]]->MQ,GaugeXi[_]->GaugeXi};


(* ::Subsection:: *)
(*The gluon tadpole loop*)


ampGluonLoop = amps[[1]]//ChangeDimension[#,D]&//SUNSimplify[#,Explicit->True]&//Contract//Simplify


(* ::Text:: *)
(*The above expression is zero in dimensional regularization, because the loop integrals have no scale*)


ampGluonLoopEval=ampGluonLoop//ToTFI[#,q,p]&


(* ::Text:: *)
(*We can compare this result to Eq. A.9 in Foundations of QCD by T. Muto. Notice that the result in the book must be multiplied by (-1) due to the way how self-energy is defined there (c.f. Eq. 2.4.4 and Eq. 2.4.6).*)


Print["Check with Muta, Eq. A.9: ",
			If[ampGluonLoopEval===0, "CORRECT.", "!!! WRONG !!!"]];


(* ::Subsection:: *)
(*The quark loop*)


ampQuarkLoopEval = amps[[2]]//ChangeDimension[#,D]&//ReplaceAll[#,DiracTrace->Tr]&//
		SUNSimplify[#,Explicit->True]&//ReplaceAll[#,SUNTr->SUNTrace]&//TID[#,q]&//
		Simplify//ToTFI[#,q,p]&//Simplify


(* ::Text:: *)
(*The contribution of the quark loop alone is  gauge invariant.*)


Contract[FVD[p,Lor1]FVD[p,Lor2]ampQuarkLoopEval]


(* ::Subsection:: *)
(*The ghost loop*)


ampGhostLoopEval = amps[[3]]//ChangeDimension[#,D]&//SUNSimplify[#,Explicit->True]&//
		ExpandScalarProduct//TID[#,q]&//Simplify//ToTFI[#,q,p]&//Simplify


(* ::Text:: *)
(*The contribution of the ghost loop alone is not gauge invariant.*)


Contract[FVD[p,Lor1]FVD[p,Lor2]ampGhostLoopEval]//Simplify


(* ::Subsection:: *)
(*The gluon loop*)


ampGluonLoop = amps[[4]]//ChangeDimension[#,D]&//SUNSimplify[#,Explicit->True]&//
		Contract//TID[#,q]&//ToTFI[#,q,p]&//Simplify


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
(*When adding all the contributions together, we multiply the quark contribution by Nf to account for the 6 quark flavours that actually run in that loop. We ignore the fact that different flavours have different masses, since the divergent piece of the gluon self-energy will not depend on the quark mass.*)


ampTotal=(ampGluonGhostEval+Nf*ampQuarkLoopEval)//TarcerRecurse


(* ::Text:: *)
(*Since we are interested only in the divergent piece of the self-energy function, we do not have to compute full loop integrals. It is sufficient to substitute just the divergent pieces. The one loop integrals in Tarcer's notation are related to the scalar Passarino-Veltman integrals via a prefactor. For example, TAI[D, 0, {{1, M}}] = (I*(Pi)^(2-D/2) (2Pi)^(D-4)) A0[M^2]. The same goes also for B0.The divergent pieces of A0 and B0 integrals can be easily found in the literature, for example in A.Denner and S. Dittmayer, Reduction of one-loop tensor 5-point integrals, Nucl.Phys.B658:175-202,2003. The preprint is available at  arXiv:hep-ph/0212259. According to the Appendix C, we have A0[M^2]= -2M^2 /(D-4) and B0[p^2, M^2, 0] = -2/(D-4).*)
(*Multiplying these results with (I*(Pi)^(2-D/2) (2Pi)^(D-4)) and keeping only terms proportional to 1/Epsilon we obtain*)


prefactor=(I*(Pi)^(2-D/2) (2Pi)^(D-4));
ampsSing=(ampTotal/.{TBI[___]:>(-2)/(D-4)*prefactor,TAI[___]:>-2 MQ^2/(D-4)*prefactor})//FCI//
		ReplaceAll[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//SelectNotFree[#,Epsilon]&


(* ::Text:: *)
(*Finally, since self-energy function is usually defined as  I*Pi = Amplitude, to obtain Sigma we need to multiply our result by -I . This gives*)


gluonSelfEnergy=-I*ampsSing


(* ::Text:: *)
(*We can compare this result to Eq. 2.5.131 and Eq. 2.5.132 in Foundations of QCD by T. Muto. *)


gaugePrefactor=(Pair[LorentzIndex[Lor1], Momentum[p]]*Pair[LorentzIndex[Lor2], Momentum[p]] - Pair[LorentzIndex[Lor1], LorentzIndex[Lor2]]*
		Pair[Momentum[p], Momentum[p]]);
gluonSelfEnergyMuta=(SMP["g_s"]^2/(4Pi)^2)*(4/3*(1/2)*Nf-(1/2)CA(13/3-GaugeXi))*1/Epsilon*
gaugePrefactor*SUNDelta[SUNIndex[Glu1], SUNIndex[Glu2]];
Print["Check with Muta, Eq 2.5.131 and Eq. 2.5.132: ",
			If[Simplify[gluonSelfEnergy-gluonSelfEnergyMuta]===0, "CORRECT.", "!!! WRONG !!!"]];


(* ::Text:: *)
(*Another cross-check is to compare to Eq. 16.71 in Peskin and Schroeder, where the authors sum up the gluon and ghost contributions in Feynman gauge. All we need to do is just set the gauge parameter GaugeXi to 1 and remove the quark contribution. The latter is easy to do, since the quark contribution is the only piece proportional to Nf.*)


ampsSingGluonQuarkFeynmanGauge=Simplify[ampsSing/.{GaugeXi->1,Nf->0}]


ampsSingFeynmanGaugePeskin=I*(-gaugePrefactor)*(-SMP["g_s"]^2/(4Pi)^2*(-5/3)*CA*(1/Epsilon))*SUNDelta[SUNIndex[Glu1], SUNIndex[Glu2]];
Print["Check with Peskin and Schroeder, Eq 16.71: ",
			If[Simplify[ampsSingGluonQuarkFeynmanGauge-ampsSingFeynmanGaugePeskin]===0, "CORRECT.", "!!! WRONG !!!"]];
