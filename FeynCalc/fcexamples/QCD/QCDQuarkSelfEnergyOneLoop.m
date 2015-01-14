(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDQuarkSelfEnergyOneLoop                                        *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the quark self-energy in QCD at 1-loop         *)

(* ------------------------------------------------------------------------ *)


(* ::Section:: *)
(*The quark self-energy in QCD at  1-loop *)


(* ::Subsection:: *)
(*Load FeynCalc, FeynArts and Tarcer*)


If[ $FrontEnd === Null,
    $FeynCalcStartupMessages = False;
    Print["Computation of the quark self-energy in QCD at 1-loop"];
];
$LoadPhi = False;
$LoadFeynArts = $LoadTARCER  = True;
<< HighEnergyPhysics`FeynCalc`
$FAVerbose=0;


(* ::Subsection:: *)
(*Generate Feynman diagrams*)


Paint[diags =
   InsertFields[
    CreateTopologies[1, 1 -> 1,
      ExcludeTopologies -> {Tadpoles}], {F[3,{1}]} -> {F[3,{1}]},
    InsertionLevel -> {Classes}, GenericModel -> "Lorentz",
    Model -> "SMQCD",ExcludeParticles->{S[1],S[2],S[3],V[1],V[2],V[3]}], ColumnsXRows -> {1, 1},
SheetHeader -> False,   Numbering -> None];


(* ::Text:: *)
(*Notice that we choose the prefactor to be 1/(2^D)*(Pi)^(D/2). This is because the 1/Pi^(D/2) piece of the general prefactor 1/(2Pi)^D goes into the definition of the loop integrals using Tarcer's notation. Furthermore, we do not fix the gauge but let the gauge parameter GaugeXi take arbitrary values.*)


amps = Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &,
   Apply[List,
    FCPrepareFAAmp[CreateFeynAmp[diags,
     Truncated -> True,GaugeRules->{},PreFactor->1/((2^D)*(Pi)^(D/2))]]]] //. {(a1__ DiracGamma[6] a2__ +
      a1__ DiracGamma[7] a2__) :> a1 a2, NonCommutative[x___] -> x,
   FermionChain -> DOT, FourMomentum[Internal, 1] -> q,
   FourMomentum[Outgoing, 1] -> p,
   Index[Lorentz, x_] :>
    LorentzIndex[ToExpression["Lor" <> ToString[x]]],
   Index[Gluon, x_] :>
    SUNIndex[ToExpression["Glu" <> ToString[x]]],
SumOver[__]:>1, SUNT[a_,_,_]:>SUNT[a],MU->M,GaugeXi[g]->GaugeXi}


ampsEval=ChangeDimension[amps[[1]],D]//Contract//SUNSimplify//DiracSimplify//TID[#,q]&//ToTFI[#,q,p]&//TarcerRecurse//FCI


(* ::Text:: *)
(*Since we are interested only in the divergent piece of the self-energy function, we do not have to compute full loop integrals. It is sufficient to substitute just the divergent pieces. The one loop integrals in Tarcer's notation are related to the scalar Passarino-Veltman integrals via a prefactor. For example, TAI[D, 0, {{1, M}}] = (I*(Pi)^(2-D/2) (2Pi)^(D-4)) A0[M^2]. The same goes also for B0.The divergent pieces of A0 and B0 integrals can be easily found in the literature, for example in A.Denner and S. Dittmayer, Reduction of one-loop tensor 5-point integrals, Nucl.Phys.B658:175-202,2003. The preprint is available at  arXiv:hep-ph/0212259. According to the Appendix C, we have A0[M^2]= -2M^2 /(D-4) and B0[p^2, M^2, 0] = -2/(D-4).*)
(*Multiplying these results with (I*(Pi)^(2-D/2) (2Pi)^(D-4)) and keeping only terms proportional to 1/Epsilon we obtain*)


prefactor=(I*(Pi)^(2-D/2) (2Pi)^(D-4));
ampsSing=(ampsEval/.{TBI[x___]:>(-2)/(D-4)*prefactor,TAI[x___]:>-2 M^2/(D-4)*prefactor})//
ReplaceAll[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//SelectNotFree[#,Epsilon]&


(* ::Text:: *)
(*Finally, since self-energy function is usually defined as  -I*Sigma = Amplitude, to obtain Sigma we need to multiply our result by I . This gives*)


quarSelfEnergy=I*ampsSing//Collect[#,M,Simplify]&


(* ::Text:: *)
(*We can compare this result to Eq. 2.5.138 in Foundations of QCD by T. Muto. Notice that the result in the book must be multiplied by (-1) due to the way how self-energy is defined there (c.f. Eq. 2.4.4 and Eq. 2.4.6). Furthermore, in the book the quark self-energy is multiplied by a unit matrix in the fundamental representation of SU(N). Our result contains it only implicitly, since SUNSimplify[SUNT[a,a]] gives just CF and the unit matrix is always understood.*)


quarSelfEnergyMuta=-(-Gstrong^2/(4Pi)^2 CF*(3+GaugeXi)(1/Epsilon)*M+GS[p]*Gstrong^2/(4Pi)^2*CF*GaugeXi*(1/Epsilon));
Print["Check with Muta, Eq 2.5.138: ",
      If[Simplify[quarSelfEnergy-FCI[quarSelfEnergyMuta]]===0, "Correct.", "Mistake!"]];


(* ::Text:: *)
(*Another cross-check is to compare to Eq. 16.76 in Peskin and Schroeder, where the authors compute the self-energy diagram for massless quarks in Feynman gauge. All we need to do is just set the quarks mass M to zero and the gauge parameter GaugeXi to 1.*)


ampsSingMassless=(ampsEval/.{M->0,TBI[x___]:>(-2)/(D-4)*prefactor,GaugeXi->1})//
ReplaceAll[#,D->4-2Epsilon]&//Series[#,{Epsilon,0,0}]&//Normal//SelectNotFree[#,Epsilon]&


ampsSingMasslessPeskin=I*Gstrong^2/(4Pi)^2*GS[p]*CF*(1/Epsilon);
Print["Check with Peskin and Schroeder, Eq 16.76: ",
      If[Simplify[ampsSingMassless-FCI[ampsSingMasslessPeskin]]===0, "Correct.", "Mistake!"]];
