(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QEDComptonScatteringTree                                         *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for Compton
              scattering in QED at tree level                               *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
    $FeynCalcStartupMessages = False;
    Print["Computation of the matrix element squared for Compton scattering in QED at tree level"];
];
$LoadTARCER = False;
$LoadPhi =$LoadFeynArts= True;
<<HighEnergyPhysics`FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topCompton = CreateTopologies[0, 2 -> 2];
diagsCompton =
  InsertFields[topCompton, {F[2, {1}], V[1]} -> {F[2, {1}], V[1]}, InsertionLevel -> {Classes},
   Model -> "SM", ExcludeParticles -> {S[1], S[2], V[2]}];
Paint[diagsCompton, ColumnsXRows -> {2, 1}, Numbering -> None];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampCompton =
 Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &,
   Apply[List,
    CreateFeynAmp[diagsCompton,
     Truncated -> False]]] //. {(a1__ DiracGamma[6] a2__ +
      a1__ DiracGamma[7] a2__) :> a1 a2, NonCommutative[x___] -> x,
   FermionChain -> DOT,
   FourMomentum[Outgoing, 1] -> p2, FourMomentum[Outgoing, 2] -> k2,
   FourMomentum[Incoming, 1] -> p1, FourMomentum[Incoming, 2] -> k1,
   DiracSpinor -> Spinor,Index[Lorentz, x_] :> LorentzIndex[ToExpression["Lor" <> ToString[x]]],
	Conjugate[PolarizationVector][_, x_,y_]:>Conjugate[PolarizationVector[x,y]],
PolarizationVector[_, x_,y_]:>Conjugate[PolarizationVector[x,y]]}


(* ::Section:: *)
(*Unpolarized process  e^- gamma -> e^- gamma *)


SetMandelstam[s, t, u, p1, k1, -p2, -k2, ME, 0, ME, 0];
sqAmpCompton =
 (Total[ampCompton] Total[(ComplexConjugate[ampCompton]//FCRenameDummyIndices)])//PropagatorDenominatorExplicit//Expand//
DoPolarizationSums//Contract//FermionSpinSum[#, ExtraFactor -> 1/2^2, SpinorCollect -> True]&//
ReplaceAll[#, DiracTrace[x___] :> DiracTrace[x, DiracTraceEvaluate -> True]] & // Contract//
Simplify//TrickMandelstam[#,{s,t,u,2ME^2}]&//Simplify


sqAmpComptonPeskin=(2EL^4(ScalarProduct[p1,k2]/ScalarProduct[p1,k1]+ScalarProduct[p1,k1]/ScalarProduct[p1,k2]+
2ME^2 (1/ScalarProduct[p1,k1]-1/ScalarProduct[p1,k2])+ME^4 (1/ScalarProduct[p1,k1]-1/ScalarProduct[p1,k2])^2))//Simplify;
Print["Check with Peskin and Schroeder, Eq 5.87: ",
      If[(sqAmpComptonPeskin-sqAmpCompton)===0, "Correct.", "Mistake!"]];


masslessSqAmpCompton = (sqAmpCompton /. {ME -> 0})//Simplify
