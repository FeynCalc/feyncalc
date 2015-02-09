(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDQiQBariToGGTree                                          *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
              q_i qbar_i -> g g scattering in QCD at tree level      *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
    $FeynCalcStartupMessages = False;
    Print["Computation of the matrix element squared for the q_i qbar_i -> g g scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadTARCER = False;
$LoadPhi =$LoadFeynArts= True;
<<HighEnergyPhysics`FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topQiQBariToGG = CreateTopologies[0, 2 -> 2];
diagsQiQBariToGG =
  InsertFields[topQiQBariToGG, {F[3, {1}], -F[3, {1}]} -> {V[5],V[5]}, InsertionLevel -> {Classes},
   Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[1],V[2]}];
Paint[diagsQiQBariToGG, ColumnsXRows -> {3, 1}, Numbering -> None];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampQiQBariToGG =
 Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &,
   Apply[List,
    FCPrepareFAAmp[CreateFeynAmp[diagsQiQBariToGG,
     Truncated -> False]]]] //. {(a1__ DiracGamma[6] a2__ +
      a1__ DiracGamma[7] a2__) :> a1 a2, NonCommutative[x___] -> x,
   FermionChain -> DOT,
   FourMomentum[Outgoing, 1] -> k1, FourMomentum[Outgoing, 2] -> k2,
   FourMomentum[Incoming, 1] -> p1, FourMomentum[Incoming, 2] -> p2,
   DiracSpinor -> Spinor,Index[Lorentz, x_] :> LorentzIndex[ToExpression["Lor" <> ToString[x]]]
,Index[Gluon,x_]:>SUNIndex[ToExpression["Glu"<>ToString[x]]],
Index[Colour,x_]:>SUNFIndex[ToExpression["Col"<>ToString[x]]],
SumOver[__]:>1,MetricTensor->MT,PolarizationVector[_, x_, y_] :>
PolarizationVector[x, y, Transversality->True], Conjugate[PolarizationVector][_, x_, y_] :>
Conjugate[PolarizationVector[x, y, Transversality->True]]
}/.{SUNT->SUNTF}


(* ::Section:: *)
(*Unpolarized process  q_i qbar_i -> g g *)


SetMandelstam[s, t, u, p1, p2, -k1, -k2, MU, MU, 0, 0];
sqAmpQiQBariToGG =(1/3^2)*
 (Total[ampQiQBariToGG] Total[(ComplexConjugate[ampQiQBariToGG]//FCRenameDummyIndices)])//PropagatorDenominatorExplicit//SUNSimplify[#,Explicit->True,
SUNNToCACF->False]&//FermionSpinSum[#, ExtraFactor -> 1/2^2, SpinorCollect -> True]&//Contract//ReplaceAll[#,{DiracTrace->Tr,
SUNN->3}]&//DoPolarizationSums[#,k1,k2]&//DoPolarizationSums[#,k2,k1]&//Simplify


masslesssqAmpQiQBariToGG = TrickMandelstam[(sqAmpQiQBariToGG /. {MU -> 0})//Simplify,{s,t,u,0}]


masslesssqAmpQiQBariToGGEllis=(32/27)Gstrong^4 (t^2+u^2)/(t u)-(8/3)Gstrong^4 (t^2+u^2)/(s^2);
Print["Check with Ellis, Stirling and Weber, Table 7.1: ",
      If[TrickMandelstam[Simplify[masslesssqAmpQiQBariToGGEllis-masslesssqAmpQiQBariToGG],{s,t,u,0}]===0, "Correct.", "Mistake!"]];
