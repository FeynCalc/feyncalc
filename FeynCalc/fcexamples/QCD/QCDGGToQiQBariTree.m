(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QCDGGToQiQBariTree                                               *)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Computation of the matrix element squared for the
              g g -> q_i qbar_i scattering in QCD at tree level             *)

(* ------------------------------------------------------------------------ *)



(* ::Section:: *)
(*Load FeynCalc, FeynArts and PHI*)


If[ $FrontEnd === Null,
    $FeynCalcStartupMessages = False;
    Print["Computation of the matrix element squared for the g g -> q_i qbar_i scattering in QCD at tree level"];
];
If[$Notebooks === False, $FeynCalcStartupMessages = False];
$LoadTARCER = False;
$LoadPhi =$LoadFeynArts= True;
<<HighEnergyPhysics`FeynCalc`
$FAVerbose = 0;


(* ::Section:: *)
(*Generate Feynman diagrams*)


topGGToQiQBari = CreateTopologies[0, 2 -> 2];
diagsGGToQiQBari =
  InsertFields[topGGToQiQBari,  {V[5],V[5]}-> {F[3, {1}], -F[3, {1}]}, InsertionLevel -> {Classes},
   Model -> "SMQCD", ExcludeParticles -> {S[1], S[2], V[1],V[2]}];
Paint[diagsGGToQiQBari, ColumnsXRows -> {3, 1}, Numbering -> None];


(* ::Section:: *)
(*Obtain corresponding amplitudes*)


ampGGToQiQBari =
 Map[ReplaceAll[#, FeynAmp[_, _, amp_, ___] :> amp] &,
   Apply[List,
    FCPrepareFAAmp[CreateFeynAmp[diagsGGToQiQBari,
     Truncated -> False]]]] //. {(a1__ DiracGamma[6] a2__ +
      a1__ DiracGamma[7] a2__) :> a1 a2, NonCommutative[x___] -> x,
   FermionChain -> DOT,
   FourMomentum[Outgoing, 1] -> p1, FourMomentum[Outgoing, 2] -> p2,
   FourMomentum[Incoming, 1] -> k1, FourMomentum[Incoming, 2] -> k2,
   DiracSpinor -> Spinor,Index[Lorentz, x_] :> LorentzIndex[ToExpression["Lor" <> ToString[x]]]
,Index[Gluon,x_]:>SUNIndex[ToExpression["Glu"<>ToString[x]]],
Index[Colour,x_]:>SUNFIndex[ToExpression["Col"<>ToString[x]]],
SumOver[__]:>1,MetricTensor->MT,PolarizationVector[_, x_, y_] :>
PolarizationVector[x, y, Transversality->True], Conjugate[PolarizationVector][_, x_, y_] :>
Conjugate[PolarizationVector[x, y, Transversality->True]]
}/.{SUNT->SUNTF}


(* ::Section:: *)
(*Unpolarized process  g g -> q_i qbar_i*)


SetMandelstam[s, t, u, k1, k2, -p1, -p2, 0, 0, MU, MU];
sqAmpGGToQiQBari =(1/8^2)*
 (Total[ampGGToQiQBari] Total[(ComplexConjugate[ampGGToQiQBari]//FCRenameDummyIndices)])//PropagatorDenominatorExplicit//SUNSimplify[#,Explicit->True,
SUNNToCACF->False]&//FermionSpinSum[#,  SpinorCollect -> True]&//Contract//ReplaceAll[#,{DiracTrace->Tr,
SUNN->3}]&//DoPolarizationSums[#,k1,k2,ExtraFactor->1/2]&//DoPolarizationSums[#,k2,k1,ExtraFactor->1/2]&//Simplify


masslesssqAmpGGToQiQBari = TrickMandelstam[(sqAmpGGToQiQBari /. {MU -> 0})//Simplify,{s,t,u,0}]


masslesssqAmpGGToQiQBariEllis=(1/6)Gstrong^4 (t^2+u^2)/(t u)-(3/8)Gstrong^4 (t^2+u^2)/(s^2);
Print["Check with Ellis, Stirling and Weber, Table 7.1: ",
      If[TrickMandelstam[Simplify[masslesssqAmpGGToQiQBariEllis-masslesssqAmpGGToQiQBari],{s,t,u,0}]===0, "Correct.", "Mistake!"]];
