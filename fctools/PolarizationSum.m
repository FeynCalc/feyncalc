(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PolarizationSum *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: various (bosonic) polarization sums *)


(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`PolarizationSum`",
             "HighEnergyPhysics`FeynCalc`"];

PolarizationSum::"usage"=
"PolarizationSum[ mu,nu, ... ] defines
(as abbreviations) different polarization sums.
PolarizationSum[mu, nu] = -g(mu nu);
PolarizationSum[mu, nu, k] = -g(mu nu) + k(mu) k(nu)/k^2;
PolarizationSum[mu, nu, k, n] = polarization sum for spin 1 fields;
(n = external momentum).
PolarizationSum[mu, nu, k, 0] is equivalent to -g(mu nu)";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Collect2                 = MakeContext["Collect2"];
Dimension                = MakeContext["Dimension"];
Factor2                  = MakeContext["Factor2"];
FourVector               = MakeContext["FourVector"];
fci                      = MakeContext["FeynCalcInternal"];
MetricTensor             = MakeContext["MetricTensor"];
Momentum                 = MakeContext["Momentum"];
ScalarProduct            = MakeContext["ScalarProduct"];
Pair                     = MakeContext["Pair"];
scev                     = MakeContext["ExpandScalarProduct"];

Options[PolarizationSum] = {Dimension -> 4};

PolarizationSum[mu_,nu_, ops___Rule]:= fci[
    -MetricTensor[mu, nu, Dimension -> 
          (Dimension /. {ops} /. Options[PolarizationSum])]];  
PolarizationSum[mu_,nu_,k_, ops___Rule]:= fci[
   -MetricTensor[mu,nu, Dimension ->
          (Dimension /. {ops} /. Options[PolarizationSum])] +
     FourVector[k,mu] FourVector[k,nu]/
       Factor2[scev[ScalarProduct[k,k]]]];
PolarizationSum[mu_,nu_,k_,0, ops___Rule] := fci[
 -MetricTensor[mu, nu,
 Dimension -> (Dimension /. {ops} /. Options[PolarizationSum])]];
PolarizationSum[mu_,nu_,k_,n_, ops___Rule] := fci[Collect2[
  -MetricTensor[mu,nu, Dimension ->
          (Dimension /. {ops} /. Options[PolarizationSum])
               ] - FourVector[k,mu] FourVector[k,nu]/
               Factor2[scev[Momentum[k],Momentum[n]]^2] *
               Factor2[scev[Momentum[n],Momentum[n]]] +
            ( FourVector[n,mu] FourVector[k,nu] +
              FourVector[n,nu] FourVector[k,mu] )/
               Factor2[scev[Momentum[k],Momentum[n]]], Pair]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PolarizationSum | \n "]];
Null
