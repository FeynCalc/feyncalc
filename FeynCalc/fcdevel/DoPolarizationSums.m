(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DoPolarizationSums												*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2015 Rolf Mertig
   Copyright (C) 1997-2015 Frederik Orellana
   Copyright (C) 2014-2015 Vladyslav Shtabovenko
*)

(* :Summary:  Compute polarization sums of vector bosons *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcdevel`DoPolarizationSums`",{"HighEnergyPhysics`FeynCalc`"}];

DoPolarizationSums::"usage"="EXPERIMENTAL \n
DoPolarizationSums[exp] sums over 4 vector polarizations
for expressions with a factor of the form\n\n
Pair[LorentzIndex[rho1_], Momentum[Polarization[p_, -I]]]
Pair[LorentzIndex[rho2_], Momentum[Polarization[p_, I]]].\n\n
Warning: Do not contract an expression before using DoPolarizationSums on it.
Contract assumes transversality, that is, 2 polarizations, which is inconsistent
with assuming 4 polarizations";

PolarizationUncontract::"usage"= "PolarizationUncontract does Uncontract
on scalar products involving polarization vectors.";

EpsUncontract::"usage"= "EpsUncontract does Uncontract
on scalar products involving Eps.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


DoPolarizationSums::"noresolv" =
    "Could not resolve polarization structure of `1`.";

DiracGamma = MakeContext["CoreObjects","DiracGamma"];
Eps = MakeContext["CoreObjects","Eps"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];
Polarization = MakeContext["CoreObjects","Polarization"];

MakeContext[ Uncontract, ScalarProductExpand, MomentumExpand, EpsEvaluate ];

EpsUncontract[expr_, opts:OptionsPattern[]] :=
    Block[{},
      expr /. (a : Eps[___, Momentum[__], ___]) :>
          Uncontract[a,
            Sequence @@ ((#[[1]]) & /@ Cases[a, Momentum[__], 1]),
            FilterRules[{opts}, Options[Uncontract]]]];


PolarizationUncontract[expr_, opts:OptionsPattern[]] :=
    Block[{},
      expr /. {(a : (Pair[___, Momentum[Polarization[__], ___], ___] |
                    DiracGamma[Momentum[Polarization[__], ___], ___])) :>
            Uncontract[a,
              Sequence @@ ((#[[1]]) & /@ Cases[a, Momentum[__], 1]),
              Join[FilterRules[FilterRules[{opts}, Options[Uncontract]],Except[Pair]],{Pairs->All}]]}];


DoPolarizationSum[expr_] :=
Which[Count[expr, Polarization, Infinity, Heads -> True] === 0, 4 expr,
      Count[expr, Polarization, Infinity, Heads -> True] // EvenQ,
      expr //. Pair[LorentzIndex[rho1_, d___],
            Momentum[Polarization[p_, -I, OptionsPattern[]], d___]] Pair[
            LorentzIndex[rho2_, d___],
            Momentum[Polarization[p_, I, OptionsPattern[]], d___]] :> -Pair[
            LorentzIndex[rho1, d], LorentzIndex[rho2, d]], True,
      Message[DoPolarizationSums::"noresolv", StandardForm[expr]]; expr];

DoPolarizationSums[expr_, opts:OptionsPattern[]] :=
    Block[{exp1, exp2},
      exp1 = (# // EpsEvaluate //
              EpsUncontract[#, FilterRules[{opts}, Options[Uncontract]]]& //
              PolarizationUncontract[#, FilterRules[{opts}, Options[PolarizationUncontract]]]&)& /@
          Expand[ScalarProductExpand[MomentumExpand[expr]]];
      If[Head[exp1] === Plus, exp2 = List @@ exp1, exp2 = {exp1}];
      Plus @@ (DoPolarizationSum /@ exp2)];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DoPolarizationSums | \n "]];
Null
