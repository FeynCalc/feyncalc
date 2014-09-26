(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DoPolarizationSums *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 25 October 2002 at 18:21 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcdevel`DoPolarizationSums`",{"HighEnergyPhysics`FeynCalc`"}];

DoPolarizationSums::"usage"= "***EXPERIMENTAL***\n
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

EpsUncontract[exp_, opts___?OptionQ] :=
    Block[{a},
      exp /. (a : Eps[___, Momentum[__], ___]) :>
          Uncontract[a,
            Sequence @@ ((#[[1]]) & /@ Cases[a, Momentum[__], 1]),
            Sequence@@OptionsSelect[Uncontract, opts]]];


PolarizationUncontract[exp_, opts___?OptionQ] :=
    Block[{a},
      exp /. {(a : (Pair[___, Momentum[Polarization[__], ___], ___] |
                    DiracGamma[Momentum[Polarization[__], ___], ___])) :>
            Uncontract[a,
              Sequence @@ ((#[[1]]) & /@ Cases[a, Momentum[__], 1]),
              Pair -> All,
              Sequence@@Select[OptionsSelect[Uncontract, opts],FreeQ[#, Pair->_]&]]}];


DoPolarizationSum[exp_] :=
    exp //. Pair[LorentzIndex[rho1_, d___],
            Momentum[Polarization[p_, -I], d___]] Pair[
            LorentzIndex[rho2_, d___],
            Momentum[Polarization[p_, I], d___]] :> -Pair[
            LorentzIndex[rho1, d], LorentzIndex[rho2, d]];


DoPolarizationSum1[exp_] :=
Which[Count[exp, Polarization, Infinity, Heads -> True] === 0, 4 exp,
      Count[exp, Polarization, Infinity, Heads -> True] // EvenQ,
      DoPolarizationSum[exp], True,
      Message[DoPolarizationSums::"noresolv", StandardForm[exp]]; exp];

DoPolarizationSums[exp_, opts___?OptionQ] :=
    Block[{exp1, exp2},
      exp1 = (# // EpsEvaluate //
              EpsUncontract[#, Sequence@@OptionsSelect[Uncontract, opts]]& //
              PolarizationUncontract[#, Sequence@@OptionsSelect[PolarizationUncontract, opts]]&)& /@
          Expand[ScalarProductExpand[MomentumExpand[exp]]];
      If[Head[exp1] === Plus, exp2 = List @@ exp1, exp2 = {exp1}];
      Plus @@ (DoPolarizationSum1 /@ exp2)];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DoPolarizationSums | \n "]];
Null
