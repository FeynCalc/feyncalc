(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: expands DiracGamma[ exp_Plus ] *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracGammaExpand`",
             "HighEnergyPhysics`FeynCalc`"];

DiracGammaExpand::"usage"=
"DiracGammaExpand[exp] expands all DiracGamma[Momentum[a+b+..]] in \
exp into (DiracGamma[Momentum[a]] + DiracGamma[Momentum[b]] + ...).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DiracGamma, MomentumExpand, Momentum];

(* Catch DiracGamma[Momentum[a] + Momentum [b] +...].
   F.Orellana. 26/2-2003 *)
(* Redundant I think.*)
(*extraDiracRule = DiracGamma[b : HoldPattern[
      Plus[(___*Momentum[__] | Momentum[__]) ..]], dim___]  :>
      (DiracGamma[#, dim]& /@ b);*)

DiracGammaExpand[x_] :=
If[FreeQ[x, DiracGamma], MakeContext["FeynCalcInternal"][x], x
  ] /. DiracGamma -> gaev /. gaevlin -> DiracGamma (*/. extraDiracRule*);
gaev[x_,di___]       := gaevlin[Expand[x//MomentumExpand, Momentum], di];
gaevlin[n_Integer]             := DiracGamma[n]; (* necessary !!!!!! *)
gaevlin[x_Plus, di___]         := Map[gaevlin[#, di]&, x];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracGammaExpand | \n "]];
Null
