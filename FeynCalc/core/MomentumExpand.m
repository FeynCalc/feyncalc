(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: MomentumExpand *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`MomentumExpand`",{"HighEnergyPhysics`FeynCalc`"}];

MomentumExpand::"usage" =
"MomentumExpand[expr] expands Momentum[a+b+ ...] in expr into
Momentum[a] + Momentum[b] + ... .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Momentum = MakeContext["CoreObjects","Momentum"];

(*MomentumExpanddef*)

hold[]=Sequence[];
fourvecevlin[n_?NumberQ z_, dime___]  := n Momentum[z, dime];
  fourvecev[y_,di___] := ( fourvecev[y,di] =
    Distribute[fourvecevlin[
      Expand[y, Momentum], hold[di]]
              ] /. {hold :> Identity, fourvecevlin :> Momentum}
                         );

MomentumExpand[x_] := x /. Momentum -> fourvecev;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "MomentumExpand | \n "]];
Null
