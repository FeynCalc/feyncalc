(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The head for momenta *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Momentum`",
             "HighEnergyPhysics`FeynCalc`"];

Momentum::"usage"=
"Momentum is the head of a four momentum (p).
The internal representation of a four-dimensional p is
Momentum[p]. For other than four dimensions: Momentum[p, Dimension].
Momentum[p, 4] simplifies to Momentum[p].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[GaugeXi];

SetAttributes[Momentum, Constant];
Momentum[x_ GaugeXi[y_], dim___] := GaugeXi[y] Momentum[x,dim];
Momentum[x_ n_?NumberQ, di___] := n Momentum[x, di];
Momentum[x_, 4]                := Momentum[x];
Momentum[0, ___]               := 0;
Momentum[_, 0]                 := 0;
(* hm ... *)
Momentum[Momentum[x_, di___], ___] := Momentum[x, di];

Momentum /:
   MakeBoxes[ Momentum[p_, in___], TraditionalForm
            ] := MakeBoxes[p, TraditionalForm];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Momentum | \n "]];
Null
