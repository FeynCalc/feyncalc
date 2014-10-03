(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PowerFactor *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`PowerFactor`",{"HighEnergyPhysics`FeynCalc`"}];

PowerFactor::"usage"=
"PowerFactor[exp] replaces x^a y^a with (x y)^a.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[SelectFree, SelectNotFree];

PowerFactor[exp_Plus] := PowerFactor /@ exp;

PowerFactor[exp_] := If[Head[exp] =!= Times,
  exp //. {x_^a_ y_^a_ :> (x y)^a},
  SelectFree[exp, Power] (SelectNotFree[exp, Power] //.
                          {x_^a_ y_^a_ :> (x y)^a}
                         )
                       ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PowerFactor | \n "]];
Null
