(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: XYT *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`general`XYT`",
             "HighEnergyPhysics`FeynCalc`"];

XYT::"usage"= "XYT[exp, x,y] transforms  (x y)^m away ...";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Factor2, PowerSimplify];

(* integral transformation only valid if nonsingular in x, y = 0,1 *)
XYT[exp_, x_, y_] := Block[{z, t, u},
    t = 1/x Factor2[PowerSimplify[Factor2[exp] /. y -> (z/x)]];
    Factor2[PowerSimplify[(1-z) (t /. x :> (1-z) u + z)]/.{u:>y,z:>x}]
                          ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "XYT | \n "]];
Null
