(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PartitHead *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: separation of expression according to a head *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`general`PartitHead`",
             "HighEnergyPhysics`FeynCalc`"];

PartitHead::"usage"=
"PartitHead[expr, h] returns a list {ex1, h[ex2]} with ex1 free of
expressions with head h, and h[ex2] having head h.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

PartitHead[x_, y_]     := {1, x} /; Head[x] === y;
PartitHead[x_Times, y_]:= {x, 1} /; FreeQ[x, y];
PartitHead[x_, y_]     := {x, 0} /; FreeQ[x, y];
PartitHead[x_Plus, y_] := {#, x - #}& @ Select[x, FreeQ[#, y[___]]&];
PartitHead[x_Times,y_] := {x/#, #}& @ Select[x,If[Head[#]===y,True]&];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PartitHead | \n "]];
Null