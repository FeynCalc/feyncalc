(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Explicit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 March '98 at 19:08 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Explicit *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Explicit`",
             "HighEnergyPhysics`FeynCalc`"];

Explicit::usage = 
"Explicit is an option for FieldStrength, GluonVertex,
SUNF, and Twist2GluonOperator. 
If set to True the full form of the operator is inserted. 
Explicit[exp] inserts explicit expressions of FieldStrength, 
GluonVertex and Twist2GluonOperator in exp. SUNF's are replaced 
by SUNTrace objects.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Explicit, ReadProtected];

MakeContext[ExpandScalarProduct];

FieldStrength := FieldStrength = MakeContext["FieldStrength"];

Explicit[y_] := Block[{gv, t2g, fi, r = y}, 
        If[
           CheckContext["GluonVertex"]
           ,
           gv[x__]  := gv[x] = 
           Expand[ExpandScalarProduct[
           MakeContext["GluonVertex"][x, Explicit->True]]];
           r = r /. MakeContext["GluonVertex"] -> gv
          ];
        If[
           CheckContext["Twist2GluonOperator"],
           t2g[x__] := t2g[x] = 
           MakeContext["Twist2GluonOperator"][x, Explicit->True];
           r = r /. MakeContext["Twist2GluonOperator"] -> t2g 
          ];
        If[
           CheckContext["FieldStrength"]
           ,
           fi[x__] := 
           MakeContext["FieldStrength"][x, Explicit->True];
           r = r /. MakeContext["FieldStrength"] -> fi 
          ];
                   r];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Explicit | \n "]];
Null
