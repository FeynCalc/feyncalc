(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Kronecker delta for SU(N) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SUNDeltaContract`",{"HighEnergyPhysics`FeynCalc`"}];

SUNDeltaContract::"usage"=
"SUNDeltaContract[expr] substitues for all
SUNDelta in expr SUNDeltaContract, contracts
the SUN(N) indices and resubstitutes SUNDelta.
\n
SUNDeltaContract[i, j] is the Kronecker-delta for SU(N)
with contraction properties. SUNDeltaContract wraps also the
head SUNIndex around its arguments.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

ExplicitSUNIndex = MakeContext["CoreObjects","ExplicitSUNIndex"];
sundelta := sundelta = MakeContext["CoreObjects","SUNDelta"];
sunn     := sunn     = MakeContext["CoreObjects","SUNN"];
SUNIndex     = MakeContext["CoreObjects","SUNIndex"];

SetAttributes[SUNDeltaContract, Orderless];

(* Added check for integers - noint. F.Orellana, 11/1-2001 *)
noint[x___] :=
    Not[Or @@
        Join[IntegerQ /@ {x}, IntegerQ /@
  ({x} /. {SUNIndex -> Identity, ExplicitSUNIndex -> Identity})]];

SUNDeltaContract[expr_] := (expr //. sundelta ->
  SUNDeltaContract /. SUNDeltaContract -> sundelta)/; !FreeQ[expr,sundelta];

SUNDeltaContract[x_ /; FreeQ[x, SUNIndex] && !IntegerQ[x] &&
                 FreeQ[x, ExplicitSUNIndex],
                 y_ /; FreeQ[y, SUNIndex] && !IntegerQ[y] &&
                 FreeQ[y, ExplicitSUNIndex]
        ] := SUNDeltaContract[SUNIndex[x], SUNIndex[y]];

SUNDeltaContract[x_SUNIndex, x_SUNIndex
       ] := (sunn^2 - 1) /; noint[x];

SUNDeltaContract /: SUNDeltaContract[
                               j_ExplicitSUNIndex, i_SUNIndex]^2 :=
                    SUNDeltaContract[ExplicitSUNIndex[j],
                                     ExplicitSUNIndex[j]];

SUNDeltaContract /: SUNDeltaContract[i_SUNIndex, j_SUNIndex]^2 :=
                     (sunn^2 - 1) /; (i =!= j) && noint[i,j];

SUNDeltaContract/: SUNDeltaContract[i_SUNIndex, j_] *
                   SUNDeltaContract[a_, i_SUNIndex ] :=
                   SUNDeltaContract[a,j] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[a_, i_SUNIndex ] *
                   SUNDeltaContract[i_SUNIndex, j_] :=
                   SUNDeltaContract[a,j] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[i_SUNIndex, j_] *
                   SUNDeltaContract[i_SUNIndex, k_] :=
                   SUNDeltaContract[j,k] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[a_, i_SUNIndex ] *
                   SUNDeltaContract[b_, i_SUNIndex ] :=
                   SUNDeltaContract[a,b] /; noint[i];

SUNDeltaContract/: SUNDeltaContract[i_SUNIndex, j_SUNIndex ] y_[z__] :=
             ( y[z] /. i -> j ) /; (*Added SumOver stuff. F.Orellana. 20/8-2002*)
               FreeQ[y[z], _HighEnergyPhysics`FeynArts`SumOver] &&
               !FreeQ[y[z]//Hold, i] &&
               FreeQ[y[z], SUNDeltaContract[__]^n_Integer?Negative] /;
               noint[i,j];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNDeltaContract | \n "]];
Null
