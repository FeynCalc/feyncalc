(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Integrate4 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 13 April '98 at 11:12 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`Integrate4`",
             "HighEnergyPhysics`FeynCalc`"];

Integrate4::"usage"=
"Integrate4 is like Integrate5, but interruptable if $VeryVerbose >2.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Integrate5 = MakeContext["Integrate5"];



Integrate4[a__] := If[$VeryVerbose > 2, (*Global`INT = {a};*)
(*
                    InputForm[
                       hold[int[a]]/.hold->Hold/.int->Integrate5]
*)
                             Integrate5[a],
                     Integrate5[a]
                     ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Integrate4 | \n "]];
Null
