(* :Title: StringChomp *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: copied from MathXLS, similar to Perl's chomp function *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`StringChomp`",{"HighEnergyPhysics`FeynCalc`"}];

StringChomp::usage="StringChomp[string] chops initial and final white space of string.";

Begin["`Private`"];


SetAttributes[StringChomp, Listable];
chomp[""] = chomp[" "] = "";
StringChomp[s_] := FixedPoint[chomp, s];
chomp[s_] :=
    (* chomp[s] =*)
    StringJoin @@ (Flatten[
       Replace[Split[ Characters[s]],
           {{{(" " | "\t" | "\n" | "\r") ..}, b___,
             {(" " | "\n" | "\t" | "\r") ..}} :> {b},
             {{(" " | "\n" | "\t" | "\r") ..}, r___} :> {r},
             {a___, {(" " | "\n" | "\t" | "\r") ..}} :> {a}}]]);

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "StringChomp| \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

