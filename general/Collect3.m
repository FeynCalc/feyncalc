(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Collect3*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Collect3`",
             "HighEnergyPhysics`FeynCalc`"];

Collect3::"usage"= 
"Collect3[expr, {x, y, ...}] collects terms involving the same powers
of monomials x^n1*y^n2 ... An option Factor -> True/False can be 
given, which factors the coefficients. The option Head (default Plus)
determines the applied function to the list of monomials 
mulitplied by their coefficients.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Options[Collect3] = {Factor -> False, Head -> Plus};

Collect3[expr_, vars_List, opts___Rule] :=
     Apply[Head/.{opts}/.Options[Collect3],
           If[Factor/.{opts}/.Options[Collect3],Map[Factor, #], #]& @
           MonomialList[expr, vars, CoefficientDomain->RationalFunctions]
          ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Collect3 | \n "]];
Null
