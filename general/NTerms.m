(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NTerms *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: created 4 March '97 at 14:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  NTerms is like Length *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`NTerms`",
             {"HighEnergyPhysics`FeynCalc`"}];

NTerms::"usage"=
"NTerms[x] is equivalent to Length if x is a sum; otherwise
NTerms[x] returns 1, except NTerms[0] -> 0."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   NTerms[x_Plus] := Length[x];    (*NTermsdef *)
   NTerms[x_] := Block[{ntermslex = Expand[x]},
                       If[ Head[ntermslex]===Plus,
                           ntermslex = Length[ntermslex],
                           If[x===0, ntermslex = 0, ntermslex = 1]
                         ];
             ntermslex];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "NTerms | \n "]];
Null
