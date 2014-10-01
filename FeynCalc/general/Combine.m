(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Combine *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 4 February '99 at 1:41 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Combine is an extension of Together *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Combine`",{"HighEnergyPhysics`FeynCalc`"}];

Combine::"usage"=
"Combine[expr] puts terms in a sum over a common denominator, and
cancels factors in the result. Combine is similar to Together,
but accepts the option Expanding and works usually 
better than Together on polynomials involving rationals with
sums in the denominator.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
Expanding = MakeContext["CoreOptions","Expanding"];

Options[Combine] = {Expanding -> False };

Combine[x_, y___Rule] := Block[{combinet1, combinet2, expanding, num, le},
  expanding = (Expanding /. {y} /. Options[Combine]);
  combinet2 = Together[ x /. Plus -> (If[FreeQ[{##}, _^_?Negative] && 
                        FreeQ[{##}, Rational], combinet1[##], Plus[##] ]&) 
                      ] /. combinet1 -> Plus;
Which[expanding === All,
      combinet2 = ExpandNumerator[combinet2 // ExpandDenominator] ,
      expanding === True,
      num = Numerator[combinet2];
      If[Head[num] =!= Plus,
         combinet2 = Expand[num]/Denominator[combinet2],
         If[LeafCount[num]<1000, 
            combinet2 = Expand[num]/Denominator[combinet2],
            le = Length[num];
            combinet2 = Sum[FCPrint[2,"expanding ", i," out of ",le];
                            Expand[num[[i]]],{i,Length[num]}]/
                            Denominator[combinet2]
           ]
        ],True, combinet2
       ];                       
combinet2];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Combine | \n "]];
Null
