(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SumP *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`SumP`",{"HighEnergyPhysics`FeynCalc`"}];

SumP::"usage"=
"SumP[k, n/2] is 2^(k-1) Sum[(1+(-1)^j)/j^k, {j,1,n}].
 (or SumP[k,m] = 2^(k-1) Sum[(1+(-1)^j)/j^k, {j,1,2 m}].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Explicit = MakeContext["Explicit"];
SumS = MakeContext["SumS"];

(* m = n/2 *) 
SumP[k_Integer, m_ /; (Head[m] === Integer || 
                       Head[m] === Rational) 
    ] := 2^(k-1) Sum[(1+(-1)^j)/j^k, {j,1,2 m}];

SumP /: Explicit[SumP[k_Integer, m_]] := 
 1/2 (1+(-1)^(2 m)) SumS[k,m] + 1/2 (1-(-1)^(2 m)) SumS[k,Factor[m-1/2]];

   SumP /:
   MakeBoxes[ SumP[i_Integer, m_], TraditionalForm
            ] := RowBox[{SubsuperscriptBox["S", i,"'"], 
                  "\[NoBreak]", "(", "\[NoBreak]", 
                  ToBoxes[m,TraditionalForm], "\[NoBreak]",")"}];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SumP | \n "]];
Null
