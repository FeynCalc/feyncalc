(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SumT *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`SumT`",{"HighEnergyPhysics`FeynCalc`"}];

SumT::"usage"=
"SumT[r, n] represents Sum[(-1)^i/i^r, {i,1,n}],
 SumT[r,s, n] is Sum[1/k^r (-1)^j/j^s, {k, 1, n}, {j, 1, k}], 
 SumT[n] is Sum[(-1)^j/j^2 SumS[1,j],{j,1,n}].
For purely integer arguments SumT is evaluated.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Options[SumT] = {Reduce -> False};

PowerSimplify = MakeContext["PowerSimplify"];
SumS = MakeContext["SumS"];

SumT[n__Integer, em_Symbol -1, Reduce -> True] := SumT[n, em-1];


SumT[n_Integer,opt___Rule] := Sum[(-1)^j/j^2 SumS[1, j],{j, 1, n}];

SumT[i_Integer, n_Symbol + a_., opt___Rule] := PowerSimplify[
 (SumT[i, n+a-1] + (-1)^(n+a)/(n+a)^i)           ] /; (a >= 0) &&
  (Reduce /. {opt} /. Options[SumT]);

SumT[i_Integer, n_Symbol + a_Integer, opt___Rule] := 
 PowerSimplify[(SumT[i, n+a+1] - (-1)^(n+a+1) /(n+a+1)^i) ] /;(a<-1)&&
  (Reduce /. {opt} /. Options[SumT]);

SumT[r_Integer, n_Integer] := Sum[(-1)^i/i^r, {i,1,n}];

SumT[r_Integer, s_Integer, n_Integer] := 
 Sum[1/k^r (-1)^j/j^s, {k, 1, n}, {j, 1, k}];

SumT[1,2,n_Symbol + a_., opt___Rule] := 
 ((-1)^(n+a)/(n+a)^3 + 1/(n+a) SumT[2,n+a-1] + SumT[1,2, n+a-1]
 ) /; (a >= 0) && (Reduce /. {opt} /. Options[SumT]);

 MakeBoxes[
             SumT[i_Integer, m_,opt___Rule]^n_Integer, TraditionalForm
          ] := RowBox[{Subsuperscript[OverscriptBox["S","~"], 
                    Tbox[i], n], "(", m , ")"}];
   SumT /:
   MakeBoxes[
             SumT[m_,opt___Rule], TraditionalForm
            ] := RowBox[{OverscriptBox["S","~"], 
                         "(", Tbox[m] , ")"}];

   SumT /:
   MakeBoxes[
             SumT[i_Integer, m_,opt___Rule], TraditionalForm
            ] := RowBox[{SubscriptBox[OverscriptBox["S","~"], i], 
                         "(", Tbox[m] , ")"}];

   SumT /:
   MakeBoxes[ SumT[i_Integer,j__Integer, m_,o___Rule], TraditionalForm
            ] := RowBox@@{{SubscriptBox[OverscriptBox["S","~"], 
                       StringJoin@@Map[ToString,{i,j}]], "(", 
                         Tbox[m] , ")"}
                         };

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SumT | \n "]];
Null
