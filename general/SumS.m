(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SumS *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 23 October '98 at 18:34 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`SumS`",
             "HighEnergyPhysics`FeynCalc`"];

SumS::"usage"=
"SumS[r, n] denotes Sum[Sign[r]^i/i^Abs[r], {i, 1, n}], 
 SumS[r,s, n] is Sum[Sign[r]^k/k^Abs[r] Sign[s]^j/j^Abs[s], 
     {k, 1, n}, {j, 1, k}], etc.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Options[SumS] = {Reduce -> False};

(*

susu={
SumS[-2, m_] :> 1/4*(1 - (-1)^m)* SumS[2, 1/2*(-1 + m)] + 1/4*(1 + (-1)^m)*
    SumS[2, m/2] - SumS[2, m],
SumS[-1, m_] :> SumS[1, (-1 + m)/2]/2 - ((-1)^m* SumS[1, (-1 + m)/2])/2 +
SumS[1, m/2]/2 + ((-1)^m* SumS[1, m/2])/2 - SumS[1, m]
}

dit is nul:

((1 + (-1)^N)*SumS[1, (-2 + N)/2]*SumS[2, -1 + N])/2 -
  ((1 - (-1)^N)*SumS[1, (-1 + N)/2]*SumS[2, -1 + N])/2 -
  (-1)^N*SumS[1, -1 + N]*SumS[2, -1 + N] +
  ((1 + (-1)^N)*SumS[3, (-2 + N)/2])/8 -
  ((1 - (-1)^N)*SumS[3, (-1 + N)/2])/8 -
  (-1)^N*SumS[3, -1 + N] - (-1)^N*SumS[-1, 2, -1 + N] -
  (-1)^N*SumS[2, -1, -1 + N]


SumS[2, 1, m_, opt___Rule] :=
  SumS[1, m]*SumS[2, m] + SumS[3, m] - SumS[1, 2, m]

SumS[1,1,n_, opt___Rule] := 1/2 SumS[1,n]^2 + 1/2 SumS[2,n]


SumS[1, 1, 1, n_,opt___Rule] :=
  SumS[1, n]^3/6 + (SumS[1, n]*SumS[2, n])/2 + SumS[3, n]/3
*)

SumS[n__Integer, em_Symbol -1, Reduce -> True] := SumS[n,em-1];

SumS[r_Integer, n_Integer, opt___Rule] := Sum[Sign[r]^i/i^Abs[r],{i,1,n}];
SumS[r_Integer, s__Integer, n_Integer, opt___Rule] := 
 Sum[Sign[r]^k/k^Abs[r] SumS[s,k], {k, 1, n}];

SumS[r_Integer, n_Symbol + a_. , opt___Rule] := 
 (SumS[r, n+a-1] + Sign[r]^(n+a)/(n+a)^Abs[r]) /; a >= 0 &&
  (Reduce /. {opt} /. Options[SumS]);

SumS[r_Integer, n_Symbol + a_Integer, opt___Rule] := 
 (SumS[r, n+a+1] - Sign[r]^(n+a+1)/(n+a+1)^Abs[r] ) /; a < -1 &&
  (Reduce /. {opt} /. Options[SumS]);

SumS[1, 2, n_Symbol + a_. , opt___Rule] :=
 (1/(n+a)^3 + 1/(n+a) SumS[2,n+a-1] + SumS[1,2,n+a-1]) /; a>=0 &&
  (Reduce /. {opt} /. Options[SumS]);
   
SumS[2, 1, n_Symbol + a_. , opt___Rule] :=
 (1/(n+a)^3 + 1/(n+a)^2  SumS[1,n+a-1] + SumS[2,1,n+a-1]) /; a>=0 &&
  (Reduce /. {opt} /. Options[SumS]);

SumS[1,1, n_Symbol +a_., opt___Rule] := SumS[1,1, n+a-1] + 
1/(n+a) SumS[1, n+a]  /; a>=0 &&
    (Reduce /. {opt} /. Options[SumS]);

   MakeBoxes[
             SumS[i_Integer, m_]^n_Integer, TraditionalForm
            ] := RowBox[{SubsuperscriptBox["S", i, n], "(", 
                         Tbox[m] , ")"}];

   SumS /:
   MakeBoxes[ SumS[i_Integer, m_], TraditionalForm
            ] := RowBox[{SubscriptBox["S", i], "(", Tbox[m] , ")"}];

   SumS /:
   MakeBoxes[ SumS[i_Integer,j__Integer, m_], TraditionalForm
            ] := RowBox[{
          SubscriptBox["S", StringJoin@@Map[ToString,{i,j}]], 
                       "(", Tbox[m], ")"}];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SumS | \n "]];
Null
