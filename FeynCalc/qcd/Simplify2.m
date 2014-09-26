(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Simplify2*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`Simplify2`",{"HighEnergyPhysics`FeynCalc`"}];

Simplify2::"usage"= "Simplify2 is a special ordering function."; 

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
Factoring = MakeContext["CoreOptions","Factoring"];

MakeContext[CA, CF, Collect2, DeltaFunction,
            Epsilon, Tf, PlusDistribution,
            Select1, Select2];

Simplify2[y_] := Block[{t1,t2,t3,null1, null2, cct, col,min},
col =  Collect2[##, Factoring->False];
cct = {CA, CF, Tf};
llt = {Log, PlusDistribution, PolyLog, Zeta2};
map[a_,b_Plus]               := Map[a, b];
map[a_,b_/;Head[b] =!= Plus] := Apply[a, {b}];
t1 = col[Expand[y], Epsilon];



ll = ((Select1[#, llt] col[Select2[#, llt], llt]) /. 
       (1/(x_ -1) :> min (1/(1-x))) /. min :> (-1) 
     )&;

dd = (Select1[#, cct] col[Select2[#, cct], DeltaFunction])&;

cc = (Select1[#, Epsilon] col[Select2[#, Epsilon], cct])&;


(* maybe *)
t2 =  map[ll, map[dd, map[cc, t1]]]
];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Simplify2 | \n "]];
Null
