(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SelectFree *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`SelectFree`",
             "HighEnergyPhysics`FeynCalc`"];

SelectFree::"usage"=
"SelectFree[expr, a, b, ...] is equivalent to
Select[expr, FreeQ2[#, {a,b, ...}]&], except the
special cases: SelectFree[a, b] returns a and
SelectFree[a,a] returns 1 (where a is not a product or
a sum).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
FreeQ2 = MakeContext["FreeQ2"];


SelectFree[0,_] := 0;
SelectFree[a_, b__] := If[(Head[a] === Plus) ||
                       (Head[a] === Times),
                       select1[a,b],
(* need two dummy-vars in case "a" is an integer *)
                       select1[a dum1 dum2, b] /. 
                       {dum1 :> 1, dum2 :> 1}
                      ];

select1[x_, b_ /; Head[b] =!= List
                   ]  := Select[x, FreeQ[#, b]&];
select1[x_, b_List ]  := Select[x, FreeQ2[#, b]&];
select1[x_, b_, c__]  := Select[x, FreeQ2[#, Flatten[{b, c}]]&];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SelectFree | \n "]];
Null
