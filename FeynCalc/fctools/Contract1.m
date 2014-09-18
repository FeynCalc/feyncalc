(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Contract1*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 6 March '98 at 15:35 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Contract1`",{"HighEnergyPhysics`FeynCalc`"}];

Contract1::"usage"=
"Contract1[exp] contracts Upper and Lower indices.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
Dimension,
Expand2,
LorentzIndex, 
Lower,
Pair, 
SelectFree, 
SelectNotFree,
Upper
];



SetAttributes[Contract1, HoldAll];
SetAttributes[ct, HoldAll];
SetAttributes[cot, HoldRest];

ct[a_] := a;

ct[a__] := cot[Evaluate[SelectNotFree[{a}, Upper]],
               Evaluate[SelectFree[{a}, Upper]]
              ];

SetAttributes[TTimes, HoldAll];
SetAttributes[ttimes, HoldAll];

cot[{},{z___}] := TTimes[z];
cot[{z___},{}] := TTimes[z];

cot[{a_, b___}, {z___}] := cot[{b},{a, z}]  /; Head[a]=!=Pair;
(*
cot[{a_, b___}, {z___}] := a cot[{b},{z}]  /; FreeQ[a,Lower];
*)

cot[{Pair[LorentzIndex[Upper[l1_],di1___], 
          LorentzIndex[Upper[l2_],di2___]
         ], y___}, {z___}
    ] :=
   If[!FreeQ[{y,z}, LorentzIndex[Lower[l1],di1]],
      cot@@({{y},{z}}/.LorentzIndex[Lower[l1],di1]->
                       LorentzIndex[Upper[l2],di2]
           ),
      If[!FreeQ[{y,z}, LorentzIndex[Lower[l2],di2]],
         cot@@({{y},{z}}/.LorentzIndex[Lower[l2],di2]->
                          LorentzIndex[Upper[l1],di1]
              ),
         cot@@{{y}, {Pair[LorentzIndex[Upper[l1],di1],
                          LorentzIndex[Upper[l2],di2]
                         ] ,z
                    }
              }
        ]
     ];

cot[{Pair[LorentzIndex[Upper[l_],di___], b_/;FreeQ[b,Upper]
         ], y___}, {z___}] :=
   If[!FreeQ[{y,z}, LorentzIndex[Lower[l],di]],
      cot@@({{y},{z}}/.LorentzIndex[Lower[l],di]->b),
      cot[{y},{Pair[LorentzIndex[Upper[l],di],b], z}]
     ];

cot[{Pair[b_ /;FreeQ[b,Upper],
     LorentzIndex[Upper[l_],di___]], y___}, {z___}] :=
   If[!FreeQ[{y,z}, LorentzIndex[Lower[l],di]],
      cot@@({{y},{z}}/.LorentzIndex[Lower[l],di]->b),
      cot@@{{y},{Pair[LorentzIndex[Upper[l],di],b], z}}
     ];


ttimes[{a___},{b___}] := TTimes[a, b];
SetAttributes[con,HoldAll];
SetAttributes[Contract1,HoldAll];

(*
Contract1[exp_] := FixedPoint[con, exp, 1000];
*)
Contract1[exp_] := con[exp];

fdim[] = 4;
fdim[x_] := x;

con[exp_] := (*con[exp] =*)  Block[{temp},
If[FreeQ[exp, Upper], 
   temp = exp,
   temp = exp /. Times -> ct /. cot -> ttimes;
   If[!FreeQ[exp, Plus],
      temp = Expand2[exp, Upper] /. Dispatch[{TTimes :> ct, Times -> ct}], 
      temp = exp /. Dispatch[{TTimes :> ct, Times -> ct}]
  ]]; 
If[!FreeQ[temp,Lower],
   temp = temp /. Pair[LorentzIndex[Lower[ll_],di___],
                       LorentzIndex[Upper[ll_],di___]] :> fdim[di];
  ];
     temp/.TTimes->Times];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Contract1 | \n "]];
Null
