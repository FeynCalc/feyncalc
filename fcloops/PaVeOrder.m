(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
 
(* :Title: PaVeOrder *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`PaVeOrder`",
             {"HighEnergyPhysics`FeynCalc`"}];


PaVeOrder::"usage"=
"PaVeOrder[expr] orders the arguments of all D0 in expr in a standard way.
PaVeOrder[expr, PaVeOrderList -> { {..., s, u, ...},
{... m1^2, m2^2, ...}, ...}] orders the arguments of all D0 in expr
according to the specified ordering lists.
The lists may contain only a subsequence of the D0-variables.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
A0,B0,B1,B00,B11,
C0, 
D0,
PaVeOrderList
]; 
small = MakeContext["SmallVariable"];



 Options[PaVeOrder] = {PaVeOrderList -> {}};

(* smallLL is intermediately introduced for small *)
(* PaVeOrderdef *)
 PaVeOrder[expr_,opt___Rule]:=Block[{new, dordering, opli, cordering,
             be0, be1, aa0, be11, be00, dordering0,j,nulL },
   opli = PaVeOrderList/.{opt}/. Options[PaVeOrder];
   If[opli === False, new = expr,
   new = expr/.B0->be0/.B1->be1/.A0->aa0/.B00->be00/.B11->be11/.
         small->smallLL/. 0->nulL;
   opli = opli /. 0 -> nulL /. small->smallLL;
   dordering0[ten__]:=(D0@@(oldper[ten][[1]]));
   If[ Length[opli]>0, 
       If[ Head[opli[[1]]]=!=List, opli = {opli}];
       If[expr=!=(D0@@opli[[1]]),
          new = new /. D0 -> dordering0;
          For[j=1, j<=Length[opli], j++,  
              dordering[j][ten10__]:= D0 @@ dord[ D0[ten10], opli[[j]]];
              cordering[j][six06__]:= C0 @@ cord[ C0[six06], opli[[j]]];
              new = new/.D0->dordering[j]/.C0 -> cordering[j]
             ]
        ],
       new = new /. D0 -> dordering0 /. C0 -> cord;
      ];
   new = new(*/.C0->cord*) /. nulL -> 0 /. smallLL -> small;
   new = new/.be0->B0/.be1->B1/.aa0->A0/.be00->B00/.be11->B11;
    ];
    new];

cord[a_,b_,c_, m1_,m2_,m3_]:=
     C0@@( Sort[{ {a,b,c, m1,m2,m3}, {c,b,a, m1,m3,m2},
                  {a,c,b, m2,m1,m3}, {b,c,a, m2,m3,m1},
                  {c,a,b, m3,m1,m2}, {b,a,c, m3,m2,m1} } ][[1]] );

   cord[C0[six__],{}]:=cord[six];
   cord[C0[te__], argu_List ]:= Block[{int, puref, arg, smalist, six,
                                       varg, sma, pw},
       six =  {te}/. smallLL->sma;
       If[FreeQ[six, sma],
          arg = argu,
          smalist = Select[Variables[six/.Power->pw], 
                           (!FreeQ[#, sma])&]/.pw->Power;
          If[!FreeQ[smalist, Power],
             arg = (argu/.smallLL->Identity) /.
                   Map[(#[[1,1]] -> (#[[1]]) )&, smalist ],
             arg = argu/.smallLL->sma
            ];
         ];
       varg = Variables[arg];
       For[iv=1,iv<=Length[varg],iv++,
           If[(!FreeQ[six, varg[[iv]]^2]) && FreeQ[arg,varg[[iv]]^2],
              arg = arg/.varg[[iv]]->(varg[[iv]]^2)];
          ];
       puref = func[Apply[or,(stringmatchq[slot[1], #]& /@ tomatch[arg])
                         ]]/.slot->Slot/.func->Function/.or->Or/.
                          stringmatchq->StringMatchQ;
       int = Select[ tostring /@ (oldper@@six),
                     func[ stringmatchq[slot[1],tomatch[arg]]
                         ]/.slot->Slot/.func->Function/.
                           stringmatchq->StringMatchQ
                          ];
       If[Length[int] === 0, int = six,int=ToExpression[int[[1]]]];
       int/.sma->smallLL] /; Length[{te}]===6 && Length[argu]>0;


(* Make use of the nice new StringReplace *)
   
   tostring = ToString[InputForm[#], PageWidth -> 4711]&;
   tomatch[{li:{__}..}]:= tomatch /@ {li};
   tomatch[{li__}]:=StringReplace[tostring[{li}],{"{"->"*","}"->"*"}]/;
                                  Head[{li}[[1]]]=!=List;
   dord[D0[ten__],{}]:=dord[D0[ten]];
   dord[D0[te__], argu_List ]:= Block[{int, puref, arg, smalist, ten, 
                                       varg, sma, pw},
       ten =  {te}/. smallLL->sma;
       If[FreeQ[ten, sma], 
          arg = argu,
          smalist = Select[Variables[ten/.Power->pw], 
                           (!FreeQ[#, sma])&]/.pw->Power;
          If[!FreeQ[smalist, Power], 
             arg = (argu/.smallLL->Identity) /. 
                   Map[(#[[1,1]] -> (#[[1]]) )&, smalist ],
             arg = argu/.smallLL->sma
            ];
         ];
       varg = Variables[arg];
       For[iv=1,iv<=Length[varg],iv++,
           If[(!FreeQ[ten, varg[[iv]]^2]) && FreeQ[arg,varg[[iv]]^2], 
              arg = arg/.varg[[iv]]->(varg[[iv]]^2)];
          ];
       puref = func[Apply[or,(stringmatchq[slot[1], #]& /@ tomatch[arg])
                         ]]/.slot->Slot/.func->Function/.or->Or/.
                          stringmatchq->StringMatchQ;
       int = Select[ tostring /@ (oldper@@ten), 
                     func[ stringmatchq[slot[1],tomatch[arg]]
                         ]/.slot->Slot/.func->Function/.
                           stringmatchq->StringMatchQ 
                          ];
       If[Length[int] === 0, int = ten,int=ToExpression[int[[1]]]];
       int/.sma->smallLL] /; Length[{te}]===10 && Length[argu]>0;

(* If no ordering list is given, a standard representative is returned *)
 dord[D0[ten__]]:=(oldper[ten][[1]])/;Length[{ten}]===10;

oldper[a_,b_,c_, m1_,m2_,m3_] := 
Sort[{ {a,b,c, m1,m2,m3}, {c,b,a, m1,m3,m2},
                  {a,c,b, m2,m1,m3}, {b,c,a, m2,m3,m1},
                  {c,a,b, m3,m1,m2}, {b,a,c, m3,m2,m1} } 
    ];

(* This list has been calculated with FeynCalc! *)
oldper[p10_,p12_,p23_,p30_,p20_,p13_,m0_,m1_,m2_,m3_]:=Sort[{
   {p10, p12, p23, p30, p20, p13, m0, m1, m2, m3},
   {p10, p13, p23, p20, p30, p12, m0, m1, m3, m2},
   {p20, p12, p13, p30, p10, p23, m0, m2, m1, m3},
   {p20, p23, p13, p10, p30, p12, m0, m2, m3, m1},
   {p30, p13, p12, p20, p10, p23, m0, m3, m1, m2},
   {p30, p23, p12, p10, p20, p13, m0, m3, m2, m1},
   {p10, p20, p23, p13, p12, p30, m1, m0, m2, m3},
   {p10, p30, p23, p12, p13, p20, m1, m0, m3, m2},
   {p12, p20, p30, p13, p10, p23, m1, m2, m0, m3},
   {p12, p23, p30, p10, p13, p20, m1, m2, m3, m0},
   {p13, p30, p20, p12, p10, p23, m1, m3, m0, m2},
   {p13, p23, p20, p10, p12, p30, m1, m3, m2, m0},
   {p20, p10, p13, p23, p12, p30, m2, m0, m1, m3},
   {p20, p30, p13, p12, p23, p10, m2, m0, m3, m1},
   {p12, p10, p30, p23, p20, p13, m2, m1, m0, m3},
   {p12, p13, p30, p20, p23, p10, m2, m1, m3, m0},
   {p23, p30, p10, p12, p20, p13, m2, m3, m0, m1},
   {p23, p13, p10, p20, p12, p30, m2, m3, m1, m0},
   {p30, p10, p12, p23, p13, p20, m3, m0, m1, m2},
   {p30, p20, p12, p13, p23, p10, m3, m0, m2, m1},
   {p13, p10, p20, p23, p30, p12, m3, m1, m0, m2},
   {p13, p12, p20, p30, p23, p10, m3, m1, m2, m0},
   {p23, p20, p10, p13, p30, p12, m3, m2, m0, m1},
   {p23, p12, p10, p30, p13, p20, m3, m2, m1, m0}       }];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PaVeOrder | \n "]];
Null
