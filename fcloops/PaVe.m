(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: PaVe *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fcloops`PaVe`",
             "HighEnergyPhysics`FeynCalc`"];

PaVe::"usage"=
"PaVe[ i,j,... {p10,p12,...},{m1^2, mw^2, ...} ] denotes the invariant
(and scalar)
Passarino-Veltman integrals, i.e. the coefficient functions of
the tensor integral decomposition.  Joining plist and mlist gives the same
conventions as for A0, B0, C0, D0.  Automatic simlifications are
performed for the coefficient functions of two-point integrals and
for the scalar integrals.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

ClearAttributes[PaVe, ReadProtectecd];

MakeContext[A0, B0, C0, D0, B00, B1,B11, BReduce];
small = MakeContext["SmallVariable"];

(* Symmetry in the indices *)
 PaVe[i_,j__,  pl_List, ml_List ]  := PaVe @@Join[Sort[{i,j}],{pl,ml}]/;
                                      !OrderedQ[{i,j}];
(* Special cases of PaVe: *)
 PaVe[0, {}, {x_}]      := A0[x];
 PaVe[0, {p2}, {x_,y_}] := B0[p2,x,y];
 PaVe[1,{pp_},{mm1_,mm2_}]  := B1[pp, mm1, mm2];

(* there is no tensorial 1-point function *)
 PaVe[_,{},{_}] := 0;
(* but a non-zero coefficient of g_munu *)
 PaVe[0,0,{},{m2_}] := (m2/4 A0[m2] + m2^2/8) /; $LimitTo4 === True;
 PaVe[0, {p_}, {m1_, m2_}] := B0[p, m1, m2];
 PaVe[0,0,{p_},{m1_,m2_}]  := B00[p,m1,m2] /; $LimitTo4 === True;;
 PaVe[1,1,{pp_},{mm1_,mm2_}]  := B11[pp,mm1,mm2] /; $LimitTo4 === True;
(* ****************************************************************** *)
(* Notation :   p10 = p1^2;  p12 = (p1-p2)^2;  etc.                   *)
(* ****************************************************************** *)
(* C2 --> C1, C22 --> C11,  C002 --> C001, C222 --> C111,   *)
(* if p10=p20  and  m2=m3    *)
PaVe[2,{p10_,p12_,p10_},{m1_,m2_,m2_}]  :=PaVe[1,{p10,p12,p10},{m1,m2,m2}];
PaVe[2,2,{p10_,p12_,p10_},{m1_,m2_,m2_}]:=PaVe[1,1,{p10,p12,p10},{m1,m2,m2}];
PaVe[0,0,2,{p10_,p12_,p10_},{m1_,m2_,m2_}]:=
   PaVe[0,0,1,{p10,p12,p10},{m1,m2,m2}];
PaVe[1,2,2,{p10_,p12_,p10_},{m1_,m2_,m2_}]:=
   PaVe[1,1,2,{p10,p12,p10},{m1,m2,m2}];
PaVe[2,2,2,{p10_,p12_,p10_},{m1_,m2_,m2_}]:=
   PaVe[1,1,1,{p10,p12,p10},{m1,m2,m2}];
(* a special case *)
 PaVe[ 2,{p10_, pp_,pp_},{m_,m_,m2_} ]:=
  - 2 PaVe[1,{p10,pp,pp},{m,m,m2}] - PaVe[0,{p10,pp,pp},{m,m,m2}];
(* *********************************************************************** *)
(*  D's: The argument list is (in general) : p10, p12, p23, p30, p20, p13  *)
(* *********************************************************************** *)
 pav[{a__},pl_List,ml_List]:=PaVe[a,pl,ml];
(*  1 <---> 2;   p20=p10,  p23=p13 , m3 = m2  *)
 PaVe[x__,{p10_,p12_,p13_,p30_,p10_,p13_},{m1_,m2_,m2_,m4_}]:=
  pav[{x} /. {1:>2, 2:>1}, {p10,p12,p13,p30,p10,p13},{m1,m2,m2,m4} ]/;
   Count[{x}, 2] > Count[{x}, 1];

(*  1 <---> 3;   p10=p30,  p12=p23 , m2 = m4  *)
 PaVe[x__,{p10_,p12_,p12_,p10_,p20_,p13_},{m1_,m2_,m3_,m2_}]:=
    pav[{x} /. {1:>3, 3:>1}, {p10,p12,p12,p10,p20,p13},{m1,m2,m3,m2} ]/;
     Count[{x}, 3] > Count[{x}, 1];

(*  2 <---> 3;   p30=p20,  p13=p12 , m3 = m4  *)
 PaVe[x__,{p10_,p12_,p23_,p20_,p20_,p12_},{m1_,m2_,m3_,m3_}]:=
  pav[{x} /. {2:>3, 3:>2}, {p10,p12,p23,p20,p20,p12},{m1,m2,m3,m3}]/;
     Count[{x}, 3] > Count[{x}, 2];

(* in order to canonize the C0's  (args:   p1^2, (p2-p1)^2, p2^2)  *)
 PaVe[0, {p10_, p12_, p20_}, {m1_, m2_, m3_}] := 
   cord[p10, p12, p20,m1,m2,m3];
 PaVe[0, {p10_, p12_, p23_, p30_, p13_, p20_}, {m1_, m2_, m3_, m4_}]:=
   D0[p10, p12, p23, p30, p13, p20, m1, m2, m3, m4](*//PaVeOrder*);


 cord[a_,b_,c_, m1_,m2_,m3_]:=
     C0@@( Sort[{ {a,b,c, m1,m2,m3}, {c,b,a, m1,m3,m2},
                  {a,c,b, m2,m1,m3}, {b,c,a, m2,m3,m1},
                  {c,a,b, m3,m1,m2}, {b,a,c, m3,m2,m1} } ][[1]] );

   cord[C0[six__],{}]:=cord[six];
   cord[C0[te__], argu_List ]:= Block[{int, puref, arg, smalist, six,
                                       varg, sma, pw},
       six =  {te}/. small->sma;
       If[FreeQ[six, sma],
          arg = argu,
          smalist = Select[Variables[six/.Power->pw], 
                           (!FreeQ[#, sma])&]/.pw->Power;
          If[!FreeQ[smalist, Power],
             arg = (argu/.small->Identity) /.
                   Map[(#[[1,1]] -> (#[[1]]) )&, smalist ],
             arg = argu/.small->sma
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
       int/.sma->small] /; Length[{te}]===6 && Length[argu]>0;


   PaVe /:
   MakeBoxes[PaVe[ij___,{a_,b_,c_,d_,e_,f_},{m1_,m2_,m3_}], TraditionalForm
            ] :=
   RowBox[{SubscriptBox["C",TBox[ij]]}];
   PaVe /:
   MakeBoxes[PaVe[ij___,{__},{m1_,m2_,m3_,m4_}], TraditionalForm] :=
   RowBox[{SubscriptBox["D",TBox[ij]]}];
   PaVe /:
   MakeBoxes[PaVe[ij___,{__},{m1_,m2_,m3_,m4_,m5_}], TraditionalForm] :=
   RowBox[{SubscriptBox["E",TBox[ij]]}];
   PaVe /:
   MakeBoxes[PaVe[ij___,{__},{m1_,m2_,m3_,m4_,m5_,m6_}], TraditionalForm
            ] :=
   RowBox[{SubscriptBox["F",TBox[ij]]}];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "PaVe | \n "]];
Null
