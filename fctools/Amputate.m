(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Amputate*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:57 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Amputate`",{"HighEnergyPhysics`FeynCalc`"}];

Amputate::"usage"= "Amputate[exp,q1,q2, ...] amputates Eps
and DiracGamma. Amputate[exp,q1,q2, Pair->{p}] amputates
also p.q1 and p.q2; Pair -> All amputates all except
OPEDelta.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[Cases2, Dimension, DiracSlash,
            DiracGamma, DotSimplify, Eps, Expanding, 
            FeynCalcInternal, FourVector, FreeQ2, 
            LeviCivita, LorentzIndex, Momentum, OPEDelta, Pair,
            ScalarProduct,Select1
           ];

Options[Amputate] = {Dimension -> D, Pair -> {}, Unique -> True};

Amputate[x_Plus, q__] := Map[Amputate[#,q]&, x];
Amputate[x_List, q__] := Map[Amputate[#,q]&, x];

(* change September 2003 by Rolf Mertig,
otherwise 
Amputate[Pair[Momentum[k1, D], Momentum[q2, D]], q1, q2, Pair -> k1]
would not work
*)
Amputate[x_, ___?OptionQ]:=x;
Amputate[x_, q1_, q2__, opt___?OptionQ
        ] := Amputate[Amputate[x,q1,opt],q2, opt];

Amputate[ex_, qi_ /; Head[qi]=!=Rule, opt___Rule
        ] := Block[{q,exp,eeps,nex,li,li1,li2,dim,par,dummy,inc,a$AL},
   exp = FeynCalcInternal[ex];
   dim = Dimension /. {opt} /. Options[Amputate];
   If[Head[qi]===Momentum, q = First[qi], q = qi];
   par = Flatten[{Pair /. {opt} /. Options[Amputate]}];
   If[(Unique /. {opt} /. Options[Amputate]) === True, 
      a$AL = Unique[$AL], a$AL = $AL
     ];
   If[par===All, 
      par = Select1[Map[First, Select1[Cases2[exp, Momentum], OPEDelta]],q]
     ];

If[(par === {} && FreeQ2[exp, {Eps, DiracGamma}]) || 
   (Head[dummy exp] =!= Times),
   exp,
   nex = exp;
   If[FreeQ[nex, a$AL], inc = 0, 
      inc = (Max @@ Map[First, Cases2[nex, a$AL]]);
     ];
   If[!FreeQ[nex,Eps],
      nex = nex /. Eps -> eeps;
      nex = nex /. eeps[aaa__]^2 :> TIMES[eeps[aaa],eeps[aaa]];
      nex = nex //. {eeps[a___,Momentum[q,___],b___] :>
                    (li=LorentzIndex[a$AL[inc=inc+1],dim];
                     Pair[Momentum[q,dim], li] *
                     eeps[a,li,b]
                    )} /. eeps -> Eps /. TIMES -> Times;
     ];
      If[par=!={} && Length[par]>0 && Head[par]===List,
         nex = nex /. Pair[aa__/;!FreeQ2[{aa}, par]
                          ]^n_Integer?Positive :>
               Apply[times, Table[Pair[aa], {j,n}]];
         If[MemberQ[par, q],
            nex = nex //. Pair[Momentum[q,dim], Momentum[q,dim]] :>
                          (li1 = LorentzIndex[a$AL[inc=inc+1], dim]; 
                           li2 = LorentzIndex[a$AL[inc=inc+1], dim]; 
                           Pair[Momentum[q, dim], li1] *
                           Pair[Momentum[q, dim], li2] *Pair[li1, li2]
                          );
            par = Select1[par, q];
           ];
         
         nex = nex //.{Pair[Momentum[q,___], Momentum[pe_,___]
                          ] :> (li=LorentzIndex[a$AL[inc=inc+1],dim];
                      Pair[Momentum[q,dim], li] *    
                      Pair[Momentum[pe,dim],li])/;MemberQ[par,pe]
                     } /. times -> Times;
        ];
   If[!FreeQ[nex, DiracGamma],
      nex = nex /. DiracGamma -> dirg;
      nex = nex //. dirg[Momentum[q,___],b___] :>
                    (li = LorentzIndex[a$AL[inc=inc+1],dim];
                     Pair[Momentum[q,dim], li] *
                     dirg[li,dim]
                    ) /. dirg -> DiracGamma;
      nex = DotSimplify[nex,Expanding -> False];
     ];
        nex/.dummy->1]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Amputate | \n "]];
Null
