(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Uncontract*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Uncontract`",
             "HighEnergyPhysics`FeynCalc`"];

Uncontract::usage= "Uncontract[exp,q1,q2, ...] uncontracts Eps
and DiracGamma. Uncontract[exp,q1,q2, Pair->{p}] uncontracts 
also p.q1 and p.q2; Pair -> All uncontracts all except
OPEDelta.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Uncontract, ReadProtected];

MakeContext[Cases2, Dimension, 
            DimensionalReduction,
            DiracSlash,
            DiracGamma, DotSimplify, Eps, Expanding, 
            FeynCalcInternal, FourVector, FreeQ2, 
            LeviCivita, LorentzIndex, Momentum, OPEDelta, Pair,
            ScalarProduct,Select1
           ];

Options[Uncontract] = {Dimension -> D, 
                       DimensionalReduction -> False,
                       Pair -> {}, Unique   -> True};

Uncontract[x_Plus, q__] := Map[Uncontract[#,q]&, x];
Uncontract[x_List, q__] := Map[Uncontract[#,q]&, x];

Uncontract[x_, q1__, q2_ /; Head[q2] =!= Rule, opt___Rule
        ] := Uncontract[Uncontract[x,q2],q1, opt];

Uncontract[ex_, q_ /; Head[q]=!=Rule, opt___Rule
        ] := Block[{exp,eeps,nex,li,li1,li2,dim,par,dummy,inc,
                    a$AL,dr, lidr},
   dre = DimensionalReduction/.{opt} /. Options[Uncontract];
   lidr[z_] := If[dre === True, z/.{LorentzIndex[aa_,_]:>LorentzIndex[aa]/.
                                    Momentum[bb_,_]  :> Momentum[bb]
                                   },
                  z
                 ];
   exp = FeynCalcInternal[ex];
   par = Pair /.      {opt} /. Options[Uncontract];
   If[(Unique /. {opt} /. Options[Uncontract]) === True, 
      a$AL = Unique[$AL], a$AL = $AL
     ];
   If[par===All, 
      par = Map[First, Select1[Cases2[exp, Momentum], OPEDelta]]
     ];

If[(par === {} && FreeQ2[exp, {Eps, DiracGamma}]) || 
   (Head[dummy exp] =!= Times),
   exp,
   nex = exp;
   dim = Dimension /. {opt} /. Options[Uncontract];
   If[FreeQ[nex, a$AL], inc = 0, 
      inc = (Max @@ Map[First, Cases2[nex, a$AL]]);
     ];
   If[!FreeQ[nex,Eps],
      nex = nex /. Eps -> eeps;
      nex = nex /. eeps[aaa__]^2 :> TIMES[eeps[aaa],eeps[aaa]];
      nex = nex //. {eeps[a___,Momentum[q,___],b___] :>
                    (li=LorentzIndex[a$AL[inc=inc+1],dim];
                     Pair[Momentum[q,dim], li] *
                     eeps[a,lidr[li],b]
                    )} /. eeps -> Eps /. TIMES -> Times;
     ];
      If[par=!={} && Length[par]>0 && Head[par]===List,
         nex = nex /. Pair[aa__/;!FreeQ2[{aa}, par]
                          ]^n_Integer?Positive :>
               Apply[times, Table[Pair[aa], {j,n}]];
         If[MemberQ[par, q],
            nex = nex //. Pair[Momentum[q,___], Momentum[q,___]] :>
                          (li1 = LorentzIndex[a$AL[inc=inc+1], dim]; 
                           li2 = LorentzIndex[a$AL[inc=inc+1], dim]; 
                           Pair[Momentum[q, dim], li1] *
(* das ist vielleicht ein Bloedsinn mit dieser dimensionalen Reduktion:
  HIER darf man li1 und li2 nicht 4-dimensional setzen
*)
                           Pair[Momentum[q, dim], li2] *Pair[li1, li2]
                          );
            par = Select1[par, q];
           ];
         
         nex = nex //.{Pair[Momentum[q,___], Momentum[pe_,___]
                          ] :> (li=LorentzIndex[a$AL[inc=inc+1],dim];
                      Pair[Momentum[q,dim], li] *    
                      Pair[Momentum[pe,dim],lidr[li]])/;MemberQ[par,pe]
                     } /. times -> Times;
        ];
   If[!FreeQ[nex, DiracGamma],
      nex = nex /. DiracGamma -> dirg;
      nex = nex //. dirg[Momentum[q,___],b___] :>
                    (li = LorentzIndex[a$AL[inc=inc+1],dim];
                     Pair[Momentum[q,dim], li] *
                     dirg[lidr[li],dim]
                    ) /. dirg -> DiracGamma;
      nex = DotSimplify[nex,Expanding -> False];
     ];
        nex/.dummy->1]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Uncontract | \n "]];
Null
