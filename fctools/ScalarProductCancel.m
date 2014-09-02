(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ScalarProductCancel*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 3 July '98 at 15:45 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`ScalarProductCancel`",{"HighEnergyPhysics`FeynCalc`"}];

ScalarProductCancel::"usage"= 
"ScalarProductCancel[exp, q1, q2, ...] cancels scalar products \
with propagators. ScalarProductCancel[exp] cancels simple cases.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[FCPrint,
			Cases2,
            ChangeDimension,
            Collect2,
            Collecting,
            ExpandScalarProduct, Expand2, 
            Factoring,
            FeynAmpDenominator,
            FeynAmpDenominatorCombine,
            FeynAmpDenominatorSimplify, IFPD, IFPDOn, IFPDOff,
            MemSet,
            Momentum,
            MomentumExpand,
            OPEDelta,
            Pair,
            PropagatorDenominator,
            Select1, Select2];

Options[ScalarProductCancel] = 
{ChangeDimension -> D,
 Collecting -> True,
 FeynAmpDenominatorSimplify -> False,
 FeynAmpDenominatorCombine -> True};


cd[z_,op___Rule] := Block[{nd, moms, momr},
 If[FreeQ[z, Momentum[_]], z,
    If[(nd=(ChangeDimension/.{op}/.Options[ScalarProductCancel]))=!=False,
       If[nd === True, nd = D];
       moms = Select2[Cases2[z, Momentum], Momentum[_]];
       momr = Map[(# -> ChangeDimension[#,nd])&, moms];
       z /. momr
       ,
       z 
      ]
   ]];


Expand3 = Expand2;

ScalarProductCancel[iexp_,opt___Rule] := Block[{sim,exp= cd[iexp,opt]},
sim = Cases2[ Select2[ Cases2[exp, PropagatorDenominator],
                        PropagatorDenominator[Momentum[__],_]
                     ], Momentum
            ];
If[sim === {}, exp,
   sim = Sequence @@ Map[First, sim];
    IFPDOff[Expand2[IFPDOn[exp, sim], IFPD], sim]//FeynAmpDenominatorCombine
  ]                               ];

ScalarProductCancel[iex_,qs___, qlast_ /; Head[qlast] =!= Rule, 
                    opt___Rule
                   ] := 
MemSet[ScalarProductCancel[iex,qs,qlast,opt],
Block[{prp, exp,pqs, pexp, nexp, prule,P1,re,ex},
ex = cd[iex,opt];
(* translate eventually *)
prp = First /@ Select[Cases2[ex, PropagatorDenominator]//MomentumExpand,
             !FreeQ[#,PropagatorDenominator[w_Plus /; Length[w]>2,_]]&
            ];
prp = Select2[Map[Select1[#, {qs,qlast}]&, prp] /. 
              Momentum[a_,___] :> a, Plus
             ];
If[prp === {}, exp = ex, 
(*changemaybelater*)
   prp  = First[prp];
   psol = First[Variables[prp]];
   prul  = Solve[prp == P1, psol][[1,1]];
   prulb = P1 -> prp;
   exp = ExpandScalarProduct[ex /. prul];
 ];

 pqs = Select1[Select2[Cases2[exp,Pair], {qs, qlast}],OPEDelta];
 re = 
  If[pqs === {},exp,
     If[Head[exp]=!=Plus, 
        nexp = 0; pexp = exp
        ,
        nexp = Select1[exp, pqs];
        pexp = ScalarProductCancel[exp - nexp];
       ];
   nexp + 
   Expand3[FixedPoint[sp[#, qs, qlast, opt]&, pexp, 2],{qs,qlast}]
                             
    ];
If[prp =!= {}, re = ExpandScalarProduct[re /. prulb]];
re
]];

SetAttributes[holdf, HoldAll];
holdf[spi[i_]] := FixedPoint[ReleaseHold, spi[i]];

(*
(* broken in V2.3 *)
spib /: HoldForm[spib[i_]] := ReleaseHold[spi[i]] /. spi->spib;
*)

checkpair[x_Plus,qu__] := Map[checkpair[#,qu]&,x];
checkpair[y_ /; Head[y] =!= Plus,qu__] :=
 Block[{c1,dc, aliens, pas, sub},
 If[Head[y] =!= Times, y,
    pas = Cases2[Select1[Select2[y,{qu}], OPEDelta], Pair];
    If[pas === {}, y /. Pair -> noPair,
    c1 = Select1[Variables[Cases2[pas ,Momentum
                                 ]/.Momentum[a_,___]:>a
                          ], {qu}
                ];
       dc = Select1[Variables[Cases2[Select2[y,FeynAmpDenominator],
                    Momentum ]/.Momentum[a_,___]:>a],{qu}];
       aliens = Select2[pas, Select1[c1,dc]];
       sub = Table[aliens[[ij]] -> (aliens[[ij]] /. Pair->noPair),
                   {ij,Length[aliens]} 
                  ];
    If[sub === {},y, y/.sub]
   ]]];
    
sp[exp_,qq___, ql_ /; Head[ql] =!= Rule, opt___Rule] := 
(*
sp[exp,qq,q,opt] = 
*)
Block[{t1=exp,t2,t3,t4,t5,fads,facs,col},
       col  = Collecting /. {opt} /.
              Options[ScalarProductCancel];
       fads = FeynAmpDenominatorSimplify /. {opt} /. 
              Options[ScalarProductCancel];
       facs = FeynAmpDenominatorCombine /. {opt} /. 
              Options[ScalarProductCancel];
If[FreeQ[exp, FeynAmpDenominator] || FreeQ[exp, Pair], exp,

t4 = Catch[

          If[col === True,
          t1 = Collect2[t1,{qq,ql}, Factoring -> False
                       ];
            ];

          t1 = checkpair[t1,qq,ql];
          If[FreeQ[t1, Pair], Throw[t1 /. noPair -> Pair]];
          t1 = IFPDOn[t1, qq, ql];

FCPrint[2,"IFPDOn done in ScalarProductCancel"];
       If[LeafCount[t1]<200 && 
           FreeQ[t1, a_^(pp_ /;Head[pp]=!=Integer)],
          t2 = Expand[t1],
          t2 = Expand2[t1, IFPD]
         ];
(* if q^2/q^2 occured then now there are terms without IFPD *)
(* in dim. reg. these are 0 *)

If[FreeQ[t2, IFPD], t2 = 0];
If[Head[t2] === Plus, Select1[t2,IFPD];
   t2 = Select2[t2, IFPD] ];


 FCPrint[2,"cancelling done in ScalarProductCancel"];
       t3 = IFPDOff[t2, qq, ql];
       If[FreeQ[t3, Pair], Throw[t3 /. noPair -> Pair]];
       t3  = t3 /. noPair -> Pair; 
 FCPrint[2,"IFPDOff done in ScalarProductCancel"];
(* Dialog[Length[t3]]; *)

pex[a_,b_] := pex[a,b] = ExpandScalarProduct[a,b];
   t4 = Expand2[t3 /. Pair -> pex, {qq,ql}];
FCPrint[2,"ExpandScalarProduct done in ScalarProductCancel"];  
   t4 = IFPDOff[IFPDOn[t4,qq,ql],qq,ql] /. noPair->Pair ;
FCPrint[2,"IFPD again, done"];
t4
 ];
       
       If[facs===True, 
FCPrint[3,"combining in SPC"];
          t4 = FeynAmpDenominatorCombine[t4];
FCPrint[3,"combining in SPC done "];
(* this is dangerous ........  COMMENTED out 04/95
   can be done by FDS 
          tadfeyn[qu_][a___,PropagatorDenominator[Momentum[qu_,___],0]..,
                       b___ ] := 0 /; FreeQ[{a,b},qu];
          tadfeyn[qu_,uq_][a___,PropagatorDenominator[Momentum[qu_,___],0]..,
                       b___ ] := 0 /; FreeQ[{a,b},qu];
          tadfeyn[qu_,uq_][a___,PropagatorDenominator[Momentum[uq_,___],0]..,
                       b___ ] := 0 /; FreeQ[{a,b},uq];
          t4 = t4/. FeynAmpDenominator -> tadfeyn[qq,ql] /.
               tadfeyn[qq,ql] -> FeynAmpDenominator;
*)
		  FCPrint[2,"FeynAmpDenominatorCombine done in ScalarProductCancel"];
         ];
       If[fads===True, 
       	  FCPrint[2,"FeynAmpDenominatorSimplify starting on: ",StandardForm[t4]];          
          t4 = FeynAmpDenominatorSimplify[t4,qq,ql];
          FCPrint[2,"FeynAmpDenominatorSimplify done in ScalarProductCancel: ",t4];
         ];
 t4
  ]
     ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ScalarProductCancel | \n "]];
Null
