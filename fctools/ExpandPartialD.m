(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ExpandPartialD *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: Leibniz Rule on products of QuantumField *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`ExpandPartialD`",{"HighEnergyPhysics`FeynCalc`"}];

ExpandPartialD::"usage"=
"ExpandPartialD[exp] expands DOT products of QuantumField's in exp
using the Leibniz rule."

(* Added 22/2-2003 in order to use FieldDerivative in a
   flexible way. F.Orellana *)
PartialDRelations::"usage"=
"PartialDRelations is an option for ExpandPartialD. It is a list of rules
applied by ExpandPartialD at the end."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

CovariantD := CovariantD = MakeContext["CovariantD"];
DeclareNonCommutative = MakeContext["DeclareNonCommutative"];
DiracGamma   = MakeContext["DiracGamma"];
DotSimplify  = MakeContext["DotSimplify"];
Eps     := Eps = MakeContext["Eps"];
EpsEvaluate  := EpsEvaluate = MakeContext["EpsEvaluate"];
Explicit := Explicit = MakeContext["Explicit"];
FCI          = MakeContext["FeynCalcInternal"];
FieldStrength := FieldStrength = MakeContext["FieldStrength"];
(* this is needed : *)
FieldDerivative = MakeContext["FieldDerivative"];
FreeQ2       = MakeContext["FreeQ2"];
LorentzIndex = MakeContext["LorentzIndex"];
MemSet       = MakeContext["MemSet"];
Momentum     = MakeContext["Momentum"];
NonCommFreeQ     = MakeContext["NonCommFreeQ"];
OPEDelta     = MakeContext["OPEDelta"];
OPEk         = MakeContext["OPEk"];
OPESum       = MakeContext["OPESum"];
PartialD      = MakeContext["PartialD"];
ExplicitPartialD= MakeContext["ExplicitPartialD"];
LeftPartialD  = MakeContext["LeftPartialD"];
LeftRightPartialD  = MakeContext["LeftRightPartialD"];
RightPartialD = MakeContext["RightPartialD"];
QuantumField = MakeContext["QuantumField"];
SUND         = MakeContext["SUND"];
SUNDelta     = MakeContext["SUNDelta"];
SUNDeltaContract= MakeContext["SUNDeltaContract"];
SUNIndex     = MakeContext["SUNIndex"];
SUNT         = MakeContext["SUNT"];
(*
SUNSimplify  := SUNSimplify = MakeContext["SUNSimplify"];
*)

(* ******************************************************************** *)

With[{lor=LorentzIndex, dot=DOT, fd = FieldDerivative},
Options[ExpandPartialD] =
  (*Expensive, I'm afraid... F.Orellana*)
  {PartialDRelations -> {dot[Global`a___, PartialD[Global`x_,lor[Global`mu_]], Global`b___] :>
   dot[Global`a, fd[dot[Global`b], Global`x, lor[Global`mu]]]}}
];

(* Expand one multiplication at a time. F.Orellana *)
ExpandPartialD[x_, opts___Rule] := Fold[ExpandPartialD1[#1, #2, opts]&, x,
                                   Complement[$Multiplications, {Times}]];

(*
ExpandPartialD1[x_] := Expand[FixedPoint[qfe, FCI[x], 7], QuantumField];
*)

(* moet dat ???? *)
DeclareNonCommutative[OPESum];

$OPEKCOUNT = 0;

sunsi[xx_] := If[FreeQ[xx, SUNIndex], xx, 
                 xx /. SUNDelta -> SUNDeltaContract /. 
                       SUNDeltaContract -> SUNDelta
                ];

(* suggested by Peter Cho; ADDED 03/16/1998 *)
epskill[exp_ /; FreeQ[exp, Eps]] := exp;

epskill[exp_Plus]:= Map[epskill, exp];

epskill[any_ /; FreeQ2[any,{RightPartialD, LeftPartialD}]] := any;

(* careful, ..., LeftPartialD and RightPartialD are operators,
   thus here is no need to look at QuantumField's 
*)
epskill[prod_Times]:=( (prod /. (*DOT*)(fcdot/.Options[epskill]) -> mydot) //. {
       (Eps[a___, LorentzIndex[mu_,___],b___, 
            LorentzIndex[nu_, ___], c___
 ] * mydot[pa1___, (LeftPartialD | RightPartialD)[LorentzIndex[mu_,___]],
           pa2___,
           (LeftPartialD | RightPartialD)[LorentzIndex[nu_,___]],
           pa3___]
       ) :> 0,
       (Eps[a___, LorentzIndex[mu_,___],b___, 
            LorentzIndex[nu_, ___], c___
 ] * mydot[pa1___, (LeftPartialD | RightPartialD)[LorentzIndex[nu_,___]],
           pa2___,
           (LeftPartialD | RightPartialD)[LorentzIndex[mu_,___]],
           pa3___]
       ) :> 0,
(* however, here (03/21/98) one has to be careful ... *)
(*BUGFIXED ?*)
       (
        ( Eps[a___, LorentzIndex[mu_,___], b___, 
                    LorentzIndex[nu_,___], c___ ] |
          Eps[a___, LorentzIndex[nu_,___], b___, 
                    LorentzIndex[mu_,___], c___ ] 
        ) * 
        (
        SUND[de1___SUNIndex, SUNIndex[s1_], de2___SUNIndex, 
                             SUNIndex[s2_], de3___SUNIndex ] |
        SUND[de1___SUNIndex, SUNIndex[s2_], de2___SUNIndex, 
                             SUNIndex[s1_], de3___SUNIndex ] 
        ) *
      mydot[pa1___, QuantumField[___PartialD,
                                 PartialD[LorentzIndex[mu_,___]],
                                 ___PartialD,
                                 fieldname_, LorentzIndex[_,___],
                                 SUNIndex[s1_]
                                ],
            pa2___,
                    QuantumField[___PartialD,
                                 PartialD[LorentzIndex[nu_,___]],
                                 ___PartialD,
                                 fieldname_, LorentzIndex[_,___],
                                 SUNIndex[s2_]
                                ],
           pa3___]
       ) :> 0 ,
        
      (
       (Eps[a___, LorentzIndex[nu_,___], b___, 
                  LorentzIndex[mu_, ___],c___
           ] |
        Eps[a___, LorentzIndex[mu_,___], b___, 
                  LorentzIndex[nu_, ___],c___
           ]
       ) *  QuantumField[___PartialD, 
                    PartialD[LorentzIndex[mu_,___]],
                    ___PartialD,
                    PartialD[LorentzIndex[nu_,___]],
                    ___PartialD,
                    name_ /; (Head[name]=!=PartialD) && 
                             (Head[name]=!=LorentzIndex),
                    LorentzIndex[_,___], colormaybe___
                        ]
       ) :> 0 /; Length[{colormaybe}]<2
      ,
      (
      (ep : (
       Eps[a___, LorentzIndex[nu_,___], b___,
                  LorentzIndex[mu_, ___],c___
           ] |
        Eps[a___, LorentzIndex[mu_,___], b___,
                  LorentzIndex[nu_, ___],c___
           ]
            )    
      ) * QuantumField[p1___PartialD, PartialD[LorentzIndex[mu_,di___]], 
                       p2___PartialD,
                         name_ /; (Head[name] =!= PartialD) &&
                                  (Head[name] =!= LorentzIndex),
                         LorentzIndex[nu_,de___], rest___
                      ] 
       ) :> (EpsEvaluate[ep/.{mu:>nu, nu:>mu}] QuantumField[
              p1,PartialD[LorentzIndex[nu,de]], p2, name, 
              LorentzIndex[mu, di],rest]
            ) /; !OrderedQ[{mu,nu}]
      ,
      (
      (ep : (
       Eps[a___, LorentzIndex[nu_,___], b___,
                  LorentzIndex[mu_, ___],c___
           ] |
        Eps[a___, LorentzIndex[mu_,___], b___,
                  LorentzIndex[nu_, ___],c___
           ]
            )    
      ) * mydot[quf1___,
                QuantumField[p1___PartialD, 
                             PartialD[LorentzIndex[mu_,de___]], 
                             p2___PartialD,
                             name_ /; (Head[name] =!= PartialD) &&
                                      (Head[name] =!= LorentzIndex),
                             LorentzIndex[nu_,di___], rest___
                            ],
                quf2___]
       ) :> (EpsEvaluate[ep/.{mu:>nu, nu:>mu}] mydot[quf1,
             QuantumField[p1,PartialD[LorentzIndex[nu, di]], p2, name, 
                          LorentzIndex[mu,de], rest], quf2]
            ) /; !OrderedQ[{mu, nu}]
                                                  } /. mydot->(*DOT*)(fcdot/.Options[epskill])
                     ) /; !FreeQ2[prod, {Eps,LeftPartialD,RightPartialD}
                                 ] && $EpsRules===True;
$EpsRules = True;

ExpandPartialD1[x_Plus, dot_, opts___Rule] := Map[ExpandPartialD1[#,dot,opts]&,x];

(*Hack to allow other multiplications. F.Orellana. 24/2-2003*)
Options[quanf]={fcdot->DOT};
Options[epskill]={fcdot->DOT};

ExpandPartialD1[x_, dot_, opts___Rule] := If[FreeQ2[x, {PartialD, LeftPartialD, 
                                   RightPartialD, LeftRightPartialD, 
                                   FieldStrength,
                                   QuantumField}
                              ], x,
                        SetOptions[quanf, fcdot->dot];
                        SetOptions[epskill, fcdot->dot];
                        epskill[Expand[
                                   Expand[FixedPoint[qfe[dot,#]&,FCI[x],3],
                                 SUNIndex] // sunsi,Eps]
                               ] /. epskill -> Identity //.
                       (*Allow for other products through setting
                         of PartialDRelations. This will of course
                         manipulate these products only by applying
                         PartialDRelations. Still...
                         F.Orellana, 22/2-2003.*)
                       (PartialDRelations/.{opts}/.Options[ExpandPartialD])
                       ] /; Head[x] =!= Plus;

fcovcheck[y_] := If[CheckContext["CovariantD"] || 
                    CheckContext["FieldStrength"],
                   y /. FieldStrength[ab__] :>
                        FieldStrength[ab, Explicit -> True]/.
                   (*Small change (condition) 26/2-2003. F.Orellana.
                     Ignore CovariantD[x, LorentzIndex[mu]] - it will be
                     caught by PartialDRelations*)
                   CovariantD[ab__] :> CovariantD[ab, Explicit -> True] /;
                   Length[{ab}] =!= 2 || Or@@((Head[#]===List)&/@{ab}),
                   y
                  ];

opesumplus2[y_,b__] := If[Head[y]===Plus,
                          Map[OPESum[#,b]&, y], OPESum[y,b]
                         ];
opesumplus[a_,b__] := opesumplus2[Expand[a],b];

qfe[dot_, x_] := MemSet[qfe[dot,x],
           DotSimplify[
           (*The replacement below commented out by F.Orellana, 12/3/2005.
             It breaks the procedure of applying the expansion to all
             products in $Multiplications (see above). What is it good for?*)
            DotSimplify[ExplicitPartialD[fcovcheck[x(*/.Times->dot*)]] /. 
 { PartialD[Momentum[OPEDelta]]^ (mm_ (*/; Head[mm]=!=Integer*)):> 
    PartialD[Momentum[OPEDelta]^mm],
   LeftPartialD[Momentum[OPEDelta]]^ (mm_ (*/; Head[mm]=!=Integer*)):> 
    LeftPartialD[Momentum[OPEDelta]^mm],
   RightPartialD[Momentum[OPEDelta]]^ (mm_ (*/; Head[mm]=!=Integer*)):> 
    RightPartialD[Momentum[OPEDelta]^mm]
  }                    ] /. dot -> qf1 /. qf1 -> qf2 /. qf2 -> qf1 /. 
                       qf1 -> qf3 /. 
                       qf3 -> qf5 /. qf5 -> dot /. QuantumField ->
                       quanf /. quanf -> QuantumField /. OPESum -> 
                       opesumplus
                      ]
                 ];
(* linearity *)
qf1[1,b___] := qf1[b];
qf1[a__, OPESum[b_, c__], d___] := OPESum[qf1[a, b, d], c];

qf1[a___, b_Plus, c___] := Expand[Map[qf1[a, #, c]&, b]];

qf2[b___, n_ ,c___] := ( n qf2[b, c] ) /; 
    FreeQ2[n, {Pattern, Blank, qf1, qf2}] && NonCommFreeQ[n] === True;

qf2[b___, n_ f1_, c___] := 
If[FreeQ2[n, {Pattern, Blank, qf1, qf2}] && NonCommFreeQ[n] === True,
   n qf2[b, f1, c] ,
   qf2[b, n, f1, c]
  ];

qf2[a___, RightPartialD[x_]^px_. , LeftPartialD[y_]^py_. , b___] :=
   qf2[a, LeftPartialD[y]^py, RightPartialD[x]^px, b];

(* move all DiracGamma, etc. to the left *)
qf2[a___, b_, n_, c___] := qf2[a, n, b, c] /;
 (!NonCommFreeQ[n]) && 
 FreeQ2[n, {Pattern, PartialD, LeftPartialD, RightPartialD,
            QuantumField, Blank, qf1, qf2} ] &&
 ((Head[b] === PartialD) || (Head[b] === QuantumField) ||
  (Head[b] === LeftPartialD) || (Head[b] === RightPartialD));

qf3[f1_qf3] := f1;
qf3[a___, PartialD[Momentum[OPEDelta]^m_], 
          PartialD[Momentum[OPEDelta]^n_], b___
   ] := qf3[a, PartialD[Momentum[OPEDelta]^(m+n)], b];
qf3[a___, LeftPartialD[Momentum[OPEDelta]^m_], 
          LeftPartialD[Momentum[OPEDelta]^n_], b___
   ] := qf3[a, LeftPartialD[Momentum[OPEDelta]^(m+n)], b];
qf3[a___, RightPartialD[Momentum[OPEDelta]^m_], 
          RightPartialD[Momentum[OPEDelta]^n_], b___
   ] := qf3[a, RightPartialD[Momentum[OPEDelta]^(m+n)], b];

(* Keine Experimente!!!! (...) *)
qf3[a___, n_. f1_QuantumField f2_QuantumField, b___] := 
 qf3[a, n f1, f2, b];

(* Leibnitz rule *)
qf5[] = 1;

qf5[a___, RightPartialD[mu_], QuantumField[f1__]] := 
 qf5st[a, quanf[PartialD[mu], f1]] /. quanf -> QuantumField /.
  qf5st -> qf5 ;

qf5[a___, QuantumField[f1__], LeftPartialD[mu_], b___] := 
(qf5st[a, quanf[PartialD[mu], f1], b] /. quanf -> QuantumField /.
  qf5st -> qf5) /; FreeQ2[{a}, {PartialD, QuantumField, LeftPartialD,
                                RightPartialD}] ;

qf5[a___, QuantumField[f2__], 
          QuantumField[f1__], LeftPartialD[mu_], b___
   ] := 
 ((qf5st[a, quanf[f2],  quanf[PartialD[mu], f1], b] + 
   qf5st[a, quanf[f2], LeftPartialD[mu], quanf[f1], b]
  ) /. quanf -> QuantumField /. qf5st -> qf5
 ) /; ((Head[mu] === LorentzIndex) || (Head[mu] === Momentum));

qf5[a___, RightPartialD[mu_], QuantumField[f1__], 
                             QuantumField[f2__], b___] := 
 ((qf5st[a, quanf[PartialD[mu], f1], quanf[f2],  b] + 
   qf5st[a, quanf[f1], RightPartialD[mu], quanf[f2], b]
  ) /. quanf -> QuantumField /. qf5st -> qf5
 ) /; ((Head[mu] === LorentzIndex) || (Head[mu] === Momentum));

qf5st[a___, RightPartialD[Momentum[OPEDelta]^m_], 
            RightPartialD[Momentum[OPEDelta]^n_], b___
   ] := qf5st[a, RightPartialD[Momentum[OPEDelta]^(m+n)], b];

qf5st[a___, LeftPartialD[Momentum[OPEDelta]^m_], 
            LeftPartialD[Momentum[OPEDelta]^n_], b___
   ] := qf5st[a, LeftPartialD[Momentum[OPEDelta]^(m+n)], b];

qf5st[a___, PartialD[Momentum[OPEDelta]^m_], 
            PartialD[Momentum[OPEDelta]^n_], b___
   ] := qf5st[a, PartialD[Momentum[OPEDelta]^(m+n)], b];

(* start from the right*) (* who wants to do this in FORM or Maple ??? *)
qf5[c_/;Head[c] =!= LeftPartialD, b___, QuantumField[f1__],
    LeftPartialD[Momentum[OPEDelta]^m_], a___
   ] := ( (opk = OPEk[$OPEKCOUNT++];
          OPESum[Binomial[m, opk] *
                 qf5st[c, b, 
                          LeftPartialD[Momentum[OPEDelta]^(m-opk)], 
                          quanf[PartialD[Momentum[OPEDelta]^opk], f1],
                        a
                      ],
                 {opk, 0, m}
                ]
          )/. quanf -> QuantumField /. qf5st -> qf5
        )(* /; FreeQ[{a}, PartialD]*);

(* start from the left *) (* who wants to do this in FORM or Maple ??? *)

qf5[a___,RightPartialD[Momentum[OPEDelta]^m_], 
         QuantumField[f1__], b___,c_/;FreeQ[{PartialD,RightPartialD},Head[c]]
   ] := ( (opk = OPEk[$OPEKCOUNT++];
          OPESum[Binomial[m, opk] *
                 qf5st[a, quanf[PartialD[Momentum[OPEDelta]^opk], f1],
                          RightPartialD[Momentum[OPEDelta]^(m-opk)], b, c
                      ],
                 {opk, 0, m}
                ]
          )/. quanf -> QuantumField /. qf5st -> qf5
(* CHANGE 06/94 *)
        )(* /; FreeQ[{a}, PartialD]*);

quanf[quanf[a__], b___, c_ /; Head[c] =!= PartialD] :=
      ((*DOT*)(fcdot/.Options[quanf])[quanf[a], qf5[b,c]]);

quanf[f1___, PartialD[Momentum[OPEDelta]^m_],
             PartialD[Momentum[OPEDelta]],  
      f2___
     ] := quanf[f1, PartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, LeftPartialD[Momentum[OPEDelta]^m_],
             LeftPartialD[Momentum[OPEDelta]],  
      f2___
     ] := quanf[f1, LeftPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, RightPartialD[Momentum[OPEDelta]^m_],
             RightPartialD[Momentum[OPEDelta]],  
      f2___
     ] := quanf[f1, RightPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, PartialD[Momentum[OPEDelta]],
             PartialD[Momentum[OPEDelta]^m_],  
      f2___
     ] := quanf[f1, PartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, RightPartialD[Momentum[OPEDelta]],
             RightPartialD[Momentum[OPEDelta]^m_],  
      f2___
     ] := quanf[f1, RightPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, LeftPartialD[Momentum[OPEDelta]],
             LeftPartialD[Momentum[OPEDelta]^m_],  
      f2___
     ] := quanf[f1, LeftPartialD[Momentum[OPEDelta]^(m+1)], f2];

quanf[f1___, PartialD[Momentum[OPEDelta]^m_],
             PartialD[Momentum[OPEDelta]^n_],  
      f2___
     ] := quanf[f1, PartialD[Momentum[OPEDelta]^(m+n)], f2];

quanf[f1___, LeftPartialD[Momentum[OPEDelta]^m_],
             LeftPartialD[Momentum[OPEDelta]^n_],  
      f2___
     ] := quanf[f1, LeftPartialD[Momentum[OPEDelta]^(m+n)], f2];

quanf[f1___, RightPartialD[Momentum[OPEDelta]^m_],
             RightPartialD[Momentum[OPEDelta]^n_],  
      f2___
     ] := quanf[f1, RightPartialD[Momentum[OPEDelta]^(m+n)], f2];

(* new 03/98 commutativity in partial derivatives *)
quanf[par1_PartialD, parr__PartialD, 
      fname_/;Head[fname]=!=PartialD, rest___] :=
      (((quanf[##, fname, rest])&)@@Sort[{par1,parr}]) /; 
      !OrderedQ[{par1,parr}]

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExpandPartialD | \n "]];
Null
