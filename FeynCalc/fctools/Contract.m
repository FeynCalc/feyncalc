(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Contract *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 23 March '98 at 15:07 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: contraction routines for Lorentz algebra *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Contract`",{"HighEnergyPhysics`FeynCalc`"}];

Contract::"usage"=
"Contract[expr] contracts pairs of Lorentz indices of metric tensors,
four-vectors and (depending on the option EpsContract) of
Levi-Civita tensors in expr. For the contraction of Dirac matrices
with each other use DiracSimplify. \n \n
Contract[exp1, exp2] contracts (exp1*exp2), where exp1 and exp2 may be
larger products of sums of  metric tensors and 4-vectors.
Contract[exp1, exp2] should be used for polarization sums, where
exp2 should be the product (or expanded sum) of the polarization
sums for the vector bosons. \n \n
Contract is also an option for DoPolarizationSums. When set to True,
Contract is applied to the amplitude immediately after replacing
polarization vectors by the corresponding sum. This usually improves
performance.";


Contract2::"usage"=
"Contract2[expr] (still experimental).";

Contract3::"usage"=
"Contract3[expr] (still experimental).";

Rename::"usage" =
"Rename is an option for Contract. If set to True,
dummy indices in Eps are renamed, using $MU[i].";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

   Cases2                      = MakeContext["Cases2"];
   Collect2                    = MakeContext["Collect2"];
   Collect3                    = MakeContext["Collect3"];
   Collecting                  = MakeContext["CoreOptions","Collecting"];
   Contract1                   = MakeContext["Contract1"];
   DataType                    = MakeContext["DataType"];
   DiracGamma  := DiracGamma   = MakeContext["CoreObjects","DiracGamma"];
   DotSimplify  := DotSimplify = MakeContext["DotSimplify"];
   Eps         := Eps          = MakeContext["CoreObjects","Eps"];
   EpsContract                 = MakeContext["CoreOptions","EpsContract"];
   EpsEvaluate := EpsEvaluate  = MakeContext["EpsEvaluate"];
   Expanding                   = MakeContext["CoreOptions","Expanding"];
   Expand2                     = MakeContext["Expand2"];
   ExpandScalarProduct         = MakeContext["ExpandScalarProduct"];
   fcint                       = MakeContext["FeynCalcInternal"];
   Factor2                     = MakeContext["Factor2"];
   Factoring                   = MakeContext["CoreOptions","Factoring"];
   FreeIndex                   = MakeContext["CoreObjects","FreeIndex"];
   FreeQ2                      = MakeContext["FreeQ2"];
   LorentzIndex                = MakeContext["CoreObjects","LorentzIndex"];
   MemSet                      = MakeContext["MemSet"];
   Momentum                    = MakeContext["CoreObjects","Momentum"];
   MomentumCombine             = MakeContext["MomentumCombine"];
   Pair                        = MakeContext["CoreObjects","Pair"];
   sCO                         = MakeContext["PairContract"];
   sCOS                         = MakeContext["PairContract3"];
   ScalarProduct               = MakeContext["ScalarProduct"];
   Schouten                    = MakeContext["Schouten"];
   Upper  := Upper = MakeContext["CoreObjects","Upper"];
   Lower  := Lower = MakeContext["CoreObjects","Lower"];
   Twist2GluonOperator         := Twist2GluonOperator =
                                 MakeContext["Twist2GluonOperator"];

   fci[x_] := If[(fcint /. Options[Contract]) === True, fcint[x], x ];

 (* #################################################################### *)

(*
    expair[a_, b_] := If[!FreeQ[{a,b}, LorentzIndex], Pair[a,b],
                         Expand[ExpandScalarProduct[a,b]]];
*)

   Contract3[x_Plus] := Map[Contract3, x];

   Contract3[x_ /; (Head[x] =!= Times) && Head[x] =!= Plus] := Contract[x, Contract3->False];
   Contract3[x_Times] :=
    If[!FreeQ[x, DiracGamma | Eps],
       Contract[x, Contract3 -> False],
       If[FreeQ[fci[x], LorentzIndex], fci[x],
       Block[{nx = x, nonli, lipa, nec = 0, ic,epli},
         nx = Contract[x , Expanding -> False, Contract3->False];
 (*
         nx = Contract[ExpandScalarProduct[x] , Expanding -> False, Contract3->False];
 *)
           If[Head[nx] =!= Times, nec = Contract[nx, Contract3->False],
              nonli = Select[nx, FreeQ[#, LorentzIndex]&];
              lipa  = Select[nx,!FreeQ[#, LorentzIndex]&];
(*
             If[Head[lipa] =!= Times, epli = 1,
                 epli  = Select[lipa, !FreeQ2[#,{Eps, DiracGamma}]&];
                 lipa = lipa / epli;
                ];
*)
              If[Head[lipa] =!= Times,
                 If[Head[lipa] === Plus,
                    nec = Contract3[lipa (*epli*)],
                    nec = Contract[lipa (*epli*), Contract3->False]
                   ],
                 If[Length[lipa] < 2, nec = Contract[lipa (*epli*), Contract3->False],
                    nec = lipa[[1]] (*epli*);
                    For[ic = 2, ic <= Length[lipa], ic++,
                        FCPrint[2,"ic = ", ic, " out of ",Length[lipa]];
                        If[LeafCount[nec] < LeafCount[lipa[[ic]]] ||
                           If[CheckContext["Twist2GluonOperator"],
                              !FreeQ[lipa[[ic]],
                    MakeContext["Twist2GluonOperator"]],
                              False
                             ]
                           ,
                           nec = Contract[lipa[[ic]], nec, Contract3->False],
                           nec = Contract[nec, lipa[[ic]], Contract3->False]
                          ];
                        FCPrint[2,"expand scalar products"];
      nec = ExpandScalarProduct[nec];
(*
                        nec = nec /. Pair -> expair;
*)
                        FCPrint[2,"expand scalar productsdone"];
(*
         nec = Collect2[nec, LorentzIndex, Factoring -> True];
*)
         If[!FreeQ[nec, LorentzIndex],
                        FCPrint[2,"expanding LorentzIndex now"];
tim = TimeUsed[];
         nec = Expand[nec, LorentzIndex];
                        FCPrint[2,"expanding LorentzIndex DONE ",
                               TimeUsed[] - tim];
            ];
                       ];
   If[Global`DIALOG === True, Dialog[nec]];
                   ];
                ];
                  nec = nec nonli;
             ];
               nec]]];

 (* #################################################################### *)

   Options[Contract2] = {Collecting -> False};

 (* bb is assumed to be collected w.r.t. to LorentzIndex !!! *)
   Contract2[a_, bb_, ops___Rule]:= Block[
           {sel, ct, rc, lco, lct, lastct, nop, b = bb, col, conT},
     col = Collecting /. {ops} /. Options[Contract2];
     If[Head[a] =!= Times, rc = Contract[a, b],
        lco[x_,y_] := If[Length[x]>Length[y], True, False];
        sel = Select[a, FreeQ[#, LorentzIndex]&];
        ct  = a/sel;
        nop = Select[ct, FreeQ[#, Plus]&];
        ct = ct/nop;
        If[Head[ct] =!= Times, rc = sel Contract[ct nop, b],
           ct = Sort[List @@ ct, lco];
           If[ nop =!= 1,
               lastct = contract21[b, nop],
               lastct = b nop
             ];

           lct = Length[ct];
           If[lct === 1, rc = sel contractLColl[ct[[1]], lastct] ];
           If[lct > 1,
              rc = sel Contract[Times @@ Take[ct, lct-1],
                                ct[[lct]], lastct ]
             ];
          ];
       ];
   FCPrint[2,"lct = ",lct];
   If[!FreeQ[rc, LorentzIndex],
      rc = Contract[rc, Expanding -> False];
     ];
   If[!FreeQ[rc, LorentzIndex],
      FCPrint[1,"contracting agagin at the end of Contract2 "];
      rc = Contract[rc]
     ];
         rc];

   Contract2[a_] := Block[{sel, ct, rc, lco, lct, lastct, nop},
     If[Head[a] =!= Times, rc = Contract[a],
        lco[x_,y_] := If[Length[x]>Length[y], True, False];
        sel = Select[a, FreeQ[#, LorentzIndex]&];
        ct  = a/sel;
        nop = Select[ct, FreeQ[#, Plus]&];
        ct = ct/nop;
        If[Head[ct] =!= Times, rc = sel Contract[ct nop],
           ct = Sort[List @@ ct, lco];
           If[ nop =!= 1,
               lastct = contract21[Last[ct], nop],
               lastct = Last[ct] nop;
             ];
           lct = Length[ct];
           If[lct === 2, rc = sel contractLColl[ct[[1]], lastct] ];
           If[lct > 2,
              rc = sel Contract[Times @@ Take[ct, lct-2],
                                ct[[lct-1]], lastct ]
             ];
          ];
       ];
   FCPrint[2,"lct = ",lct];

   If[!FreeQ[rc, LorentzIndex],
      FCPrint[1,"contracting agagin at the end of Contract2 "];
      rc = Contract[rc]
     ];
         rc];

 (* ******************************************************************** *)


 (* Added 3/11-2002 to contract also denominators. F.Orellana.
    Unfortunately it slows down things, so we might want to add an option
    to disble it...*)
(* This seems artificial.
   It can never occur by normal Feynman rule application.
   If anybody needs it they should copy this functions and call it differently.
   Commented out September 16th 2003 by Rolf Mertig, in order to not slow
   down things. Contract is a very vital function ...

 Contract[x__, opts___Rule] := (Contract[x /. Times[a___, b : Pair[_, __]^-1, c___] :>
    inv[(1/Times @@ Select[{a, b, c}, MatchQ[#, _^-1] &])](Times @@
          Select[{a, b, c}, ! MatchQ[#, _^-1] &]), opts] /. inv -> ((1/#)&))/;
   !FreeQ[{x}, _Pair^-1];
*)

 Contract[a_, b_ /;Head[b] =!= Rule, c_ /; Head[c] =!= Rule, ops___Rule] :=
    Block[{lc, new = 0, i},            FCPrint[2,"longcontract1"];
          lc = Contract[b, c, ops];    FCPrint[2,"longcontract1done"];
          new = Contract[lc, a, ops];
      new];

 Contract[x_, y_ /; Head[y]=!=Rule, c___?OptionQ] :=
          (Contract[fci[x], c] y) /; FreeQ2[fci[y], {LorentzIndex,Eps}];

 Contract[x_, y_Times, OptionsPattern[]] := Block[{a=fci[x], b=fci[y], bb},
   If[MatchQ[b, Apply[HoldPattern, {Times__Pair}]], contract21[ a, b ],

     If[MatchQ[b, HoldPattern[Times__Pair]], contract21[ a, b ],
(*
        bb = Collect2[b, LorentzIndex, Factoring -> False];
*)
        bb = Collect3[b, Pair, Factoring-> False];
        If[Head[bb] === Plus,
           contractLColl[a, bb],
           contract21[a, bb]
          ]
       ]]                        ];

(* Is this really needed ? RM Sept 16th 2003
   Contract[x_ /; FreeQ2[x, {DiracGamma, Eps}],
            y_ /; !FreeQ2[y, {DiracGamma, Eps}]] := Contract[y,x];
*)

   Contract[a_, b_ /; ((Head[b]=!=Times) && (Head[b] =!= Plus) &&
                       (Head[b] =!= Rule)), c___?OptionQ
           ] := Contract[ a b, c ];

   Contract[a_, b_Plus, ops___Rule] :=
     If[(Collecting /. {ops} /. Options[Contract]) === True,
        contractLColl[fci[a],
          If[FreeQ[List@@b, Plus], fci[b],
             Collect2[fci[b], LorentzIndex]]
                     ],
        contractLColl[fci[a], fci[b]]
       ];

 (* contractLColldef *)
   contractLColl[a_, b_ /; Head[b] =!= Plus] :=
    If[Head[b] === Pair,  contract21[a, b],
       If[Head[a] === Plus, contractLColl[b, a], Contract[a b]]
      ];

   contractLColl[lon_, shor_Plus] := Block[{neew = {}, long = lon,
                                            short = shor, tet},
   FCPrint[1,"Long contraction ", Length[long], " * ", Length[short], " \n ",UseWriteString->True];
   For[ij = 1, ij <= Length[short], ij++,
       FCPrint[3,"stdout"," | ", ij, "  XXX | ",UseWriteString->True];      ;
       FCPrint[1, "before contract21 "];
          tet = contract21[long, short[[ij]] ];
   FCPrint[1, "after contract21 "];

      If[!FreeQ[tet, LorentzIndex],
         tet = tet /. Pair->pairsave /. pair2 -> Pair];
      If[!FreeQ[tet, LorentzIndex],
        FCPrint[1,"expanding in contractLColl ",UseWriteString->True];
         tet = Expand[Expand[tet] /. Pair->pairsave /. pair2 -> Pair];
(*
       tet = Expand2[tet, LorentzIndex] /. Pair->pairsave /. pair2 -> Pair;
*)
        ];
         If[Head[tet] === Plus,
            neew  = Join[neew, Apply[List, tet]],
            AppendTo[neew, tet]
           ];
      ];
   FCPrint[2,"applying plus to neew "];
                        neew = Apply[Plus, neew];
   FCPrint[2,"exiting contractLColl"];
   neew];

(* local easy contraction rules *) (* paird *)
  fdi[]=4;
  fdi[_Symbol] := 4;
  fdi[xx_Symbol, xx_Symbol] := xx;
  fdi[4, _Symbol] := 4;
  fdi[_Symbol, 4] := 4;

  SetAttributes[{pairsave, pair2}, Orderless];
  pairsave[a_, b_] := pairsave[a, b] =
   If[FreeQ[{a,b},LorentzIndex], ExpandScalarProduct[a,b],
      pair2[a, b]
     ];
  pair2[LorentzIndex[a_, di1___], LorentzIndex[a_, di2___]] := fdi[di1, di2];
  pair2/: pair2[LorentzIndex[a_, dim1___], LorentzIndex[b_, dim2___]]^2 :=
          fdi[dim1, dim2];

  pair2/: pair2[LorentzIndex[a_,de1___], Momentum[b_, de2___]]^2 :=
          Pair[Momentum[b, fdi[de1,de2]], Momentum[b,fdi[de1,de2]]];
  pair2/: pair2[LorentzIndex[al_,di___], z_] pair2[LorentzIndex[al_,di2___],
                                                   w_] := pair2[z, w];
(* ???????? BLOEDSINN; PairContract does it
(*NEW*)
  pair2/: pair2[LorentzIndex[al_,di___], z_] Twist2GluonOperator[ww__] :=
           (Twist2GluonOperator[ww] /. LorentzIndex[al,di] -> z) /;
             !FreeQ[{ww}, LorentzIndex[al,di]];
*)

(* contract21 can still have products in the first argument *)
(* by construction the second argument will always be either a
     product or just Pair[  ,  ]
*)

  contra3a[xx_, {pr_, prl__}] :=
        contra3a[contra3a[xx, {pr}], {prl}];

  contra3b[xx_, {alien_ /; Head[alien] =!= pair2}]:=Expand2[xx alien, Pair];

  contra3c[xx_, {Pair[LorentzIndex[mu_,di___], alpha_]} ] :=Block[{nxx},
      If[FreeQ[xx, LorentzIndex[mu,___]],
         nxx = Expand2[xx Pair[LorentzIndex[mu, di], alpha], Pair],
         (* else *)
  FCPrint[1,"contra3c1111check"];
         nxx = xx;
(*
         If[Head[xx]===Plus, nxx = Apply[List, xx], nxx = {xx}];
*)
  FCPrint[1,"contra3c2222check"];
  FCPrint[2,"contra3c : Length of xx now ", Length[nxx]];
         nxx = nxx /. LorentzIndex[mu, ___] -> alpha;
  FCPrint[2,"contra3c3333check"];
(*
         nxx = Apply[Plus, nxx];
  FCPrint[2,"contra3c4check"];
*)
        ];
           nxx];

  contract21[z_, yy_ /; ((Head[yy] =!= Pair) && Head[yy] =!= Times)] :=
   Contract[z yy];

  contract21[xx_Plus, yy_] := contract22[xx, yy /. Pair -> pairsave] /.
        contra4 -> contra3a /.  contra3a -> contra3b /.
        Pair -> pairsave /.  contra3b -> contra3c /. pair2 -> Pair;

  list2[x_] := If[Head[x] === Times, List @@ x, {x}];
  contract22[xx_, 0] := 0;
  contract22[xx_, yy_Pair] := contra3a[xx, {yy}] /.  contra3a -> contra3b /.
        Pair -> pairsave /.  contra3b -> contra3c /. pair2 -> Pair;
  contract22[xx_, yy_Times]:= ( (yy/#) contra4[xx, list2[#]] )&[
                               Select[yy, !FreeQ[#, LorentzIndex]&]];


(*
  contract21[xx_Plus, yy_] :=(iCcount=1; Apply[Plus,
                     Table[ (xx[[ii]] yy) /. Pair -> pairsave /.
                                                 pair2 -> Pair
                                , {ii, 1, Length[xx]}] ]);
*)

  contract21[xx_ /;(Head[xx] =!= Plus) && (Head[xx] =!= Times), yy_] :=
    Contract[xx yy,Expanding -> False];

  contract21[xxx_Times, yyy_] := ( (xxx/#) contit[#, yyy] )&[
                               Select[xxx, !FreeQ[#, LorentzIndex]&] ];
  contit[xx_ , yy_] :=
    If[FreeQ[xx, LorentzIndex],
       xx Contract[yy],
       If[Head[xx] =!= Times, Contract[xx yy],
          If[Length[xx] =!= 2, Contract[xx yy],
             If[(Head[xx[[1]]] === Plus) && (Head[xx[[2]]] === Plus),
  iCcount = 1;
  FCPrint[2,"contracting a product of a ",Length[xx[[1]]], " term sum  by a",
         Length[xx[[2]]], " term sum"];
(* that's the common situation !! *)
                Apply[ Plus, Flatten[Table[ (xx[[1, ii]] xx[[2, jj]] yy
                                                 ) /. Pair -> pairsave /.
                                                      pair2  -> Pair
                                                , {ii,1,Length[xx[[1]]]},
                                                   {jj,1,Length[xx[[2]]]}
                     ]              ]     ],
                Contract[xx yy]
               ] ] ]
      ];


(* #################################################################### *)

(*
(* coneinsdef    *)
     coneins[ x_ ]  := MemSet[coneins[x], x/.Pair->sCO/.sCO->Pair ];
*)
(* contractlidef *)
    contractli[x_] := MemSet[contractli[x],x] /; FreeQ[x//Hold,LorentzIndex];
    contractli[x_] := Contract[ x, Expanding->True, Factoring->False,
                       EpsContract->False ];
    conall[ x_ ] := Contract[ x,                               (*conalldef*)
                    Expanding->True, EpsContract->True, Factoring->False ];
                                       (*Contractdef*)
   Options[Contract] = { Collecting      -> True,
                         Contract3       -> True,
                         EpsContract     -> True,
                         Expanding       -> True,
                         Factoring       -> False,
                         fcint           -> True,
                         MomentumCombine -> True,
                         Rename          -> False,
                         Schouten        -> 0
                       };

 dosi[x_, z___] := If[FreeQ2[x, {LorentzIndex,Eps}],x,
                  If[( Union[DataType[#, FreeIndex]& /@
                             Map[First, Cases2[x,LorentzIndex]]
                            ]
                     ) === {True}, x,
                  If[CheckContext["CoreObjects"],
                     If[!FreeQ2[x, {Upper,Lower}], Contract1[x],
                        contracT[
                                If[!FreeQ[x, DOT],
                                DotSimplify[x, Expanding -> False], x
                                ],z],
                        contracT[
                                If[!FreeQ[x, DOT],
                                DotSimplify[x, Expanding -> False], x
                                ],z]
                    ]  ]
                 ]];

   Contract[Equal[a_, b_], ops___Rule] :=
    Contract[a,ops] == Contract[b, ops];

   Contract[y_,z___Rule] := dosi[fci[y], z];

(*epscondef*)
  epscon/: epscon[a1_,a2_,a3_,a4_,OptionsPattern[Eps]]^n_Integer?Positive :=  (   (
           ( - ($LeviCivitaSign)^2 Det[{{sCO[a1,a1],sCO[a1,a2],sCO[a1,a3],sCO[a1,a4]},
                   {sCO[a2,a1],sCO[a2,a2],sCO[a2,a3],sCO[a2,a4]},
                    {sCO[a3,a1],sCO[a3,a2],sCO[a3,a3],sCO[a3,a4]},
                    {sCO[a4,a1],sCO[a4,a2],sCO[a4,a3],sCO[a4,a4]}}
                  ]//Expand
           )/.sCO->Pair ) epscon[a1,a2,a3,a4,OptionsPattern[Eps]]^(n-2) );
  epscon/: epscon[a1_,a2_,a3_,a4_,OptionsPattern[Eps]] epscon[b1_,b2_,b3_,b4_,OptionsPattern[Eps]] :=
           ( - ($LeviCivitaSign)^2 Det[{{sCO[a1,b1],sCO[a1,b2],sCO[a1,b3],sCO[a1,b4]},
                   {sCO[a2,b1],sCO[a2,b2],sCO[a2,b3],sCO[a2,b4]},
                   {sCO[a3,b1],sCO[a3,b2],sCO[a3,b3],sCO[a3,b4]},
                   {sCO[a4,b1],sCO[a4,b2],sCO[a4,b3],sCO[a4,b4]}}
                 ]//Expand
           )/.sCO->Pair;                              (*epsevdef*)

     sceins[0,_]:=0;                               (*sceinsdef*)
     sceins[a_LorentzIndex b_, c_] := b sceins[a, c];
     sceins[a_Momentum b_, c_] := b sceins[a, c];


dim[]=4;
dim[d_]:=d;
(* do this immediately, Oct. 2003 *)
simplerules = {Pair[LorentzIndex[a_, di___], b_]*
        Pair[LorentzIndex[a_, di___], c_] :> Pair[b, c],
      Pair[LorentzIndex[x_, di___], LorentzIndex[x_, di___]] :>
       dim[di], Pair[LorentzIndex[x_, di___], y_]^2 :>
       Pair[y, y]};
(*
   contracT[x_,opt___Rule] := x /; FreeQ2[ x,{LorentzIndex,Eps,Momentum} ];
*)
   contracT[x_,opt___Rule] := Module[{ contractres=x,contractepsopt,
           contractexpandopt, rename, es,
           lip,contractopt = Join[{opt},Options[Contract]]//Flatten,
             contract3, schout },
     contractexpandopt   = Expanding/.contractopt;
     contractepsopt      = EpsContract/.contractopt;
     contractfactoring   = Factoring/.contractopt;
     contract3           = Contract3/.contractopt;

     If[(MomentumCombine/.contractopt) === True,
        contractres =  MomentumCombine[contractres,LeafCount -> 1000]
       ];

(* NEW: September 16th 2003: adding Contract3 directly here ... *)
     If[contract3 && contractexpandopt,
     If[MemberQ[{Plus, Times}, Head[contractres]] (*&& !FreeQ[contractres, Plus]*),
        contractres = Contract3[contractres];
FCPrint[1,"Contract3 done"];
       ]];
     contractres = contractres /. Pair -> sCOS /. sCOS -> sCO/.
                   sCO -> sceins /. sceins -> Pair;
     rename      = Rename /. contractopt;
FCPrint[1,"extra done"];
(* optimization *)
     If[Head[contractres === Plus] && Length[contractres > 47],
       If[!FreeQ[contractres, Eps],
          es = {Pair[LorentzIndex[a_,D], b_] *
                Eps[c___,LorentzIndex[a_],d___] :> Eps[c,b,d],
                Pair[LorentzIndex[a_,D], b_] *
                Eps[c___,LorentzIndex[a_,D],d___] :> Eps[c,b,d]
               };
          contractres = contractres //. es
         ]
       ];
FCPrint[1,"check1"];
     If[ contractexpandopt === True && contract3 === False && !FreeQ[contractres, LorentzIndex],
(* NEW October 2003, this speeds things up in general*)
         contractres = Expand[Expand[contractres, LorentzIndex] //. simplerules];
         contractres = contractres /.
                       {((yy_Plus)  /;!FreeQ[yy, LorentzIndex])^2 :>
                        ((Contract @@ {yy/.sCO->Pair, yy/.sCO->Pair}
                        ) /. Pair -> sCO /. sCO -> Pair)
                       };
       ];
     schout = Schouten /. contractopt;
          If[ contractexpandopt === True &&
              !FreeQ[contractres, LorentzIndex]
              ,
              contractres = Expand[contractres,LorentzIndex]
            ];
          If[ (!FreeQ[contractres, Eps]) && rename === True,
              contractres = doubleindex[
                               Expand[ contractres//EpsEvaluate, Eps
                                     ] ];
            ];
          If[ contractepsopt === True,
              If[ !FreeQ[contractres, Eps],
                  contractres = EpsEvaluate[contractres/.
                                            Eps->epscon/.epscon->Eps];
                ](*,
              contractres = contractres//EpsEvaluate*)
            ];
          If[ contractexpandopt=== True &&  !FreeQ[contractres, LorentzIndex],
              contractres = Expand[ contractres, LorentzIndex ] ];
          If[ (contractexpandopt===True) && (!FreeQ[contractres, Eps]) &&
              (contractepsopt===True),
              contractres = Expand[ contractres, Eps ]
            ];
(*
          If !FreeQ[ contractres,Eps ],
*)
          If[ (contractepsopt===True) && (!FreeQ[ contractres,Eps ]),
              contractres = contractres//EpsEvaluate//EpsEvaluate
            ];
(*Global`CCC3=contractres;*)
          contractres = contractres /. Pair->sCOS /.
                                      sCOS -> sCO /.sCO->Pair;
(*
          contractres = contractres /. Pair->sCOS /.
                        sCOS -> ExpandScalarProduct/.
                         Pair -> sCO /.sCO->Pair;
*)
(*
         If[!FreeQ[contractres, ScalarProduct],
            contractres = fcint[contractres];
           ];
*)
         If[schout =!= 0,
            If[(contractepsopt===True) && (!FreeQ[contractres, Eps]) &&
                                          (!FreeQ[contractres, Pair]),
               contractres = Schouten[contractres, schout];
               If[rename === True, contractres = doubleindex[contractres]]
              ]
           ];
(*
          If[ contractexpandopt=== True,
              contractres = Expand[contractres, Pair]
            ];
*)
          If[ contractfactoring=== True,
              contractres = Factor2[ contractres ]
            ];
          If[ contractfactoring=== Factor,
              contractres = Factor[ contractres ]
            ];
          If[ contractfactoring=== Factor1,
              contractres = Factor1[ contractres ]
            ];
    contractres                 ](* EndcontracT *);

    ident3[a_,_]:=a;

(* #################################################################### *)

  (* decide whether the (first) appearance of inds in expr is ordered *)
  (*ordqdef*)
  ordq[expr_,inds_List]:= Block[{pos, min},
  pos = Position[expr, #]& /@ inds;
  pos = pos /. {} -> Sequence[];
  If[Length[pos]>0, pos = Map[First,pos]];
  min = Min[Length/@pos];
  pos = Map[Take[#,min]&, pos];
(*Global`TEST={expr,inds,pos, OrderedQ[pos]};*)
                              OrderedQ[pos] ];


  eps2rules = {Eps[LorentzIndex[a_,dia___], b_Momentum,
                   c_Momentum, d_Momentum, opts:OptionsPattern[]]^2 :>
               Eps[LorentzIndex[$MU[1], dia], b, c, d, opts]^2,
               Eps[LorentzIndex[a_,dia___], LorentzIndex[b_,dia___],
                   c_Momentum, d_Momentum, opts:OptionsPattern[]]^2 :>
               Eps[LorentzIndex[$MU[1], dia], LorentzIndex[$MU[2],dia],
                   c, d, opts]^2,
               Eps[LorentzIndex[a_,dia___], LorentzIndex[b_,dia___],
                   LorentzIndex[c_,dia___], d_Momentum, opts:OptionsPattern[]]^2 :>
               Eps[LorentzIndex[$MU[1], dia], LorentzIndex[$MU[2],dia],
                   LorentzIndex[$MU[3], dia], d, opts]^2,
               Eps[LorentzIndex[a_,dia___], LorentzIndex[b_,dia___],
                   LorentzIndex[c_,dia___], LorentzIndex[d_,dia___], opts:OptionsPattern[]]^2 :>
               Eps[LorentzIndex[$MU[1], dia], LorentzIndex[$MU[2],dia],
                   LorentzIndex[$MU[3], dia], LorentzIndex[$MU[4], dia], opts
                  ]^2
              };

     (*  doubleindexdef *)
    (* For canonizing dummy indices between Eps and other functions *)

    doubleindex[0] = 0;
    doubleindex[x_] :=  Block[{xy = x, suli = {}, muUU},
     For[ijj = 1, ijj < 7, ijj ++,
         If[EvenQ[Length[Position[x, $MU[ijj]]]] && !FreeQ[x, $MU[ijj]],
            AppendTo[suli, RuleDelayed @@ {$MU[ijj], muUU[ijj]}]
           ];
        ];
  If[Length[suli] > 0,
     FCPrint[1,"suli == ",suli];
     xy = xy /. suli
    ];
                    xy = doubleindex0[x];
(*
                       If[!FreeQ[x, Eps] && !FreeQ[x, $MU],
                          doubleindex0[doubleindex0[x]],
                          doubleindex0[x]
                         ];
*)
                       If[xy === 0,
                FCPrint[1,"doubleindexTROUBLE???????????? "];
                           Print["entering with", x];
                         ];
(*
*)
                        xy];

    doubleindex0[x_] :=  Block[{double2, double3a},
                      If[FreeQ[x, Eps], x,
                          If[Head[x] === Plus,
                             Map[doubleindex, x],
    double2[y_] := double3a[y /. {Eps :> eepp} , 1
                           ] /. double3a -> double3;
    double3a[y_, i_] := double3a[y, i+1] /;
                  Length[Position[y, $MU[i]]] > 0;
                             double2[x] /.  eepp -> Eps /.
                             double3 -> ident3/.  eepp -> Eps
                            ]
                         ] /. eps2rules];
    double2[x_] := If[Length[Position[x, $MU]] > 0,
                      double3a[x/.Eps->eepp/.$MU->Unique[Global`lI], 1] /.
                       double3a-> double3,
                      double3a[x/.Eps->eepp, 1] /.  double3a-> double3
                     ];

(*
    double2[x_] := If[EvenQ[Length[Position[x, $MU]]],
                      double3a[x/.Eps->eepp/.$MU->Unique[Global`lI], 1] /.
                       double3a-> double3,
                      double3a[x/.Eps->eepp, 1] /.  double3a-> double3
                     ];
*)

    double3a[x_, i_] := If[FreeQ[x, $MU[i+1]],
                           double3a[x, i+1],
                           double3a[x, i+2]
                          ] /; !FreeQ[x, $MU[i]];

    lorhipa[a_,___]  := LorentzIndex[a,  BlankNullSequence[]];

    double3[ m_. eepp[a1___, LorentzIndex[be_, di___], a2___], j_ ] :=
            (m/.be->$MU[j]) Eps[a1,LorentzIndex[$MU[j],di],a2]/;
           (!FreeQ[m, LorentzIndex[be, ___]]) &&
             FreeQ2[m, Select[{a1,a2}, Head[#]===LorentzIndex&] /.
                       LorentzIndex -> lorhipa];

(*XXX*)
    double3[ m_. eepp[a1___, LorentzIndex[mu1_, di1___], a2___,
                             LorentzIndex[mu2_, di2___],a3___], j_ ] :=
          (
            (m/.mu1->$MU[j]/.mu2->$MU[j+1]) *
             Eps[a1,LorentzIndex[$MU[j],di1],a2,
                    LorentzIndex[$MU[j+1],di2],a3]
          )/;(FreeQ2[{m,a1,a2,a3}, {$MU[j], $MU[j+1]}] &&
              (!FreeQ[m, LorentzIndex[mu1, ___]]) &&
              (!FreeQ[m, LorentzIndex[mu2, ___]]) &&
              (FreeQ2[m, Select[{a1,a2,a3}, Head[#]===LorentzIndex&]/.
                       LorentzIndex -> lorhipa]) &&
              ordq[m, {mu1,mu2}]
              );

    double3[ m_. eepp[a1___, LorentzIndex[mu1_, di1___], a2___,
                             LorentzIndex[mu2_, di2___],a3___], j_ ] :=
          (
            (m/.mu2->$MU[j]/.mu1->$MU[j+1]) *
             Eps[a1,LorentzIndex[$MU[j+1],di1],a2,
                    LorentzIndex[$MU[j],di2],a3]
          )/;(FreeQ2[{m,a1,a2,a3}, {$MU[j], $MU[j+1]}] &&
              (!FreeQ[m, LorentzIndex[mu1, ___]]) &&
              (!FreeQ[m, LorentzIndex[mu2, ___]]) &&
              (FreeQ2[m, Select[{a1,a2,a3}, Head[#]===LorentzIndex&]/.
                       LorentzIndex -> lorhipa]) &&
              ordq[m, {mu2,mu1}]
              );

    double3[ m_. eepp[a1___, LorentzIndex[mu1_, di1___], a2___,
                             LorentzIndex[mu2_, di2___], a3___,
                             LorentzIndex[mu3_, di3___], a4___ ], j_ ]:=
       Block[{dte,a,b,c},
       dte = (  (m/.mu1->$MU[j]/.mu2->$MU[j+1]/.mu3->$MU[j+2]) *
             Eps[a1,LorentzIndex[$MU[j],di1], a2,
                    LorentzIndex[$MU[j+1],di2],a3,
                    LorentzIndex[$MU[j+2],di3], a4]
             );
       a = $MU[j]; b = $MU[j+1]; c = $MU[j+2];
       Which[ordq[m, {mu1,mu2,mu3}],
             dte,
             ordq[m, {mu1,mu3,mu2}],
             dte /.  {b :>c, c:>b},
             ordq[m, {mu2,mu1,mu3}],
             dte /.  {a:>b,b:>a},
             ordq[m, {mu2,mu3,mu1}],
             dte /.  {b:>a, c:>b, a:>c},
             ordq[m, {mu3,mu1,mu2}],
             dte /.  {c:>a, a:>b, b:>c},
             ordq[m, {mu3,mu2,mu1}],
             dte /.  {c:>a, a:>c}
            ]
        ]/; FreeQ2[{m,a1,a2,a3,a4}, {$MU[j], $MU[j+1], $MU[j+2]}] &&
              (!FreeQ2[m, LorentzIndex[mu1,___]] &&
               !FreeQ2[m, LorentzIndex[mu2,___]] &&
               !FreeQ2[m, LorentzIndex[mu3,___]]
              ) &&
             FreeQ2[m, Select[{a1,a2,a3,a4}, Head[#]===LorentzIndex&]/.
                       LorentzIndex -> lorhipa];

 double3[ m_. eepp[LorentzIndex[mu1_,di1___],LorentzIndex[mu2_,di2___],
                   LorentzIndex[mu3_,di3___],LorentzIndex[mu4_,di4___]], _
         ]:= Block[{dte,a,b,c,d},
          dte = (m/.mu1->$MU[1]/.mu2->$MU[2]/.mu3->$MU[3]/.mu4->$MU[4]) *
           Eps[LorentzIndex[$MU[1],di1],  LorentzIndex[$MU[2],di2],
               LorentzIndex[$MU[3],di3], LorentzIndex[$MU[4],di4]];
          a = $MU[1]; b = $MU[2]; c = $MU[3]; d = $MU[4];
Which[
ordq[m, {mu1, mu2, mu3, mu4}], ReplaceAll[dte,
  {a -> a, b -> b, c -> c, d -> d}],
ordq[m, {mu1, mu2, mu4, mu3}],
 ReplaceAll[dte, {a -> a, b -> b, c -> d, d -> c}],
 ordq[m, {mu1, mu3, mu2, mu4}], ReplaceAll[dte,
  {a -> a, b -> c, c -> b, d -> d}], ordq[m, {mu1, mu3, mu4, mu2}],
 ReplaceAll[dte, {a -> a, b -> c, c -> d, d -> b}],
 ordq[m, {mu1, mu4, mu2, mu3}], ReplaceAll[dte,
  {a -> a, b -> d, c -> b, d -> c}], ordq[m, {mu1, mu4, mu3, mu2}],
 ReplaceAll[dte, {a -> a, b -> d, c -> c, d -> b}],
 ordq[m, {mu2, mu1, mu3, mu4}], ReplaceAll[dte,
  {a -> b, b -> a, c -> c, d -> d}], ordq[m, {mu2, mu1, mu4, mu3}],
 ReplaceAll[dte, {a -> b, b -> a, c -> d, d -> c}],
 ordq[m, {mu2, mu3, mu1, mu4}], ReplaceAll[dte,
  {a -> b, b -> c, c -> a, d -> d}], ordq[m, {mu2, mu3, mu4, mu1}],
 ReplaceAll[dte, {a -> b, b -> c, c -> d, d -> a}],
 ordq[m, {mu2, mu4, mu1, mu3}], ReplaceAll[dte,
  {a -> b, b -> d, c -> a, d -> c}], ordq[m, {mu2, mu4, mu3, mu1}],
 ReplaceAll[dte, {a -> b, b -> d, c -> c, d -> a}],
 ordq[m, {mu3, mu1, mu2, mu4}], ReplaceAll[dte,
  {a -> c, b -> a, c -> b, d -> d}], ordq[m, {mu3, mu1, mu4, mu2}],
 ReplaceAll[dte, {a -> c, b -> a, c -> d, d -> b}],
 ordq[m, {mu3, mu2, mu1, mu4}], ReplaceAll[dte,
  {a -> c, b -> b, c -> a, d -> d}], ordq[m, {mu3, mu2, mu4, mu1}],
 ReplaceAll[dte, {a -> c, b -> b, c -> d, d -> a}],
 ordq[m, {mu3, mu4, mu1, mu2}], ReplaceAll[dte,
  {a -> c, b -> d, c -> a, d -> b}], ordq[m, {mu3, mu4, mu2, mu1}],
 ReplaceAll[dte, {a -> c, b -> d, c -> b, d -> a}],
 ordq[m, {mu4, mu1, mu2, mu3}], ReplaceAll[dte,
  {a -> d, b -> a, c -> b, d -> c}], ordq[m, {mu4, mu1, mu3, mu2}],
 ReplaceAll[dte, {a -> d, b -> a, c -> c, d -> b}],
 ordq[m, {mu4, mu2, mu1, mu3}], ReplaceAll[dte,
  {a -> d, b -> b, c -> a, d -> c}], ordq[m, {mu4, mu2, mu3, mu1}],
 ReplaceAll[dte, {a -> d, b -> b, c -> c, d -> a}],
 ordq[m, {mu4, mu3, mu1, mu2}], ReplaceAll[dte,
  {a -> d, b -> c, c -> a, d -> b}], ordq[m, {mu4, mu3, mu2, mu1}],
 ReplaceAll[dte, {a -> d, b -> c, c -> b, d -> a}]
]
        ] /; (!FreeQ2[m, LorentzIndex[mu1,___]] &&
              !FreeQ2[m, LorentzIndex[mu2,___]] &&
              !FreeQ2[m, LorentzIndex[mu3,___]] &&
              !FreeQ2[m, LorentzIndex[mu4,___]]
             ) && FreeQ2[m, {$MU[1], $MU[2], $MU[3], $MU[4]}];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Contract | \n "]];
Null
