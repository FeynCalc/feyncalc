(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracTrace *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 21 February '99 at 0:06 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Dirac trace calculation *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`fctools`DiracTrace`",
             "HighEnergyPhysics`FeynCalc`"];

DiracTrace::"usage" =
"DiracTrace[expr] is the head of Dirac traces. \
Whether the trace is  evaluated depends on the option \
DiracTraceEvaluate. See also Tr. \
The argument expr may be a product of Dirac matrices or slashes \
separated by the Mathematica Dot \".\".";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ Contract, Collect2, DiracCanonical, DiracGamma,
DiracGammaCombine, DiracGammaExpand, DiracGammaT, DiracOrder,
DiracSigmaExplicit, DiracSimplify, DiracTrick, DiracTraceEvaluate];

dot := dot  = MakeContext["DOT"];

MakeContext[ DotSimplify, Eps, EpsEvaluate, Factor2, EpsContract,
Expanding, Expand2, Factoring, FeynCalcInternal, FeynCalcExternal,
FreeQ2, InsideDiracTrace, LeviCivitaSign, LorentzIndex, Mandelstam,
MemSet, Momentum, Pair, PairContract, PairCollect, PartitHead ];

sCO := sCO = MakeContext["PairContract"];

MakeContext[ ExpandScalarProduct, Schouten,
Spinor, (*SUNSimplify, SUNT,*) Tr, TraceOfOne, TrickMandelstam];

scev[a__] := scev[a] = ExpandScalarProduct[a];

Options[DiracTrace] = {EpsContract         -> False,
                       Factoring           -> False,
                       FeynCalcExternal   -> False,
                       Mandelstam          -> {},
                       PairCollect         -> True,
                       DiracTraceEvaluate  -> False,
                       Schouten            -> 0,
                       LeviCivitaSign      -> (-1),
                       (*Added 27/8-2002, F.Orellana*)
                       TraceOfOne          -> 4
                      };


dotLin[x_] := DotSimplify[x, Expanding -> False];
 (* gamma67backdef: reinsertion of gamma6 and gamm7 *)
   gamma67back[x_] := x/.DiracGamma[6]->( 1/2 + DiracGamma[5]/2 )/.
                         DiracGamma[7]->( 1/2 - DiracGamma[5]/2 );

DiracTrace[0,___]:=0;

DiracTrace[a_ /; (FreeQ[a, DiracGamma] && !FreeQ[a, DiracGammaT]),
             b___Rule] :=
    DiracTrace[(a//Transpose)//Reverse, b];
DiracTrace[a___, x_,y_, z___]:=DiracTrace[a,x.y,z]/;
       FreeQ2[y,{Rule,BlankNullSequence}]&&
       FreeQ2[x,{Rule,BlankNullSequence}];

                                               (*DiracTracedef*)
fcit[y_] := If[CheckContext["DiracSigma"],
               FeynCalcInternal[DiracSigmaExplicit[y]],
               FeynCalcInternal[y]
              ];

fcex[ops___Rule][z_] := If[(FeynCalcExternal /. {ops} /.
                            Options[DiracTrace]
                           ) === True,
                           FeynCalcExternal[z], z
                          ];
DiracTrace[x_,op___Rule] := fcex[op][
                         ( diractraceevsimple[
                    fcit[x] ,{op}] /.  diractraceevsimple -> diractraceev /.
                         diractraceev->diractraceev2
                         )          ]/;
 (DiracTraceEvaluate/.{op} /. (Join[{op},Options[DiracTrace]]//Flatten)
 ) === True;

 (*  special cases *)
(*XXXX*)
diractraceevsimple[xx_,{opt___}] :=
 ( ( TraceOfOne /. {opt} /.Options[Tr] /. Options[DiracTrace] ) * xx
 )/;  FreeQ[xx, DiracGamma];

diractraceevsimple[y_ x_Dot,{opt___}] :=
 y diractraceevsimple[x,{opt}] /; FreeQ[y, DiracGamma];


diractraceevsimple[x_Plus , {opt___}]:=Map[diractraceevsimple[#,{opt}]&, x];

(* in order to inhibit loops below *)
diractraceevsimpleplus[x_Plus,{opt___}] :=
 Map[diractraceevsimple[#,{opt}]&, x];

(*Fix by Rolf in response to Broniowski's observation that
  Tr[DiracSlash[p,p]] gives p^2 instead of 4p^2. F.Orellana.*)
diractraceevsimpleplus[x_/;Head[x]=!=Plus,{opt___}] := x *
 (TraceOfOne /. {opt} /.Options[Tr] /. Options[DiracTrace] );
(*diractraceevsimpleplus[x_/;Head[x]=!=Plus,{opt___}] := x;*)

diractraceevsimple[x_Dot, {opt___}]:=
(If[FreeQ[#,LorentzIndex],#, #/.Pair->sCO/.sCO->Pair]&[
     If[(*Length[x] > Length[Union[Variables /@ Apply[List,x]]],*)
   (*More restrictive condition. I'm not sure about this... But the old stuff commented out above
      is wrong on e.g. Tr[DiracGamma[Momentum[p]].DiracGamma[Momentum[p]].DiracGamma[Momentum[r]]]
      see, Kapusta's bug report http://www.feyncalc.org/forum/0079.html. F.Orellana, 10/2002*)
        Union[Length /@ Split[Sort[ Variables /@ Apply[List,x] ]]] === {2},
        If[$VeryVerbose >2, Print["using diractraceevsimpleplus on ", StandardForm[x]]];
        Factor[diractraceevsimpleplus[Expand[DiracTrick[x]], {opt}]],
        If[$VeryVerbose >2, Print["using spursav on ", StandardForm[x]]];
       (TraceOfOne /. {opt} /.Options[Tr] /. Options[DiracTrace] )*
        (spursav @@ x)
      ] ]
)  /; (MatchQ[Apply[doo, x], doo[
       DiracGamma[(LorentzIndex | Momentum)[_,_],_]..]] ||
         MatchQ[Apply[doo, x], doo[
         DiracGamma[(LorentzIndex | Momentum)[_]]..]] ||
       MatchQ[Apply[doo, x], doo[
       DiracGamma[(LorentzIndex | Momentum)[_,_],_]..,
       DiracGamma[5 | 6 | 7]]] ||
         MatchQ[Apply[doo, x], doo[
         DiracGamma[(LorentzIndex | Momentum)[_]]..,
         DiracGamma[5 | 6 | 7]]]
      );

dirli[LorentzIndex[xx_, ___],___] := xx;

diractraceev[DiracGamma[LorentzIndex[a1_,dii_],dii_],
             DiracGamma[LorentzIndex[a2_,dii_],dii_],
             DiracGamma[LorentzIndex[a3_,dii_],dii_],
             a4:DiracGamma[LorentzIndex[_,dii_],dii_]..,
             DiracGamma[LorentzIndex[a1_,dii_],dii_],
             DiracGamma[LorentzIndex[a2_,dii_],dii_],
             DiracGamma[LorentzIndex[a3_,dii_],dii_],
             a4:DiracGamma[LorentzIndex[_,dii_],dii_]..
            ]:=4 dcs[dii]@@Join[{a1,a2,a3}, {a4}/.DiracGamma->dirli,
                              {a1,a2,a3}, {a4}/.DiracGamma->dirli
                             ];

dcs[dim_][x___] := dcs[dim][x] = (dics[dim][x] /. dics->dc);
dc[_][]=1; dics[_][]=1;
dics[dI_][a___, n_, n_, b___] := dI * dics[dI][a, b];
dics[dI_][a___, n_, z_, n_, b___ ] := (2-dI) * dics[dI][a, z, b];
dics[dI_][a___, n_, v_, w_, n_, b___
        ] := (dI-4) * dics[dI][a, v,w, b] + 4 (dics[dI]@@({a, b}/. v -> w));
dics[dI_][a___, n_, v_, w_, z_, n_, b___
        ] := (4-dI) * dics[dI][a, v,w,z, b] - 2 dics[dI][a, z,w,v,b];
dics[dI_][a___, n_, mu_, nu_, ro_,si_, n_, b___
        ] := (dI-4) * dics[dI][a, mu,nu,ro,si, b] +
             2 dics[dI][a, ro,nu,mu,si,b] + 2 dics[dI][a, si,mu,nu,ro,b];
dics[dI_][a___, n_, mu_, nu_, ro_, si_, de_, n_, b___
        ] := (4-dI) * dics[dI][a, mu,nu,ro,si,de, b] -
                 2 dics[dI][a, mu,de,nu,ro,si, b] -
                 2 dics[dI][a, mu,si,ro,nu,de, b] +
                 2 dics[dI][a, nu,ro,si,de,mu, b];
dicsav[dd_][x___] := dicsav[dd][x] = dics[dd][x];
dc[di_][a___, mu_, lim__, mu_, b___] :=
Expand[
Block[{m = Length[{lim}], i, j},
      (-1)^m ( (di-2 m) dicss[di][a,lim,b] -
      4 Sum[(-1)^(j-i) If[{lim}[[j]] === {lim}[[i]],
                           di (dicss[di] @@
                               Join[{a}, Delete[{lim}, {{i},{j}}], {b}]
                              ),
                              dicss[di] @@
                               (Join[{a}, Delete[{lim}, {{i},{j}}], {b}]
                                /. ({lim}[[j]]) -> ({lim}[[i]]))
                         ],
            {i,1,m-1}, {j,i+1,m}])
     ] /. dicss -> dicsav//. dics -> dcs];
 (* ****************************************************** *)
                             (*conalldef*)
conall[ x_,opt_:{}] := Contract[ x,
      Expanding->True, EpsContract->
       (EpsContract /. opt /. Options[DiracTrace]),
        Factoring->False ];


                                                  (*diractraceevdef*)
diractraceev[x_, opt___] := Block[{trfa = 1, enx = x},
 If[Head[x] === Times,
    (*Get rid of this SU(N) stuff. Separate SU(N) and Dirac functionality.
    F.Orellana, 23/1-2003*)
    trfa = Select[x, FreeQ2[#, {DiracGamma, LorentzIndex, (*SUNIndex,*) Eps}]&];
    enx = x / trfa;
   ];
  (*If[!FreeQ[x, SUNT],
     enx = SUNSimplify[DiracTrace[enx,opt]] /.DiracTrace -> diractraceev2;
    ];*)
(*CHANGE MAy 94 *)
(*
          diractraceev2[enx, opt] trfa;
*)
          diractraceev2[conall[enx], opt] trfa];

diractraceev2[x_,opt_:{}]:=
    ( TraceOfOne /. opt /.Options[Tr] /. Options[DiracTrace] ) * x /;
        FreeQ[x,DiracGamma];

 (* fr567def, special FreeQ - checking function *)
   fr567[x__]:=FreeQ2[FixedPoint[ReleaseHold,{x}],
                      {DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

 (* coneinsdef    *)
   coneins[ x_ ]  := MemSet[coneins[x], x/.Pair->sCO/.sCO->Pair ];

 (* If no dot's  but DiracGamma's are present *)
(*XXX *)
 diractraceev3[y_,opt_:{}]:=Block[
                              {diractrpa,diractrtemp,diractrresu,four},
  four = TraceOfOne/.opt /. Options[Tr] /. Options[DiracTrace];
  diractrtemp = Expand[ conall[ y, opt ]//gamma67back];
  If[ Head[diractrtemp]===Plus,
      diractrresu = Map[ Apply[Tr,Prepend[opt,#]]&,diractrtemp],
        diractrpa = PartitHead[ diractrtemp,DiracGamma ];
        diractrresu = diractrpa[[1]] four spursav[ diractrpa[[2]] ]
    ];
  If[!FreeQ[diractrresu, LorentzIndex],
     diractrresu = diractrresu /. Pair -> sCO /. sCO -> scev
    ];
  diractrresu = Expand[diractrresu];
  If[!FreeQ[diractrresu, LorentzIndex],
     diractrresu = diractrresu /. Pair -> sCO /. sCO -> scev
    ];
                  diractrresu] /;( FreeQ[y,dot] && !FreeQ[y,DiracGamma]);


 (* #################################################################### *)
 (*                             Main48                                   *)
 (* #################################################################### *)

 diractraceev2[nnx_,in_:{}]:= Block[{diractrjj,diractrlnx,diractrres,
                                    diractrny=0,mand,diractrfact,nx ,
                                    diractrcoll,traceofone,schoutenopt},
   opt = Join[ Flatten[{in}],Options[Tr], Options[DiracTrace] ];
   mand=Mandelstam/.opt;
   diractrfact=Factoring/.opt;
   diractrcoll=PairCollect/.opt;
   schoutenopt = Schouten /. opt;
   traceofone = TraceOfOne /.  opt;
   nx = Collect2[coneins[nnx], dot, Factoring -> False];
   nx = DiracGammaCombine[nx];
   If[ Head[nx]===Plus && Length[nx] > 142,
       diractrlnx = Length[nx]; diractrjj = 0;
       While[ diractrjj<diractrlnx,diractrjj++;
If[$VeryVerbose > 1, Print["diractrjj = ", diractrjj,
     " out of ",diractrlnx]
  ];
       diractrny = diractrny +
        If[FreeQ[nx,DiracGamma],
           diractrny = nx[[diractrjj]],
             diractrny = Expand2[
       DiracSimplify[ nx[[diractrjj]],
                                    InsideDiracTrace->True,
                                     Factoring->False,
                                     FeynCalcInternal -> True,
                                     DiracCanonical->False
                    ],           Pair
                                 ];
              If[!FreeQ[diractrny, DiracGamma],
                                (*DotSimplify added 16/10-2002, F.Orellana*)
                 diractrny = Expand2[DotSimplify[diractrny,Expanding -> False] /.
                                      dot->spursav /.
                                      DiracGamma[5]->0/.
                                       DiracGamma[6]->(1/2)/.
                                        DiracGamma[7]->(1/2),
                                      Pair
                                    ];
                ];
         ]
            ]
   ,

        If[FreeQ[nx,DiracGamma],
           diractrny = nx,
             diractrny = Expand2[
       DiracSimplify[ nx,
                                    InsideDiracTrace->True,
                                     Factoring->False,
                                     FeynCalcInternal -> True,
                                     DiracCanonical->False
                    ],           Pair
                                 ];
              If[!FreeQ[diractrny, DiracGamma],
                  (*DotSimplify added 16/10-2002, F.Orellana*)
                 diractrny = Expand2[DotSimplify[diractrny, Expanding -> True] /.
                                     dot->spursav /.
                                      DiracGamma[5]->0/.
                                       DiracGamma[6]->(1/2)/.
                                        DiracGamma[7]->(1/2),
                                      Pair
                                    ];
                ];
         ]
       ];

If[!FreeQ[diractrny, DiracGamma],
   diractrny =
          DiracSimplify[ diractrny,
                                    InsideDiracTrace->True,
                                     Factoring->False,
                                     FeynCalcInternal -> True,
                                     DiracCanonical->False
                    ]
  ];

If[$VeryVerbose > 1, Print["CH2"]; Print[TimeUsed[]]];
   If[!FreeQ[diractrny, LorentzIndex],
      If[!FreeQ[diractrny, Eps],
         es = {Pair[LorentzIndex[a_,D], b_] *
               Eps[c___,LorentzIndex[a_],d___] :>
         Eps[c,b,d],
         Pair[LorentzIndex[a_,D], b_]  Eps[c___,LorentzIndex[a_,D],d___] :>
         Eps[c,b,d]
              };
         diractrny = diractrny //. es
        ];
      diractrny = diractrny /. Pair -> sCO /. sCO -> scev
     ];
If[$VeryVerbose > 1, Print["CH3"]; Print[TimeUsed[]]];

   If[!FreeQ[diractrny, Eps],
(*
      diractrny = Collect2[diractrny, Eps, Factoring -> False];
*)
      diractrny = EpsEvaluate[diractrny]//Expand;
      diractrny = Contract[ diractrny,
         EpsContract -> (EpsContract /. in /. Options[DiracTrace])
          , Schouten->schoutenopt, Expanding -> False ];
     ];
   If[ diractrfact===True, diractrres = Factor2[traceofone diractrny],
 (* this  2@#$^^$#%^@*#$ ... !!!!;
                           diractrres = Expand[ traceofone diractrny ]
 *)
                           diractrres = traceofone diractrny
     ];
   If[ Length[ mand ] >0,
       diractrres = TrickMandelstam @@ Prepend[ {mand}, diractrres ]
     ];

   diractrpc[x__]:=Plus[x]/;FreeQ[{x},Pair];
   If[ diractrcoll===True,
   diractrpc[x__]:=Collect2[ Plus[x],Pair ,Factoring -> False];
       diractrres = diractrres/.Plus->diractrpc ];
                      diractrres]/;!FreeQ2[nnx,{dot,DiracGamma}];
 (* endof diractraceev1 *)
 (* ************************************************************** *)

 (* #################################################################### *)
 (*                             Main49                                   *)
 (* #################################################################### *)
spursav[0 ..] := 0;
fdim[] = 4;
fdim[a_] = a;
spursavg[x___, LorentzIndex[a_, de___], LorentzIndex[a_, de___], y___] :=
  (fdim[de] spursavg[x, y]) /. spursavg -> spug;
diracga[DiracGamma[h_Integer]] := DiracGamma[h];
diracga[LorentzIndex[mu_, dii_]] := diracga[LorentzIndex[mu,dii],dii];
diracga[Momentum[p_, dii_]] := diracga[Momentum[p, dii],dii];
spug[x___] := spursav@@(Map[diracga, {x}] /. diracga -> DiracGamma);

 (* calculation of traces (recursively) --  up to a factor of 4 *)
   spursav[x_DiracGamma,y_DiracGamma,r_DiracGamma,z_DiracGamma,
           DiracGamma[5]]:=Block[{dirsign},
        dirsign = LeviCivitaSign /. Options[Tr];
        dirsign I Apply[ Eps, {x,y,r,z}/.
                              DiracGamma[vl_[mp_,di___],di___]->vl[mp,di]
                 ]//EpsEvaluate];

(* there is the problem with different Gamma5-schemes ...
*)
(*
   spursav[x___DiracGamma]:=MemSet[ spursav[x], spur[x] ];
*)

   spursav[x__DiracGamma] := MemSet[spursav[x], spur[x]];
   (*Added 28/2-2001 by F.Orellana. Fix to bug reported by A.Kyrielei*)
   spursav[x : ((DiracGamma[__] | HoldPattern[
   Plus[__HighEnergyPhysics`FeynCalc`DiracGamma`DiracGamma]]) ..)] :=
   MemSet[spursav[x], spur[x]];


(*
   spursav = spur;
*)
   spur[]=1;
   spur[DiracGamma[5]]=0;
   spur[x_[y__],DiracGamma[5]]:=0;
   spur[a_[b__],x_[y__],DiracGamma[5]]:=0;
   spur[a_[b__],c_[d__],x_[y__],DiracGamma[5]]:=0;
   spur[a_[b__],c_[d__],x_[y__], _[__], odd__, DiracGamma[5]]:=0 /;
                                         OddQ[Length[{odd}]];
   spur[a__] := (spur @@ Reverse[Transpose[{a}]]) /;
                (!FreeQ[{a}, DiracGammaT]) && FreeQ[{a},DiracGamma];

 (* This is a definition of   Trace( 1.2.3.4. gamma[5] ) *)
   spur[x_,y_,r_,z_,DiracGamma[5]]:=Block[{dirsign},
        dirsign = LeviCivitaSign /. Options[Tr];
        dirsign I Apply[Eps, {x,y,r,z}/.DiracGamma[vl_[mp_,dii___],___
                                                   ]->vl[mp,dii]
                       ]//EpsEvaluate];

     (* Dropped by F.Orellana, 14/1-2002.
        Dropped Kreimer scheme. According to
        Rolf it's wrong *)
   (*spur[m_,n_,r_,s_,l_,t_,DiracGamma[5]]:= Block[{dirsign, sres, ltr},
     If[($Kreimer === True) && (!OrderedQ[{m,n,r,s,l,t}]),
           Tr[1/(TraceOfOne/.Options[Tr]) DiracOrder[ m.n.r.s.
                                              l.t.DiracGamma[5] ]
             ],
        If[$Larin === True &&
           !FreeQ[{m,n,r,s,l,t}, DiracGamma[LorentzIndex[_,_],_]]
           ,
           ltr[a1_, a2_, a3_, a4_, a5_][
                 DiracGamma[LorentzIndex[in_,di___], di___]
                                       ] :=
    Block[{f1, f2, f3,drsi},
          drsi = LeviCivitaSign /. Options[Tr];
          drsi = drsi/(TraceOfOne/.Options[Tr]);
 (*drsi is usually -1/4 *)
          {f1, f2, f3} = LorentzIndex[#, D]& /@ Unique[{"L","L","L"}];
          Tr[drsi I/6 Eps[LorentzIndex[in, di], f1, f2, f3] *
             a1.a2.a3.a4.a5.DiracGamma[f1, D] . DiracGamma[f2, D] .
                            DiracGamma[f3, D]
            ]
         ];
           Which[ MatchQ[t, DiracGamma[ LorentzIndex[__], ___]],
                  ltr[m,n,r,s,l][t],
                  MatchQ[l, DiracGamma[ LorentzIndex[__], ___]],
                  -ltr[m,n,r,s,t][l],
                  MatchQ[s, DiracGamma[ LorentzIndex[__], ___]],
                  ltr[m,n,r,t,l][s],
                  MatchQ[r, DiracGamma[ LorentzIndex[__], ___]],
                  -ltr[m,n,s,t,l][r],
                  MatchQ[n, DiracGamma[ LorentzIndex[__], ___]],
                  ltr[m,r,s,t,l][n],
                  MatchQ[m, DiracGamma[ LorentzIndex[__], ___]],
                  -ltr[n,r,s,t,l][m]
                ]
      ,(* nix Larin *)
        dirsign = LeviCivitaSign /. Options[Tr];
       Expand[ + dirsign I (
        scev[ m//gc,n//gc ]  Apply[ Eps, {l,r,s,t}//gc ] -
        scev[ m//gc,r//gc ]  Apply[ Eps, {l,n,s,t}//gc ] +
        scev[ n//gc,r//gc ]  Apply[ Eps, {l,m,s,t}//gc ] +
        scev[ s//gc,l//gc ]  Apply[ Eps, {m,n,r,t}//gc ] +
        scev[ l//gc,t//gc ]  Apply[ Eps, {m,n,r,s}//gc ] +
        scev[ s//gc,t//gc ]  Apply[ Eps, {l,m,n,r}//gc ]
                                                       )//EpsEvaluate
                                               ] ] ]
         ] /; $West =!= True; *)     (*spurdef*)

 (* this trace is calculated via expressing  DiracMatrix[w1,w2,w3]
   by the Chisholm - identity;
   thus it is only valid in four dimensions and in the naive
   gamma5 prescription
 *)
spur[w1_,w2_,w3_,w4_,w5_,w6_,w7_,w8_,DiracGamma[5]
    ]:= Block[{trsign,z1,z2,z3,z4,z5,z6,z7,z8},
{z1,z2,z3,z4,z5,z6,z7,z8} =
{w1,w2,w3,w4,w5,w6,w7,w8} /.DiracGamma[vl_[mp_,dii___],___]->vl[mp,dii];
trsign = LeviCivitaSign /. Options[Tr];
 (* trsign is usually  =  -1 *)
 (* factor 4 is put later *)
trsign*I*(Eps[w5, w6, w7, w8]*Pair[w1, w4]*Pair[w2, w3] -
     Eps[w4, w6, w7, w8]*Pair[w1, w5]*Pair[w2, w3] -
     Eps[w5, w6, w7, w8]*Pair[w1, w3]*Pair[w2, w4] +
     Eps[w4, w6, w7, w8]*Pair[w1, w3]*Pair[w2, w5] +
     Eps[w5, w6, w7, w8]*Pair[w1, w2]*Pair[w3, w4] -
     Eps[w4, w6, w7, w8]*Pair[w1, w2]*Pair[w3, w5] +
     Eps[w3, w6, w7, w8]*Pair[w1, w2]*Pair[w4, w5] -
     Eps[w2, w6, w7, w8]*Pair[w1, w3]*Pair[w4, w5] +
     Eps[w1, w6, w7, w8]*Pair[w2, w3]*Pair[w4, w5] +
     Eps[w1, w2, w3, w8]*Pair[w4, w7]*Pair[w5, w6] -
     Eps[w1, w2, w3, w7]*Pair[w4, w8]*Pair[w5, w6] -
     Eps[w1, w2, w3, w8]*Pair[w4, w6]*Pair[w5, w7] +
     Eps[w1, w2, w3, w6]*Pair[w4, w8]*Pair[w5, w7] +
     Eps[w1, w2, w3, w7]*Pair[w4, w6]*Pair[w5, w8] -
     Eps[w1, w2, w3, w6]*Pair[w4, w7]*Pair[w5, w8] +
     Eps[w3, w4, w5, w8]*Pair[w1, w2]*Pair[w6, w7] -
     Eps[w2, w4, w5, w8]*Pair[w1, w3]*Pair[w6, w7] +
     Eps[w1, w4, w5, w8]*Pair[w2, w3]*Pair[w6, w7] +
     Eps[w1, w2, w3, w8]*Pair[w4, w5]*Pair[w6, w7] -
     Eps[w1, w2, w3, w5]*Pair[w4, w8]*Pair[w6, w7] +
     Eps[w1, w2, w3, w4]*Pair[w5, w8]*Pair[w6, w7] -
     Eps[w3, w4, w5, w7]*Pair[w1, w2]*Pair[w6, w8] +
     Eps[w2, w4, w5, w7]*Pair[w1, w3]*Pair[w6, w8] -
     Eps[w1, w4, w5, w7]*Pair[w2, w3]*Pair[w6, w8] -
     Eps[w1, w2, w3, w7]*Pair[w4, w5]*Pair[w6, w8] +
     Eps[w1, w2, w3, w5]*Pair[w4, w7]*Pair[w6, w8] -
     Eps[w1, w2, w3, w4]*Pair[w5, w7]*Pair[w6, w8] +
     Eps[w3, w4, w5, w6]*Pair[w1, w2]*Pair[w7, w8] -
     Eps[w2, w4, w5, w6]*Pair[w1, w3]*Pair[w7, w8] +
     Eps[w1, w4, w5, w6]*Pair[w2, w3]*Pair[w7, w8] +
     Eps[w1, w2, w3, w6]*Pair[w4, w5]*Pair[w7, w8] -
     Eps[w1, w2, w3, w5]*Pair[w4, w6]*Pair[w7, w8] +
     Eps[w1, w2, w3, w4]*Pair[w5, w6]*Pair[w7, w8])
              ] /; ($Larin =!= True) && ($West =!= True);

 (* this trace has been calculated according to Larin,
   i.e. expression DiracMatrix[w8].DiracGamma[5] by
   (-I/6) LeviCivita[w8,mu,nu,la] DiracMatrix[mu,nu,la]
 *)

spur[w1_,w2_,w3_,w4_,w5_,w6_,w7_,w8_,DiracGamma[5]
    ]:= Block[{trsign,z1,z2,z3,z4,z5,z6,z7,z8},
{z1,z2,z3,z4,z5,z6,z7,z8} =
{w1,w2,w3,w4,w5,w6,w7,w8} /.DiracGamma[vl_[mp_,dii___],___]->vl[mp,dii];
trsign = LeviCivitaSign /. Options[Tr];
 (* trsign is usually  =  -1 *)
 (* factor 4 is put later *)
trsign*I*(Eps[z5, z6, z7, z8]*Pair[z1, z4]*Pair[z2, z3] -
    Eps[z4, z6, z7, z8]*Pair[z1, z5]*Pair[z2, z3] +
    Eps[z4, z5, z7, z8]*Pair[z1, z6]*Pair[z2, z3] -
    Eps[z4, z5, z6, z8]*Pair[z1, z7]*Pair[z2, z3] -
    Eps[z5, z6, z7, z8]*Pair[z1, z3]*Pair[z2, z4] +
    Eps[z3, z6, z7, z8]*Pair[z1, z5]*Pair[z2, z4] -
    Eps[z3, z5, z7, z8]*Pair[z1, z6]*Pair[z2, z4] +
    Eps[z3, z5, z6, z8]*Pair[z1, z7]*Pair[z2, z4] +
    Eps[z4, z6, z7, z8]*Pair[z1, z3]*Pair[z2, z5] -
    Eps[z3, z6, z7, z8]*Pair[z1, z4]*Pair[z2, z5] +
    Eps[z3, z4, z7, z8]*Pair[z1, z6]*Pair[z2, z5] -
    Eps[z3, z4, z6, z8]*Pair[z1, z7]*Pair[z2, z5] -
    Eps[z4, z5, z7, z8]*Pair[z1, z3]*Pair[z2, z6] +
    Eps[z3, z5, z7, z8]*Pair[z1, z4]*Pair[z2, z6] -
    Eps[z3, z4, z7, z8]*Pair[z1, z5]*Pair[z2, z6] +
    Eps[z3, z4, z5, z8]*Pair[z1, z7]*Pair[z2, z6] +
    Eps[z4, z5, z6, z8]*Pair[z1, z3]*Pair[z2, z7] -
    Eps[z3, z5, z6, z8]*Pair[z1, z4]*Pair[z2, z7] +
    Eps[z3, z4, z6, z8]*Pair[z1, z5]*Pair[z2, z7] -
    Eps[z3, z4, z5, z8]*Pair[z1, z6]*Pair[z2, z7] +
    Eps[z5, z6, z7, z8]*Pair[z1, z2]*Pair[z3, z4] -
    Eps[z2, z6, z7, z8]*Pair[z1, z5]*Pair[z3, z4] +
    Eps[z2, z5, z7, z8]*Pair[z1, z6]*Pair[z3, z4] -
    Eps[z2, z5, z6, z8]*Pair[z1, z7]*Pair[z3, z4] +
    Eps[z1, z6, z7, z8]*Pair[z2, z5]*Pair[z3, z4] -
    Eps[z1, z5, z7, z8]*Pair[z2, z6]*Pair[z3, z4] +
    Eps[z1, z5, z6, z8]*Pair[z2, z7]*Pair[z3, z4] -
    Eps[z4, z6, z7, z8]*Pair[z1, z2]*Pair[z3, z5] +
    Eps[z2, z6, z7, z8]*Pair[z1, z4]*Pair[z3, z5] -
    Eps[z2, z4, z7, z8]*Pair[z1, z6]*Pair[z3, z5] +
    Eps[z2, z4, z6, z8]*Pair[z1, z7]*Pair[z3, z5] -
    Eps[z1, z6, z7, z8]*Pair[z2, z4]*Pair[z3, z5] +
    Eps[z1, z4, z7, z8]*Pair[z2, z6]*Pair[z3, z5] -
    Eps[z1, z4, z6, z8]*Pair[z2, z7]*Pair[z3, z5] +
    Eps[z4, z5, z7, z8]*Pair[z1, z2]*Pair[z3, z6] -
    Eps[z2, z5, z7, z8]*Pair[z1, z4]*Pair[z3, z6] +
    Eps[z2, z4, z7, z8]*Pair[z1, z5]*Pair[z3, z6] -
    Eps[z2, z4, z5, z8]*Pair[z1, z7]*Pair[z3, z6] +
    Eps[z1, z5, z7, z8]*Pair[z2, z4]*Pair[z3, z6] -
    Eps[z1, z4, z7, z8]*Pair[z2, z5]*Pair[z3, z6] +
    Eps[z1, z4, z5, z8]*Pair[z2, z7]*Pair[z3, z6] -
    Eps[z4, z5, z6, z8]*Pair[z1, z2]*Pair[z3, z7] +
    Eps[z2, z5, z6, z8]*Pair[z1, z4]*Pair[z3, z7] -
    Eps[z2, z4, z6, z8]*Pair[z1, z5]*Pair[z3, z7] +
    Eps[z2, z4, z5, z8]*Pair[z1, z6]*Pair[z3, z7] -
    Eps[z1, z5, z6, z8]*Pair[z2, z4]*Pair[z3, z7] +
    Eps[z1, z4, z6, z8]*Pair[z2, z5]*Pair[z3, z7] -
    Eps[z1, z4, z5, z8]*Pair[z2, z6]*Pair[z3, z7] +
    Eps[z3, z6, z7, z8]*Pair[z1, z2]*Pair[z4, z5] -
    Eps[z2, z6, z7, z8]*Pair[z1, z3]*Pair[z4, z5] +
    Eps[z2, z3, z7, z8]*Pair[z1, z6]*Pair[z4, z5] -
    Eps[z2, z3, z6, z8]*Pair[z1, z7]*Pair[z4, z5] +
    Eps[z1, z6, z7, z8]*Pair[z2, z3]*Pair[z4, z5] -
    Eps[z1, z3, z7, z8]*Pair[z2, z6]*Pair[z4, z5] +
    Eps[z1, z3, z6, z8]*Pair[z2, z7]*Pair[z4, z5] +
    Eps[z1, z2, z7, z8]*Pair[z3, z6]*Pair[z4, z5] -
    Eps[z1, z2, z6, z8]*Pair[z3, z7]*Pair[z4, z5] -
    Eps[z3, z5, z7, z8]*Pair[z1, z2]*Pair[z4, z6] +
    Eps[z2, z5, z7, z8]*Pair[z1, z3]*Pair[z4, z6] -
    Eps[z2, z3, z7, z8]*Pair[z1, z5]*Pair[z4, z6] +
    Eps[z2, z3, z5, z8]*Pair[z1, z7]*Pair[z4, z6] -
    Eps[z1, z5, z7, z8]*Pair[z2, z3]*Pair[z4, z6] +
    Eps[z1, z3, z7, z8]*Pair[z2, z5]*Pair[z4, z6] -
    Eps[z1, z3, z5, z8]*Pair[z2, z7]*Pair[z4, z6] -
    Eps[z1, z2, z7, z8]*Pair[z3, z5]*Pair[z4, z6] +
    Eps[z1, z2, z5, z8]*Pair[z3, z7]*Pair[z4, z6] +
    Eps[z3, z5, z6, z8]*Pair[z1, z2]*Pair[z4, z7] -
    Eps[z2, z5, z6, z8]*Pair[z1, z3]*Pair[z4, z7] +
    Eps[z2, z3, z6, z8]*Pair[z1, z5]*Pair[z4, z7] -
    Eps[z2, z3, z5, z8]*Pair[z1, z6]*Pair[z4, z7] +
    Eps[z1, z5, z6, z8]*Pair[z2, z3]*Pair[z4, z7] -
    Eps[z1, z3, z6, z8]*Pair[z2, z5]*Pair[z4, z7] +
    Eps[z1, z3, z5, z8]*Pair[z2, z6]*Pair[z4, z7] +
    Eps[z1, z2, z6, z8]*Pair[z3, z5]*Pair[z4, z7] -
    Eps[z1, z2, z5, z8]*Pair[z3, z6]*Pair[z4, z7] +
    Eps[z3, z4, z7, z8]*Pair[z1, z2]*Pair[z5, z6] -
    Eps[z2, z4, z7, z8]*Pair[z1, z3]*Pair[z5, z6] +
    Eps[z2, z3, z7, z8]*Pair[z1, z4]*Pair[z5, z6] -
    Eps[z2, z3, z4, z8]*Pair[z1, z7]*Pair[z5, z6] +
    Eps[z1, z4, z7, z8]*Pair[z2, z3]*Pair[z5, z6] -
    Eps[z1, z3, z7, z8]*Pair[z2, z4]*Pair[z5, z6] +
    Eps[z1, z3, z4, z8]*Pair[z2, z7]*Pair[z5, z6] +
    Eps[z1, z2, z7, z8]*Pair[z3, z4]*Pair[z5, z6] -
    Eps[z1, z2, z4, z8]*Pair[z3, z7]*Pair[z5, z6] +
    Eps[z1, z2, z3, z8]*Pair[z4, z7]*Pair[z5, z6] -
    Eps[z3, z4, z6, z8]*Pair[z1, z2]*Pair[z5, z7] +
    Eps[z2, z4, z6, z8]*Pair[z1, z3]*Pair[z5, z7] -
    Eps[z2, z3, z6, z8]*Pair[z1, z4]*Pair[z5, z7] +
    Eps[z2, z3, z4, z8]*Pair[z1, z6]*Pair[z5, z7] -
    Eps[z1, z4, z6, z8]*Pair[z2, z3]*Pair[z5, z7] +
    Eps[z1, z3, z6, z8]*Pair[z2, z4]*Pair[z5, z7] -
    Eps[z1, z3, z4, z8]*Pair[z2, z6]*Pair[z5, z7] -
    Eps[z1, z2, z6, z8]*Pair[z3, z4]*Pair[z5, z7] +
    Eps[z1, z2, z4, z8]*Pair[z3, z6]*Pair[z5, z7] -
    Eps[z1, z2, z3, z8]*Pair[z4, z6]*Pair[z5, z7] +
    Eps[z3, z4, z5, z8]*Pair[z1, z2]*Pair[z6, z7] -
    Eps[z2, z4, z5, z8]*Pair[z1, z3]*Pair[z6, z7] +
    Eps[z2, z3, z5, z8]*Pair[z1, z4]*Pair[z6, z7] -
    Eps[z2, z3, z4, z8]*Pair[z1, z5]*Pair[z6, z7] +
    Eps[z1, z4, z5, z8]*Pair[z2, z3]*Pair[z6, z7] -
    Eps[z1, z3, z5, z8]*Pair[z2, z4]*Pair[z6, z7] +
    Eps[z1, z3, z4, z8]*Pair[z2, z5]*Pair[z6, z7] +
    Eps[z1, z2, z5, z8]*Pair[z3, z4]*Pair[z6, z7] -
    Eps[z1, z2, z4, z8]*Pair[z3, z5]*Pair[z6, z7] +
    Eps[z1, z2, z3, z8]*Pair[z4, z5]*Pair[z6, z7])
] /; $Larin === True;

   spur[x__,DiracGamma[6]]:=1/2 spur[x] + 1/2 spur[x,DiracGamma[5]];
   spur[x__,DiracGamma[7]]:=1/2 spur[x] - 1/2 spur[x,DiracGamma[5]];


   spur[x__]:=( DiracTrace@@ ( gamma67backj[ {x} ] )
              ) /; !FreeQ2[{x},{DiracGamma[6],DiracGamma[7]}];

   gc[x_]:=x/.DiracGamma->gach;
   gach[ex_,___]:=ex /; Length[ex]>0;                     (*gachdef*)
   gach[n_Integer]=DiracGamma[n];

   spur[y__] :=Block[ {spx,le=Length[{y}],tempres,i,spurjj,tempr,
                       temp2 = 0,fi,spt, resp,scx,dirsign},
                spx = ( {y}//DiracGammaExpand )/.DiracGamma->gach;
                scx[a_,b_]:=scev[spx[[a]],spx[[b]]];

                resp =
   Which[
        OddQ[le] && fr567[spx],
         0 ,
        le===2,
         scev[spx[[1]],spx[[2]]]/.Pair->sCO/.sCO->Pair,
        le===4,
         (scx[1,2] scx[3,4]-scx[1,3] scx[2,4]+scx[1,4] scx[2,3]
         )//Expand,
        le===6,
         (
          scx[1,6] scx[2,5] scx[3,4] - scx[1,5] scx[2,6] scx[3,4] -
          scx[1,6] scx[2,4] scx[3,5] + scx[1,4] scx[2,6] scx[3,5] +
          scx[1,5] scx[2,4] scx[3,6] - scx[1,4] scx[2,5] scx[3,6] +
          scx[1,6] scx[2,3] scx[4,5] - scx[1,3] scx[2,6] scx[4,5] +
          scx[1,2] scx[3,6] scx[4,5] - scx[1,5] scx[2,3] scx[4,6] +
          scx[1,3] scx[2,5] scx[4,6] - scx[1,2] scx[3,5] scx[4,6] +
          scx[1,4] scx[2,3] scx[5,6] - scx[1,3] scx[2,4] scx[5,6] +
          scx[1,2] scx[3,4] scx[5,6]
                )//Expand ,

       FreeQ[spx,DiracGamma[5]],
        For[i=2, i<le+1, i++,
            temp2 += ((-1)^i) * (*coneins[*)
                     scev[spx[[1]],spx[[i]]] spt@@Rest[Drop[spx,{i,i}]]
                                    (* ] *)
           ];
       Expand[ temp2/.spt->spursavg/.spursavg->spug] ,
      True,
       If[($BreitMaison === True) && ($West =!= True),
        dirsign = LeviCivitaSign /. Options[Tr];
    fi = Table[LorentzIndex[ Unique[] ],{spurjj,1,4}];
    DiracTrace @@
           ( {spx}/.DiracGamma[5]->
             (dirsign I/24 (DiracGamma[fi[[1]]].DiracGamma[fi[[2]]].
                    DiracGamma[fi[[3]]].DiracGamma[fi[[4]]]
                   ) (Eps@@fi)
             )
           )
      ,
       (*If[$Kreimer === True, NochNichtFertig,*)
          If[$Larin === True,
             {fi1, fi2, fi3} = LorentzIndex[#,D]& /@ Unique[{"a","b","c"}];
              drsi = LeviCivitaSign /. Options[Tr];
              drsi = drsi/(TraceOfOne/.Options[Tr]);
             (*drsi is usually -1/4 *)
             temp2 = spx /. {a___, lomo_[mUU_,di___], DiracGamma[5]} :>
                     Tr[ drsi I/6 Eps[lomo[mUU,di], fi1, fi2, fi3] *
                         dot @@ Map[DiracGamma[#,D]&, {a,fi1,fi2,fi3}]];
         ]  (*]*);

    If[($Larin === False) (*&& ($Kreimer === False)*) && ($West === True) &&
       FreeQ[Drop[spx,-1], DiracGamma[5]] &&
       Length[spx] > 6,
(*Print["CHECKWEst"];*)
       temp2 = Expand[2/(Length[spx]-5) Sum[(-1)^(i+j+1) *
         scev[spx[[i]], spx[[j]]] spt@@Delete[spx,{{j},{i}}],
                                            {i,2,Length[spx]-1},
                                            {j,1,i-1}
                                           ]
                     ];

      ];

    temp2 = temp2/.spt->spursavg/.spursavg->spug;
    temp2
     (*if $BreitMaison===True*)
      ]
    ];
            resp];

 (* #################################################################### *)
 (*                             Main50                                   *)
 (* #################################################################### *)

 (* ************************************************************** *)
 (* Properties and special cases of traces (up to a factor 4) *)
   tris[x___] := tris[x] = trI[x];                  (*trisdef*)
   trI[a_+b_] := tris[a] + tris[b];                  (*trIdef*)
   trI[] = 1;
   trI[ DiracGamma[5] ] = 0;
   trI[ DiracGamma[6] ] = 1/2;
   trI[ DiracGamma[7] ] = 1/2;


   trI[ a:DiracGamma[_[__]].. ,DiracGamma[n_] ] := 0 /;
      (OddQ[Length[{a}]]&&(n==5 || n==6 || n==7));

       trI[ a:DiracGamma[_[__],___].. ,DiracGamma[n_] ] := 0 /;
          ($BreitMaison === False) &&
          (OddQ[Length[{a}]]&&(n==5 || n==6 || n==7))

   trI[ d:DiracGamma[__].. ] := 0/;(OddQ[Length[{d}]] && fr567[ d ]);

   trI[ d:DiracGamma[_[__],___].. ,DiracGamma[5] ] := 0/;Length[{d}]<4;

   trI[x_] :=  x /; FreeQ[ {x},DiracGamma ];

   trI[ DiracGamma[a_[b__],___],DiracGamma[c_[d__],___],
        DiracGamma[6] ] := 1/2 scev[ a[b],c[d] ];

   trI[ DiracGamma[a_[b__],___],DiracGamma[c_[d__],___],
        DiracGamma[7] ] := 1/2 scev[ a[b],c[d] ];

   trI[ x__ ] := spursav[ x ]/;( Length[{x}]<11 && fr567[x]);

 (* #################################################################### *)
 (*                             Main51                                   *)
 (* #################################################################### *)

                       (*trICdef*)
 (* cyclic property *) (* Changed by F.Orellana, 14/1-2002.
                         Dropped Kreimer scheme. According to
                         Rolf it's wrong *)
   trIC[y___]:=tris @@ cyclic[y];
   (*trIC[y___]:=If[$Kreimer =!= True,
                  tris @@ cyclic[y],
                  tris[y]
                 ];*)
   cyclic[x__]:=RotateLeft[{x},Position[{x},First[Sort[{x}]]][[1,1]]];
   cyclic[]:={};

   DiracTrace /:
   MakeBoxes[DiracTrace[a__, opts___Rule], TraditionalForm
            ] :=
   RowBox[{"tr","(",TBox[a], ")"}]

   DiracTrace /:
   MakeBoxes[DiracTrace[a__], TraditionalForm
            ] :=
   RowBox[{"tr","(",TBox[a], ")"}]

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracTrace | \n "]];
Null
