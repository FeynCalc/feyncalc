(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracSimplify *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 December '98 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: like DiracTrick, but including non-commutative expansion *)

(* ------------------------------------------------------------------------ *)

(* NonCommQ replaced with NonCommFreeQ everywhere due to change (fix) of
   definitions of these functions. F.Orellana, 13/9-2002 *)

MyBeginPackage["HighEnergyPhysics`fctools`DiracSimplify`",
             "HighEnergyPhysics`FeynCalc`"];

ChisholmSpinor::"usage"=
"ChisholmSpinor[x] uses the Chisholm identity on a DiraGamma between spinors. \
As an optional second argument 1 or 2 may be \
given, indicating that ChisholmSpinor should only act on the first \
resp. second part of a product of spinor chains.";

DiracCanonical::"usage"=
"DiracCanonical is an option for DiracSimplify. \
If set to True DiracSimplify uses the function DiracOrder \
internally.";

InsideDiracTrace::"usage"=
"InsideDiracTrace is an option of DiracSimplify.
If set to True, DiracSimplify assumes to operate
inside a DiracTrace, i.e., products of an odd number
of Dirac matrices are discarded. Furthermore simple
traces are calculated (but divided by a factor 4,
i.e. :  DiracSimplify[DiracMatrix[a,b], InsideDiracTrace->True]
 yields  ScalarProduct[a,b]) \n
Traces involving more than
four DiracGammas and DiracGamma[5] are not performed.";

DiracSimplify::"usage"=
"DiracSimplify[expr] simplifies products of Dirac matrices \
in expr and expands non-commutative products. \
Double Lorentz indices and four vectors are contracted. \
The Dirac equation is applied. \
All DiracMatrix[5], DiracMatrix[6] and DiracMatrix[7] are moved to \
the right. The order of the Dirac matrices is not changed.";

$SpinorMinimal::"usage"=
"$SpinorMinimal is a global switch for an additional simplification \
attempt in DiracSimplify for more than one Spinor-line. \
The default is False, since otherwise it costs too much time.";

DiracSimpCombine::"usage"=
"DiracSimpCombine is an option for DiracSimplify. If set to \
True, sums of DiracGamma's will be merged as much as \
possible in DiracGamma[ .. + .. + ]'s.";

DiracSubstitute67::"usage"=
"DiracSubstitute67 is an option for DiracSimplify. If set to \
True the chirality-projectors DiracGamma[6] and DiracGamma[7] are \
substituted by their definitions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

print1 = If[$VeryVerbose>0, Print[##]]&;
print2 = If[$VeryVerbose>1, Print[##]]&;
print3 = If[$VeryVerbose>2, Print[##]]&;

MakeContext[ Collect2, Contract, DiracOrder,
DiracEquation, DiracGamma, DiracGammaCombine, DiracGammaExpand,
DiracMatrix, DiracOrder, DiracSigmaExplicit, DiracSlash,
DiracTrace];

dR  := dR   = MakeContext["DiracTrick"];

Eps := Eps  = MakeContext["Eps"];
fcinte := fcinte = MakeContext["FeynCalcInternal"];

MakeContext[ DotSimplify, EpsContract, Expanding, Expand2,
Factor2, FreeQ2, Factoring,
LorentzIndex, MemSet, NonCommFreeQ ];

sCO := sCO   = MakeContext["PairContract"];
scev := scev = MakeContext["ExpandScalarProduct"];

MakeContext[ Pair, PartitHead, Spinor, GA, GAD, GS, GSD, SUNT, TR];

Options[DiracSimplify] = {
 DiracCanonical -> False,
 DiracSigmaExplicit -> True,
 DiracSimpCombine->False,
 DiracSubstitute67 -> False,
 Expanding -> True,
 Factoring -> False,
 fcinte -> False,
 InsideDiracTrace -> False};

fcinter[x_] := If[ (fcinte(*r*)(*Typo? F.Orellana. 22/2-2003*) /. Options[DiracSimplify]) === True,
                   x, fcinte[x] ];

dotLin[x_] := If[FreeQ[x, DOT], x, DotSimplify[x, Expanding -> False]];
diracEq[x_]:= If[FreeQ[x, Spinor], x, DiracEquation[x]];

Options[diracSimplify] =
        {diracInfo->False, DiracCanonical->False,
         InsideDiracTrace->False, DiracSubstitute67->False,
         Factoring -> False, DiracSimpCombine->False
        };

dit[x_,ops___Rule]:=DiracTrace[diracSimplify@@Join[{x},{ops},
                    Flatten[Prepend[{Options[DiracSimplify]},
                                     InsideDiracTrace -> True]]
                                       ]
                   ];
(* DiracSimplifydef*)
DiracSimplify[x_,y__, z___Rule]:=DiracSimplify[DOT[x,y], z];

diracSimplify[z_, ru___Rule]:=
    (Contract[z]/.DiracTrace->dit)/;!FreeQ[z,DiracTrace];

dS[x__] := MemSet[dS[x], dR[x]];

(* ****************************************************************** *)
DiracSimplify[a_, opt___Rule] := (a /.
        (*Added 19/9-2002. F.Orellana. Covariant normalization convention
          NOT Bjorken&Drell convention*)
        DOT[xx___,y_Spinor,y_Spinor,z___] :> 2 y[[2]] rdot[xx,z] /. rdot[]:>Sequence[] /. rdot -> DOT(**))/;
         FreeQ2[a, {DiracGamma,DiracSlash,DiracMatrix,
                    GA[__],GS[__],GAD[__],GSD[__]}];

DiracSimplify[a_, opts___Rule] :=
  If[ (Expanding /. {opts} /. Options[DiracSimplify]) === False,
     If[(DiracSigmaExplicit /. {opts} /.
                   Options[DiracSimplify]) === True,
(* extra change on 9/8-2003 by Rolf, due to missed copying from 27/3-2003 *)
        DiracSigmaExplicit[diracEq[dotLin[a // fcinter](* /. DOT -> dS*)]
                                                  ],
        diracEq[dotLin[a // fcinter] (*/. DOT -> dS*)]
       ],
       If[$VeryVerbose>2, Print["doing oldDiracSimplify on ", StandardForm[a]]];
       oldDiracSimplify[
              If[(DiracSigmaExplicit /. {opts} /.
                 Options[DiracSimplify]) === True,
                 DiracSigmaExplicit[
                 fcinter[a] /. Pair -> sCO (*/. DOT -> dS*)
                               (*27/3-2003. Fix by Rolf in response to bug report by
                                 Francesco Tramontano. See http://www.feyncalc.org/forum/0126.html *)],
                 fcinter[a] /. Pair -> sCO (*/. DOT -> dS*)
                ],
                        opts
                       ] /. sCO -> Pair
    ];
(* ****************************************************************** *)

oldDiracSimplify[x_,y___Rule] := diracSimplify[x,y] /; FreeQ[x, Spinor];

oldDiracSimplify[x_,yy___Rule] := Block[{dre},
If[$VeryVerbose>2, Print["entering oldDiracSimplify", x]];
factoring = Factoring /. {yy} /. Options[DiracSimplify];
If[factoring === True, factoring = Factor2];
(*NEW0796*)
dre = Collect[DotSimplify[dR[DiracGammaCombine[x]]]/.
DOT->dooo,dooo[__]]/.dooo->DOT;
                     dre =  FixedPoint[ SpinorChainEvaluate, dre, 142];
                     If[ !FreeQ[dre, Eps],
                         dre = Contract[dre, EpsContract -> True];
                         dre = FixedPoint[ SpinorChainEvaluate, dre, 142]
                         ,
                         If[!FreeQ[dre, LorentzIndex],
                            dre = Contract[dre, Expanding -> False]
                           ];
                         dre = FixedPoint[ SpinorChainEvaluate, dre, 142];
                       ];
   If[!FreeQ[dre, LorentzIndex],
print2["contracting in oldDiracSimpify"];
      dre = Contract[dre];
print2["contracting in oldDiracSimpify done"];
     ];
   If[Length[DownValues[SpinorsandPairs]
            ] > 1,
      dre = (dre /. DOT -> SpinorsandPairs/. SpinorsandPairs->DOT
            )//dotLin
     ];
   If[!FreeQ[dre, DiracGamma], dre = Expand2[dre, DiracGamma]];

If[factoring =!= False, dre = factoring[dre]];
       dre                 ] /; !FreeQ[x,Spinor];

 collone[x_,y_]:=Collect2[x,y, Factoring -> False];

(* #################################################################### *)

gamma67back[x_] := x/. {DiracGamma[6] :> (1/2 + DiracGamma[5]/2),
                        DiracGamma[7] :> (1/2 - DiracGamma[5]/2)};

DiracSubstitute67[x_] := gamma67back[x];

contractli[x_] := MemSet[contractli[x], Contract[ x, Expanding -> True,
                                                     Factoring -> False,
                         EpsContract -> False ]
                        ];

(*diracSimplifydef *)
(*XXX1 *)

diracSimplify[x_,in___] := x /; NonCommFreeQ[x];

(*CHANGE 1298 *)
diracSimplify[x_,in___Rule]:= If[FreeQ[x, DiracGamma], x,
MemSet[diracSimplify[x,in], Block[
       {diracopt,diracdt,diracndt=0,diraccanopt,diracpdt,diracgasu,
        diracldt,diracjj=0,info,diractrlabel,diracga67,diracsifac,
        diracpag,colle
       },
        (* There are several options *)
        diracopt     = Join[ Flatten[{in}],Options[diracSimplify] ];
        info         = diracInfo/.diracopt;
        diraccanopt  = DiracCanonical/.diracopt;
        diractrlabel = InsideDiracTrace/.diracopt;
        diracga67    = DiracSubstitute67/.diracopt;
        diracgasu    = DiracSimpCombine/.diracopt;
        diracsifac   = Factoring/.diracopt;

        diracdt = dotLin[ x//DiracGammaExpand ];

If[$VeryVerbose > 2,Print["dir1"]];
        If[ diracgasu === True,
            diracdt = contractli[DiracGammaCombine[diracdt/.Pair->sCO]
                                ] (*/. DOT -> dS*)(*Commented out 27/3-2003, see above*),
            diracdt = contractli[ diracdt ] (*/. DOT->dS*)
          ];
(*
Commented out Sept. 9 203 by RM, in order to fix the 27/3 2003 bug
If[$VeryVerbose > 2,Print["dir2a"]];
        diracdt = Expand2[ scev[diracdt//fEx], {Pair, DOT}];
*)
        If[diractrlabel===True,
(*
Dialog[Global`D2 = diracdt];
*)

(* bug fix 2005-02-05: this is a problem because of Flat and OneIdentity of Dot ... *)
(* 
           diracdt = diracdt/.DOT->trIC/.
*)
(*  only do cyclicity simplification if there is a simple structure of Dirac matrices *)
           If[FreeQ[diracdt/. DOT -> dooT, dooT[a__/; !FreeQ[{a}, dooT]]],
               diracdt = diracdt/.DOT->trIC/.  
(* bug fix on September 25th 2003 (RM): due to earlier changes this was overseen:*)
               {trI:>dS, HighEnergyPhysics`fctools`DiracTrace`Private`spursav:> dS};
             ];
(*
Dialog[Global`D3 = diracdt];
*)
              (* optimization *)
           colle[a_]:=If[ (Length[a]<200(*0*))||(Head[a]=!=Plus), a,
                          Collect2[a, DOT, Factoring -> False] ];
           dirfun[exp_]:=colle[exp/.DOT->dS (*/.DOT -> trIC /. trI->DOT*)];
(* careful: can run into infinite loop ..., adding a cut in FixedPoint, 10.9.2003 *)
(* even be more careful: and get rid of cyclic simplifications hrere ... *)
           diracdt = FixedPoint[dirfun, diracdt, 5](*/.DOT ->trIC/.trI->dS*);
(*
Dialog[Global`D4 = diracdt];
*)
If[$VeryVerbose>2,Print["dir2done"]];
           If[ FreeQ[ diracdt, DOT ],
               diracdt = diracdt/.DiracGamma[_[__],___]->0;
               diracpag=PartitHead[diracdt,DiracGamma];
                   If[ diracpag[[2]] === DiracGamma[5], diracdt = 0 ];
                   If[ diracpag[[2]] === DiracGamma[6] ||
                       diracpag[[2]] === DiracGamma[7],
                       diracdt = 1/2  diracpag[[1]]
                     ]
             ]
          ];

(* Change 27/3-2003 by Rolf Mertig, see above (27/3-2003)*)
If[$VeryVerbose > 2,Print["dir2a"]];
(*
        diracdt = Expand2[ scev[diracdt//fEx], {Pair, DOT}];
*)
        diracdt = Expand[ scev[diracdt//fEx], DOT | Pair];

If[$VeryVerbose>2,Print["dir3"]];
        If[FreeQ[diracdt,DOT],
           diracndt=Expand[(diracdt/.sCO->scev)//DiracGammaExpand];
           If[diracga67 === True, diracndt = Expand[diracndt//gamma67back]]
           ,
If[$VeryVerbose>2,Print["dir3 expanding "]];
(*
           diracdt = Expand[ diracdt ];
*)
           diracdt = Expand[ diracdt, DOT ];
If[$VeryVerbose>2,Print["dir3 expanding done ", Length[diracdt]]];
         If[ Head[diracdt] === Plus, diracldt=Length[diracdt],
             If[ diracdt===0, diracldt = 0, diracldt = 1 ]
           ];
If[$VeryVerbose>2,
   Print["in diracSimplify: working with ",diracldt," terms"]];
      While[diracjj<diracldt,diracjj++;
            If[diracldt==1,
               diracpdt = diracdt, diracpdt = diracdt[[diracjj]]
              ];
            If[diractrlabel===True,
(*
change 2005-02-05
               diracpdt = diracpdt/.DOT->trIC/.trI->dS//.
                          DOT -> trIC/.trI->dS;
*)
               diracpdt = diracpdt//.DOT -> dS
              ];
(* maybe insert some TimeConstrained here later *)
If[$VeryVerbose>2,
   Print["in diracSimplify: contraction done, expand now."]];
       diracpdt = scev[ diracpdt ]//Expand;

            If[diractrlabel===True,
               diracpdt = fEx[(diracpdt//DiracGammaExpand)//.DOT->dS](*/.
                                    DOT->trIC/.trI->dS//.DOT->dS/.
                                    DOT->trIC/.trI->dS *) ,
               diracpdt = fEx[DiracGammaExpand[diracpdt]/.DOT->dS]//.
                                    DOT->dS
              ];
             If[ diracga67===True,
                 diracpdt = gamma67back[ diracpdt/.DOT->dr67 ],
                 diracpdt = fEx[ diracpdt ]
               ];
             diracndt = diracndt + Expand2[ diracpdt, DOT ];
             If[ info===True || $VeryVerbose > 2,
                 Print["# ",diracjj," / ",diracldt," = ",
                        Length[diracndt] ]
               ]
           ];
  If[ diracga67===True,
      If[!FreeQ[diacndt, DiracGamma[6]|DiracGamma[7]],
         diracndt = gamma67back[ diracndt/.DOT->dr67 ],
         diracndt = fEx[ diracndt ]
        ]
     ];
   diracndt = diracndt/.dr->DOT/.sCO->scev;
   diracndt = Expand[dotLin[diracndt]];
   If[ (diraccanopt===True ),
   print3["diracordering in diracSimplify"];
        diracndt = DiracOrder[ diracndt ] ;
        diracndt = Expand[dotLin[diracndt]]
     ];
          ] (* If FreeQ[diracdt,dr] *);
If[$VeryVerbose>2, Print["dir4 ",diracdt]];
print3["diracdt = ", diracdt ];
    diracndt = dotLin[diracndt];
If[$VeryVerbose>2, Print["dir5"]];
   If[ diracsifac === True,
       diracndt = Factor2[ diracndt ] ];
If[$VeryVerbose>2, Print["dir6"]];
print3["exiting diracSimplify"];
  diracndt/.HighEnergyPhysics`fctools`DiracTrace`Private`spursav:> DOT
]]];  (* end of diracSimplify *)

(* #################################################################### *)
                                                        (*dr67def*)
   dr67[ b___ ] := dS[ b ]/;FreeQ2[{b},{DiracGamma[6],DiracGamma[7]}];
   dr67[ b___,DiracGamma[6],z___ ] := 1/2 dS[b,z] +
                                      1/2 dS[ b,DiracGamma[5],z ];
   dr67[ b___,DiracGamma[7],z___ ] := 1/2 dS[b,z] -
                                      1/2 dS[ b,DiracGamma[5],z ];

   dIex[ a___,x_ + y_, b___] := dS[a,x,b] + dS[a,y,b];   (*dIexdef*)
                                                         (*dixdef*)

   dix[y_] :=  y/.DOT->dIex/.dIex->dS;
(* #################################################################### *)
(* ************************************************************** *)

(* This is the tricky function which does the expansion of the dr's *)
   fEx[z_]:=FixedPoint[ dix, z/.DOT -> dS ];                (*fExdef*)
(* ************************************************************** *)

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

(* ************************************************************** *)
(* fr567def, frlivcdef : two special FreeQ - checking functions *)
   fr567[x__] := True /; FreeQ2[FixedPoint[ReleaseHold, {x}],
    {DiracGamma[5],DiracGamma[6],DiracGamma[7]}];

(* Properties and special cases of traces (up to a factor 4) *)
   tris[x___] := tris[x] = trI[x];                  (*trisdef*)
   trI[a_Plus] := Map[tris, a];
   trI[] = 1;
   trI[ DiracGamma[5] ] = 0;
   trI[ DiracGamma[6] ] = 1/2;
   trI[ DiracGamma[7] ] = 1/2;

   trI[ a:DiracGamma[_[__]].. ,DiracGamma[n_] ] := 0 /;
      (OddQ[Length[{a}]]&&(n==5 || n==6 || n==7));

    trI[ a:DiracGamma[_[__],___].. ,DiracGamma[n_] ] := 0 /;
         (OddQ[Length[{a}]]&&(n==5 || n==6 || n==7)) &&
         ($BreitMaison === False);

   trI[ d:DiracGamma[__].. ] := 0/;(OddQ[Length[{d}]] && fr567[ d ]);

   trI[ d:DiracGamma[_[__],___].. ,DiracGamma[5] ] := 0/;Length[{d}]<4;

   trI[x_] :=  x /; FreeQ[ {x},DiracGamma ];

   trI[ DiracGamma[a_[b__],___],DiracGamma[c_[d__],___],
        DiracGamma[6] ] := 1/2 scev[ a[b],c[d] ];

   trI[ DiracGamma[a_[b__],___],DiracGamma[c_[d__],___],
        DiracGamma[7] ] := 1/2 scev[ a[b],c[d] ];

   trI[ x__] :=
     HighEnergyPhysics`fctools`DiracTrace`Private`spursav[x]/;
        ( Length[{x}] < 11 && fr567[x]) ||
        ( Length[{x}] <  6 && (!fr567[x]));

(* #################################################################### *)


(* SpinorChainEvaluatedef *)

(* #################################################################### *)
(*                             Main43                                   *)
(* #################################################################### *)

spinlin[x_Plus]:=spinlin/@x;
spinlin[a_] :=( (a/.DOT->ddot)//.{
              ddot[x___,z_ b__,c___] :> z ddot[x,b,c]/;NonCommFreeQ[z]===True,
              ddot[x___,z_ ,c___]    :> z ddot[x,c]/;NonCommFreeQ[z]===True,
              ddot[x_Spinor,b___,c_Spinor,d_Spinor,e___,f_Spinor,g___]:>
              ddot[x,b,c] ddot[d,e,f,g] }
              )/.ddot[]->1/.ddot->DOT;
SetAttributes[ SpinorChainEvaluate, Listable ];
SpinorChainEvaluate[y_]:=y /; FreeQ[y,Spinor];

 (* #################################################################### *)
 (*                             Main44                                   *)
 (* #################################################################### *)

 SpinorChainEvaluate[z_Plus]:= Block[{nz},
   nz = DotSimplify[z];
   If[Length[nz]>20, nz= Collect2[ nz, Spinor,Factoring -> False] ];
   If[Head[nz]=!=Plus, nz = SpinorChainEvaluate[nz],
      If[$sirlin =!= True, nz = Map[ spcev0, nz ],
         If[ FreeQ[nz, DOT[Spinor[p1__] ,
                            (a__ /; FreeQ[{a}, DiracGamma[_,_]]
                            ) , Spinor[p2__]] *
                       DOT[Spinor[p3__] , (b__ /; FreeQ[{b}, DiracGamma[_,_]]
                            ) , Spinor[p4__]]
                  ], nz = Map[ spcev0,nz ],
(* added ,Spinor, Nov. 2003 , RM*)
       nz = sirlin00[ Expand[Map[ spcev0,z//sirlin0 ], Spinor] ]
           ] ] ];                  nz];
 SpinorChainEvaluate[x_]:=
  If[$sirlin =!= True, Expand[spcev0[x], Spinor],
  If[ FreeQ[x//DotSimplify,
                       DOT[Spinor[p1__] ,
                            (a__ /; FreeQ[{a}, DiracGamma[_,_]]
                            ) , Spinor[p2__]] *
                       DOT[Spinor[p3__] , (b__ /; FreeQ[{b}, DiracGamma[_,_]]
                            ) , Spinor[p4__]]
           ],
(* added ,Spinor, Nov. 2003 , RM*)
     Expand[spcev0[x], Spinor],
     sirlin00[ Expand[FixedPoint[spcev0, x//sirlin0, 3 ], Spinor] ]
    ]]/; !Head[x]===Plus;

(* #################################################################### *)
(*                             Main45                                   *)
(* #################################################################### *)

   spcev0[x_] := spcev000[x]/.spcev000->spcev0ev;
(*
   spcev000[ a_ b_ ] := a spcev000[b] /; NonCommFreeQ[a] === True;
*)
   spcev000[y_] := y /; NonCommFreeQ[y] === True;
   spcev000[y_Times] := Select[ y, FreeQ[#, Spinor]& ] spcev0ev[
                       Select[ y,!FreeQ[#, Spinor]& ]          ];
   spcev0ev[x_] := scev[Contract[
                     Expand[spinlin[x](*, Spinor*)]/.DOT->spcevs/.
                                     spcev->DOT, Expanding->False
                                      ]
                             ](*//Expand*);

   spcevs[xx___] := MemSet[ spcevs[xx], FixedPoint[ spcev,{xx},4 ] ];
(*spcevsdef*)

  (*spcevdef*)
   spcev[y_List]:=spcev@@y;
   spcev[a___,b_ /; FreeQ2[b,{Pattern, BlankSequence, BlankNullSequence}],
         c___] := b spcev[a,c] /; NonCommFreeQ[b] === True;
   (*added to allow nested structures like phi.(gamm1.gamm2+gamma3.gamma4).phi
     F.Orellana, 26/9-2002*)
   spcev[a__] := DOT[a] /; FreeQ[{a}, Spinor];
   (**)
   spcev[] = 1;
    spcev[x___,Spinor[a__],y___] :=
     Expand[ DiracOrder[ DiracEquation[fEx[DiracGammaExpand[
                                               DOT[x,Spinor[a],y]]](*/.dR->DOT*)
                                          ] ] ]/; FreeQ[{x,y},Spinor];
    spcev[x___,Spinor[a__],b___,Spinor[c__],y___] :=
      Block[ {spcevdi,spcevre,spcevj},
If[$VeryVerbose > 2, Print["entering spcev with ",
InputForm[DOT@@{x,Spinor[a],b,Spinor[c],y}]]];
        spcevdi = diracSimplify[DOT[Spinor[a],b,Spinor[c]],
                                     InsideDiracTrace->False,
                                     DiracCanonical->False,
                                     diracInfo->False,
                                     Factoring->False,
                                     DiracSimpCombine->True
                               ];
        spcevdi = Expand[ scev[ spcevdi ] ];
        spcevdi = Expand[ spcevdi ];
        If[ !(Head[spcevdi]===Plus),
            spcevre = spinlin[ spcevdi ];
            spcevre = DiracEquation[ spcevre ];,
            spcevre = Sum[DiracEquation[ spinlin[ spcevdi[[spcevj]] ] ],
                           {spcevj,1,Length[spcevdi]}
                         ];
          ];
        spcevre = DotSimplify[DOT[spcevs[x],spcevre,spcevs[y]]];
        If[ !FreeQ[spcevre, SUNT],
            spcevre = (spcevre/.DOT->dS)
          ];
         spcevre = spcevre//DotSimplify;
If[$VeryVerbose > 2, Print["exiting spcev with ",InputForm[spcevre]]];
        spcevre] /; FreeQ[{b}, Spinor];

(* Reference of Sirlin-relations: Nuclear Physics B192 (1981) 93-99;
   Note that we take another sign in front of the Levi-Civita tensor
   in eq. (7), since we take (implicitly) \varepsilon^{0123} = 1
*)

 (* #################################################################### *)
 (*                             Main441                                  *)
 (* #################################################################### *)

  $SpinorMinimal = False;

  sirlin00[x_]:= x/;($SpinorMinimal === False) || ($sirlin===False);
  sirlin00[x_]:=MemSet[sirlin00[x],
                     Block[{te, tg5, ntg5},
print3["sirlin001"];
(*
                       te = sirlin0[x]//ExpandAll;
*)
                       te = sirlin0[x]//Expand;
print3["sirlin002"];
                       If[FreeQ2[te,{DiracGamma[6],DiracGamma[7]}]&&
                          Head[te]===Plus && !FreeQ[te,DiracGamma[5]],
                          tg5 = Select[te, !FreeQ[#,DiracGamma[5]]& ];
                          ntg5 = te - tg5;
(*i.e. te = tg5 + ntg5 *)
                          test = Expand[tg5 + ChisholmSpinor[ntg5], Spinor];
                          If[nterms[test] < Length[te], te=test]
                         ];
print3["exiting sirlin00"];
                  te]] /; $SpinorMinimal ===  True;

(* ident3def *)

ident3[a_,_]:=a;

 (* #################################################################### *)
 (*                             Main442                                  *)
 (* #################################################################### *)
 (* canonize different dummy indices *)  (*sirlin3def*)
 sirlin3a[x_]:=((sirlin3[Expand[Contract[x](*,Spinor*)]/.
                         $MU->dum$y]/.dum$y->$MU)/.  sirlin3 -> Identity
	       )//Contract;
 sirlin3[a_Plus]:=sirlin3 /@ a;
 sirlin3[ m_. DOT[Spinor[p1__] , (ga1___) ,
	     DiracGamma[ LorentzIndex[la_] ] , (ga2___) ,
	     Spinor[p2__]] *
	     DOT[Spinor[p3__], (ga3___) ,
	     DiracGamma[ LorentzIndex[la_] ], (ga4___) ,
             Spinor[p4__]]
        ]:= Block[{counter},
                   counter = 1;

             While[!FreeQ2[{m,ga1,ga2,ga3,a4},
                           {$MU[counter], dum$y[counter]} ],
                   counter = counter + 1
                  ];
       sirlin3[
         m DOT[Spinor[p1] , ga1 ,
         DiracGamma[ LorentzIndex[$MU[counter]] ] , ga2 ,  Spinor[p2]] *
         DOT[Spinor[p3] , ga3 ,  DiracGamma[ LorentzIndex[$MU[counter]] ] ,
                      ga4 ,
         Spinor[p4]]
              ]  ] /; FreeQ[la, $MU];

 sirlin3[ m_. DOT[Spinor[p1__],(ga1___),
             DiracGamma[ LorentzIndex[la_,di_],di_ ], (ga2___) ,
             Spinor[p2__]] *
             DOT[Spinor[p3__],(ga3___),
             DiracGamma[ LorentzIndex[la_,di_],di_ ], (ga4___) ,
             Spinor[p4__]]
        ] := ( m DOT[Spinor[p1] , ga1 ,
                 DiracGamma[ LorentzIndex[$MU[1], di],di ] , ga2 ,
                 Spinor[p2]] *
                 DOT[Spinor[p3] , ga3 ,
                   DiracGamma[LorentzIndex[$MU[1], di], di] , ga4 ,
                 Spinor[p4]]
              ) /; FreeQ2[{ga1,ga2,ga3,ga4}, DiracGamma[_,_]];


(* this is far from optimal, but for the moment sufficient *)
 $sirlin = True;


 (* #################################################################### *)
 (*                             Main443                                  *)
 (* #################################################################### *)

(* The Sirlin - identities are only valid in 4 dimensions and are
only needed, if Dirac matrices are around
*)
 sirlin0[x_]:=If[$sirlin=!=True, x,
                 If[ FreeQ2[x, {LorentzIndex, Momentum}],  x,
                     If[ FreeQ[x, Spinor], x,
                         If[ !FreeQ[x, DiracGamma[_,_]],
                             sirlin3[x]/.sirlin3->Identity,
                             sirlin0doit[(x//sirlin2)/.sirlin2->Identity]
                   ]   ]   ]
                ];

$sirlintime = 242;
SetAttributes[timeconstrained, HoldAll];
If[$OperatingSystem === "Unix",
   timeconstrained[x__] := TimeConstrained[x],
    timeconstrained[x_,__] := x
  ];

 sirlin0doit[a_Plus]:=timeconstrained[
sirlin3a[Contract[
		   (Expand[Map[sirlin1, a](*, DOT*)]/.
		    sirlin1->sirlin2) /.
		   sirlin2 -> sirlin1/.sirlin1->sirlin2/.
                    sirlin2 -> Identity,EpsContract->True]
			 ] // spcev0,
                                     2 $sirlintime, a
                                    ];
 sirlin0doit[a_]:=timeconstrained[
                    (sirlin3a[sirlin1[a]/.sirlin1->sirlin2/.
                        sirlin2 -> Identity
                       ] // spcev0),
                                  $sirlintime, a
                                 ] /;Head[a]=!=Plus;

(*sirlin2def*)
 sirlin2[a_Plus]:=sirlin2/@a;


 sirlin2[m_. DOT[Spinor[pa__] , DiracGamma[Momentum[pj_]] ,
                            DiracGamma[Momentum[pi_]] ,
                            DiracGamma[LorentzIndex[mu_]],(vg5___),
             Spinor[pb__]] *
             DOT[Spinor[Momentum[pi_],0,qf___] ,
                    DiracGamma[LorentzIndex[mu_]] , (vg5___),
             Spinor[Momentum[pj_],0,qf___]]
        ] := (-sirlin2[ m DOT[Spinor[pa] , DiracSlash[pi,pj] ,
                                       DiracMatrix[mu] , vg5 ,
                          Spinor[pb]] *
                          DOT[Spinor[Momentum[pi],0,qf] ,
                                       DiracMatrix[mu] , vg5 ,
                          Spinor[Momentum[pj],0,qf]]
                      ] +
                2 m scev[Momentum[pi],Momentum[pj]] *
                DOT[Spinor[pa] , DiracMatrix[mu] , vg5 ,
                Spinor[pb]] *
                          DOT[Spinor[Momentum[pi],0,qf] ,
                                       DiracMatrix[mu] , vg5 ,
                          Spinor[Momentum[pj],0,qf]]
             )/; ({vg5}==={}) || ({vg5}==={DiracGamma[5]});


 sirlin2[m_. DOT[Spinor[pa__] , DiracGamma[Momentum[pi_]] ,
                            DiracGamma[Momentum[pj_]] ,
                            DiracGamma[LorentzIndex[mu_]],(vg5___),
             Spinor[pb__]] *
             DOT[Spinor[Momentum[pi_],0,qf___] ,
                    DiracGamma[LorentzIndex[mu_]] , (vg5___),
             Spinor[Momentum[pj_],0,qf___]]
        ] :=(m scev[Momentum[pi], Momentum[pj]] *
              DOT[Spinor[pa] , DiracMatrix[$MU[1]] ,
              Spinor[pb]] *
              DOT[Spinor[Momentum[pi],0,qf] , DiracMatrix[$MU[1]] ,
              Spinor[Momentum[pj],0,qf]] +
             m scev[Momentum[pi], Momentum[pj]] *
              DOT[Spinor[pa] , DiracMatrix[$MU[1]], DiracGamma[5] ,
              Spinor[pb]] *
              DOT[Spinor[Momentum[pi],0,qf] , DiracMatrix[$MU[1]] ,
              DiracGamma[5] , Spinor[Momentum[pj],0,qf]]
            ) /; ({vg5}==={}) || ({vg5}==={DiracGamma[5]});


 sirlin2[m_. DOT[Spinor[p1__], (ga1___) ,
	     DiracGamma[ LorentzIndex[la_] ],
	     DiracGamma[ LorentzIndex[nu_] ],
	     DiracGamma[6] ,
	     Spinor[p2__]] *
	     DOT[Spinor[p3__], (ga2___) ,
	     DiracGamma[ LorentzIndex[la_] ],
	     DiracGamma[ LorentzIndex[nu_] ],
	     DiracGamma[7] ,
	     Spinor[p4__]] ] :=  (
    m 4 DOT[Spinor[p1] , ga1 , DiracGamma[6] , Spinor[p2] *
        Spinor[p3] , ga2 , DiracGamma[7] , Spinor[p4]] );

 sirlin2[m_. DOT[Spinor[p1__], (ga1___) ,
	     DiracGamma[ LorentzIndex[la_] ],
	     DiracGamma[ LorentzIndex[nu_] ],
	     DiracGamma[7] ,
	     Spinor[p2__]] *
	     DOT[Spinor[p3__], (ga2___) ,
	     DiracGamma[ LorentzIndex[la_] ],
	     DiracGamma[ LorentzIndex[nu_] ],
	     DiracGamma[6] ,
	     Spinor[p4__]] ] :=  (
    m 4 DOT[Spinor[p1] , ga1 , DiracGamma[7] , Spinor[p2] *
        Spinor[p3] , ga2 , DiracGamma[6] , Spinor[p4]] );
 (* #################################################################### *)
 (*                             Main444                                  *)
 (* #################################################################### *)


(* eq. (8) *)
 sirlin2[m_. DOT[Spinor[p1__], (ga1___) ,
              DiracGamma[ LorentzIndex[mu_] ],
              DiracGamma[ lv_[rho_] ] ,
              DiracGamma[ LorentzIndex[nu_] ], (ga2___) ,
            Spinor[p2__]] *
            DOT[Spinor[p3__], (ga3___) ,
              DiracGamma[ LorentzIndex[mu_] ],
              DiracGamma[ lvt_[tau_] ] ,
              DiracGamma[ LorentzIndex[nu_] ], (ga4___) ,
            Spinor[p4__]]
       ] := Block[{ii=1, ind, la, grho, gtau, gam5},
                    While[!FreeQ[{ga1,ro,ga2,ga3,tau,ga4}, $MU[ii]],
                          ii++];
             la = DiracGamma[LorentzIndex[$MU[ii]]];
             grho = DiracGamma[lv[rho]]; gtau = DiracGamma[lvt[tau]];
             gam5 = DiracGamma[5];
             Contract[
               2 m Pair[lv[rho], lvt[tau]] *
                   DOT[Spinor[p1] , ga1 , la , ga2 ,   Spinor[p2]] *
                   DOT[Spinor[p3] , ga3 , la , ga4 ,   Spinor[p4]] +
               2 m *
                   DOT[Spinor[p1] , ga1 , gtau , ga2 , Spinor[p2]] *
                   DOT[Spinor[p3] , ga3 , grho , ga4 ,   Spinor[p4]] +
               2 m Pair[lv[rho], lvt[tau]] *
                   DOT[Spinor[p1] , ga1 , la , ga2 , gam5 , Spinor[p2]] *
                   DOT[Spinor[p3] , ga3 , la , ga4 , gam5 , Spinor[p4]] -
               2 m *
                   DOT[Spinor[p1] , ga1 , gtau , ga2 , gam5 , Spinor[p2]] *
                   DOT[Spinor[p3] , ga3 , grho , ga4 , gam5 , Spinor[p4]]
                     ]
                   ];

(* eq. (12) of Sirlin *)

 sirlin2[m_. DOT[Spinor[p1__], DiracGamma[ LorentzIndex[mu_] ],
                           DiracGamma[ lv_[rho_] ] ,
                           DiracGamma[ LorentzIndex[sigma_] ],
                           DiracGamma[ lvt_[tau_] ],
                           DiracGamma[ LorentzIndex[nu_] ], om_ ,
             Spinor[p2__]] *
             DOT[Spinor[p3__], DiracGamma[ LorentzIndex[mu_] ],
                           DiracGamma[ lva_[alpha_] ] ,
                           DiracGamma[ LorentzIndex[sigma_] ],
                           DiracGamma[ lvb_[beta_] ],
                           DiracGamma[ LorentzIndex[nu_] ], om_ ,
             Spinor[p4__]]
       ] := Contract[ m 16 Pair[lvt[tau],lvb[beta]] *
                            Pair[lv[rho], lva[alpha]] *
                           DOT[Spinor[p1] , DiracMatrix[mu] , om ,
                           Spinor[p2]] *
                           DOT[Spinor[p3] , DiracMatrix[mu] , om ,
                           Spinor[p4]]
                     ] /; (om===DiracGamma[6]) ||
                          (om===DiracGamma[7]);

(* eq. (13) of Sirlin *)
 sirlin2[m_. DOT[Spinor[p1__], DiracGamma[ LorentzIndex[mu_] ],
                           DiracGamma[ lv_[rho_] ] ,
                           DiracGamma[ LorentzIndex[sigma_] ],
                           DiracGamma[ lvt_[tau_] ],
                           DiracGamma[ LorentzIndex[nu_] ], om1_ ,
             Spinor[p2__]] *
             DOT[Spinor[p3__], DiracGamma[ LorentzIndex[mu_] ],
                           DiracGamma[ lva_[alpha_] ] ,
                           DiracGamma[ LorentzIndex[sigma_] ],
                           DiracGamma[ lvb_[beta_] ],
                           DiracGamma[ LorentzIndex[nu_] ], om2_ ,
             Spinor[p4__]]
       ] :=(m 4 DOT[Spinor[p1] , DiracMatrix[mu],DiracGamma[lv[rho]],
                              DiracGamma[lv[beta]], om1 ,
                 Spinor[p2]] *
                 DOT[Spinor[p3] , DiracMatrix[mu],DiracGamma[lva[alpha]],
                              DiracGamma[lvt[tau]], om2 ,
                                            Spinor[p4]]
            ) /; ( (om1===DiracGamma[6])&& (om2===DiracGamma[7]) )||
                 ( (om1===DiracGamma[7])&& (om2===DiracGamma[6]) );
 (* #################################################################### *)
 (*                             Main445                                  *)
 (* #################################################################### *)


(* in case if no chiral projectors are present: *)
 sirlin2[m_. DOT[Spinor[p1__], DiracGamma[ LorentzIndex[mu_] ],
                           DiracGamma[ lv_[rho_] ] ,
                           DiracGamma[ LorentzIndex[sigma_] ],
                           DiracGamma[ lvt_[tau_] ],
                           DiracGamma[ LorentzIndex[nu_] ],
             Spinor[p2__]] *
             DOT[Spinor[p3__]. DiracGamma[ LorentzIndex[mu_] ],
                           DiracGamma[ lva_[alpha_] ] ,
                           DiracGamma[ LorentzIndex[sigma_] ],
                           DiracGamma[ lvb_[beta_] ],
                           DiracGamma[ LorentzIndex[nu_] ],
             Spinor[p4__]]
       ] := Block[{tmp,re},
                    tmp[ome1_,ome2_]:= sirlin2[ m DOT[Spinor[p1],
   DiracMatrix[mu],DiracGamma[lv[rho]],DiracMatrix[sigma],
   DiracGamma[lvt[tau]],DiracMatrix[nu],DiracGamma[ome1] ,
   Spinor[p2]] *
   DOT[Spinor[p3],DiracMatrix[mu],DiracGamma[lva[alpha]],
   DiracMatrix[sigma],DiracGamma[lvb[beta]],DiracMatrix[nu],
   DiracGamma[ome2],  Spinor[p4]]              ];
                   re = tmp[6,6] + tmp[6,7] + tmp[7,6] + tmp[7,7];
               re];

 (* #################################################################### *)
 (*                             Main446                                  *)
 (* #################################################################### *)

(* These are the ones calculated by FeynCalc  *)

sirlin2[
m_.  DOT[Spinor[pi__] , x1___ , DiracGamma[ LorentzIndex[mu_] ] ,
               DiracGamma[ LorentzIndex[nu_] ] , x2___ ,
Spinor[pj__]] *
DOT[Spinor[pk__] ,  x3___ , DiracGamma[ vm_[a_] ] ,
                DiracGamma[ LorentzIndex[mu_] ] ,
               DiracGamma[ LorentzIndex[nu_] ] , x4___ ,
Spinor[pl__]]
       ] := Contract[ m (
2*DOT[Spinor[pi] , x1 , x2 , Spinor[pj]]*
   DOT[Spinor[pk] , x3 , DiracGamma[vm[a]] , x4 ,
    Spinor[pl]] +
  2*DOT[Spinor[pk] , x3 , DiracGamma[LorentzIndex[al$mu]] , x4 ,
    Spinor[pl]]*
   DOT[Spinor[pi] , x1 , DiracGamma[vm[a]] ,
    DiracGamma[LorentzIndex[al$mu]] , x2 , Spinor[pj]] -
  2*DOT[Spinor[pi] , x1 , DiracGamma[5] , x2 ,
    Spinor[pj]]*
   DOT[Spinor[pk] , x3 , DiracGamma[vm[a]] , DiracGamma[5] , x4 ,
    Spinor[pl]] +
  2*DOT[Spinor[pk] , x3 , DiracGamma[LorentzIndex[al$mu]] ,
    DiracGamma[5] , x4 ,Spinor[pl]]*
   DOT[Spinor[pi] , x1 ,  DiracGamma[vm[a]] ,
    DiracGamma[LorentzIndex[al$mu]] , DiracGamma[5] , x2 , Spinor[pj]]
             )];

sirlin2[ m_. *
DOT[Spinor[Momentum[pi_], 0, fq___] , DiracGamma[Momentum[pk_]] ,
     Spinor[Momentum[pj_], 0, fq___]]*
    DOT[Spinor[Momentum[pl_], 0, fq___] , DiracGamma[Momentum[pj_]] ,
     Spinor[Momentum[pk_], 0, fq___]]
       ] := Contract[ m (
   -((DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pl]] ,
          Spinor[Momentum[pj], 0, fq]]*
         DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pi]] ,
          Spinor[Momentum[pk], 0, fq]]*Pair[Momentum[pj], Momentum[pk]])/
       Pair[Momentum[pi], Momentum[pl]]) +
    (DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
        DiracGamma[5] , Spinor[Momentum[pj], 0, fq]]*
       DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
        DiracGamma[5] , Spinor[Momentum[pk], 0, fq]]*
       (-(Pair[Momentum[pi], Momentum[pl]]*
            Pair[Momentum[pj], Momentum[pk]]) +
         Pair[Momentum[pi], Momentum[pk]]*
          Pair[Momentum[pj], Momentum[pl]] -
         Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]]))
      /(2*Pair[Momentum[pi], Momentum[pl]]) +
    (DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
        Spinor[Momentum[pj], 0, fq]]*
       DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
        Spinor[Momentum[pk], 0, fq]]*
       (3*Pair[Momentum[pi], Momentum[pl]]*
          Pair[Momentum[pj], Momentum[pk]] +
         Pair[Momentum[pi], Momentum[pk]]*Pair[Momentum[pj], Momentum[pl]] -
        Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]]))/
     (2*Pair[Momentum[pi], Momentum[pl]])
             ) ];
sirlin2[ m_. *
  DOT[Spinor[Momentum[pi_], 0, fq___] , DiracGamma[Momentum[pk_]] ,
     Spinor[Momentum[pj_], 0, fq___]]*
    DOT[Spinor[Momentum[pl_], 0, fq___] , DiracGamma[Momentum[pi_]] ,
     Spinor[Momentum[pk_], 0, fq___]]
       ] := Contract[ m (
   -((DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pl]] ,
          Spinor[Momentum[pj], 0, fq]]*
         DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pj]] ,
          Spinor[Momentum[pk], 0, fq]]*Pair[Momentum[pi], Momentum[pk]])/
       Pair[Momentum[pj], Momentum[pl]]) +
    (DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
        Spinor[Momentum[pj], 0, fq]]*
       DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
        Spinor[Momentum[pk], 0, fq]]*
       (Pair[Momentum[pi], Momentum[pl]]*Pair[Momentum[pj], Momentum[pk]] +
         3*Pair[Momentum[pi], Momentum[pk]]*
          Pair[Momentum[pj], Momentum[pl]] -
         Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]]))
      /(2*Pair[Momentum[pj], Momentum[pl]]) +
    (DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
        DiracGamma[5] , Spinor[Momentum[pj], 0, fq]]*
       DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
        DiracGamma[5] , Spinor[Momentum[pk], 0, fq]]*
       (-(Pair[Momentum[pi], Momentum[pl]]*
            Pair[Momentum[pj], Momentum[pk]]) +
         Pair[Momentum[pi], Momentum[pk]]*Pair[Momentum[pj], Momentum[pl]] +
        Pair[Momentum[pi], Momentum[pj]]*Pair[Momentum[pk], Momentum[pl]]))/
     (2*Pair[Momentum[pj], Momentum[pl]])
               ) ] /; First[
  DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pk]],
    Spinor[Momentum[pj], 0, fq]]*
         DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pi]] ,
          Spinor[Momentum[pk], 0, fq]]]===
    DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pk]],
    Spinor[Momentum[pj], 0, fq]];

sirlin2[ m_. *
  DOT[Spinor[Momentum[pi_], 0, fq___] , DiracGamma[Momentum[pk_]] ,
     DiracGamma[5] ,
     Spinor[Momentum[pj_], 0, fq___]]*
    DOT[Spinor[Momentum[pl_], 0, fq___] , DiracGamma[Momentum[pj_]] ,
         DiracGamma[5] ,
     Spinor[Momentum[pk_], 0, fq___]]
       ] := Contract[ m (
   DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pk]] ,
      Spinor[Momentum[pj], 0, fq]]*
     DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pj]] ,
      Spinor[Momentum[pk], 0, fq]] -
    DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      Spinor[Momentum[pj], 0, fq]]*
     DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      Spinor[Momentum[pk], 0, fq]]*Pair[Momentum[pj], Momentum[pk]] +
    DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      DiracGamma[5] , Spinor[Momentum[pj], 0, fq]]*
     DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      DiracGamma[5] , Spinor[Momentum[pk], 0, fq]]*
     Pair[Momentum[pj], Momentum[pk]]
             )      ];

sirlin2[ m_. *
DOT[Spinor[Momentum[pi_], 0, fq___] , DiracGamma[Momentum[pk_]] ,
     DiracGamma[5] ,
     Spinor[Momentum[pj_], 0, fq___]]*
DOT[Spinor[Momentum[pl_], 0,fq___], DiracGamma[Momentum[pi_]] ,
      DiracGamma[5] ,
     Spinor[Momentum[pk_], 0, fq___]]
       ] :=  Contract[ m (
   -(DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pk]] ,
        Spinor[Momentum[pj], 0, fq]]*
       DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pi]] ,
        Spinor[Momentum[pk], 0, fq]]) +
    DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      Spinor[Momentum[pj], 0, fq]]*
     DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      Spinor[Momentum[pk], 0, fq]]*Pair[Momentum[pi], Momentum[pk]] +
    DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      DiracGamma[5] , Spinor[Momentum[pj], 0, fq]]*
     DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      DiracGamma[5] , Spinor[Momentum[pk], 0, fq]]*
     Pair[Momentum[pi], Momentum[pk]]
              ) ];

sirlin2[ m_. *
DOT[Spinor[Momentum[pi_], 0, fq___] , DiracGamma[Momentum[pl_]] ,
     DiracGamma[5] ,
     Spinor[Momentum[pj_], 0, fq___]]*
DOT[Spinor[Momentum[pl_], 0, fq___] , DiracGamma[Momentum[pj_]] ,
        DiracGamma[5] ,
     Spinor[Momentum[pk_], 0, fq___]]
       ] := Contract[ m (
   -(DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[Momentum[pl]] ,
        Spinor[Momentum[pj], 0, fq]]*
       DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[Momentum[pj]] ,
        Spinor[Momentum[pk], 0, fq]]) +
    DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      Spinor[Momentum[pj], 0, fq]]*
     DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      Spinor[Momentum[pk], 0, fq]]*Pair[Momentum[pj], Momentum[pl]] +
    DOT[Spinor[Momentum[pi], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      DiracGamma[5] , Spinor[Momentum[pj], 0, fq]]*
     DOT[Spinor[Momentum[pl], 0, fq] , DiracGamma[LorentzIndex[la]] ,
      DiracGamma[5] , Spinor[Momentum[pk], 0, fq]]*
     Pair[Momentum[pj], Momentum[pl]]
              ) ];

 (* #################################################################### *)
 (*                             Main447                                  *)
 (* #################################################################### *)

dig[LorentzIndex[a_,___]]:=a;
dig[Momentum[a_,___]]:=a;
dig[x_]:=x/;(Head[x]=!=LorentzIndex)&&(Head[x]=!=Momentum);
dig[n_?NumberQ]:={};
getV[x_List]:=Select[Flatten[{x}/.DOT->List]/.DiracGamma -> dige ,
		     Head[#]===dige&]/.dige->dig;

(* Get a list of equal gamma matrices *)
schnitt[x___][y___]:=Intersection[
Select[Flatten[{x}/.DOT->List],!FreeQ[#,LorentzIndex]&],
Select[Flatten[{y}/.DOT->List],!FreeQ[#,LorentzIndex]&]
                                 ];

(* get a list of not equal slashes and matrices *)
comp[x___][y___]:=Select[ Complement[Flatten[Union[{x},{y}]/.DOT->List],
                             schnitt[x][y] ],
                          !FreeQ2[#, {LorentzIndex, Momentum}]&
                        ];

(* sirlin1def *)
(* do some ordering with sirlin1 ... *)
   sirlin1[m_. DOT[Spinor[p1__], (gam1__) , Spinor[p2__]] *
               DOT[Spinor[p3__], (gam2__) , Spinor[p4__]]
          ] :=  MemSet[sirlin1[m DOT[Spinor[p1],gam1,Spinor[p2]] *
                               DOT[Spinor[p3],gam2,Spinor[p4]]
                              ],
Block[{schnittmenge, compmenge, result,order, orderl,orderr},
                      schnittmenge = schnitt[gam1][gam2];
                       compmenge   = comp[gam1][gam2];
                        leftind    = comp[gam1][schnittmenge];
                        rightind   = comp[gam2][schnittmenge];
print3["entering sirlin1"];
(* We need at least two dummy indices for the sirlin relations *)
                 If[ Length[schnittmenge] > 1,

(* Test for eq. (12) *)
    If[(Length[schnittmenge] === 3) && (Length[compmenge] > 3),
       orderl = Join[ Drop[leftind, {1,2}], {schnittmenge[[1]],
                      leftind[[1]], schnittmenge[[2]],
                      leftind[[2]], schnittmenge[[3]]}
                    ] // getV;
       orderr = Join[ Drop[rightind, {1,2}], {schnittmenge[[1]],
                      rightind[[1]], schnittmenge[[2]],
                      rightind[[2]], schnittmenge[[3]]}
                    ] // getV;
       result =
       Expand[m Contract[
                 DiracOrder[ DOT[Spinor[p1],gam1,Spinor[p2]], orderl ]*
                 DiracOrder[ DOT[Spinor[p3],gam2,Spinor[p4]], orderr ] ]
             ]//sirlin2
       ];


(* ... *)
 (* Test for eq. (8) *)
    If[(Length[schnittmenge] === 2) && (Length[compmenge] > 1),
       order = Join[{First[schnittmenge]}, compmenge,
                    {Last[schnittmenge]} ] // getV;
       result = sirlin2[ Expand[ m  DiracOrder[
                         DOT[Spinor[p1],gam1,Spinor[p2]] *
                         DOT[Spinor[p3],gam2,Spinor[p4]], order]
                                                ]//Contract
                       ]
       ];
                ];
           If[!ValueQ[result],
              result = sirlin2[m *
                         DOT[Spinor[p1],gam1,Spinor[p2]] *
                         DOT[Spinor[p3],gam2,Spinor[p4]]
                                     ]
             ];
print3["exiting sirlin1"];
           result]] /; !FreeQ[{gam1}, LorentzIndex];


(*ChisholmSpinordef*)
 dsimp[x_]:=sirlin0[spcev0[x]];
 ChisholmSpinor[x_, choice_:0]:=MemSet[ChisholmSpinor[x,choice],
                             Block[{new=x, indi},
print3["entering ChisholmSpinor "];
  new = DotSimplify[new];
  If[choice===1, new = new/.{ DOT[Spinor[a__],b__ ,Spinor[c__]] *
                              DOT[Spinor[d__],e__ ,Spinor[f__]]:>
                             DOT[nospinor[a],b,nospinor[c]] *
                              DOT[Spinor[d],e,Spinor[f]]
                            }
    ];
  If[choice===2, new = new/.{ DOT[Spinor[a__],b__ ,Spinor[c__]] *
                              DOT[Spinor[d__],e__ ,Spinor[f__]]:>
                              DOT[Spinor[a],b,Spinor[c]] *
                              DOT[nospinor[d],e,nospinor[f]]
                            }
    ];

                    dsimp[Contract[dsimp[new/.{
               (DOT[Spinor[pe1_, m_, ql___] , DiracGamma[lv_[k_]] , h___ ,
                Spinor[pe2_, m2_, ql___]]) :> Block[{indi},
                      indi = Unique["alpha"];
     -1/Pair[pe1,pe2] ( DOT[Spinor[pe1, m, ql], DiracGamma[pe1],
                        DiracGamma[lv[k]] , DiracGamma[pe2],h,
                        Spinor[pe2, m2, ql]] -
                        Pair[pe1,lv[k]] DOT[Spinor[pe1, m, ql],
                            DiracGamma[pe2], h , Spinor[pe2, m2, ql]] -
                        Pair[lv[k],pe2] DOT[Spinor[pe1, m, ql],
                            DiracGamma[pe1] , h , Spinor[pe2, m2, ql]]-
                          I Eps[pe1,lv[k],pe2,LorentzIndex[indi]] *
                        DOT[Spinor[pe1, m, ql],
                            DiracGamma[LorentzIndex[indi]],
                            DiracGamma[5],h,
                        Spinor[pe2, m2, ql]]
                      )] }/.nospinor->Spinor], EpsContract->True] ] ]];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSimplify | \n "]];
Null
