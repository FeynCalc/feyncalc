(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynRule *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 12 March '98 at 0:04 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: derivation of feynman rules via functional differentiation *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FeynRule`",
             "HighEnergyPhysics`FeynCalc`"];

FeynRule::"usage"=
"FeynRule[lag, {fields}] gives the Feynman rule corresponding
to the field configuration fields of the lagrangian lag.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
FeynCalcForm = MakeContext["FeynCalcForm"];

Anti5 = MakeContext["Anti5"];
Cases2 = MakeContext["Cases2"];
Collect2    := Collect2 = MakeContext["Collect2"];
Contract    := Contract = MakeContext["Contract"];
CovariantD := CovariantD = MakeContext["CovariantD"];
DotSimplify  = MakeContext["DotSimplify"];
DiracGamma   = MakeContext["DiracGamma"];
diractrick  := diractrick = MakeContext["DiracTrick"];
Eps          = MakeContext["Eps"];
EpsEvaluate  = MakeContext["EpsEvaluate"];
Expanding                = MakeContext["Expanding"];
Expand2 = MakeContext["Expand2"];
ExpandScalarProduct      = MakeContext["ExpandScalarProduct"];
Explicit    := Explicit  = MakeContext["Explicit"];
Factor1     := Factor1   = MakeContext["Factor1"];
Factor2     := Factor2   = MakeContext["Factor2"];
Factoring   := Factoring = MakeContext["Factoring"];
fci         := fci       = MakeContext["FeynCalcInternal"];
fce          = MakeContext["FeynCalcExternal"];
FieldStrength := FieldStrength = MakeContext["FieldStrength"];
FinalSubstitutions = MakeContext["FinalSubstitutions"];
FreeQ2       = MakeContext["FreeQ2"];
FunctionalD  = MakeContext["FunctionalD"];
lorind       = MakeContext["LorentzIndex"];
mom          = MakeContext["Momentum"];
MemSet       = MakeContext["MemSet"];
NTerms       = MakeContext["NTerms"];
OPEDelta     = MakeContext["OPEDelta"];
OPEi        := OPEi = MakeContext["OPEi"];
OPEj        := OPEj = MakeContext["OPEj"];
OPEk        := OPEk = MakeContext["OPEk"];
OPEl        := OPEl = MakeContext["OPEl"];
OPEm        := OPEm = MakeContext["OPEm"];
OPESum      := OPESum = MakeContext["OPESum"];
OPESumSimplify := OPESumSimplify = MakeContext["OPESumSimplify"];
OPESumExplicit := OPESumExplicit= MakeContext["OPESumExplicit"];
Pair         = MakeContext["Pair"];
PairContract = MakeContext["PairContract"];
PartialD      := PartialD = MakeContext["PartialD"];
Power2       := Power2 = MakeContext["Power2"];
PowerFactor  := PowerFactor= MakeContext["PowerFactor"];
PowerSimplify:= PowerSimplify = MakeContext["PowerSimplify"];
RightPartialD      := RightPartialD = MakeContext["RightPartialD"];
QuantumField = MakeContext["QuantumField"];
ExpandPartialD = MakeContext["ExpandPartialD"];
Schouten  := Schouten = MakeContext["Schouten"];
SelectFree      = MakeContext["SelectFree"];
SelectNotFree      = MakeContext["SelectNotFree"];
sund        := sund = MakeContext["SUNDelta"];
sundc        := sundc = MakeContext["SUNDeltaContract"];
SUNIndex       = MakeContext["SUNIndex"];
sunf         = MakeContext["SUNF"];
SUNT         = MakeContext["SUNT"];
sunftotraces := sunftotraces = MakeContext["Explicit"];
sunsimplify  := sunsimplify  = MakeContext["SUNSimplify"];
SymbolicSum2 := SymbolicSum2 = MakeContext["SymbolicSum2"];
UnDeclareNonCommutative := UnDeclareNonCommutative = 
  MakeContext["UnDeclareNonCommutative"];
ZeroMomentumInsertion = MakeContext["ZeroMomentumInsertion"];
ExplicitSUNIndex     = MakeContext["ExplicitSUNIndex"];


(* Functions that are applied to the expression first.
Added 3/8-2000 by Frederik Orellana to allow interoperability with Phi:
InitialFunction could e.g. be set to PhiToFC *)

InitialFunction = MakeContext["InitialFunction"];


(* ******************************************************************** *)
 lorunique[a_] := lorunique[a] = lorind[Unique["Global`li"]];
 sununique[a_] := sununique[a] = SUNIndex[Unique["Global`si"]];

(*Added ExplicitSUNIndex. F.Orellana, 16/9-2002*)

  pluc[xx__] := If[!FreeQ[{xx}, SUNIndex],
    Map[(#/.Plus->((Factor1 /@ Collect[Plus[##],Variables[Plus[##]]] )&))&, 
        Factor1 /@ Collect2[Plus[xx], SUNIndex|ExplicitSUNIndex, Factoring -> False]],
         Map[Factor1, 
           Collect2[Plus[xx], {Pair[lorind[_], lorind[_]] },
                    Factoring->False] ]
                  ];

frex[nl_] := frex[nl] = Block[
  {nla = DotSimplify[nl],sdum, flag, ff, fm,ff1, cli,tem,
   nlafirst, newlorlist, lorindlist, sunindlist, newsunlist},
  sdum = SUNIndex[ToExpression[StringJoin @@         (
                  ToString /@ {Unique[System`D], "k"})]
                 ];
  flag = Select[Expand2[Select[nla, FreeQ[#, DOT]&], sunf] + null1 + null2, 
                (Count[#, sunf[__]] === 2)&
               ] /. null1 ->1 /. null2 ->0;
  If[flag =!= 0, If[Head[flag] === Times, 
                    ff = Select[flag, !FreeQ[#, sunf]&],
                    If[Head[flag] === Plus,
                       ff = Select[flag[[1]], !FreeQ[#, sunf]&];
                      ];
                   ];
                 If[Length[ff] === 2,
                    ff1 = List @@ ff[[1]]; ff2 = List@@ff[[2]];
                    cli = Complement[ff1, Complement[ff1, ff2]];
                    If[Length[cli] > 0, sdum = cli[[1]]];
                   ];
     ];
(* change 05/94 *)

nlafirst = If[Head[nla]===Plus, nla[[1]], nla];
(* get a list of all LorentzIndex *)
lorindlist = Cases2[nla, lorind];
sunindlist = Cases2[nla, SUNIndex|ExplicitSUNIndex];
(* select those which occur an even number of times *)
newlorlist = {};
newsunlist = {};
For[iij = 1, iij <= Length[lorindlist], iij++,
    If[EvenQ[Length[Position[nlafirst, lorindlist[[iij]]]]],
       AppendTo[newlorlist, lorindlist[[iij]]]
      ];
   ];
For[iij = 1, iij <= Length[sunindlist], iij++,
    If[EvenQ[Length[Position[nlafirst, sunindlist[[iij]]]]],
       AppendTo[newsunlist, sunindlist[[iij]]]
      ]
   ];
uniquelist = Join[Table[newlorlist[[ij]] -> 
                        (newlorlist[[ij]]/.lorind -> lorunique),
                        {ij, Length[newlorlist]}
                       ],
                  Table[newsunlist[[jj]] -> 
                        (newsunlist[[jj]]/.SUNIndex -> sununique),
                        {jj, Length[newsunlist]}
                       ]
                 ];
If[$VeryVerbose > 0, Print["uniquelist = ", uniquelist]];

nla = nla /. uniquelist;

  nla = DotSimplify[nla, Expanding -> True];
  tem = Contract[Expand2[nla /. QuantumField -> 
        (QuantumField[##][]&) ]] + null1;
 fm/: (fm[aa___][bb___] * fm[xx___][yy___] )   := fm[aa][bb]**fm[xx][yy];
 fm/: fm[aa___][bb___]^n_Integer?Positive := 
  (fm[aa][bb]^(n-1))**fm[aa][bb];
 tem = tem /. QuantumField -> fm /. fm -> QuantumField;
{tem, sdum}];

fcis[x_] := fcis[x] = fci[x];

getpes[__][pe_] := pe/. mom -> Identity;

enm[h_Plus, pel_List] := FixedPoint[enm2[#, pel]&, h, 5];

enm2[ha_, pl_List] := MemSet[enm2[ha,pl], ha] /; Head[ha] =!= Plus;
enm2[ha_Plus, pl_List] := MemSet[enm2[ha,pl],
Block[{ttt,nn, nnli, pall = Plus@@pl, i},
 ttt = Catch[
(* check for 1-term *)
             For[i = 1, i<=Length[pl], i++,
                 If[!FreeQ[ha, pl[[i]]],
                    nn = Expand2[ExpandScalarProduct[ha /. pl[[i]]->
                                  (-(pall-pl[[i]]))]
                               ];
                    If[Head[nn] =!= Plus, Throw[nn]];
                   ]
                ];
(* check for smaller length *)
             nnli = {};
             For[i = 1, i<=Length[pl], i++,
                 If[!FreeQ[ha, pl[[i]]],
                    nn = Expand2[ExpandScalarProduct[ha /. pl[[i]] ->
                                                 (-(pall-pl[[i]]))]
                               ];
                    If[(NTerms[nn] < NTerms[ha]),
                       AppendTo[nnli, nn];
                      ]
                   ];
                ];
             If[Length[nnli] > 0, Throw[Sort[nnli][[1]]]];
(* check for equal length *)
             nnli = {ha};
             For[i = 1, i<=Length[pl], i++,
                 If[!FreeQ[ha, pl[[i]]],
                    nn = Expand2[ExpandScalarProduct[ha /. pl[[i]] ->
                                                 (-(pall-pl[[i]]))]
                               ];
                    If[(NTerms[nn] ===  NTerms[ha]),
                       AppendTo[nnli, nn];
                      ]
                   ];
                ];
                Throw[Last[Sort[nnli]]];
(*
             If[SelectNotFree[nnli,-1] =!= {},
                Throw[Sort[SelectNotFree[nnli,-1]][[1]]],
                Throw[Sort[nnli][[1]]]
               ];
*)
             ha
            ];             ttt]];
enm3[ha_Plus, pl_List] := 
If[$NONZERO === True, ha,
MemSet[enm3[ha,pl],
  Block[{ttt,nn, nnli, pall = Plus@@pl, i},
 ttt = Catch[
(* check for equal length *)
             nnli = {ha};
             For[i = 1, i<=Length[pl], i++,
                 If[!FreeQ[ha, pl[[i]]],
                    nn = Expand2[ExpandScalarProduct[ha /. pl[[i]] ->
                                                 (-(pall-pl[[i]]))]
                               ];
                    If[(NTerms[nn] === NTerms[ha]),
                       AppendTo[nnli, nn];
                      ]
                   ];
                ];
             If[SelectNotFree[nnli,-1] =!= {},
                Throw[Sort[SelectNotFree[nnli,-1]][[1]]],
                Throw[Sort[nnli][[1]]]
               ];
             ha
            ];             ttt]]
  ];

            
enmomcon[aa_, pli_List] := 
If[$NONZERO === True, aa,
PowerSimplify[
 aa /. {Power2[h_Plus,w_] :> Power2[enm2[h,pli],w]} /.
       {Power2[-1,po_] :> (-1)^po} /.
       {Power[h_Plus,w_ /; Head[w] =!= Integer] :> 
        Power[enm2[h,pli],w]}            ]
  ];

enmomcon3[aa_, pli_List] := aa /. Power2[h_Plus,w_] :> 
    PowerSimplify[Power2[enm3[h,pli],w]];

sumtrick1[ex_, {i_,0,j_}, {j_, 0, n_}] := 
 sumtrick1[ex, {j,0,n}, {i,0,j}];

sumtrick1[ex_, {j_, 0, n_}, {i_, 0, j_}] := 
MemSet[sumtrick1[ex, {j,0,n}, {i,0,j}],
OPESumSimplify[
OPESum[
 PowerSimplify[
  PowerSimplify[ex] /. { ((-1)^(i+em_.)*
                        pow_[(a_+b_) , w_ /; (w===(j-i))] *
                        pow_[a_ /; !FreeQ[a, OPEDelta],
                             v_ /; (v === (n-j))] *
                        pow_[c_ /; !FreeQ[c, OPEDelta],
                             i]
                       ) :> 
                       ((-1)^(n+em+j) pow[a+b, j-i] pow[a, i] pow[c, n-j]
                       ) /; ( ((pow === Power) || (pow === Power2)) &&
                              FreeQ[em, i]
                            )
                      }                                  
             ], {j,0,n}, {i, 0, j}] ]];

sumtrick1[ex_, {i_, 0, n_}] := MemSet[sumtrick1[ex, {i,0,n}],
OPESumSimplify[ OPESum[
PowerSimplify[
 PowerSimplify[ex] /. {(-1)^(i+em_.)*
                       (
                        pow_[a_ /; !FreeQ[a, OPEDelta], 
                          v_ /;(Variables[v]==={i})] * 
                        pow_[b_ /; !FreeQ[b, OPEDelta], 
                          w_/;(Variables[w] === Variables[{i,n}])]
                       ) :> (((-1)^n (-1)^(i+em) pow[a, v + n - 2 i]*
                                                 pow[b, w - n + 2 i] 
                             ) /. (-1)^(xa_ + 2 xb_) :> ((-1)^xa /; 
                                   MemberQ[{OPEi,OPEj,OPEk,OPEl,OPEm},xb]
                                                      )
                            ) /; (!OrderedQ[{a,b}]) && 
                                 ((pow === Power) || (pow === Power2)) &&
                                 FreeQ[em, i]
                      }                      ], {i,0,n}]]];

suback[a_,___] := a;
(*
sutr[_][x_,_,__] := x;
sutr[pli_List][x_, {i_, 0, j_}] := 
*)

sutr[pli_List][x_, {i_, 0, j_}, b__] := 
MemSet[sutr[pli][x,{i,0,j}, b],
Block[{xx=x, te},
 te =  If[FreeQ[x, (-1)^(_. i + em_.)], enmomcon[x, pli], x];
(*
If[te =!= x,
Print["sutred ", xx//FeynCalcForm , "  -->  ",te//FeynCalcForm];
 ];*) te]];

sutr[pli_List][x_, {i_, 0, j_}] := 
MemSet[sutr[pli][x,{i,0,j}],
Block[{xx=x, te},
 te =  If[FreeQ[x, (-1)^(_. i + em_.)], enmomcon3[x, pli], x ];
(*
If[te =!= x,
Print["sutred ", xx//FeynCalcForm , "  -->  ",te//FeynCalcForm];
 ];*) te]];


opesback[y_Times, fi_List] := 
SelectFree[y, {Power2, OPEi, OPEj, OPEk, OPEl, OPEm}] *
opesbac3[SelectNotFree[y, {Power2, OPEi, OPEj, OPEk, OPEl, OPEm}], fi];

opesbac3[a__] := MemSet[opesbac3[a], opesbac2[a]];

opesbac2[1,__] = 1;

opesbac2[y_ opes[b__List], fi_List] :=  
( sumtrick1[sutr[fi /. QuantumField -> getpes
                ][enmomcon[y, fi /. QuantumField -> getpes], b]
                   , b
           ] /. OPESum -> sumtrick1 /. sumtrick1 -> suback
 ) /; FreeQ[y, opes];

opesbac2[y_ opes[b__List] opes[c__List], fi_List] := 
(
enmomcon[
sutr[fi /. QuantumField -> getpes
    ][
      sumtrick1[ enmomcon[y, fi /. QuantumField -> getpes
                         ], b, c
               ] /. OPESum -> sumtrick1 /. sumtrick1 -> suback,
              b, c
     ],  fi /. QuantumField -> getpes
        ] /. OPESum -> sumtrick1 /. sumtrick1 -> suback
 ) /; FreeQ[y, opes];

opesbac2[y_ opes[a__List] opes[b__List] opes[c__List], fi_List] :=  
 ( sumtrick1[enmomcon[y, fi /. QuantumField -> getpes], a, b, c
            ] /. OPESum -> sumtrick1 /. sumtrick1 -> suback
 ) /; FreeQ[y, opes];

opesbac2[y_ /; FreeQ[y, opes], fi_List] := 
           enmomcon5[enmomcon[y, fi/.QuantumField -> getpes],
                                 fi/.QuantumField -> getpes
                    ];
(* only for Eps *)
enmomcon5[xx_, pli_List] := If[ 
   ((Plus@@pli) === 0) || (Head[Expand[Last[pli]]] === Plus) ||
   FreeQ[xx, Eps], PowerSimplify[xx],
     PowerSimplify[ExpandScalarProduct[xx /. Last[pli] :>
                     (-(Plus@@Drop[pli,-1]))]]
                              ];

 dirdot[yy_] := If[FreeQ[yy, DOT], yy,
                    If[FreeQ[yy, DiracGamma],
                       DotSimplify[yy, Expanding -> False],
                       diractrick[yy]
                      ]
                   ];
opsum[y_] := 
  y //. {OPESum[w_ /; FreeQ[SelectFree[w, OPESum], Binomial], b__List] :> 
         (w opes[b]) /; (Head[w] === Times || Head[w] === OPESum ||
                         Head[w] === DOT),
         OPESum[b__List] :> opes[b]
        };

Options[FeynRule] = {Anti5 -> -Infinity,
                     Contract -> False,
                     Factor1 -> False,
                     FinalSubstitutions -> {}, 
                     PartialD -> RightPartialD,
                     Schouten -> False,
                     ZeroMomentumInsertion -> True,
                     InitialFunction -> Identity
                    };

(*FeynRuledef*)
FeynRule[a_,b_ /; Head[b] =!=Rule && Head[b]=!= List, c___,
            d_ /; Head[d] =!= Rule && Head[d] =!= List, e___Rule
        ] := FeynRule[a, {b,c,d}, e];

 FeynRule[lag_, fii_List, ru___Rule] := 
  If[Length[lag] === 0, Print["well well ", lag, " does not look like a lagrangian"], 
                             Block[{(*InitialFunction stuff added by F.Orellana 3/8-2000*)
			            initf = InitialFunction /. {ru} /. Options[FeynRule],
			            nlag = fcis[initf[lag]], temp1, temp,
                                    fili = fii, lfili, qli,specope,
                                    groupindices,
                                    result,fields,tfields,plist,
                                    vert,sdummy, getsu,gsu,subs,
                                    qfi, qqq, oldnoncomm, onepm, onemm, 
                                    plho,schouten,opsumb,opsumb2,
                                    $binindices, indd, oplei,anti5,
                                    leib,coup,cdp,cedepe,opexbin,
                                    zeromomentum,partiald
                                    },
anti5    = Anti5 /. {ru} /. Options[FeynRule];
subs     = FinalSubstitutions /. {ru} /. Options[FeynRule];
schouten = Schouten /. {ru} /. Options[FeynRule];
zeromomentum = ZeroMomentumInsertion /. {ru} /. Options[FeynRule];
partiald = PartialD /. {ru} /. Options[FeynRule];
SetOptions[CovariantD, PartialD -> partiald];

(* somewhat hacky, ... *)
If[zeromomentum === False, $NONZERO = True, $NONZERO = False];


(* remeber which indices are around *)
$binindices = {};
oplei[Binomial[up_, ind_] w_, {ind_, 0, up_}] := 
( If[!MemberQ[$binindices, ind], AppendTo[$binindices, ind]];
  sumBinomial[up, ind] w);

leib[y_] := y /. OPESum -> oplei /. oplei -> OPESum;

  fields = Map[ ( QuantumField[___, #, 
           Pattern @@ {Unique[dm], ___}][___])&, #[[0, 1]]& /@ fili];

If[!FreeQ[nlag, Sum], nlag = nlag /. Sum -> OPESum];
If[$VeryVerbose > 0, Print["non-commutative expansion"]];
If[!FreeQ[nlag, OPESum], nlag = opsum[nlag]];

If[!FreeQ[nlag, OPEDelta],
   nlag = nlag /. PartialD[mom[OPEDelta]]^(mm_/; Head[mm]=!=Integer):>
                  PartialD[mom[OPEDelta]^mm]
  ];

 If[CheckContext["FieldStrength"],
    nlag = nlag /. FieldStrength[a__] :> 
                   FieldStrength[a, Explicit->True];
   ];
(* CHANGE 28.6.94 *)
If[!CheckContext["CovariantD"], 
   nlag = DotSimplify[nlag]
   ,
 If[!FreeQ[nlag, CovariantD[w__/;FreeQ[{w}, Rule]
                           ]^hh_ /; Head[hh]=!=Integer]
    ,
cdp /: cdp[aa__]^(h_ /; Head[h]=!=Integer) := 
   cedepe[aa, {h, Length[fii] - 2}];


    nlag = nlag /. CovariantD -> cdp /. 
          {cdp[aa__] :> CovariantD[aa, Explicit->True], 
           cedepe :> CovariantD
          };
    coup = CouplingConstant /. Options[CovariantD];
    nlag = DotSimplify[nlag] /.coup^(nn_ /; nn>(Length[fii]-2)) :> 0;
    ,
    nlag = DotSimplify[nlag];
  ]
 ];
If[!FreeQ[nlag, sund],
   nlag = Expand2[nlag, SUNIndex|ExplicitSUNIndex]/.sund-> sundc/.
   sundc->sund 
  ];


If[$VeryVerbose > 0, Print["Leibniz rule"]];
nlag = ExpandPartialD[nlag](* /. DOT -> dotsunt /. dotsunt -> DOT*);

If[$VeryVerbose > 0, Print["Leibniz rule done "]];

(* check for Leibniz - sums *) (* trick17 *)
If[!FreeQ[nlag, Binomial], nlag  = leib[nlag]//opsum ];

nlag = ExpandPartialD[nlag];

temp1 = frex[nlag];
temp = temp1[[1]];
sdummy = temp1[[2]];

vert = Select[temp, (Length[Position[#, QuantumField]]===
                     Length[fields]) &];
tfields = fields;
vert = vert + null1 + null2;
While[(Length[tfields] > 0) && (Head[vert] === Plus),
      vert = Select[vert, !FreeQ[#, First[tfields]]&];
      tfields = Rest[tfields];
     ];
(* there might be still a sum ... *)
 If[Head[vert] === Plus,
    qfi[___PartialD, fiii_, ___lorind, ___SUNIndex|___ExplicitSUNIndex][___] := qqq[fiii];
    qfi[___PartialD, fiii_, ___mom, ___SUNIndex|___ExplicitSUNIndex][___] := qqq[fiii];
    qfi[___BlankNullSequence, fiii_, ___Pattern][___] := qqq[fiii];
    puref = (Sort[Select[Variables[# /.   QuantumField -> qfi /.
                                              (*Plus-> List /.*)
                                               {OPESum[aaa_,__]:>aaa} /.
                                               DOT -> Times /.
                                               NonCommutativeMultiply ->
                                               Times
                                  ]//Flatten//Union,
                                      Head[#]===qqq&
                             ]      ] ===
                               Sort[Variables[fields /. QuantumField -> qfi]]
                        )&;
    vert = Select[vert, puref];
   ];


 If[vert === 0, result = 0,
 vert = vert /. NonCommutativeMultiply -> Times;
vert = Expand[ sunsimplify[dirdot[vert],Explicit->False] ];
If[$VeryVerbose > 0, Print["functional differentiation "]];

groupindices = Map[First,Cases2[{vert,fili},{SUNIndex,ExplicitSUNIndex,LorentzIndex}]];
UnDeclareNonCommutative[groupindices];

If[Head[vert] === Plus,
   result = 0;
   For[iij = 1, iij <= Length[vert], iij++,
       If[$VeryVerbose > 1, Print["iij of FunctionalD = ",iij,
          " out of ", Length[vert]]];
       result = result +
       DotSimplify[FunctionalD[vert[[iij]], fili],
                   Expanding -> False
                  ] /. (a_ /; (a =!= (-1)))^
         (w_ /; (Head[w] =!= Integer)) :> Power2[a, w];
      ];
  ,
result = DotSimplify[FunctionalD[vert, fili],Expanding -> False
                    ] /. (a_ /; (a =!= (-1)))^
         (w_ /; (Head[w] =!= Integer)) :> Power2[a, w];
  ];

If[$VeryVerbose > 0, Print["functional differentiation done"]];

qli[a__,el__LorentzIndex, ___][_] := {el};
lfili  = Flatten[fili /. QuantumField  -> qli];

result = Expand2[result/.sund->sundc/.sundc->sund];


If[$VeryVerbose > 0, Print["there are now ",Length[result]," terms"]];
result = result /. Pair -> PairContract /. Pair -> PairContract /.
          PairContract -> Pair;
If[$VeryVerbose > 0, Print["simple Contraction done"]];
If[!FreeQ[result, Eps], result = EpsEvaluate[result];
   If[$VeryVerbose > 0, Print["EpsEvaluate done"]];
  ];
If[!FreeQ[result, DOT],
   result = dirdot[result];
   If[$VeryVerbose > 0, Print["dirdot done"]];
  ];

 If[$VeryVerbose > 0, Print["cases "]];
alllor = Union[Cases[result, LorentzIndex[__], Infinity]];
 If[$VeryVerbose > 0, Print["cases done"]];
If[alllor =!= Sort[lfili],
If[(Contract /. Options[FeynRule]) === True,
   If[$VeryVerbose > 0, Print["Contracting again"]];
   result = result// Contract;
   If[$VeryVerbose > 0, Print["Contracting again done"]];
  ]
  ];
   If[$VeryVerbose > 0, Print["PowerSimplify"]];
result = PowerSimplify[result /. subs];
   If[$VeryVerbose > 0, Print["PowerSimplify done"]];
(*
result = Contract[dirdot[PowerSimplify[result]] /. dDelta -> Pair /.subs];
*)
If[!FreeQ[result, Eps], result = EpsEvaluate[result]];
If[!FreeQ[result, DOT],
   result = DotSimplify[result, Expanding -> False]
  ];
If[$VeryVerbose > 0, Print["there are ", Length[result], " terms "]];
If[!FreeQ[result, sund],
   result = result /. sund -> sundc /. sundc -> sund;
  ];
If[$VeryVerbose > 0, Print["several simplications done"]];
plist =  fii /. QuantumField -> getpes /. mom -> Identity;
(*
 If[ Length[fili] === 2, 
     result = dirdot[result];
     If[!FreeQ[result, DiracGamma] ]
   ]
*);
(*
If[Length[fili] > 2, result = I result];
result = I result;
*)
result = I result;

(* FISHY!!!!!!!!!!!!*)
If[(!FreeQ[result, OPEDelta])(* && (Length[plist] < 4)*),
   result = -(*I*) result;
  ];

result = Expand[result] /. Power2 -> Power;
If[!FreeQ[result, sunf],
   result = sunsimplify[result,Explicit->False];
   If[!FreeQ[result, SUNT], result = sunsimplify[result,Explicit->False]]
  ];
result = PowerSimplify[result]//Expand;
If[$VeryVerbose > 0, Print["expansion done; length = ",Length[result] ]]; 
If[!FreeQ[result, sumBinomial],
   $binindices = Reverse[ Sort[$binindices] ];
   opexbin[a_, b_] := Factor2[PowerSimplify[SymbolicSum[a,b]
                             ]             ] /; !FreeQ[a,Binomial];
   For[ib = 1, ib <= Length[$binindices], ib++,
If[$VeryVerbose > 0, Print["ib = ",ib, " out of ", Length[$binindices]]];
       indd = $binindices[[ib]];

       If[!FreeQ[result, sumBinomial],
          Clear[opsumb, opsumb2];
          opsumb[xx_Plus] := Map[opsumb, xx];
          opsumb[xx_] := If[FreeQ[xx, indd], xx,
                            SelectFree[xx, indd] *
                            opsumb2[SelectNotFree[xx,indd]]
                           ];
          opsumb2[xx_. sumBinomial[up_, indd]] := 
              SymbolicSum2[xx Binomial[up,indd], {indd,0,up}
                          ] /. SymbolicSum2 -> SymbolicSum /. 
               SymbolicSum ->  OPESum;

          result = opsumb[result] /. opsumupv -> OPESum;
If[Length[result] < 500,
If[$VeryVerbose > 0, Print["before OPESumSimplify"]];
          result = OPESumSimplify[result];
  ];
          result = result /. OPESum -> opexbin /. opexbin -> OPESum;
        ]; 
    ];
     If[!FreeQ[result, opes], 
        result = Collect2[result, opes, Factoring->False]];
  ];
If[$VeryVerbose > 0, Print["sumBinomial  ",FreeQ[result,sumBinomial]]];
result = result /. (a_ /; (a =!= (-1)))^
         (w_ /; (Head[w] =!= Integer)) :> Power2[a, w];

If[zeromomentum === True,
If[!FreeQ[result, OPEDelta],
powsu[aa_Plus] := Power2[ExpandScalarProduct[Plus[aa] /.
                         plist[[1]] :> (-(Plus @@ Rest[plist]))
                       ]];
result = PowerSimplify[result /. Power2 -> powsu /. powsu -> Power2];
  ];
  ];

If[!FreeQ[result, Power2],
   If[Head[result] =!= Plus, result = opesback[result, fii],
nres = 0;
For[ij=1, ij<=Length[result], ij++, 
If[$VeryVerbose > 0, Print["ij = ", ij]];
    nee = opesback[result[[ij]], fii];
    nres = nres  + nee;
   ];
result = nres;
     ]
  ];

If[$VeryVerbose > 0, Print["opesback done; "]];

(* in case the incoming momenta are a sum *)
If[!FreeQ[fii, Plus],
   result = ExpandScalarProduct[result]
  ];

If[!FreeQ[result, DiracGamma[5]], result = Anti5[result, anti5]];

result = Expand[result] /. {(-1)^a_      :> PowerSimplify[(-1)^a],
                            Power2[-1,a] :> PowerSimplify[(-1)^a]
                           } /. subs;

If[(!FreeQ2[result, {SUNT, OPESum, DiracGamma}]) &&
   Length[Cases2[result, OPESum]] <= 1,
If[$VeryVerbose>1, Print["YESSSSSSSSSSSSSS"]];
If[zeromomentum === True,
   result = Collect2[ExpandScalarProduct[result /. 
            plist[[1]] :> (-(Plus @@ Rest[plist]))],
                     OPESum]//Factor1;
  ];
   result = result /. Power2 -> Power;
   ,

If[!FreeQ[result, SUNIndex|ExplicitSUNIndex],
   result = Collect2[result, SUNIndex|ExplicitSUNIndex, Factoring -> False, 
                     Expanding -> False 
                    ];
  ];
If[$VeryVerbose > 0, Print["collect2ing done; "]];

If[LeafCount[result]<1000,
   result = OPESumSimplify[result];
  ];

If[(Length[plist]<4) && FreeQ[result, OPEDelta],
   result = Factor1[result];
  ];


If[FreeQ[result, OPEDelta], 
   result = result /. Plus :> pluc;
  ];
If[$VeryVerbose > 0, Print["feinarbeit "]];

If[((Factor1 /. {ru} /. Options[FeynRule]) === True),
If[(Head[result] === Plus) && 
   result  = Factor1[Map[#/.Plus->plho&, 
                         Map[feinarbeit[#, plist]&, result]
                        ]] /. plho -> Plus,
   result  = feinarbeit[result, plist]
  ];
  ];
];
];
If[LeafCount[result]<10^4,
   result//PowerSimplify//OPESumSimplify,
   result
  ]/.Power2->Power
]];
(* ******************************************************************** *)

feinarbeit[fey_Times, pl_List] := SelectNotFree[fey, SUNIndex|ExplicitSUNIndex]  *
          feinarbeit[SelectFree[fey,  SUNIndex|ExplicitSUNIndex], pl] /; !FreeQ[fey,
             SUNIndex|ExplicitSUNIndex];

feinarbeit[fey_ /; FreeQ[fey, SUNIndex|ExplicitSUNIndex], pli_List] := Block[
{uniqli,onepm, onemm, resu, legs, fop, foop, ores, nopres,
 ple,pleps,power3,schau},

If[FreeQ[fey, OPEDelta], resu = fey,

resu = fey;
 
legs = Length[pli];

If[FreeQ2[resu, {DiracGamma, Eps}],
   onepm /: onepm^2 = onepm;
   resu = resu /. {(-1)^OPEm :> (2 onepm -1)},
   onemm /: onemm^2 = onemm;
   resu = resu /. {(-1)^OPEm :> (-2 onemm +1)}
  ];

If[!FreeQ[resu, OPESum] && Head[resu] === Plus && legs < 5, 
   nopres = SelectFree[resu, OPESum];
   ores   = resu - nopres;
   nopres = nopres /. Power2 -> Power;
   ple[a__] := Collect2[Plus[a] /. Power -> power3 /.
      {power3[ab_ /; !FreeQ[ab, OPEDelta], OPEm-1] :> 
       (ab power3[ab, OPEm -2]) }, power3] /. power3 -> Power;

   pleps[xy_] := xy /. Plus -> ple;
   resu = pleps[nopres//Expand] + 
            Collect2[ores, OPESum, Factoring -> Factor2, 
                                     Expanding -> True];
   If[(!FreeQ[resu, Eps]) && schouten == True,
If[$VeryVerbose > 0 , Print["using Schouten identity "]];
      schau[y__] := MemSet[schau[y],
                   If[FreeQ2[{y},{(aa_ /; !FreeQ[aa, Pair])^
                                   (vv_ /; Head[vv] =!= Integer),      
                               Power2[aaa_ /; !FreeQ[aaa, Pair],
                                      vvv_ /; Head[vvv] =!= Integer
                                     ]
                                  }
                            ], Factor1[Schouten[Plus[y]]], Plus[y]
                      ]];
      resu  = resu /. Plus -> schau;
     ];

   fop[yy_] := If[Head[yy] =!= Times, yy,
                  SelectFree[yy, OPEDelta] *
                  foop[SelectNotFree[yy, OPEDelta]]
                 ];
   resu = Map[fop, resu];
  ];

If[legs<5,
 resu = Factor2[resu /. Complex[0,be_]->(be iI)
               ] /. {(onepm (-1)^OPEm) :>   onepm } /.
                    {(onemm (-1)^OPEm) :> (-onemm)} /.
                    {iI^(2 a_)  :> (-1)^a} /.
                     { iI :> I, (*Plus :> pluc, *)
                       onepm :> ((1+(-1)^OPEm)/2),
                       onemm :> ((1-(-1)^OPEm)/2)
                     },
 resu = Expand[resu /. {onepm :> ((1+(-1)^OPEm)/2),
                        onemm :> ((1-(-1)^OPEm)/2)
                       }  
              ];
  ];
If[$VeryVerbose > 0, Print["Factoring done; "]];

If[!FreeQ[resu, $SU], uniqli = {};
   getsu[xx_] := (AppendTo[uniqli, xx]; gsu[xx]);
   resu = resu /. $SU -> getsu; 
   uniqli = Union[uniqli];
   resu = resu /.gsu[uniqli[[1]]] -> sdummy /. gsu -> Identity;
  ];
If[(legs < 4) && FreeQ[resu, OPEDelta],
resu = fce[resu] /. Power2 -> Power /. I^(2 a_) :> (-1)^a;
  ];


resu =  resu /. Power2 -> Power;
If[$VeryVerbose > 0, Print["last global factoring "]];
resu = PowerSimplify[resu /. foop -> Identity];

If[Head[resu] === Times && !FreeQ[resu, OPEDelta],
   resu = PowerSimplify[SelectFree[resu, OPEDelta] /. 
{(-1)^m_ (1+(-1)^m_) :> (1+(-1)^m) /; MemberQ[{OPEi,OPEj,OPEm},m]} 
                         ] (resu/SelectFree[resu, OPEDelta]) 
   ];
If[ legs < 3, resu = Factor2[resu] ];
];

If[!FreeQ[resu, _Complex^em_],
    resu = PowerSimplify[resu]];

resu];


(*
InvertScalar[exp_ /; FreeQ2[exp, {sundc, DiracGamma, lor}]
            ] := 1/exp;

InvertFermion[n_. DiracGamma[mom[pe_]] + m_.] := 
    -I (n DiracSlash[pe] - m)/(n^2 ScalarProduct[pe, pe] - m^2);

InvertBoson[exp_, k_, m_, n_] := Block[{geteq, la},
geteq[x_ ] := ({#[[1]]==0, #[[2]]==0}& @ 
                   Collect2[Contract[x], lor]) /. 
                         Pair[___, lor[__], ___] -> 1;
(A MetricTensor[m, n] + B FourVector[k, m] FourVector[k, n]) /.
   Solve[geteq[exp (A MetricTensor[m, la] + 
                    B FourVector[k, m] FourVector[k, la]) - 
                          MetricTensor[la, n]], {A, B}][[1]]
                                      ];
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynRule | \n "]];
Null
