(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Schouten *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Option and Function *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`Schouten`",{"HighEnergyPhysics`FeynCalc`"}];

Schouten::"usage" =
"Schouten[expr] applies the Schouten identity for four-vectors on at most
42 terms in a sum. If Schouten should operate on larger
expression you can give a second argument, e.g.:
Schouten[expr, 4711] which will work
on sums with less than 4711 terms.\n\n

Schouten is also an option of Contract and
DiracTrace. It may be set to an integer
indicating the maximum number of terms onto which the
function Schouten will be applied .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

collect2                 = MakeContext["Collect2"];
eps                      = MakeContext["CoreObjects","Eps"];
epsevaluate              = MakeContext["EpsEvaluate"];
expandscalarproduct      = MakeContext["ExpandScalarProduct"];
factoring                = MakeContext["CoreOptions","Factoring"];
fci                      = MakeContext["FeynCalcInternal"];
freeq2                   = MakeContext["FreeQ2"];
lorentzindex             = MakeContext["CoreObjects","LorentzIndex"];
memset                   = MakeContext["MemSet"];
pair                     = MakeContext["CoreObjects","Pair"];
partithead               = MakeContext["PartitHead"];

Schouten[y_, 0] := y;
Schouten[y_, oparg_:42] := FixedPoint[ schouten[#, oparg]&, fci[y], 14];

liget[a_. eps[x1_[y1_], x2_[y2_], x3_[y3_], x4_[y4_]] *
      pair[x5_[y5_], x6_[y6_]]
     ] := {x1[y1],x2[y2],x3[y3],x4[y4],x5[y5],x6[y6]};

lisch[{a1_,a2_,a3_,a4_,a5_,a6_}]:=pair[a5,a6] eps[a1,a2,a3,a4];

schouten[x_,opar_:42]:=memset[schouten[x, opar],
  Block[{i,nx,temp0,temp,lind,ltemp,ntemp,dummIlabel = False,DUMMI,
   schou,sor, all,result,epc, epsnterms,numberofli, optarg = opar,
    temp1, nxf = 1},
   epc[a_, b_] := If[ Length[Position[partithead[a, eps][[2]],
                                      lorentzindex]
                            ] <
                      Length[Position[partithead[b, eps][[2]],
                                      lorentzindex]],
                            True, False, False
                    ];
   epsnterms[0] = 0;
   epsnterms[a_] :=
         Block[{tem}, tem = collect2[a, eps, factoring ->False];
                      If[Head[tem]===Plus, Length[tem], 1]
              ];
   nx = epsevaluate[expandscalarproduct[x]//Expand]//Expand;
(* Split the sum into two parts *)
   result = nx;
   If[(optarg === I) && (Head[nx] === Times),
      nxf = Select[nx, !FreeQ[#, _^(a_/;Head[a] =!= Integer)]&];
      nx = nx/nxf + DUMMI; dummIlabel = True; optarg = 6
     ];
      all  = partithead[nx, eps];
   If[ Head[all[[2]]]===Plus || dummIlabel === True,
       If[dummIlabel === True, temp0 = {0,all[[2]]},
          temp0 = partithead[all[[2]], pair];
         ];
       temp = temp0[[2]];
       If[((Head[temp]===Plus) && Length[temp] > 1 &&
           If[IntegerQ[optarg], Length[temp] < optarg, False]
          ) || dummIlabel === True,
          ltemp = Length[temp];
          If[dummIlabel =!= True,
             temp1 = temp[[1]],
             temp1 = temp; ltemp = 7
            ];
          numberofli = Length[Position[temp1, lorentzindex]];
          i = 0;
          temp =
           If[dummIlabel === True,
              Catch[
              lind = liget[ temp ];
              If[Length[lind]===6,
                 schou = lisch /@ Map[
                                Append[#, Last[lind]]&,
                                NestList[RotateLeft,Take[lind,5],4]
                                     ];
                 sor = schou;
                        Do[If[FreeQ[temp, sor[[1]]],
                              sor = RotateLeft[sor]
                             ], {6}];
                 If[!FreeQ[temp, sor[[1]]],
                        ntemp = Expand[epsevaluate[temp/.sor[[1]]->
                                        (-Apply[Plus, Drop[sor,1]])
                                             ] ];
                        Throw[ntemp]
                   ]
                ];
               ],

           Catch[
            While[i < Length[temp], i++;
                  If[!freeq2[temp[[i]],{eps,pair}],
                     lind = liget[ temp[[i]] ];
                     If[Length[lind]===6,
(* ----------------------------------------------------------------- *)
(* create a list of 5 possible arrangements of terms of
   Schouten ident.
*)
(* ----------------------------------------------------------------- *)
                        schou = lisch /@ Map[
                                Append[#, Last[lind]]&,
                                NestList[RotateLeft,Take[lind,5],4]
                                            ];
                        sor = Sort[ schou, epc ]//Reverse;
                        Do[If[FreeQ[temp, sor[[1]]],
                              sor = RotateLeft[sor]
                             ], {6}];
  print3["sor = ", sor];

                        If[!FreeQ[temp, sor[[1]]],
                        ntemp = Expand[epsevaluate[temp/.sor[[1]]->
                                        (-Apply[Plus, Drop[sor,1]])
                                             ] ];
                                 If[(epsnterms[ntemp] < ltemp) ||
(* or all LorentzIndices are inside all eps's *)
 (Union[Length[ Position[#, lorentzindex] ]& /@
        Select[ Variables[ntemp], Head[#]===eps& ]
       ]  === {numberofli}),
         Throw[temp=ntemp]
                                   ]
                                ]]]
                              ];
           temp]
   ];
   result = (all[[1]]/.DUMMI->0)  + temp0[[1]] + temp
                          ] ];
       result nxf] ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Schouten | \n "]];
Null
