(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEInsert*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  substituting RHI - integrals into a diagram *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEInsert`",
             "HighEnergyPhysics`FeynCalc`"];

OPEInsert::"usage"= "OPEInsert[diagram_String, name_, rhifile_String] or
OPEInsert[diagram_String, name_]. The setting of the option
EpsContract (4 or D) determines whether the Levi-Civita tensors
are contracted in 4 or D dimensions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[Cases2, ChangeDimension, Collect2, CA,CF,Tf,Nf,
            Contract, DeltaFunction, Dimension,DiracGamma,
            Divideout, Eps, EpsContract, Expanding,
            Epsilon, EpsilonOrder, Factoring, Factor2, FORM,FreeQ2, OPEm,
            FeynAmpDenominator,
            FeynCalcInternal, Isolate, IsolateHead,Power2,
            PowerSimplify, RHI, Select1, Select2, 
            Series2, Zeta2
           ];

Options[OPEInsert] = {Dimension -> D, Divideout -> 1, 
                      EpsContract -> 4, EpsilonOrder -> -1,
                      Function -> Identity};

OPEInsert[aa_String, na_, opt___Rule] := OPEInsert[aa, na, "", opt];
OPEInsert[aa_String, na_, bb_String, opt___Rule] := 
Block[{tt, null,null1, null2, plc, epord, vv, xx, dim, div, epscdi, 
       ceps,consav,cv,fun,ppp,pppp, dumm1, dumm2},
If[!FreeQ[na, RHI],
   Print["RHI's left !!"]; Dialog[na],
 dim = Dimension /. {opt} /. Options[OPEInsert];
 epscdi = EpsContract /. {opt} /. Options[OPEInsert];
 div = FeynCalcInternal[Divideout /. {opt} /. Options[OPEInsert]];
 fun = Function /. {opt} /. Options[OPEInsert];
 epord = EpsilonOrder /. {opt} /. Options[OPEInsert];
 If[$VeryVerbose > 0, Print["changing Eps-Dimension to ", epscdi]];
 SetOptions[Eps, Dimension -> epscdi];
 If[$VeryVerbose > 0, 
    Print["loading "], " dimension of eps-contract : ", epscdi
   ];
  If[bb =!= "", Get[bb]];  
  Get[aa];
SetOptions[RHI, FORM -> False, EpsilonOrder -> epord];
If[!FreeQ[na,FeynAmpDenominator], 
   Print["NOT FULLY EVALUATED !!!"];
   Quit[]
  ];

tt = FeynCalcInternal[na]/.Power2->Power /. 
           {((-1)^OPEm (1-(-1)^OPEm)) :> (-(1-(-1)^OPEm))};
If[!FreeQ[tt, DiracGamma],
   tt = Collect2[tt,DiracGamma,Factoring -> False]
  ];

If[fun =!= Identity,
   If[$VeryVerbose >1, Print["doing ",fun]];
   tt = fun[tt]
  ];

If[$VeryVerbose > 1, Print["contracting Eps"]];
consav[y_] := consav[y] = Contract[y, EpsContract->True];
If[tt =!=0,
If[!FreeQ[tt, Eps], 
   tt = ChangeDimension[tt, epscdi];
   ceps[y_] := If[Head[y] =!= Times, y, 
                  Select1[y, Eps] Factor2[consav[Select2[y, Eps]] ]
                 ];
   tt = Map[ceps, tt + null1 + null2]/.{null1 :> 0, null2 :> 0};
  ];

If[$VeryVerbose > 1, Print[" D -> 4"]];
tt = ChangeDimension[tt, 4];
tt = PowerSimplify[Expand[tt]];
If[Head[tt] === Plus,
   tt = Map[#/div &, tt], tt = tt/div
  ];
 
If[$VeryVerbose > 1, Print["Series2"]];
tt = Expand[Series2[
      Collect2[tt /. dim->(4+Epsilon), Epsilon, Factoring ->True,
                Expanding -> False
              ], Epsilon,0], Epsilon
           ];
If[$VeryVerbose > 1, Print["Series2 done"]];
If[(EpsilonOrder /. {opt} /. Options[OPEInsert]) < 0,
   tt = Select2[tt + nulll + nullllll, Epsilon];
  ];
tt = Factor2[tt];
colfa = Select2[dumm1 dumm2 tt, {CA,CF,Nf,Tf}];
If[colfa =!= 0, tt = tt / colfa];
tt = Collect2[Map[Apart, tt//Expand], Epsilon,Factoring->False];
If[(EpsilonOrder /. {opt} /. Options[OPEInsert]) < 0,
   tt = Select2[tt + null, Epsilon];
  ];
tt = tt /. {a_. (1/(xX_ -1)) :> (-a/(1-xX))};

deltaf/: b_ deltaf[xx_, a_] := deltaf[xx,a b];
deltafact[xxx_, c_] := DeltaFunction[xxx] *
                       Isolate[
                          Factor2[c//Factor2//Expand]
                              ];
ppp[y__]:=Collect2[Plus[y], {CA, CF, Tf, Nf, DeltaFunction}, 
                            Factoring->False
                  ] /. DeltaFunction[xy_] :> deltaf[xy,1] /. 
                       deltaf -> deltafact /. Plus -> pppp;
pppp[y__]:=If[!FreeQ2[{y}, {Epsilon,CA,CF,Tf,Nf}],
              Plus[y],
              Collect2[Plus[y], {Log,PolyLog,Zeta2},Factoring->False
                      ] /. Plus -> p5
             ];

p5[y__] := If[!FreeQ2[{y},{Log,PolyLog,Zeta2,DeltaFunction}], Plus[y],
             Apart[Plus[y]] /. (1/(-1+ix_)) :> (minus/(1-ix))/.minus->-1
            ];

If[Head[tt]===Plus,
   tt = Map[#/.Plus->ppp&,tt],
   tt = tt/.Plus->ppp
  ];
tt = FixedPoint[ReleaseHold,tt];

(* ad hoc *)
If[Head[tt] =!= Plus,
   If[Length[Expand[tt]] === 2, tt = Factor2[tt]]
  ];
];
colfa tt ]];

collect[y_, h_ /; Head[h] =!= List] := Factor2[y];
collect[y_, v_List] := Block[{temp},
                              temp = Collect[y, v];
                              If[Head[temp] === Plus,
                                 temp = Factor2[Select1[temp, v]] +
                                        Select2[temp, v]
                                ];
                        temp];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEInsert | \n "]];
Null
