(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalcInternal *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 December '98 at 21:07 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Changes certain objects ("Symbols") into the FeynCalc 
             internal representation *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FeynCalcInternal`",
             "HighEnergyPhysics`FeynCalc`"];

FCI::"usage"=
"FCI is just an abbreviation of FeynCalcInternal.";

FeynCalcInternal::"usage"=
"FeynCalcInternal[exp] translates exp into the internal FeynCalc
representation. User defined rules can be given
by the option FinalSubstitutions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
FCI = FeynCalcInternal;


chiralityprojector := chiralityprojector = 
                                     MakeContext["ChiralityProjector"];
diracgamma      := diracgamma      = MakeContext["DiracGamma"];
diracmatrix     := diracmatrix     = MakeContext["DiracMatrix"];
diracslash      := diracslash      = MakeContext["DiracSlash"];
diractrace      := diractrace      = MakeContext["DiracTrace"];
dimension       := dimension       = MakeContext["Dimension"];
dot             := dot             = MakeContext["DOT"];
FAD             := FAD             = MakeContext["FAD"];
factor2         := factor2         = MakeContext["Factor2"];
fourvector      := fourvector      = MakeContext["FourVector"];
freeq2          := freeq2          = MakeContext["FreeQ2"];
LeviCivita      := LeviCivita      = MakeContext["LeviCivita"];
lorentzindex    := lorentzindex    = MakeContext["LorentzIndex"];
metrictensor    := metrictensor    = MakeContext["MetricTensor"];
momentum        := momentum        = MakeContext["Momentum"];
momentumexpand  := momentumexpand  = MakeContext["MomentumExpand"];
mult            := mult            = MakeContext["Mult"];
numericalfactor := numericalfactor = MakeContext["NumericalFactor"];
OPEDelta                           = MakeContext["OPEDelta"];
pair            := pair            = MakeContext["Pair"];
(*Dropped PolarizationVectorExplicit. Not used anywhere. F.Orellana. 20/9-2002*)
(*polarizationvectorexplicit:= 
    polarizationvectorexplicit     = MakeContext[
                                             "PolarizationVectorExplicit"];*)
feynampdenominator := feynampdenominator = MakeContext[
    "FeynAmpDenominator"];
propagatordenominator := propagatordenominator =
                                     MakeContext["PropagatorDenominator"];
scalarproduct   := scalarproduct   = MakeContext["ScalarProduct"];
FinalSubstitutions = FinalSubstitutions = MakeContext["FinalSubstitutions"];
FVD := FVD = MakeContext["FVD"];
FV := FV = MakeContext["FV"];
SP             := SP             = MakeContext["SP"];
SPD             := SPD             = MakeContext["SPD"];
sd            := sd            = MakeContext["SD"];
sdelta        := sdelta        = MakeContext["SUNDelta"];
sund           := sund             = MakeContext["SUND"];
sdeltacontract:= sdeltacontract= MakeContext["SUNDeltaContract"];
sunindex        := sunindex        = MakeContext["SUNIndex"];
sunn            := sunn            = MakeContext["SUNN"];
sunt            := sunt            = MakeContext["SUNT"];
SUNF            := SUNF            = MakeContext["SUNF"];
Spinor          := Spinor          = MakeContext["Spinor"];
SpinorU         := SpinorU         = MakeContext["SpinorU"];
SpinorUBar      := SpinorUBar      = MakeContext["SpinorUBar"];
SpinorV         := SpinorV         = MakeContext["SpinorV"];
SpinorVBar      := SpinorVBar      = MakeContext["SpinorVBar"];

SetAttributes[su, HoldAll];
SetAttributes[FeynCalcInternal, HoldFirst];
(*
(* good idea , but does not work *)
su[a_String, b_] := If[CheckContext[a], 
                       {MakeContext[a][ze___] :> b[ze]}, {}];
*)
su[a_String, b_] := If[CheckContext[a], {MakeContext[a] :> b}, {}];

sdeltacont[a_, b_] := 
   sdelta[sunindex[a], sunindex[b]];

tosund[a_,b_,c_] := sund[sunindex[a], sunindex[b], sunindex[c]];

sdeltacontr[a_, b_] := 
   sdeltacontract[sunindex[a], sunindex[b]];

Options[FeynCalcInternal] = {FinalSubstitutions -> {}};

FeynCalcInternal[x_, opts___Rule] :=  Block[{ru, revru, uru},
uru = FinalSubstitutions /. {opts} /. Options[FeynCalcInternal];
uru = Flatten[{uru}];

ru =  Join[
 su["SpinorU", tospinor],
 su["SpinorV", tospinorv],
 su["SpinorVBar", tospinorv],
 su["SpinorUBar", tospinor],
 su["SUNF", tosunf],
 su["MetricTensor",metricT] ,
 su["DiracMatrix", diracM] ,  su["DiracSlash", diracS] ,
 su["FourVector", fourV] ,
 (*su["PolarizationVector", polarizationvectorexplicit] ,*)
 su["SD", sdeltacont],
 su["SUNDelta", sdeltacont],
 su["SUND", tosund],
 su["SUNDeltaContract", sdeltacontr],
 su["SUNT", sunTint],
 su["FAD", fadint],
 su["FVD", fvd],
 su["FV", fv],
 su["LC", lc],
 su["LCD", lcd],
 su["MT", mt],
 su["MTD", mtd],
 su["GA", ga],
 su["GAD", gad],
 su["GS", gs],
 su["GSD", gsd],
 su["SP", sp],
 su["SPD", spd],
 su["SO", so],
 su["SOD", sod],
 su["PropagatorDenominator", propagatorD],
 su["ScalarProduct", scalarP],
 su["MatrixTrace", diractrace], 
 If[CheckContext["DOT"], {Dot -> dot}, {}],
 If[CheckContext["ChiralityProjector"], 
    If[$BreitMaison === True,
       {chiralityprojector[1] :> 1/2 + 1/2 diracgamma[5],
        chiralityprojector[-1]:> 1/2 - 1/2 diracgamma[5]
       },
       {chiralityprojector[1] :> diracgamma[6],
        chiralityprojector[-1]:> diracgamma[7]
       }
      ],{}
    ]
         ]      ;

(* Dropped the last rules to avoid e.g.
 (1/2 + DiracGamma[5]/2 // ScalarProductExpand ---> DiracGamma[6],
  when $BreitMaison=True. 19/1-2003, F.Orellana*)
revru = If[$BreitMaison === True, Map[Reverse, Drop[ru, -2]],  Map[Reverse, ru]];

If[uru =!={}, ru = Join[ru,uru]];
(*
Print["fci time = ",ti//MakeContext["FeynCalcForm"]];
Print["ru= ",ru];
*)

If[ru =!={}, ReplaceRepeated[x, ru, MaxIterations -> 20] /.
                      {mt :> MakeContext["MT"], 
                       fv :> MakeContext["FV"], 
                       sd :> MakeContext["SD"]} /. revru, x
  ]
];

(* ---------------------------------------------------------------------- *)
(* metricT *)
(* ---------------------------------------------------------------------- *)
loin1[x_,___] := x;
metricT[ x_, y_,op_:{} ] :=
  pair[ lorentzindex[x,dimension/.op/.Options[metrictensor] ],
        lorentzindex[y,dimension/.op/.Options[metrictensor] ]
      ];
metricT[a_ b_, opt___] := metricT[a, b, opt];
metricT[a_^2 , opt___] := metricT[a, a, opt];
metricT[x__] := (metricT@@({x} /. lorentzindex -> loin1));
metricT[x_, x_,op_:{}]:=(dimension/.op/.Options[metrictensor]);
(* ---------------------------------------------------------------------- *)
(* diracM *)
(* ---------------------------------------------------------------------- *)
diracM[n_?NumberQ y_]:=n diracM[y];
diracmatrix[n_?NumberQ y_,{}]:=n diracM[y];
diracM[n_?NumberQ y_,opt_]:=n diracM[y,opt];
diracM[x_,y_]:=diracM[x].diracM[y]/;(FreeQ[y,Rule]&&y=!={});
diracM[x_,y__,{}]:= diracM[Dot[x,y]];
diracM[x_,y__,z_]:= diracM[Dot[x,y],z]/;!FreeQ[z,Rule];
diracM[x_,y__,z_]:= diracM[Dot[x,y,z]]/; FreeQ[z,Rule];
diracM[x_ y_Plus,opt_:{}]:= diracM[Expand[x y],opt];
diracM[x_Plus,opt_:{}]:= diracM[#,opt]& /@ x;
diracM[x_Dot,opt_:{}] :=  diracM[#,opt]& /@ x;
diracM[n_Integer,___]:=diracgamma[n];
diracM[5,opt_:{}]:=diracgamma[5];
diracM[6,opt_:{}]:=diracgamma[6];
diracM[7,opt_:{}]:=diracgamma[7];
diracM["+"]:=diracgamma[6];
diracM["-"]:=diracgamma[7];
diracM[x_,op_:{}] := diracgamma[lorentzindex[ x,
            (dimension/.op/.Options[diracmatrix])  ] ,
            (dimension/.op/.Options[diracmatrix])
                               ]/;(Head[x]=!=Dot && !IntegerQ[x]);
(* ---------------------------------------------------------------------- *)
(* diracS *)
(* ---------------------------------------------------------------------- *)
ndot[]=1;
ndot[a___,ndot[b__],c___] := ndot[a,b,c];
ndot[a___,b_Integer,c___] := b ndot[a,c];
ndot[a___,b_Integer x_,c___]:=b ndot[a,x,c];
diracS[x_,y_]:=diracS[ndot[x,y]]/;(FreeQ[y,Rule]&&y=!={});
diracS[x_,y__,{}]:=diracS[ndot[x,y]];
diracS[x_,y__,z_]:=diracS[ndot[x,y],z]/;!FreeQ[z,Rule];
diracS[x_,y__,z_]:=diracS[ndot[x,y,z]]/;FreeQ[z,Rule];
diracS[x__]:= (diracS@@({x}/.Dot->ndot) )/;!FreeQ[{x},Dot];
diracS[n_Integer x_ndot,opt_:{}]:=n diracS[x,opt];
diracS[x_ndot,opt_:{}] := Expand[ (diracS[#,opt]& /@ x) ]/.ndot->Dot;
(*   pull out a common numerical factor *)
diracS[x_,op_:{}] := Block[{dtemp,dix,eins,numf,resd},
         dix = factor2[ eins Expand[x]];
         numf = numericalfactor[dix];
         resd = numf diracgamma[ momentum[Cancel[(dix/.eins->1)/numf],
           (dimension/.op/.Options[diracslash])  ] ,
           (dimension/.op/.Options[diracslash])
                               ]
                          ]/;((Head[x]=!=Dot)&&(Head[x]=!=ndot));
(* ---------------------------------------------------------------------- *)
(* fourV *)
(* ---------------------------------------------------------------------- *)

fourV[ x_momentum,y___]:= fourV[x[[1]],y];
fourV[ x_,y_lorentzindex,op___]:= fourV[x,y[[1]],op];
(*   pull out a common numerical factor *)
fourV[ x_,y_,opt_:{}]:=Block[{nx,numfa,one,result},
       nx = factor2[one x];
       numfa = numericalfactor[nx];
       result = numfa pair[ lorentzindex[y, dimension/.opt/.
                                            Options[fourvector]],
                              momentum[Cancel[nx/numfa]/.one->1,
                                       dimension/.opt/.Options[fourvector]]
                          ]; result] /; !FreeQ[x, Plus];

fourV[x_, y_,opt_:{} ] := pair[
      lorentzindex[y,dimension/.opt/.Options[fourvector]],
      momentum[x,dimension/.opt/.Options[fourvector]]
                                     ] /; FreeQ[x, Plus];
(* ---------------------------------------------------------------------- *)
(* propagatorD *)
(* ---------------------------------------------------------------------- *)
propagatorD[x_] := propagatorD[x, 0] /; freeq2[x, 
                           {Pattern, pair, scalarproduct}];
propagatorD[x_, y_] := (propagatorD[x, y] =
  propagatordenominator[momentumexpand[ momentum[x,D] ],y] 
                       ) /;freeq2[x,{momentum, Pattern,HoldForm}];

propagatorD[x_, y_] := (propagatorD[x, y] =
                        propagatordenominator[x//momentumexpand, y] 
                       ) /; (FreeQ[{x,y}, Pattern] ) && 
                            (momentumexpand[x] =!= x);
propagatorD[x_, y_] := propagatordenominator[x, y] /;
                        (momentumexpand[x] === x);

(* ---------------------------------------------------------------------- *)
(* sunTint *)
(* ---------------------------------------------------------------------- *)

sunTint[x__] := (If[!MemberQ[$NonComm, sunt],
                    AppendTo[$NonComm, sunt]
                  ]; sunT[x] /. sunT -> sunt
               );

(*CHANGE Dec. 97 : inhibit wrapping SUNIndex around integer indices *)

sunT[b_]  := sunT[sunindex[b]] /; FreeQ[b, sunindex] && FreeQ[b, Pattern] && 
                                  !IntegerQ[b];

SetAttributes[setdel, HoldRest];
setdel[x_, y_] := SetDelayed[x, y];
setdel[HoldPattern[sunT[dottt[x__]]] /. dottt -> dot, dot@@( sunT /@ {x} ) ];
setdel[HoldPattern[sunT[sunind[dottt[x__]]]] /. dottt -> dot /. sunind ->
                    sunindex, dot@@( sunT /@ {x} ) ];

sunT[a_, y__] := Apply[dot, sunT /@ {a, y}];
(* ---------------------------------------------------------------------- *)
(* scalarP *)
(* ---------------------------------------------------------------------- *)

scalarP[a_ b_, opt___Rule] := scalarP[a, b, opt];
scalarP[a_^2 , opt___Rule] := scalarP[a, a, opt];

 scalarP[ x_, y_,opt___Rule ] := If[(FreeQ[x, momentum]) ||
                                    (FreeQ[y, momentum]),
  pair[ momentum[x,dimension/.{opt}/.Options[scalarproduct]],
        momentum[y,dimension/.{opt}/.Options[scalarproduct]]
      ], pair[x, y]];

 scalarP[ x_, y_,opt___BlankNullSequence] := 
                               If[(FreeQ[x, momentum]) ||
                                  (FreeQ[y, momentum]),
        pair[ momentum[x,opt], momentum[y,opt] ], pair[x, y]
                                 ];

(* ---------------------------------------------------------------------- *)
(* FAD *)
(* ---------------------------------------------------------------------- *)

fadint[a__] := fadint2 @@ Map[Flatten[{#}]&, {a}];
propp[{x_}]:=propagatordenominator[momentum[x,dimension /. dimension ->
                                              (dimension/.Options[FAD])],0
                            ] // momentumexpand;
propp[{repeated[{x_, m_}]}]:=
Repeated[
propagatordenominator[momentum[x,dimension /. dimension ->
                                 (dimension/.Options[FAD])], m
                     ] // momentumexpand
        ];
propp[{x_, m_}]:=
propagatordenominator[momentum[x,dimension /. dimension ->
                                 (dimension/.Options[FAD])], m
                     ] // momentumexpand;
fadint2[b__List] := 
 feynampdenominator @@ Map[propp, {b}/.Repeated->repeated];

(* ---------------------------------------------------------------------- *)
(* SPD *)
(* ---------------------------------------------------------------------- *)

sp[a_,b_] := pair[momentum[a], momentum[b]];
spd[a_,b_] := pair[momentum[a, D], momentum[b,D]];
so[a_] := pair[momentum[a], momentum[OPEDelta]];
sod[a_] := pair[momentum[a,D], momentum[OPEDelta,D]];

fvd[a_,b_] := pair[momentum[a, D], lorentzindex[b,D]];
fv[a_,b_] := pair[momentum[a], lorentzindex[b]];
mt[a_,b_] := pair[lorentzindex[a], lorentzindex[b]];
mtd[a_,b_] := pair[lorentzindex[a, D], lorentzindex[b, D]];

gs[a_]  :=  diracgamma[momentum[a]];
gsd[a_] :=  diracgamma[momentum[a,D],D];

ga[5] = diracgamma[5]; ga[6] = diracgamma[6];
ga[7] = diracgamma[7];
gad[5] = diracgamma[5]; gad[6] = diracgamma[6];
gad[7] = diracgamma[7];
ga[a_]  :=  diracgamma[lorentzindex[a]];
gad[a_] :=  diracgamma[lorentzindex[a,D],D];

lc[y__]  := LeviCivita[y,dimension->4];
HoldPattern[lc[y___][z___]]  := LeviCivita[y,dimension->4][z,dimension->4];
HoldPattern[lcd[y__]] := LeviCivita[y,dimension->D];
HoldPattern[lcd[y___][z___]]  := LeviCivita[y,dimension->D][z,dimension->D];

tosunf[a_, b_, c_] := SUNF@@Map[sunindex,
                                ({a,b,c} /. sunindex->Identity)
                               ];

tospinor[a__] := Spinor[a];
tospinorv[a_,0,b__] := Spinor[a,0,b];
tospinorv[a_] := Spinor[a];
tospinorv[a_,b__] := Spinor[-a,b];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynCalcInternal | \n "]];
Null
