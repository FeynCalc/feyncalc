F(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalcInternal *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 20 December '98 at 21:07 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Changes certain objects ("Symbols") into the FeynCalc
             internal representation *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FeynCalcInternal`",{"HighEnergyPhysics`FeynCalc`"}];

FCI::"usage"=
"FCI is just an abbreviation of FeynCalcInternal.";

FeynCalcInternal::"usage"=
"FeynCalcInternal[exp] translates exp into the internal FeynCalc
representation. User defined rules can be given
by the option FinalSubstitutions.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FCI = FeynCalcInternal;


ChiralityProjector := ChiralityProjector =  MakeContext["CoreObjects","ChiralityProjector"];
DiracGamma      := DiracGamma      = MakeContext["CoreObjects","DiracGamma"];
DiracMatrix     := DiracMatrix     = MakeContext["CoreObjects","DiracMatrix"];
DiracSlash      := DiracSlash      = MakeContext["CoreObjects","DiracSlash"];
DiracTrace      := DiracTrace      = MakeContext["DiracTrace"];
Dimension       := Dimension       = MakeContext["CoreOptions","Dimension"];
Eps             := Eps             = MakeContext["CoreObjects","Eps"];
FAD             := FAD             = MakeContext["CoreObjects","FAD"];
Factor2         := Factor2         = MakeContext["Factor2"];
FourVector      := FourVector      = MakeContext["CoreObjects","FourVector"];
FreeQ2          := FreeQ2          = MakeContext["FreeQ2"];
LeviCivita      := LeviCivita      = MakeContext["CoreObjects","LeviCivita"];
ExplicitLorentzIndex    := ExplicitLorentzIndex    = MakeContext["CoreObjects","ExplicitLorentzIndex"];
LorentzIndex    := LorentzIndex    = MakeContext["CoreObjects","LorentzIndex"];
MetricTensor    := MetricTensor    = MakeContext["CoreObjects","MetricTensor"];
Momentum        := Momentum        = MakeContext["CoreObjects","Momentum"];
MomentumExpand  := MomentumExpand  = MakeContext["MomentumExpand"];
NumericalFactor := NumericalFactor = MakeContext["NumericalFactor"];
OPEDelta                           = MakeContext["OPEDelta"];
Pair            := Pair            = MakeContext["CoreObjects","Pair"];
(*Dropped PolarizationVectorExplicit. Not used anywhere. F.Orellana. 20/9-2002*)
(*polarizationvectorexplicit:=
    polarizationvectorexplicit     = MakeContext[
                                             "PolarizationVectorExplicit"];*)
FeynAmpDenominator := FeynAmpDenominator = MakeContext["CoreObjects","FeynAmpDenominator"];
PropagatorDenominator := PropagatorDenominator =
                                     MakeContext["CoreObjects","PropagatorDenominator"];
ScalarProduct   := ScalarProduct   = MakeContext["ScalarProduct"];
FinalSubstitutions = FinalSubstitutions = MakeContext["CoreOptions","FinalSubstitutions"];
FVD := FVD = MakeContext["CoreObjects","FVD"];
FVE := FVE = MakeContext["CoreObjects","FVE"];
FV := FV = MakeContext["CoreObjects","FV"];
SP             := SP             = MakeContext["CoreObjects","SP"];
SPD             := SPD             = MakeContext["CoreObjects","SPD"];
SPE             := SPE             = MakeContext["CoreObjects","SPE"];
SD            := SD            = MakeContext["CoreObjects","SD"];
SDF            := SDF            = MakeContext["CoreObjects","SDF"];
SUNDelta        := SUNDelta        = MakeContext["CoreObjects","SUNDelta"];
SUNFDelta        := SUNFDelta        = MakeContext["CoreObjects","SUNFDelta"];
SUND           := SUND             = MakeContext["CoreObjects","SUND"];
SUNDeltaContract:= SUNDeltaContract= MakeContext["SUNDeltaContract"];
SUNFDeltaContract:= SUNFDeltaContract= MakeContext["SUNFDeltaContract"];
SUNIndex        := SUNIndex        = MakeContext["CoreObjects","SUNIndex"];
SUNFIndex        := SUNFIndex        = MakeContext["CoreObjects","SUNFIndex"];
ExplicitSUNIndex        := ExplicitSUNIndex        = MakeContext["CoreObjects","ExplicitSUNIndex"];
ExplicitSUNFIndex        := ExplicitSUNFIndex        = MakeContext["CoreObjects","ExplicitSUNFIndex"];
SUNT            := SUNT            = MakeContext["CoreObjects","SUNT"];
SUNTF            := SUNTF            = MakeContext["CoreObjects","SUNTF"];
SUNF            := SUNF            = MakeContext["CoreObjects","SUNF"];
Spinor          := Spinor          = MakeContext["CoreObjects","Spinor"];
SpinorU         := SpinorU         = MakeContext["CoreObjects","SpinorU"];
SpinorUBar      := SpinorUBar      = MakeContext["CoreObjects","SpinorUBar"];
SpinorV         := SpinorV         = MakeContext["CoreObjects","SpinorV"];
SpinorVBar      := SpinorVBar      = MakeContext["CoreObjects","SpinorVBar"];

SetAttributes[su, HoldAll];
SetAttributes[FeynCalcInternal, HoldFirst];
(*
(* good idea , but does not work *)
su[a_String, b_] := If[CheckContext[a],
                       {MakeContext[a][ze___] :> b[ze]}, {}];
*)
su[a_String, b_] := If[CheckContext[a], {MakeContext[a] :> b}, {}];

sdeltacont[a_, b_] :=
   SUNDelta[SUNIndex[a], SUNIndex[b]];

sfdeltacont[a_, b_] :=
   SUNFDelta[SUNFIndex[a], SUNFIndex[b]];


tosund[a_,b_,c_] := SUND[SUNIndex[a], SUNIndex[b], SUNIndex[c]];

sdeltacontr[a_, b_] :=
   SUNDeltaContract[SUNIndex[a], SUNIndex[b]];

sfdeltacontr[a_, b_] :=
   SUNFDeltaContract[SUNFIndex[a], SUNFIndex[b]];

Options[FeynCalcInternal] = {FinalSubstitutions -> {}};

FeynCalcInternal[x_, opts___Rule] :=  Block[{ru, revru, uru},
uru = FinalSubstitutions /. {opts} /. Options[FeynCalcInternal];
uru = Flatten[{uru}];

ru =  Join[
 {HighEnergyPhysics`FeynCalc`CoreObjects`SpinorU :> tospinor},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SpinorV :> tospinorv},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SpinorVBar :> tospinorv},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SpinorUBar :> tospinor},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUNF :> tosunf},
 {HighEnergyPhysics`FeynCalc`CoreObjects`MetricTensor :> metricT},
 {HighEnergyPhysics`FeynCalc`CoreObjects`DiracMatrix  :> diracM},
 {HighEnergyPhysics`FeynCalc`CoreObjects`DiracSlash :> diracS},
 {HighEnergyPhysics`FeynCalc`CoreObjects`FourVector :> fourV},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SD :> sdeltacont},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SDF :> sfdeltacont},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUNDelta :> sdeltacont},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUNFDelta :> sfdeltacont},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUND :> tosund},
 su["SUNDeltaContract", sdeltacontr],
 su["SUNFDeltaContract", sfdeltacontr],
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUNT :> sunTint},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUNTF :> sunTFint},
 {HighEnergyPhysics`FeynCalc`CoreObjects`FAD :> fadint},
 {HighEnergyPhysics`FeynCalc`CoreObjects`FVD :> fvd},
 {HighEnergyPhysics`FeynCalc`CoreObjects`FVE :> fve},
 {HighEnergyPhysics`FeynCalc`CoreObjects`FV :> fv},
 {HighEnergyPhysics`FeynCalc`CoreObjects`LeviCivita :> levicivita},
 {HighEnergyPhysics`FeynCalc`CoreObjects`LC :> lc},
 {HighEnergyPhysics`FeynCalc`CoreObjects`LCD :> lcd},
 {HighEnergyPhysics`FeynCalc`CoreObjects`MT :> mt},
 {HighEnergyPhysics`FeynCalc`CoreObjects`MTD :> mtd},
 {HighEnergyPhysics`FeynCalc`CoreObjects`MTE :> mte},
 {HighEnergyPhysics`FeynCalc`CoreObjects`GA :> ga},
 {HighEnergyPhysics`FeynCalc`CoreObjects`GAD :> gad},
 {HighEnergyPhysics`FeynCalc`CoreObjects`GAE :> gae},
 {HighEnergyPhysics`FeynCalc`CoreObjects`GS :> gs},
 {HighEnergyPhysics`FeynCalc`CoreObjects`GSD :> gsd},
 {HighEnergyPhysics`FeynCalc`CoreObjects`GSE :> gse},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SP :> sp},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SPD :> spd},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SPE :> spe},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SO :> so},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SOD :> sod},
 {HighEnergyPhysics`FeynCalc`CoreObjects`PropagatorDenominator :> propagatorD},
 su["ScalarProduct", scalarP],
 su["MatrixTrace", DiracTrace],
 (*If[CheckContext["DOT"], {*){Dot -> DOT}(*}, {}]*),
 If[CheckContext["CoreObjects"],
    If[$BreitMaison === True,
       {ChiralityProjector[1] :> 1/2 + 1/2 DiracGamma[5],
        ChiralityProjector[-1]:> 1/2 - 1/2 DiracGamma[5]
       },
       {ChiralityProjector[1] :> DiracGamma[6],
        ChiralityProjector[-1]:> DiracGamma[7]
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

If[ru =!={}, ReplaceRepeated[x//.{	HighEnergyPhysics`FeynCalc`CoreObjects`LC[a___][b___] :> lcl[{a},{b}],
    								HighEnergyPhysics`FeynCalc`CoreObjects`LCD[a___][b___] :> lcdl[{a},{b}],
									HighEnergyPhysics`FeynCalc`CoreObjects`LeviCivita[a___][b___] :> levicivital[{a},{b}]
    								}, Dispatch[ru], MaxIterations -> 20] /.
                      {mt :> MakeContext["CoreObjects","MT"],
                       fv :> MakeContext["CoreObjects","FV"],
                       SD :> MakeContext["CoreObjects","SD"],
                       SDF :> MakeContext["CoreObjects","SDF"]} /. Dispatch[revru], x
  ]
];

(* ---------------------------------------------------------------------- *)
(* metricT *)
(* ---------------------------------------------------------------------- *)
loin1[x_,___] := x;
metricT[ x_, y_,op_:{} ] :=
  Pair[ LorentzIndex[x,Dimension/.op/.Options[MetricTensor] ],
        LorentzIndex[y,Dimension/.op/.Options[MetricTensor] ]
      ];
metricT[a_ b_, opt___] := metricT[a, b, opt];
metricT[a_^2 , opt___] := metricT[a, a, opt];
metricT[x__] := (metricT@@({x} /. LorentzIndex -> loin1));
metricT[x_, x_,op_:{}]:=(Dimension/.op/.Options[MetricTensor]);
(* ---------------------------------------------------------------------- *)
(* diracM *)
(* ---------------------------------------------------------------------- *)
Options[diracM] = {Dimension -> 4, FCI -> True};
diracM[n_?NumberQ y:Except[_?OptionQ], opts:OptionsPattern[]]:=n diracM[y,opts];
diracM[x_,y:Except[_?OptionQ], opts:OptionsPattern[]]:=DOT[diracM[x,opts],diracM[y,opts]];
diracM[x_,y:Except[_?OptionQ]..,opts:OptionsPattern[]]:= diracM[DOT[x,y],opts];
diracM[x_ y_Plus,opts:OptionsPattern[]]:= diracM[Expand[x y],opts];
diracM[x_Plus,opts:OptionsPattern[]]:= diracM[#,opts]& /@ x;
diracM[DOT[x_,y__],opts:OptionsPattern[]] :=  diracM[#,opts]& /@ DOT[x,y];
diracM[5, OptionsPattern[]]:=DiracGamma[5]/; OptionValue[Dimension]===4;
diracM[6, OptionsPattern[]]:=DiracGamma[6]/; OptionValue[Dimension]===4;
diracM[7, OptionsPattern[]]:=DiracGamma[7]/; OptionValue[Dimension]===4;
diracM["+", OptionsPattern[]]:=DiracGamma[6]/; OptionValue[Dimension]===4;
diracM["-", OptionsPattern[]]:=DiracGamma[7]/; OptionValue[Dimension]===4;
diracM[x:Except[5|6|7], OptionsPattern[]] := DiracGamma[LorentzIndex[x, OptionValue[Dimension]],
            OptionValue[Dimension]]/;(Head[x]=!=DOT && !StringQ[x]);
(* ---------------------------------------------------------------------- *)
(* diracS *)
(* ---------------------------------------------------------------------- *)
Options[diracS] = {Dimension -> 4, FCI -> True};
ndot[]=1;
ndot[a___,ndot[b__],c___] := ndot[a,b,c];
ndot[a___,b_Integer,c___] := b ndot[a,c];
ndot[a___,b_Integer x_,c___]:=b ndot[a,x,c];
diracS[x_,y:Except[_?OptionQ].., opts:OptionsPattern[]]:=diracS[ndot[x,y],opts];
diracS[x:Except[_?OptionQ].., opts:OptionsPattern[]]:= (diracS@@({x,opts}/.DOT->ndot) )/;!FreeQ[{x},DOT];
diracS[n_Integer x_ndot, opts:OptionsPattern[]]:=n diracS[x,opts];
diracS[x_ndot,opts:OptionsPattern[]] := Expand[ (diracS[#,opts]& /@ x) ]/.ndot->DOT;
(*   pull out a common numerical factor *)
diracS[x_, OptionsPattern[]] := Block[{dtemp,dix,eins,numf,resd},
         dix = Factor2[ eins Expand[x]];
         numf = NumericalFactor[dix];
         resd = numf DiracGamma[ Momentum[Cancel[(dix/.eins->1)/numf],
           OptionValue[Dimension]] ,
           OptionValue[Dimension]]
                          ]/;((Head[x]=!=DOT)&&(Head[x]=!=ndot));
(* ---------------------------------------------------------------------- *)
(* fourV *)
(* ---------------------------------------------------------------------- *)
Options[fourV] = {Dimension -> 4, FCI -> True};
(*   pull out a common numerical factor *)
fourV[x_,y_, OptionsPattern[]]:=Block[{nx,numfa,one},
		nx = Factor2[one x];
		numfa = NumericalFactor[nx];
		(numfa Pair[LorentzIndex[y,OptionValue[Dimension]],
		Momentum[Cancel[nx/numfa]/.one->1, OptionValue[Dimension]]])
	] /; !FreeQ[x, Plus];

fourV[x_, y_, OptionsPattern[]] :=
	Pair[LorentzIndex[y,OptionValue[Dimension]],
	Momentum[x,OptionValue[Dimension]]]/; FreeQ[x, Plus];
(* ---------------------------------------------------------------------- *)
(* propagatorD *)
(* ---------------------------------------------------------------------- *)
propagatorD[x_] := propagatorD[x, 0] /; FreeQ2[x,
                           {Pattern, Pair, ScalarProduct}];
propagatorD[x_, y_] := (propagatorD[x, y] =
  PropagatorDenominator[MomentumExpand[ Momentum[x,D] ],y]
                       ) /;FreeQ2[x,{Momentum, Pattern,HoldForm}];

propagatorD[x_, y_] := (propagatorD[x, y] =
                        PropagatorDenominator[x//MomentumExpand, y]
                       ) /; (FreeQ[{x,y}, Pattern] ) &&
                            (MomentumExpand[x] =!= x);
propagatorD[x_, y_] := PropagatorDenominator[x, y] /;
                        (MomentumExpand[x] === x);

(* ---------------------------------------------------------------------- *)
(* sunTint *)
(* ---------------------------------------------------------------------- *)

sunTint[x__] := (If[!MemberQ[$NonComm, SUNT],
                    AppendTo[$NonComm, SUNT]
                  ]; sunT[x] /. sunT -> SUNT
               );

(*CHANGE Dec. 97 : inhibit wrapping SUNIndex around integer indices *)

sunT[b_]  := sunT[SUNIndex[b]] /; FreeQ2[b, {SUNIndex,ExplicitSUNIndex}] && FreeQ[b, Pattern] &&
                                  !IntegerQ[b];

sunT[b_?NumberQ]  := sunT[ExplicitSUNIndex[b]];

SetAttributes[setdel, HoldRest];
setdel[x_, y_] := SetDelayed[x, y];
setdel[HoldPattern[sunT[dottt[x__]]] /. dottt -> DOT, DOT@@( sunT /@ {x} ) ];
setdel[HoldPattern[sunT[sunind[dottt[x__]]]] /. dottt -> DOT /. sunind ->
                    SUNIndex, DOT@@( sunT /@ {x} ) ];

sunT[a_, y__] := Apply[DOT, sunT /@ {a, y}];

(* ---------------------------------------------------------------------- *)
(* sunTFint *)
(* ---------------------------------------------------------------------- *)
sunTFint[{a__},b_,c_] := SUNTF[Map[SUNIndex, ({a} /. SUNIndex -> Identity)],
 SUNFIndex@(b /. SUNFIndex -> Identity),
 SUNFIndex@(c /. SUNFIndex -> Identity)];

(* ---------------------------------------------------------------------- *)
(* scalarP *)
(* ---------------------------------------------------------------------- *)

scalarP[a_ b_, opt___Rule] := scalarP[a, b, opt];
scalarP[a_^2 , opt___Rule] := scalarP[a, a, opt];

 scalarP[ x_, y_,opt___Rule ] := If[(FreeQ[x, Momentum]) ||
                                    (FreeQ[y, Momentum]),
  Pair[ Momentum[x,Dimension/.{opt}/.Options[ScalarProduct]],
        Momentum[y,Dimension/.{opt}/.Options[ScalarProduct]]
      ], Pair[x, y]];

 scalarP[ x_, y_,opt___BlankNullSequence] :=
                               If[(FreeQ[x, Momentum]) ||
                                  (FreeQ[y, Momentum]),
        Pair[ Momentum[x,opt], Momentum[y,opt] ], Pair[x, y]
                                 ];

(* ---------------------------------------------------------------------- *)
(* FAD *)
(* ---------------------------------------------------------------------- *)

fadint[a__] := fadint2 @@ Map[Flatten[{#}]&, {a}];
propp[{x_}]:=PropagatorDenominator[Momentum[x,Dimension /. Dimension ->
                                              (Dimension/.Options[FAD])],0
                            ] // MomentumExpand;
propp[{repeated[{x_, m_}]}]:=
Repeated[
PropagatorDenominator[Momentum[x,Dimension /. Dimension ->
                                 (Dimension/.Options[FAD])], m
                     ] // MomentumExpand
        ];
propp[{x_, m_}]:=
PropagatorDenominator[Momentum[x,Dimension /. Dimension ->
                                 (Dimension/.Options[FAD])], m
                     ] // MomentumExpand;
fadint2[b__List] :=
 FeynAmpDenominator @@ Map[propp, {b}/.Repeated->repeated];

(* ---------------------------------------------------------------------- *)
(* SPD *)
(* ---------------------------------------------------------------------- *)

sp[a_,b_] := Pair[Momentum[a], Momentum[b]];
spd[a_,b_] := Pair[Momentum[a, D], Momentum[b,D]];
spe[a_,b_] := Pair[Momentum[a, D-4], Momentum[b,D-4]];
so[a_] := Pair[Momentum[a], Momentum[OPEDelta]];
sod[a_] := Pair[Momentum[a,D], Momentum[OPEDelta,D]];

fvd[a_,b_] := Pair[Momentum[a, D], LorentzIndex[b,D]];
fve[a_,b_] := Pair[Momentum[a, D-4], LorentzIndex[b,D-4]];
fv[a_,b_] := Pair[Momentum[a], LorentzIndex[b]];
mt[a_,b_] := Pair[LorentzIndex[a], LorentzIndex[b]];
mtd[a_,b_] := Pair[LorentzIndex[a, D], LorentzIndex[b, D]];
mte[a_,b_] := Pair[LorentzIndex[a, D-4], LorentzIndex[b, D-4]];

gs[a_]  :=  DiracGamma[Momentum[a]];
gsd[a_] :=  DiracGamma[Momentum[a,D],D];
gse[a_] :=  DiracGamma[Momentum[a,D-4],D-4];

ga[5] = DiracGamma[5];
ga[6] = DiracGamma[6];
ga[7] = DiracGamma[7];
ga[a_]  :=  DiracGamma[LorentzIndex[a]]/; !IntegerQ[a];
gad[a_] :=  DiracGamma[LorentzIndex[a,D],D]/; !IntegerQ[a];
gae[a_] :=  DiracGamma[LorentzIndex[a,D-4],D-4]/; !IntegerQ[a];
ga[a_Integer]  :=  DiracGamma[ExplicitLorentzIndex[a]]/; (a=!=5 && a=!=6 && a=!=7);
gad[a_Integer]  :=  DiracGamma[ExplicitLorentzIndex[a,D],D]/; (a=!=5 && a=!=6 && a=!=7);
gae[a_Integer]  :=  DiracGamma[ExplicitLorentzIndex[a,D-4],D-4]/; (a=!=5 && a=!=6 && a=!=7);


lc[a__]  := (Eps@@Join[(LorentzIndex/@{a}),{Dimension->4}])/;
	FreeQ[{a},Rule] && (Length[{a}] === 4);

lcd[a__]  := (Eps@@Join[(LorentzIndex[#,D]&/@{a}),{Dimension->D}])/;
	FreeQ[{a},Rule] && (Length[{a}] === 4);

lcl[{x___},{y___}]:= (Eps@@Join[LorentzIndex/@{x},
	Momentum/@{y},{Dimension->4}]/;	Length[Join[{x},{y}]]===4);

lcdl[{x___},{y___}]:= (Eps@@Join[Map[LorentzIndex[#, D]& ,{x}],
	Map[Momentum[#, D]& ,{y}],{Dimension->D}]/;	Length[Join[{x},{y}]]===4);

levicivita[x:Except[_?OptionQ].., opts:OptionsPattern[LeviCivita]] :=
	Eps@@Join[(LorentzIndex[#,OptionValue[LeviCivita,{opts},Dimension]]&/@{x}),
	{Dimension->OptionValue[LeviCivita,{opts},Dimension]}]/; Length[{x}]===4;

levicivital[{x:Except[_?OptionQ]..., opts1:OptionsPattern[LeviCivita]},{y:Except[_?OptionQ]..., opts2:OptionsPattern[LeviCivita]}] :=
	Eps@@Join[Map[LorentzIndex[#, OptionValue[LeviCivita,{opts1},Dimension]]& ,{x}],
	Map[Momentum[#, OptionValue[LeviCivita,{opts1},Dimension]]& ,{y}],
	{Dimension->OptionValue[LeviCivita,{opts1},Dimension]}]/; Length[{x,y}]===4 &&
	OptionValue[LeviCivita,{opts1},Dimension]===OptionValue[LeviCivita,{opts2},Dimension];

tosunf[a_, b_, c_] :=
	SUNF@@Map[SUNIndex, ({a,b,c} /. SUNIndex->Identity)];

tospinor[a__] := Spinor[a];
tospinorv[a_,0,b__] := Spinor[a,0,b];
tospinorv[a_] := Spinor[a];
tospinorv[a_,b__] := Spinor[-a,b];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynCalcInternal | \n "]];
Null
