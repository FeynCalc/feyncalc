(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FeynCalcExternal *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 6 October '97 at 14:31 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Changes certain objects ("Symbols") from the FeynCalc
             internal representation to the :"input" form
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FeynCalcExternal`",{"HighEnergyPhysics`FeynCalc`"}];

FCE::"usage"=
"FCE is just an abbreviation of FeynCalcExternal.";

FeynCalcExternal::"usage"=
"FeynCalcExternal[exp] translates exp from the internal FeynCalc
representation to the simpler external one
(i.e., FV, GA, GS, etc.). User defined rules can be given
by the option FinalSubstitutions. ";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

FCE = FeynCalcExternal;


FinalSubstitutions= FinalSubstitutions = MakeContext["CoreOptions","FinalSubstitutions"];

Cases2  = MakeContext["Cases2"];
chiralityprojector := chiralityprojector = MakeContext["CoreObjects","ChiralityProjector"];
diracsigma      := diracsigma      = MakeContext["CoreObjects","DiracSigma"];
diracgamma      := diracgamma      = MakeContext["CoreObjects","DiracGamma"];
diracmatrix     := diracmatrix     = MakeContext["CoreObjects","DiracMatrix"];
diracslash      := diracslash      = MakeContext["CoreObjects","DiracSlash"];
diractrace      := diractrace      = MakeContext["DiracTrace"];
dimension       := dimension       = MakeContext["CoreOptions","Dimension"];
Eps             := Eps             = MakeContext["CoreObjects","Eps"];
factor2         := factor2         = MakeContext["Factor2"];
FAD             := FAD             = MakeContext["CoreObjects","FAD"];
FV              := FV              = MakeContext["CoreObjects","FV"];
FVD             := FVD             = MakeContext["CoreObjects","FVD"];
fourvector      := fourvector      = MakeContext["CoreObjects","FourVector"];
freeq2          := freeq2          = MakeContext["FreeQ2"];
ga              := ga              = MakeContext["CoreObjects","GA"];
gad             := gad             = MakeContext["CoreObjects","GAD"];
gs              := gs              = MakeContext["CoreObjects","GS"];
gsd             := gsd             = MakeContext["CoreObjects","GSD"];
LC              := LC              = MakeContext["CoreObjects","LC"];
LCD             := LCD             = MakeContext["CoreObjects","LCD"];
lorentzindex    := lorentzindex    = MakeContext["CoreObjects","LorentzIndex"];
ExplicitLorentzIndex    := ExplicitLorentzIndex    = MakeContext["CoreObjects","ExplicitLorentzIndex"];
metrictensor    := metrictensor    = MakeContext["CoreObjects","MetricTensor"];
levicivita      := levicivita      = MakeContext["LeviCivita"];
mt              := mt              = MakeContext["CoreObjects","MT"];
mtd             := mtd             = MakeContext["CoreObjects","MTD"];
mte             := mte             = MakeContext["CoreObjects","MTE"];
momentum        := momentum        = MakeContext["CoreObjects","Momentum"];
momentumexpand  := momentumexpand  = MakeContext["MomentumExpand"];
MomentumCombine := MomentumCombine = MakeContext["MomentumCombine"];
mult            := mult            = MakeContext["Mult"];
numericalfactor := numericalfactor = MakeContext["NumericalFactor"];
OPEDelta        =  MakeContext["OPEDelta"];
pair            := pair            = MakeContext["CoreObjects","Pair"];
Power2 := Power2 = MakeContext["Power2"];
propagatordenominator :=
            propagatordenominator  = MakeContext["CoreObjects","PropagatorDenominator"];
feynampdenominator :=
            feynampdenominator= MakeContext["CoreObjects","FeynAmpDenominator"];
scalarproduct   := scalarproduct   = MakeContext["ScalarProduct"];
sd            := sd            = MakeContext["CoreObjects","SD"];
sund            := sund            = MakeContext["CoreObjects","SUND"];
sundeltacontract:= sundeltacontract= MakeContext["SUNDeltaContract"];
sunindex        := sunindex        = MakeContext["CoreObjects","SUNIndex"];
ExplicitSUNIndex    := ExplicitSUNIndex    = MakeContext["CoreObjects","ExplicitSUNIndex"];
sunn            := sunn            = MakeContext["CoreObjects","SUNN"];
sunt            := sunt            = MakeContext["CoreObjects","SUNT"];
SUNF            := SUNF            = MakeContext["CoreObjects","SUNF"];
SP              := SP              = MakeContext["CoreObjects","SP"];
SPD             := SPD             = MakeContext["CoreObjects","SPD"];
SPE             := SPE             = MakeContext["CoreObjects","SPE"];
SO             := SO             = MakeContext["CoreObjects","SO"];
SOD             := SOD             = MakeContext["CoreObjects","SOD"];

SetAttributes[FeynCalcExternal, HoldFirst];
iDent[a_,___] := a;

 Options[FeynCalcExternal] = {FinalSubstitutions -> {}};

FeynCalcExternal[x_,opts___Rule] :=
 Block[{ru, ti, r, vv, rv, uru},
sundeltanoi[y__] := sd@@({y} /. sunindex -> Identity);

uru = FinalSubstitutions /. {opts} /. Options[FeynCalcExternal];

ru =  Join[
 {HighEnergyPhysics`FeynCalc`CoreObjects`DiracGamma :> diracback},
 {HighEnergyPhysics`FeynCalc`CoreObjects`DiracSigma :> dirsig},
 {HighEnergyPhysics`FeynCalc`CoreObjects`Eps :> eps},
 {HighEnergyPhysics`FeynCalc`CoreObjects`FeynAmpDenominator :> feynampback},
 {HighEnergyPhysics`FeynCalc`CoreObjects`MetricTensor :> metricmul},
 {HighEnergyPhysics`FeynCalc`CoreObjects`Pair :> pairback},
 {HighEnergyPhysics`FeynCalc`CoreObjects`PropagatorDenominator :> propagatordback},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUND :> sundback},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUNDelta :> sundeltanoi},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUNF :> SUNFback},
 {HighEnergyPhysics`FeynCalc`CoreObjects`SUNT :> suntback},
 {HighEnergyPhysics`FeynCalc`SUNDeltaContract`SUNDeltaContract :> sundeltanoi},
 {HighEnergyPhysics`FeynCalc`ScalarProduct`ScalarProduct :> scalarmul},
 {HighEnergyPhysics`qcd`Power2`Power2 :> Power}
           ] /. lorentzindex -> iDent /. sunindex -> iDent ;
ru = Join[ru, Flatten[{uru}]];

vv = Cases2[x, Join[{},
    {
        HighEnergyPhysics`FeynCalc`CoreObjects`DiracGamma,
        HighEnergyPhysics`FeynCalc`CoreObjects`DiracSigma,
        HighEnergyPhysics`FeynCalc`CoreObjects`Eps,
        HighEnergyPhysics`FeynCalc`CoreObjects`FeynAmpDenominator,
        HighEnergyPhysics`FeynCalc`CoreObjects`MetricTensor,
        HighEnergyPhysics`FeynCalc`CoreObjects`Pair,
        HighEnergyPhysics`FeynCalc`CoreObjects`PropagatorDenominator,
        HighEnergyPhysics`FeynCalc`CoreObjects`SUND,
        HighEnergyPhysics`FeynCalc`CoreObjects`SUNDelta,
        HighEnergyPhysics`FeynCalc`CoreObjects`SUNF,
        HighEnergyPhysics`FeynCalc`CoreObjects`SUNT,
        HighEnergyPhysics`FeynCalc`SUNDeltaContract`SUNDeltaContract,
        HighEnergyPhysics`FeynCalc`ScalarProduct`ScalarProduct,
        HighEnergyPhysics`qcd`Power2`Power2
         }] /. sequence -> Sequence
           ];

rv = Map[(# ->  ((MomentumCombine[#,LeafCount -> 1000])/.ru ) )&, vv]//Dispatch;
x /. rv
(*
If[ru =!={}, (MomentumCombine[x,LeafCount -> 1000]/. HighEnergyPhysics`FeynCalc`CoreObjects`DiracSigma :> dirsig /.
              su["Pair",pairback]/.ru
             )/.DOT->doc /. doc -> DOT, x
  ]
*)
];

dirsig[a__] := diracsigma[DOT[a]];

sundback[(sunindex|ExplicitSUNIndex)[a_] | a_, (sunindex|ExplicitSUNIndex)[b_] | b_, (sunindex|ExplicitSUNIndex)[c_] | c_] := sund[a,b,c];

SUNFback[sunindex[a_], sunindex[b_], sunindex[c_]] := SUNF[a, b, c];
SUNFback[a_, b_, c_] := SUNF[a, b, c] /; FreeQ[{a,b,c},sunindex];

(*
doc[b:diracmatrix[_]..] := Apply[diracmatrix, {b}/. diracmatrix->Identity];
doc[b:diracslash[_]..]  := Apply[diracslash,  {b}/. diracslash->Identity];
*)

metricmul[a_ b_,dim__] := metrictensor[a,b,dim];
metricmul[a_, b_,dim___] := metrictensor[a,b,dim];

scalarmul[a_ b_,dim__] := scalarproduct[a,b,dim];
scalarmul[a_, b_,dim___] := scalarproduct[a,b,dim];

(*dimension d_ stuff below added by F.Orellana 19/9-2000
  to allow other dimension symbols than D*)

pairback[lorentzindex[a_], lorentzindex[b_]] := mt[a,b];
pairback[lorentzindex[a_,D], lorentzindex[b_,D]] := mtd[a,b];
pairback[lorentzindex[a_,D-4], lorentzindex[b_,D-4]] := mte[a,b];
pairback[lorentzindex[a_,d_], lorentzindex[b_,d_]] /;(d=!=D && d=!= D-4) := metrictensor[a,b,dimension->d];
(*
 metrictensor[a, b];
*)
pairback[lorentzindex[a_], momentum[b_]] := FV[b,a];
pairback[lorentzindex[a_,D], momentum[b_,D]] := FVD[b,a];
pairback[lorentzindex[a_,d_], momentum[b_,d_]] /;d=!=D := fourvector[b,a,dimension->d];
(*
pairback[lorentzindex[a_], momentum[b_]] := fourvector[b, a];
*)
pairback[momentum[OPEDelta], momentum[b_]] := SO[b];
pairback[momentum[b_], momentum[OPEDelta] ] := SO[b];
pairback[momentum[OPEDelta, D], momentum[b_, D]] := SOD[b];
pairback[momentum[OPEDelta, d_], momentum[b_, d_]] /;d=!=D := pair[momentum[OPEDelta,d], momentum[b,d]];
pairback[momentum[b_, D], momentum[OPEDelta, D]] := SOD[b];
pairback[momentum[b_, d_], momentum[OPEDelta, d_]] /;d=!=D := pair[momentum[OPEDelta,d], momentum[b,d]];
pairback[momentum[a_], momentum[b_]] := SP[a, b];
pairback[momentum[a_, D], momentum[b_, D]] := SPD[a, b];
pairback[momentum[a_, D-4], momentum[b_, D-4]] := SPE[a, b];
pairback[momentum[a_, d_], momentum[b_, d_]] /;d=!=D := scalarproduct[a, b, dimension -> d];
(*
pairback[momentum[a_], momentum[b_]] := scalarproduct[a, b];
*)
diracback[lorentzindex[a_]] := ga[a];
diracback[lorentzindex[a_,D],D] := gad[a];
diracback[lm_[a_,n_/;n=!=D],en___] :=
                (*Corrected 19/9-2000. F.Orellana*)(*DiracGamma*)diracgamma[lm[a,n],en];
diracback[momentum[a_]] := gs[a];
diracback[momentum[a_, n_Symbol], n_Symbol] := gsd[a];
diracback[5] := ga[5];
diracback[6] := ga[6];
diracback[7] := ga[7];
diracback[ExplicitLorentzIndex[x_?NumberQ]] := ga[x];
diracback[x_?NumberQ] := ga[x];
suntback[(sunindex|ExplicitSUNIndex)[a_]] := sunt[a];
suntback[a__Symbol] := sunt[a];
propagatordback[a_,b_] := propagatordenominator[a/.momentum->iDent, b];
propd[a_, 0] := a /. momentum->iDent;
propd[a_, b_/;b=!=0] := {a/.momentum->iDent, b};
feynampback[a__] := FAD @@ ({a} /. propagatordenominator -> propd);

eps[momentum[a_],momentum[b_],momentum[c_],momentum[d_]
   ] := LC[][a,b,c,d];
eps[lorentzindex[a_],momentum[b_],momentum[c_],momentum[d_]
   ] := LC[a][b,c,d];
eps[lorentzindex[a_],lorentzindex[b_],momentum[c_],momentum[d_]
   ] := LC[a,b][c,d];
eps[lorentzindex[a_],lorentzindex[b_],lorentzindex[c_],momentum[d_]
   ] := LC[a,b,c][d];
eps[lorentzindex[a_],lorentzindex[b_],
    lorentzindex[c_],lorentzindex[d_]
   ] := LC[a,b,c,d];

eps[momentum[a_,D],momentum[b_,D],momentum[c_,D],momentum[d_,D]
   ] := LCD[][a,b,c,d];
eps[lorentzindex[a_,D],momentum[b_,D],
    momentum[c_,D],momentum[d_,D]
   ] := LCD[a][b,c,d];
eps[lorentzindex[a_],lorentzindex[b_],
    momentum[c_,D],momentum[d_,D]
   ] := LCD[a,b][c,d];
eps[lorentzindex[a_,D], lorentzindex[b_,D],
    lorentzindex[c_,D], momentum[d_,D]
   ] := LCD[a,b,c][d];
eps[lorentzindex[a_,D],lorentzindex[b_,D],
    lorentzindex[c_,D],lorentzindex[d_,D]
   ] := LCD[a,b,c,d];

eps[momentum[a_,dd_],momentum[b_,dd_],momentum[c_,dd_],momentum[d_,dd_]
   ] /;dd=!=D := levicivita[dimension->dd][a,b,c,d,dimension->dd];
eps[lorentzindex[a_,dd_],momentum[b_,dd_],
    momentum[c_,dd_],momentum[d_,dd_]
   ] /;dd=!=D := levicivita[a,dimension->dd][b,c,d,dimension->dd];
eps[lorentzindex[a_],lorentzindex[b_],
    momentum[c_,dd_],momentum[d_,dd_]
   ] /;dd=!=D := levicivita[a,b,dimension->dd][c,d,dimension->dd];
eps[lorentzindex[a_,dd_], lorentzindex[b_,dd_],
    lorentzindex[c_,dd_], momentum[d_,dd_]
   ] /;dd=!=D := levicivita[a,b,c,dimension->dd][d,dimension->dd];
eps[lorentzindex[a_,dd_],lorentzindex[b_,dd_],
    lorentzindex[c_,dd_],lorentzindex[d_,dd_]
   ] /;dd=!=D := levicivita[a,b,c,d,dimension->dd];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynCalcExternal | \n "]];
Null
