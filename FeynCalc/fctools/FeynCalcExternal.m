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
chiralityprojector := chiralityprojector = 
                                     MakeContext["ChiralityProjector"];
diracsigma      := diracsigma      = MakeContext["DiracSigma"];
diracgamma      := diracgamma      = MakeContext["DiracGamma"];
diracmatrix     := diracmatrix     = MakeContext["DiracMatrix"];
diracslash      := diracslash      = MakeContext["DiracSlash"];
diractrace      := diractrace      = MakeContext["DiracTrace"];
dimension       := dimension       = MakeContext["CoreOptions","Dimension"];
Eps             := Eps             = MakeContext["Eps"];
factor2         := factor2         = MakeContext["Factor2"];
FAD             := FAD             = MakeContext["FAD"];
FV              := FV              = MakeContext["FV"];
FVD             := FVD             = MakeContext["FVD"];
fourvector      := fourvector      = MakeContext["FourVector"];
freeq2          := freeq2          = MakeContext["FreeQ2"];
ga              := ga              = MakeContext["GA"];
gad             := gad             = MakeContext["GAD"];
gs              := gs              = MakeContext["GS"];
gsd             := gsd             = MakeContext["GSD"];
LC              := LC              = MakeContext["LC"];
LCD             := LCD             = MakeContext["LCD"];
lorentzindex    := lorentzindex    = MakeContext["LorentzIndex"];
metrictensor    := metrictensor    = MakeContext["MetricTensor"];
levicivita      := levicivita      = MakeContext["LeviCivita"];
mt              := mt              = MakeContext["MT"];
mtd             := mtd             = MakeContext["MTD"];
momentum        := momentum        = MakeContext["Momentum"];
momentumexpand  := momentumexpand  = MakeContext["MomentumExpand"];
MomentumCombine := MomentumCombine = MakeContext["MomentumCombine"];
mult            := mult            = MakeContext["Mult"];
numericalfactor := numericalfactor = MakeContext["NumericalFactor"];
OPEDelta        =  MakeContext["OPEDelta"];
pair            := pair            = MakeContext["Pair"];
Power2 := Power2 = MakeContext["Power2"];
propagatordenominator := 
            propagatordenominator  = MakeContext["PropagatorDenominator"];
feynampdenominator := 
            feynampdenominator= MakeContext["FeynAmpDenominator"];
scalarproduct   := scalarproduct   = MakeContext["ScalarProduct"];
sd            := sd            = MakeContext["SD"];
sund            := sund            = MakeContext["SUND"];
sundeltacontract:= sundeltacontract= MakeContext["SUNDeltaContract"];
sunindex        := sunindex        = MakeContext["SUNIndex"];
sunn            := sunn            = MakeContext["SUNN"];
sunt            := sunt            = MakeContext["SUNT"];
SUNF            := SUNF            = MakeContext["SUNF"];
SP              := SP              = MakeContext["SP"];
SPD             := SPD             = MakeContext["SPD"];
SO             := SO             = MakeContext["SO"];
SOD             := SOD             = MakeContext["SOD"];

SetAttributes[su, HoldAll];
SetAttributes[FeynCalcExternal, HoldFirst];
su[a_String, b_] := If[CheckContext[a], {MakeContext[a] :> b}, {}];
iDent[a_,___] := a;
su[a_String] := If[CheckContext[a], ToExpression[a], sequence[]];

 Options[FeynCalcExternal] = {FinalSubstitutions -> {}};

FeynCalcExternal[x_,opts___Rule] :=
 Block[{ru, ti, r, vv, rv, uru},
sundeltanoi[y__] := sd@@({y} /. sunindex -> Identity);

uru = FinalSubstitutions /. {opts} /. Options[FeynCalcExternal];

ru =  Join[
 su["Power2",Power],
 su["Eps", eps],
 su["SUNF", SUNFback],
 su["ScalarProduct", scalarmul],
 su["MetricTensor", metricmul],
 su["DiracGamma", diracback],
 su["SUNDelta", sundeltanoi],
 su["SUND", sundback],
 su["SUNDeltaContract", sundeltanoi],
 su["SUNT", suntback],
 su["Pair", pairback],
(* su["PolarizationTensor",poltensorback],*)
 su["DiracSigma", dirsig],
 su["FeynAmpDenominator", feynampback],
 su["PropagatorDenominator", propagatordback]
           ] /. lorentzindex -> iDent /. sunindex -> iDent ;
ru = Join[ru, Flatten[{uru}]];

vv = Cases2[x, (su/@{"Power2", "Eps", "SUND", "SUNF", "ScalarProduct", 
                   "MetricTensor", "DiracGamma", "SUNDelta", "SUNT",
                   "Pair", (*"PolarizationTensor", *)"DiracSigma",
                  "FeynAmpDenominator", "PropagatorDenominator"
                   }) /. sequence -> Sequence
           ];

rv = Map[(# ->  ((#//MomentumCombine)/.ru ))&, vv]//Dispatch;
 
x /. rv
(*
If[ru =!={}, (MomentumCombine[x]/.su["DiracSigma", dirsig]/.
              su["Pair",pairback]/.ru
             )/.DOT->doc /. doc -> DOT, x
  ]
*)
];

dirsig[a__] := diracsigma[DOT[a]];

sundback[sunindex[a_], sunindex[b_], sunindex[c_]] := sund[a,b,c];

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
pairback[lorentzindex[a_,d_], lorentzindex[b_,d_]] /;d=!=D := metrictensor[a,b,dimension->d];
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
pairback[momentum[OPEDelta, d_], momentum[b_, d_]] /;d=!=D := pair[momentum[OPEDelta,d], momentum[q,d]];
pairback[momentum[b_, D], momentum[OPEDelta, D]] := SOD[b];
pairback[momentum[b_, d_], momentum[OPEDelta, d_]] /;d=!=D := pair[momentum[OPEDelta,d], momentum[q,d]];
pairback[momentum[a_], momentum[b_]] := SP[a, b];
pairback[momentum[a_, D], momentum[b_, D]] := SPD[a, b];
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
suntback[sunindex[a_]] := sunt[a];
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
