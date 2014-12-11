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


FinalSubstitutions:= FinalSubstitutions = MakeContext["CoreOptions","FinalSubstitutions"];
Cases2          := Cases2          = MakeContext["Cases2"];
DiracSigma      := DiracSigma      = MakeContext["CoreObjects","DiracSigma"];
DiracGamma      := DiracGamma      = MakeContext["CoreObjects","DiracGamma"];
Dimension       := Dimension       = MakeContext["CoreOptions","Dimension"];
FAD             := FAD             = MakeContext["CoreObjects","FAD"];
FV              := FV              = MakeContext["CoreObjects","FV"];
FVD             := FVD             = MakeContext["CoreObjects","FVD"];
FVE             := FVE             = MakeContext["CoreObjects","FVE"];
FourVector      := FourVector      = MakeContext["CoreObjects","FourVector"];
GA              := GA              = MakeContext["CoreObjects","GA"];
GAD             := GAD             = MakeContext["CoreObjects","GAD"];
GAE             := GAE             = MakeContext["CoreObjects","GAE"];
GS              := GS              = MakeContext["CoreObjects","GS"];
GSD             := GSD             = MakeContext["CoreObjects","GSD"];
GSE             := GSE             = MakeContext["CoreObjects","GSE"];
LC              := LC              = MakeContext["CoreObjects","LC"];
LCD             := LCD             = MakeContext["CoreObjects","LCD"];
LorentzIndex    := LorentzIndex    = MakeContext["CoreObjects","LorentzIndex"];
ExplicitLorentzIndex    := ExplicitLorentzIndex    = MakeContext["CoreObjects","ExplicitLorentzIndex"];
MetricTensor    := MetricTensor    = MakeContext["CoreObjects","MetricTensor"];
LeviCivita      := LeviCivita      = MakeContext["LeviCivita"];
MT              := MT              = MakeContext["CoreObjects","MT"];
MTD             := MTD             = MakeContext["CoreObjects","MTD"];
MTE             := MTE             = MakeContext["CoreObjects","MTE"];
Momentum        := Momentum        = MakeContext["CoreObjects","Momentum"];
MomentumCombine := MomentumCombine = MakeContext["MomentumCombine"];
OPEDelta        := OPEDelta        =  MakeContext["OPEDelta"];
Pair            := Pair            = MakeContext["CoreObjects","Pair"];
PropagatorDenominator :=
            PropagatorDenominator  = MakeContext["CoreObjects","PropagatorDenominator"];
ScalarProduct   := ScalarProduct   = MakeContext["ScalarProduct"];
SD            := SD            = MakeContext["CoreObjects","SD"];
SUND            := SUND            = MakeContext["CoreObjects","SUND"];
SUNIndex        := SUNIndex        = MakeContext["CoreObjects","SUNIndex"];
ExplicitSUNIndex    := ExplicitSUNIndex    = MakeContext["CoreObjects","ExplicitSUNIndex"];
SUNT            := SUNT            = MakeContext["CoreObjects","SUNT"];
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
sundeltanoi[y__] := SD@@({y} /. SUNIndex -> Identity);

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
           ] /. LorentzIndex -> iDent /. SUNIndex -> iDent ;
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

dirsig[a__] := DiracSigma[DOT[a]];

sundback[(SUNIndex|ExplicitSUNIndex)[a_] | a_, (SUNIndex|ExplicitSUNIndex)[b_] | b_, (SUNIndex|ExplicitSUNIndex)[c_] | c_] := SUND[a,b,c];

SUNFback[SUNIndex[a_], SUNIndex[b_], SUNIndex[c_]] := SUNF[a, b, c];
SUNFback[a_, b_, c_] := SUNF[a, b, c] /; FreeQ[{a,b,c},SUNIndex];

(*
doc[b:diracmatrix[_]..] := Apply[diracmatrix, {b}/. diracmatrix->Identity];
doc[b:diracslash[_]..]  := Apply[diracslash,  {b}/. diracslash->Identity];
*)

metricmul[a_ b_,dim__] := MetricTensor[a,b,dim];
metricmul[a_, b_,dim___] := MetricTensor[a,b,dim];

scalarmul[a_ b_,dim__] := ScalarProduct[a,b,dim];
scalarmul[a_, b_,dim___] := ScalarProduct[a,b,dim];

(*Dimension d_ stuff below added by F.Orellana 19/9-2000
  to allow other dimension symbols than D*)

pairback[LorentzIndex[a_], LorentzIndex[b_]] := MT[a,b];
pairback[LorentzIndex[a_,D], LorentzIndex[b_,D]] := MTD[a,b];
pairback[LorentzIndex[a_,D-4], LorentzIndex[b_,D-4]] := MTE[a,b];
pairback[LorentzIndex[a_,d_], LorentzIndex[b_,d_]] /;(d=!=D && d=!= D-4) := MetricTensor[a,b,Dimension->d];
(*
 MetricTensor[a, b];
*)
pairback[LorentzIndex[a_], Momentum[b_]] := FV[b,a];
pairback[LorentzIndex[a_,D], Momentum[b_,D]] := FVD[b,a];
pairback[LorentzIndex[a_, D-4], Momentum[b_, D-4]] := FVE[b,a];
pairback[LorentzIndex[a_,d_], Momentum[b_,d_]] /;d=!=D := FourVector[b,a,Dimension->d];
(*
pairback[LorentzIndex[a_], Momentum[b_]] := fourvector[b, a];
*)
pairback[Momentum[OPEDelta], Momentum[b_]] := SO[b];
pairback[Momentum[b_], Momentum[OPEDelta] ] := SO[b];
pairback[Momentum[OPEDelta, D], Momentum[b_, D]] := SOD[b];
pairback[Momentum[OPEDelta, d_], Momentum[b_, d_]] /;d=!=D := Pair[Momentum[OPEDelta,d], Momentum[b,d]];
pairback[Momentum[b_, D], Momentum[OPEDelta, D]] := SOD[b];
pairback[Momentum[b_, d_], Momentum[OPEDelta, d_]] /;d=!=D := Pair[Momentum[OPEDelta,d], Momentum[b,d]];
pairback[Momentum[a_], Momentum[b_]] := SP[a, b];
pairback[Momentum[a_, D], Momentum[b_, D]] := SPD[a, b];
pairback[Momentum[a_, D-4], Momentum[b_, D-4]] := SPE[a, b];
pairback[Momentum[a_, d_], Momentum[b_, d_]] /;d=!=D := ScalarProduct[a, b, Dimension -> d];
(*
pairback[Momentum[a_], Momentum[b_]] := scalarproduct[a, b];
*)
diracback[LorentzIndex[a_]] := GA[a];
diracback[LorentzIndex[a_,D],D] := GAD[a];
diracback[LorentzIndex[a_,D-4],D-4] := GAE[a];
diracback[lm_[a_,n_/;(n=!=D && n=!=D-4)],en___] :=
                (*Corrected 19/9-2000. F.Orellana*)(*DiracGamma*)DiracGamma[lm[a,n],en];
diracback[Momentum[a_]] := GS[a];
diracback[Momentum[a_, n_Symbol], n_Symbol] := GSD[a];
diracback[Momentum[a_, n_Symbol-4], n_Symbol-4] := GSE[a];
diracback[5] := GA[5];
diracback[6] := GA[6];
diracback[7] := GA[7];
diracback[ExplicitLorentzIndex[x_?NumberQ]] := GA[x];
diracback[x_?NumberQ] := GA[x];
suntback[(SUNIndex|ExplicitSUNIndex)[a_]] := SUNT[a];
suntback[a__Symbol] := SUNT[a];
propagatordback[a_,b_] := PropagatorDenominator[a/.Momentum->iDent, b];
propd[a_, 0] := a /. Momentum->iDent;
propd[a_, b_/;b=!=0] := {a/.Momentum->iDent, b};
feynampback[a__] := FAD @@ ({a} /. PropagatorDenominator -> propd);

eps[Momentum[a_],Momentum[b_],Momentum[c_],Momentum[d_]
   ] := LC[][a,b,c,d];
eps[LorentzIndex[a_],Momentum[b_],Momentum[c_],Momentum[d_]
   ] := LC[a][b,c,d];
eps[LorentzIndex[a_],LorentzIndex[b_],Momentum[c_],Momentum[d_]
   ] := LC[a,b][c,d];
eps[LorentzIndex[a_],LorentzIndex[b_],LorentzIndex[c_],Momentum[d_]
   ] := LC[a,b,c][d];
eps[LorentzIndex[a_],LorentzIndex[b_],
    LorentzIndex[c_],LorentzIndex[d_]
   ] := LC[a,b,c,d];

eps[Momentum[a_,D],Momentum[b_,D],Momentum[c_,D],Momentum[d_,D]
   ] := LCD[][a,b,c,d];
eps[LorentzIndex[a_,D],Momentum[b_,D],
    Momentum[c_,D],Momentum[d_,D]
   ] := LCD[a][b,c,d];
eps[LorentzIndex[a_],LorentzIndex[b_],
    Momentum[c_,D],Momentum[d_,D]
   ] := LCD[a,b][c,d];
eps[LorentzIndex[a_,D], LorentzIndex[b_,D],
    LorentzIndex[c_,D], Momentum[d_,D]
   ] := LCD[a,b,c][d];
eps[LorentzIndex[a_,D],LorentzIndex[b_,D],
    LorentzIndex[c_,D],LorentzIndex[d_,D]
   ] := LCD[a,b,c,d];

eps[Momentum[a_,dd_],Momentum[b_,dd_],Momentum[c_,dd_],Momentum[d_,dd_]
   ] /;dd=!=D := LeviCivita[Dimension->dd][a,b,c,d,Dimension->dd];
eps[LorentzIndex[a_,dd_],Momentum[b_,dd_],
    Momentum[c_,dd_],Momentum[d_,dd_]
   ] /;dd=!=D := LeviCivita[a,Dimension->dd][b,c,d,Dimension->dd];
eps[LorentzIndex[a_],LorentzIndex[b_],
    Momentum[c_,dd_],Momentum[d_,dd_]
   ] /;dd=!=D := LeviCivita[a,b,Dimension->dd][c,d,Dimension->dd];
eps[LorentzIndex[a_,dd_], LorentzIndex[b_,dd_],
    LorentzIndex[c_,dd_], Momentum[d_,dd_]
   ] /;dd=!=D := LeviCivita[a,b,c,Dimension->dd][d,Dimension->dd];
eps[LorentzIndex[a_,dd_],LorentzIndex[b_,dd_],
    LorentzIndex[c_,dd_],LorentzIndex[d_,dd_]
   ] /;dd=!=D := LeviCivita[a,b,c,d,Dimension->dd];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FeynCalcExternal | \n "]];
Null
