(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: ChangeDimension *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: change dimension from 4 to D or back *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`ChangeDimension`",{"HighEnergyPhysics`FeynCalc`"}];

ChangeDimension::"usage"=
"ChangeDimension[exp, dim] changes all LorentzIndex and Momenta in
exp to dimension dim (and thus also Dirac slashes and Dirac matrices
in FeynCalcInternal-representation).";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracGamma = MakeContext["CoreObjects","DiracGamma"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];
Spinor = MakeContext["CoreObjects","Spinor"];

MakeContext[FeynCalcInternal];

ChangeDimension[x_, diim_] := Block[
{xx = FeynCalcInternal[x], dirGG, dirGAMM, pAiR, ld , md,spi4},
If[ diim === 4,
    xx = xx /. {LorentzIndex[a_,___] :> LorentzIndex[a] ,
                Momentum[b_,___]     :> Momentum[b],
                DiracGamma[c_,___]   :> DiracGamma[c]
               }
   ,
    ld[a_,___]     := LorentzIndex[a, diim];
    md[a_,___]     := Momentum[a, diim];
    spi4[a_, b__]  := Spinor[a /. Momentum[bbb_, ___] -> Momentum[bbb], b];
    dirGG[aa_,___] := dirGAMM[aa, diim]/; !MatchQ[aa,5|6|7];
    dirGG[(aa: 5|6|7)] := dirGAMM[aa];
    xx = xx /. Pair -> pAiR /. DiracGamma -> dirGAMM;
    xx = xx /. {Momentum :> md, LorentzIndex -> ld} /.
               {md :> Momentum, ld :> LorentzIndex} ;
    xx = xx /. dirGAMM -> dirGG/.  dirGAMM -> DiracGamma /. pAiR -> Pair;
    xx = xx /. Spinor -> spi4;
  ];
                                 xx];
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ChangeDimension | \n "]];
Null
