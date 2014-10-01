(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DiracReduce *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: Last changed July 19th 2000 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: contraction and simplification rules for gamma matrices *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`DiracReduce`",
             {"HighEnergyPhysics`FeynCalc`"}];

DiracReduce::"usage"=
"DiracReduce[exp] reduces all four-dimensional Dirac matrices in exp \
to the standard basis (S,P,V,A,T) using the Chisholm identity (see Chisholm). \
In the result the basic Dirac structures are wrapped with a head \
DiracBasis. I.e.: S corresponds to DiracBasis[1], \
P : DiracBasis[DiracMatrix[5]], \
V : DiracBasis[DiracMatrix[mu]], A: DiracBasis[DiracMatrix[mu, 5]], \
T: DiracBasis[DiracSigma[DiracMatrix[mu, nu]]]. \
By default DiracBasis is substituted to Identity. \n
Notice that the result of DiracReduce is given in the FeynCalcExternal-way, \
i.e., evtl. you may have to use FeynCalcInternal on result.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracGamma = MakeContext["CoreObjects","DiracGamma"];
DiracMatrix = MakeContext["CoreObjects","DiracMatrix"];
DiracSigma = MakeContext["CoreObjects","DiracSigma"];
Eps = MakeContext["CoreObjects","Eps"];
Factoring = MakeContext["CoreOptions","Factoring"];
FinalSubstitutions = MakeContext["CoreOptions","FinalSubstitutions"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Pair = MakeContext["CoreObjects","Pair"];

MakeContext[
    Chisholm,
    Collect2,
    Contract,
    DiracBasis,
    DiracOrder,
    DiracSigmaExplicit,
    DiracSimplify,
    DiracSubstitute67,
    FCE,
    FCI,
    Factor2,
    Rename
    ];

Options[DiracReduce] = {Factoring -> False,
                        FinalSubstitutions -> {DiracBasis -> Identity}
                       };
DiracReduce[x_, {ops___Rule}] := DiracReduce[x, ops];
DiracReduce[x_, ops___Rule] :=
  Block[{temp = FCI[x], spart, n1, n2, ddb, res, finsub,finsub1,factoring},

finsub1 =  If[ Length[{ops}] === 0, {},
               If[!FreeQ[{ops}, FinalSubstitutions],
                  FinalSubstitutions /. {ops},
                  Select[Flatten[{ops}], FreeQ[#,Factoring]&]]
             ];
finsub = Join[finsub1, FinalSubstitutions /. Options[DiracReduce]];
factoring = Factoring /. {ops} /. Options[DiracReduce];

(* do first usual DiracSimplify *)
temp = DiracSimplify[temp, DiracSubstitute67 -> True,
                           DiracSigmaExplicit -> False];
  FCPrint[2,"DiracSimplify done"];
(* Chisholm identity recursively *)
temp = Chisholm[temp]//DiracOrder;
  FCPrint[2,"Chisholm done"];
temp = Expand[temp, DiracGamma];

(* introduce DiracSigma *)
(* use gamma[m,n, 5] = 1/2 ( eps[m,n,r,s] sig[r,s] + 2 g[m,n] gamma[5] )
*)
temp = temp /. DOT[DiracGamma[a_[xx_]], DiracGamma[b_[yy_]], DiracGamma[5]
                  ] :> ( un1 = Unique[mU1]; un2 = Unique[mU2];
                       Expand[
                         1/2 (Eps[a[xx], b[yy], LorentzIndex[un1],
                                                LorentzIndex[un2]] *
                         (I/2) (FCI[ DiracMatrix[un1, un2] -
                                     DiracMatrix[un2, un1] ])  +
                         2 Pair[a[xx], b[yy]] DiracGamma[5])
                             ]
                       );
(* for the renaming of dummy indices *)

temp = Contract[temp, Rename-> True];

(* XXX *)
temp = temp /. DOT[DiracGamma[a_[xx_]], DiracGamma[b_[yy_]]] :>
               ( -I DiracSigma[DiracGamma[a[xx]], DiracGamma[b[yy]]] +
                 Pair[b[yy], a[xx]] );
temp = Contract[DiracSimplify[temp, DiracSigmaExplicit -> False]];
temp = Collect2[temp, DiracGamma, Factoring -> factoring];
  FCPrint[2,"collecting done"];

(* get the S - part *)
spart = Select[temp + n1 + n2, FreeQ[#, DiracGamma]&] /. {n1 :> 0, n2 :> 0};

temp = temp - spart;

If[factoring === False, spart = Expand[spart] DiracBasis[1],
   If[factoring === True, spart = Factor2[spart] DiracBasis[1],
      spart = factoring[spart] DiracBasis[1]]
  ];

ddb[y__] := DiracBasis[DOT[y]];
res = spart + (temp /. DiracSigma[a__] :> DiracBasis[FCE[DiracSigma[a]]] /.
                       DOT[DiracGamma[a_], DiracGamma[5]] :>
                       DiracBasis[FCE[DOT[DiracGamma[a], DiracGamma[5]]]] /.
                       DiracGamma[a_] :> DiracBasis[FCE[DiracGamma[a]]]);
res = res /. finsub /. finsub;
res = FCE[res];
res = res /. finsub /. finsub;
res];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracReduce | \n "]];
Null
