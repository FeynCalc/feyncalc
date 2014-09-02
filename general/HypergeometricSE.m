(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: HypergeometricSE *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`HypergeometricSE`",{"HighEnergyPhysics`FeynCalc`"}];

HypergeometricSE::"usage"=
"HypergeometricSE[exp, nu] expresses Hypergeometric functions by
their series expansion in terms of a sum (the Sum is omitted and
nu, running from 0 to Infinity is the summation index).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Options[HypergeometricSE] = {Simplify -> FullSimplify};

p2g[a_, n_] := Gamma[a + n] / Gamma[a];

HypergeometricSE[exp_, nu_, opt___Rule] := Block[{simp},
 simp = Simplify /. {opt} /. Options[HypergeometricSE];
 exp /.
   {HypergeometricPFQ[{a_, b_}, {c_}, z_] :>
      simp[ p2g[a, nu] p2g[b, nu]/p2g[c,nu]/Gamma[nu+1] ] z^nu,
    Hypergeometric2F1[a_, b_, c_, z_] :>
      simp[ p2g[a, nu] p2g[b, nu]/p2g[c,nu]/Gamma[nu+1] ] z^nu,
    HypergeometricPFQ[ numlist_, denlist_, z_] :>
      simp[ (Times@@Map[p2g[#,nu]&, numlist])/
                    (Times@@Map[p2g[#,nu]&, denlist])/Gamma[nu+1]
                  ] z^nu
   }                                            ];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "HypergeometricSE | \n "]];
Null
