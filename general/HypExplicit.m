(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: HypExplicit *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`HypExplicit`",
             "HighEnergyPhysics`FeynCalc`"];

HypExplicit::usage=
"HypExplicit[exp, nu] expresses Hypergeometric functions in exp 
by their definition in terms of a sum (the Sum is omitted and
nu is the summation index).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

p2g[a_, n_] := Gamma[a + n] / Gamma[a];

HypExplicit[exp_, nu_] := exp /.
   {HypergeometricPFQ[{a_, b_}, {c_}, z_] :>
      FullSimplify[ p2g[a, nu] p2g[b, nu]/p2g[c,nu]/Gamma[nu+1] ] z^nu,
    Hypergeometric2F1[a_, b_, c_, z_] :>
      FullSimplify[ p2g[a, nu] p2g[b, nu]/p2g[c,nu]/Gamma[nu+1] ] z^nu,
    HypergeometricPFQ[ numlist_, denlist_, z_] :>
      FullSimplify[ (Times@@Map[p2g[#,nu]&, numlist])/
                    (Times@@Map[p2g[#,nu]&, denlist])/Gamma[nu+1]
                  ] z^nu
   };

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "HypExplicit | \n "]];
Null
