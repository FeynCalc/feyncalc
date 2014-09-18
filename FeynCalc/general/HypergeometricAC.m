(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: HypergeometricAC *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`HypergeometricAC`",{"HighEnergyPhysics`FeynCalc`"}];

HypergeometricAC::"usage"=
"HypergeometricAC[n][exp] analytically continues 
Hypergeometric2F1 functions in exp.
The second argument n refers to the equation number (n) in chapter 2.10 of
\"Higher Transcendental Functions\" by Ergelyi, Magnus,
Oberhettinger, Tricomi.
In case of eq. (6) (p.109) the last line is returned for 
HypergeometricAC[6][exp],
while the first equality is given by HypergeometricAC[61][exp].
((2.10.1) is identical to eq. (9.5.7) of \"Special Fucntions & their
Applications\" by N.N.Lebedev).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

Cases2   = MakeContext["Cases2"];
Collect2 = MakeContext["Collect2"];
Factor2  = MakeContext["Factor2"];

Options[HypergeometricAC] = {Collect2 -> True};

HypergeometricAC[n_Integer][exp_, opt___Rule] := Block[
{g = Gamma, A1, A2, B1, B2, f21=Hypergeometric2F1, 
 res, colopt, vars},
 colopt = Collect2 /. {opt} /. Options[HypergeometricAC];

 A1[a_,b_,c_] := g[c] g[c-a-b]/g[c-a]/g[c-b];
 A2[a_,b_,c_] := g[c] g[a+b-c]/g[a]/g[b];
 B1[a_,b_,c_] := g[c] g[b-a]/g[b]/g[c-a];
 B2[a_,b_,c_] := g[c] g[a-b]/g[a]/g[c-b];

 res = 
 exp /.
  Which[ n === 1,
                 f21[a_,b_,c_,z_]  :>
                 A1[a,b,c] f21[a,b,a+b-c+1,1-z] +
                 A2[a,b,c] (1-z)^(c-a-b) f21[c-a,c-b,c-a-b+1,1-z]
               ,
         n === 2,
                 f21[a_,b_,c_,z_]  :>
                 B1[a,b,c] (-z)^(-a) f21[a,1-c+a,1-b+a,1/z] +
                 B2[a,b,c] (-z)^(-b) f21[b,1-c+b,1-a+b,1/z]
               ,
         n === 3,
                 f21[a_,b_,c_,z_]  :>
                 B1[a,b,c] (1-z)^(-a) f21[a,c-b,a-b+1,Factor[1/(1-z)]] +
                 B2[a,b,c] (1-z)^(-b) f21[b,c-a,b-a+1,Factor[1/(1-z)]] 
               ,
         n === 4,
                 f21[a_,b_,c_,z_]  :>
                 A1[a,b,c]  (z)^(-a) f21[a,a+1-c,a+b+1-c,Factor[1-1/z]] +
                 A2[a,b,c]  (z)^(a-c) (1-z)^(c-a-b) *
                   f21[c-a,1-a,c+1-a-b,Factor[1-1/z]] 
                ,
         n === 6,
                 f21[a_,b_,c_,z_]  :>
                 Factor[1-z]^(-b) f21[b,c-a,c, Factor[z/(z-1)]]
                 ,
         n === 61,
                 f21[a_,b_,c_,z_]  :>
                 Factor[1-z]^(-a) f21[a,c-b,c, Factor[z/(z-1)]]
       ]/.(1/u_)^ep_ :> u^(-ep);
  If[colopt === True, 
     vars = Variables[Last/@ Cases2[exp /. f21 -> hyp, hyp]];
     res = Factor2 /@ Collect2[res, vars]
    ];
  res
 ];
        

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "HypergeometricAC | \n "]];
Null
