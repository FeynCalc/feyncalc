(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Factor1 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Comments: Works rather slow for sums with > 10^3 terms.
              Factor1 should be superfluos once the bugs in Factor 
              are fixed 
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Factor1`",{"HighEnergyPhysics`FeynCalc`"}];

Factor1::"usage"=
"Factor1[poly] factorizes common terms  in the summands of poly.
It uses basically PolynomialGCD.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

numericalfactor = MakeContext["NumericalFactor"];


Factor1[x_] := Block[{factor1t1,factor1t2,factor1t3,mt,mi,m1,mp1,nx=x,iIii},
mt = (((# /. Plus -> mi /. mi -> Plus) /. m1 -> (-1)/.mp1 -> (-Plus[##]&)
      ) /. iIii -> I)&;
mi[y_, z__] := (m1 mp1[y,z] )/; (If[ Head[#] === Complex, False,
               If[# < 0, True, False]]& @ numericalfactor[y]);
    nx = x /. Complex[0, b_] -> (b iIii);
    If[ Head[nx] =!= Plus, mt[nx /. Plus -> (Factor1[Plus[##]]&)],
        factor1t1 = Apply[ List, Map[# /. Plus -> factor1t3&, nx] ];
        factor1t2 = (PolynomialGCD @@ factor1t1) /. factor1t3 -> Plus;
        mt[(factor1t2 Apply[Plus, 
                         Map[((# /. factor1t3 -> Plus) / factor1t2)&, 
         factor1t1]])]
      ]               ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Factor1 | \n "]];
Null
