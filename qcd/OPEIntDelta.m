(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPEIntDelta *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 13 March '98 at 19:17 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: calculation the delta(1-x) part *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPEIntDelta`",
             "HighEnergyPhysics`FeynCalc`"];
(*
and the PlusDistribution[1/(1-x)].
*)

OPEIntDelta::"usage"=
"OPEIntDelta[expr, x, m] introduces
the delta(1-x) (DeltaFunction[1-x]).
The Mathematica Integrate function is called and each integration
(from 0 to 1) is
recorded for reference (and bug-checking) in the global list
$MIntegrate.\n
Notice that
the dimension specified by the option should also be the dimension
used in expr. It is replaced in OPEIntDelta by (4+Epsilon).";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[
ChangeDimension,
Collect2,
DeltaFunction,
Dimension,
Epsilon,
EpsilonOrder,
Factor2,
FinalSubstitutions,
Integrate2,
ScaleMu,
PlusDistribution,
Select1,
Select2,
Series2,
Trick,
Zeta2    ];

Options[OPEIntDelta] = {Dimension -> D, EpsilonOrder -> 1,
                        PolynomialDivision -> True, 
                        FinalSubstitutions -> 
                          {Log[ScaleMu^2 _] :> 0}};

OPEIntDelta[expr_, x_, m_, ops___Rule] := Block[
 {tt, locdim, null, xmpart, fypart, lpa, new, dim, reg, nonreg,
  mint, ppi, kernel, rkern, f1, kfa, intsave, eporder, res,ttii,
   ttx, finsub},
 eporder = EpsilonOrder /. {ops} /. Options[OPEIntDelta];
 dim     = Dimension /. {ops} /. Options[OPEIntDelta];
 finsub = FinalSubstitutions /. {ops} /. Options[OPEIntDelta];
 locdim = Unique["Global`D"];
 tt = ChangeDimension[Trick[expr], locdim];
 tt = Map[Factor2, Expand[tt, x] + null[1] + null[2]] /. null[_] :> 0;
 tt = tt /. dim -> ( 4 + Epsilon );
(* separate regular and non-regular part *)
 tt = tt /. ((1-x) x)^aa_ :> ((1-x)^aa x^aa);
(* drop the 1 - part *)
 tt = Collect2[tt, x] + null[1] + null[2];
(*$DROP = Select1[tt, x^(_. m + _.)]/.null[1]->0/.null[2]->0;*)
 tt = Select2[tt, x^(_. m + _.)];
 tt = Factor2[tt];

If[(PolynomialDivision /. {ops} /. Options[OPEIntDelta])=!=True,
   reg = 0;
(*
 NOT (uniquely) POSSIBLE ...
   tt = Collect2[tt, x] + null[1] + null[2];
   reg = 0;
   Off[Power::infy];
   For[it = 1, it <= Length[tt], it++,
       If[(PowerExpand[tt[[it]]] /. x->1 /. Epsilon -> 0
           ) =!= DirectedInfinity[],
          reg = reg + tt[[it]]
         ];
      ];
   reg = reg /. null[1] -> 0 /.  null[2] -> 0;
   tt = (tt - reg)/. null[1] -> 0 /.  null[2] -> 0;
*)
,

 If[Head[tt] === Times,
    ttx = Select2[tt, (w_. + v_. x)^(aa_/;Head[aa] =!= Integer)];
If[$VeryVerbose > 0, Print["ttx = ",ttx]];
    tt = tt / ttx,
    ttx = 1
   ];
 If[Head[tt] === Times,
    ttnox = Select1[tt, x];
Print["poltest"];
Dialog[tt];
    ttp   = PolynomialDivision[Select2[tt, x], 1-x, x],
    ttnox = 1,
    ttp   = PolynomialDivison[tt, 1-x, x]
   ];
 reg    = ttnox ttx Factor2[ttp[[1]]] (1-x);
(*
 reg = Collect2[reg,x];
 reg = Select2[reg + null[1] + null[2], x^(_. m + _.)];
*)
 nonreg = ttnox ttx ttp[[2]];
 tt = Collect2[nonreg, x];
 ];

If[$VeryVerbose > 1, Print["reg = ",reg]];
(*
Dialog[reg];
*)

(*
(* drop the 1 - part *)
 xmpart = Collect2[tt, x] + null[1];
 xmpart = Select2[xmpart, x^(_. m + _.)];
*)
 xmpart = Factor2[tt];

(*
Dialog[xmpart];
*)

(* nenene
If[Limit[(1-x)^(Epsilon/2), x -> 1, Direction -> 1] =!= 0,
   Unprotect[Limit];
   Limit[(1-x)^(Epsilon/2), x -> 1, Direction -> 1] = 0;
   Limit[x^(1+Epsilon/2), x -> 0, Direction -> -1] = 0;
   Protect[Limit];
  ];
*)

(* get f(y): divide by (x^(m-1) (1-x)^(Epsilon2/2 -1)) *)
 fypart = Collect2[(xmpart/x^(m-1) (1-x)^(1-Epsilon/2)
                      ) /. ((1-x) x)^aa_ :> ((1-x)^aa x^aa), x
                 ];
fypart  = fypart ;

If[$VeryVerbose > 0, Print["fypart = ",fypart]; ];
 lpa = Length[fypart];
 new = 0;

(*
 For[i = 1, i <= lpa, i++, ppi = fypart[[i]];
*)
(*
     If[Head[ppi] =!= null,
       If[$VeryVerbose > 0, Print["integrating # ",i-2, " out of ",lpa-2]];
*)
ppi = Factor2[fypart dummy];
       kernel = Select2[ppi, x];
       kfa  = (ppi/kernel) /. dummy -> 1;
       fy = PowerExpand[Simplify[(kernel (*(1-x)^(1-Epsilon/2)*))]];
If[$VeryVerbose > 0, Print["fy = ",fy//InputForm]];
       f1 = fy /. x -> 1 /. 0^(1-Epsilon/2)->0;
If[$VeryVerbose > 0, Print["f1 = ",f1//InputForm]];
       If[f1 === ComplexInfinity, Print["fy = ", fy]; Dialog[];];
       rkern = Expand[(1-x)^(Epsilon/2) (fy-f1)/(1-x)];
(* adapt for the silly Mma - Integrate function *)
       rkern = rkern /. (1-x)^e1_ x^e2_ :> ( (x(1-x))^e1 x^(e2-e1) );
       If[$VeryVerbose > 0, Print["integrate ",rkern]];
       mint = Integrate2[rkern, {x, 0, 1}];
       mint = Simplify[mint /. {Sqrt[Pi] 2^a_ :> 
                                (Gamma[1/2-a/2] Gamma[1-a/2]/Gamma[1-a])
                               }
                      ];
       mint = SimplifyPolyGamma[mint];
       intsave = rkern -> mint;
       If[!MemberQ[$MIntegrate, intsave], 
          AppendTo[$MIntegrate, intsave]
         ];
       If[$VeryVerbose > 0, Print["Taylor expansion"]];
       mint = Series2[kfa mint, {Epsilon, 0, eporder}]/.finsub;
       If[FreeQ[rkern, Pi^2], mint = mint /. Pi^2 -> (6 Zeta2)];
       If[!FreeQ[mint, PolyGamma], mint = SimplifyPolyGamma[mint]];
       new = new + mint;
(*
       ];
*)
(*
     ];
*)
res = ChangeDimension[x^(m-1) DeltaFunction[1-x] new + xmpart  + reg,
                      dim];
res];
 
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPEIntDelta | \n "]];
Null
