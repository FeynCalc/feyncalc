(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: NumericQ1 *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 6 April 2001 at 00:34 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  NumericQ1 is like NumericQ but treats a list of variables as numeric *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`NumericQ1`",
             "HighEnergyPhysics`FeynCalc`"];

NumericQ1::usage=
"NumericQ1[x,{a,b,..}] is like NumericQ, but assumes that {a,b,..} are \
numeric quantities.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

SetAttributes[tag, {NumericFunction,NHoldAll}];

NumericQ1[x_, nums_List] :=
    Block[{r, syms, res, ii=0},
      Off[$MaxExtraPrecision::"meprec"];
      syms = (++ii; tag[ii])& /@ nums;
      Off[$MaxExtraPrecision::"meprec"];
      res = NumericQ[x /. ((Rule @@ #) & /@ Transpose[{nums, syms}])];
      On[$MaxExtraPrecision::"meprec"];
      res
    ];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Solve2 | \n "]];
Null
