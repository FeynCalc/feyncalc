(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Anti5															*)

(*
   This software is covered by the GNU Lesser General Public License 3.
   Copyright (C) 1990-2014 Rolf Mertig
   Copyright (C) 1997-2014 Frederik Orellana
   Copyright (C) 2014 Vladyslav Shtabovenko
*)

(* :Summary:  Anti5 anticommutes gamma5's right or left					    *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Anti5`",{"HighEnergyPhysics`FeynCalc`"}];

Anti5::"usage" =
"Anti5[exp] anticommutes all gamma5 one time to the right. \
Anti5[exp, n] anticommutes all gamma5 n times to the right. \
Anti5[exp, -n] anticommutes all gamma5 n times to the left.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracGamma = MakeContext["CoreObjects","DiracGamma"];
MakeContext[FeynCalcInternal];

Anti5[expr_Plus, n_] := Map[Anti5[#,n]&,expr];
Anti5[expr_, _] := expr /; FreeQ[FeynCalcInternal[expr], DiracGamma[5]]  ;
Anti5[expr_, Infinity] := FixedPoint[Anti5, expr, $RecursionLimit];
Anti5[expr_, -Infinity] := FixedPoint[Anti5[#,-1]&, expr, $RecursionLimit];
Anti5[expr_, n_Integer?Positive] := Nest[Anti5, expr, n]/; (n =!= 1);
Anti5[expr_, n_Integer?Negative] := Nest[Anti5[#,-1]&, expr, -n] /; (n =!= -1);
Anti5[expr_]:= Anti5[expr, 1];

Anti5[xx_, n_] :=
    Block[ {HoldDOT, ruleBMHVRight, ruleAnticommute, ruleNaiveRight, ruleBMHVLeft, ruleNaiveLeft, temp, result},
        temp = FeynCalcInternal[xx] /. DOT -> HoldDOT;
		FCPrint[3,"Entering Anti5 with  ", temp];
        ruleAnticommute = {
            (*Naive scheme, move gamma^5 to the right*)
            HoldDOT[a___, DiracGamma[5], DiracGamma[y_[x__], di___], b___]/; (n===1) && ($BreitMaison =!= True) :>
                  (-HoldDOT[a,DiracGamma[y[x],di],DiracGamma[5],b]),

           (*BMHV scheme, move gamma^5 to the right*)
            HoldDOT[a___, DiracGamma[5], DiracGamma[y_[x_]], b___]/; (n===1) && ($BreitMaison === True) :>
                  (-HoldDOT[a,DiracGamma[y[x]],DiracGamma[5],b]),
            HoldDOT[a___, DiracGamma[5], DiracGamma[y_[x_,di_Symbol], di_Symbol ], b___ ]/; (n===1) && ($BreitMaison === True) :>
                  (-HoldDOT[a,DiracGamma[y[x], di], DiracGamma[5], b] +
                    2 HoldDOT[a,DiracGamma[y[x,di-4],di-4],DiracGamma[5],b]),

           (*Naive scheme, move gamma^5 to the left*)
            HoldDOT[a___, DiracGamma[y_[x__], di___], DiracGamma[5], b___]/; (n===-1) && ($BreitMaison =!= True) :>
              (-HoldDOT[a, DiracGamma[5], DiracGamma[y[x],di], b]),

           (*BMHV scheme, move gamma^5 to the left*)
            HoldDOT[a___,  DiracGamma[y_[x_]], DiracGamma[5], b___]/; (n===-1) && ($BreitMaison === True) :>
                  (-HoldDOT[a, DiracGamma[5], DiracGamma[y[x]], b]),
            HoldDOT[a___, DiracGamma[y_[x_,di_Symbol], di_Symbol ], DiracGamma[5], b___ ]/; (n===-1) && ($BreitMaison === True) :>
                  (-HoldDOT[a, DiracGamma[5], DiracGamma[y[x], di], b] +
                    2 HoldDOT[a,DiracGamma[y[x,di-4],di-4],DiracGamma[5],b])
        };

	result = temp /. ruleAnticommute /.HoldDOT[a___, DiracGamma[5], DiracGamma[5],b___ ] :> HoldDOT[a,b] /. HoldDOT -> DOT;
	FCPrint[3,"Leaving Anti5 with  ", result];
	result
    ]/; (n===1 || n===-1);

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Anti5 | \n "]];
Null
