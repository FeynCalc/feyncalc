(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DB1*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 19 October '97 at 0:53 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the m *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`DB1`",
             "HighEnergyPhysics`FeynCalc`"];

DB1::usage=
"DB1[p2,m1^2,m2^2] is the derivative of B1[p2,m1^2,m2^2] with respect to p2.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

BReduce = MakeContext["BReduce"];
B1 = MakeContext["B1"];
DB0 = MakeContext["DB0"];
SmallVariable = MakeContext["SmallVariable"];

 Derivative[1, 0, 0][be1_][pp_,m02_,m12_]:= DB1[pp,m02,m12] /; be1 === B1;

 DB1[m_, m_, 0, opt___] := (- DB0[m,m,0] + 1/2/m) /; (BReduce/.{opt}/.
   Options[DB1]) === True

nos[x_] := If[x =!= 0 && FreeQ[x, SmallVariable], True, False];

HoldPattern[DB1[pp_, m02_, m12_, opt___]]:=(
- (m12 - m02)/(2 pp^2) ( B0[pp,m02,m12] - B0[0,m02,m12] ) +
(m12 - m02 - pp)/(2 pp) DB0[pp,m02,m12] ) /; nos[pp] && ( (BReduce/.{opt}/.
   Options[DB1]) === True );


(*
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DB1 | \n "]];
Null
