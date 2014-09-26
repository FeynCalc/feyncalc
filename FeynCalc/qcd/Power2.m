(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Power2 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Power2 is like Power *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`Power2`",{"HighEnergyPhysics`FeynCalc`"}];

Power2::"usage"=
"Power2[x, y] represents x^y.  Sometimes Power2 is more useful than the
Mathematica Power. Power2[-a,b] simplifies to (-1)^b Power2[a,b]
(if no Epsilon is in b ...)."; 

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   
Epsilon = MakeContext["CoreObjects","Epsilon"];
MakeContext[Epsilon2, OPEm, PowerSimplify];

(*
Power2 /: Power2[a_, b_] Power2[a_, c_] := Power2[a,b+c];
*)
Power2 /: Power2[-1,OPEm]^2 := 1;
Power2[n_Integer?Positive, em_] := n^em;
Power2[n_, em_Integer] := n^em;
Power2[-1,OPEm-2] = Power2[-1,OPEm];
Power2[-a_,b_/;FreeQ[b, Epsilon]&&FreeQ[b,Epsilon2]] := 
  PowerSimplify[(-1)^b] Power2[a,b];
Format[Power2[a_, b_ /; b]] := a^b;

 Power2 /: 
   MakeBoxes[Power2[a_, b_] , TraditionalForm] := 
    ToBoxes[a^b, TraditionalForm];
   

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Power2 | \n "]];
Null
