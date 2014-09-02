(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: DB0 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 2 July '97 at 14:41 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  Derivative of B0 *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`DB0`",{"HighEnergyPhysics`FeynCalc`"}];

DB0::"usage"=
"DB0[p2,m1^2,m2^2] is the derivative of the two-point function
B0[p2,m1^2,m2^2] with respect to p2.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

B0 = MakeContext["B0"];
SmallVariable = MakeContext["SmallVariable"];

 Derivative[1, 0, 0][B0][pp_,m02_,m12_]=DB0[pp,m02,m12];
 (* also DB0 is symmetric in its mass arguments *)
  DB0[pe_,me2_,me1_,opt___]:=
     DB0 @@ Prepend[ {me1,me2,opt}, Expand[pe]] /; !OrderedQ[{me2,me1}];

(*
DB0 /: MakeBoxes[DB0 ,TraditionalForm] := 
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DB0 | \n "]];
Null
