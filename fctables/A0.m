(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: A0 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 24 March '98 at 16:16 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`A0`",
             "HighEnergyPhysics`FeynCalc`"];

A0::"usage" = 
"A0[m^2] is the Passarino-Veltman one point integral.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[A0ToB0, B0, BReduce];
small = MakeContext["SmallVariable"];

 Options[A0] = {A0ToB0 -> False};
 
 A0[small[_]^_. , ___] := 0;  (* In dimensional regularization: A0(0)=0 *)
 A0[0,___] = 0;
 (*A0[mm_, A0ToB0 -> False] := A0[mm];*)
 A0[mm_, op___Rule]:=(mm + mm B0[0,mm,mm]) /;
   ( (( A0ToB0/.{op}/.Options[A0] )===True) && (!( BReduce/.Options[B0])) 
       && FreeQ[mm,Pattern] && FreeQ[mm, BlankSequence] && 
         FreeQ[mm, Blank] && FreeQ[mm,BlankNullSequence] );

   A0 /: 
   MakeBoxes[A0[a_]  ,TraditionalForm] :=
   Tbox[Subscript["A","0"], "(", a, ")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "A0 | \n "]];
Null
