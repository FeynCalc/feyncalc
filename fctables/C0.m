(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: C0 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`C0`",
             "HighEnergyPhysics`FeynCalc`"];
C0::"usage"=
"C0[p10, p12, p20, m1^2, m2^2, m3^2] is the scalar
Passarino-Veltman C0-function.  The convention for the arguments
is that if the denominator of the integrand has the form
([q^2-m1^2] [(q+p1)^2-m2^2] [(q+p2)^2-m3^2]),
the first three arguments of C0 are the scalar products
 p10 = p1^2, p12 = (p1-p2).(p1-p2), p20 = p2^2.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

   MakeBoxes[C0[a_,b_,c_,d_,e_,f_, ___Rule] ,TraditionalForm] :=
   Tbox[Subscript["C", "0"], "(", a, ",", b, ",", 
        c, ",", d, ",", e, ",",f,")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "C0 | \n "]];
Null
