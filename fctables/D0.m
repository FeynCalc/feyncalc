(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: D0 *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: scalar integral *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`D0`",
             "HighEnergyPhysics`FeynCalc`"];

D0::usage=
"D0[ p10, p12, p23, p30, p20, p13,  m1^2, m2^2, m3^2, m4^2 ] is the
 Passarino-Veltman D0-function. The convention for the arguments is
that if the denominator of the integrand has the form
( [q^2-m1^2] [(q+p1)^2-m2^2] [(q+p2)^2-m3^2] [(q+p3)^2-m4^2] ),
 the first six arguments of D0 are the scalar products
p10 = p1^2, p12 = (p1-p2)^2, p23 = (p2-p3)^2, p30 = p3^2,
p20 = p2^2, p13 = (p1-p3)^2.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

   D0 /:
   MakeBoxes[D0[a_,b_,c_,d_,e_,f_,h_,i_,j_,k_, ___Rule]  ,TraditionalForm] :=
   Tbox[Subscript["D","0"], "(",a,",",b,",",c,",",d,",",e,",",f,
                                  ",",h,",",i,",",j,",",k,")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "D0 | \n "]];
Null
