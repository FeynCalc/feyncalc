(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: D0Convention *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:58 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: special option (for Ansgar Denner) for Write2 *) 

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`D0Convention`",{"HighEnergyPhysics`FeynCalc`"}];

D0Convention::"usage" = 
"D0Convention is an option for Write2. If set to 1, the convention for
the arguments of D0 is changed when writing a Fortran file with Write2:
The fifth and sixth argument of D0 are interchanged and the square root is
taken of the last four arguments.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "D0Convention | \n "]];
Null
