(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Collect3*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Collect3`",{"HighEnergyPhysics`FeynCalc`"}];

Collect3::"usage"= 
"Collect3[expr, head] collects all monoomials of the form head[..]*head[..]*..
 in expr. \n
Collect3[expr, {x, y, ...}] collects terms involving the same powers
of monomials x[...]^n1*y[...]^n2 ... The option Factoring can be set to False, True or Factor2;
the latter two of these cause the coefficients to be factored. The option Head (default Plus)
specified the function applied to the list of monomials multiplied by their coefficients.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[Factoring];
   
Options[Collect3] = {Factoring -> False, Head -> Plus};

Collect3[a_,b_,c_Symbol, opts___?OptionQ]:= Collect3[a, b, Factoring -> c, opts];

Collect3[expr_, vars_/;Head[vars]=!=List, opts___Rule] :=
 Collect3[expr, {vars}, opts];
(*
 Collect3[expr, Cases[expr, HoldPattern[vars[___]], -1], opts];
*)

Collect3[expr_, vars_List, opts___?OptionQ] := Block[{fac, hva, mli},
     fac = Factoring/.{opts}/.Options[Collect3];
     If[fac === True, fac = Factor];
(* remember that in FeynCalc.m there is:
    System`MonomialList =
    Internal`FromDistributedTermsList[
      Internal`DistributedTermsList[#1, Sequence@@Drop[{##}, 1]],
    List]&,
*)
     hva = (Hold[HoldPattern][#[___]]& /@ ( Hold/@vars ) ) /. Hold[a_] :> a;
     hva = Alternatives @@ hva;
     mli = MonomialList[expr, Union@Cases[expr,hva,-1]];
     If[fac =!= False, mli = Map[fac, mli]];
     Apply[Head/.{opts}/.Options[Collect3], mli]
];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Collect3 | \n "]];
Null
