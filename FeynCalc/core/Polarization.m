(* ------------------------------------------------------------------------ *)

(* :Summary: Head for polarization vectors *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Polarization`",{"HighEnergyPhysics`FeynCalc`"}];

Polarization::"usage"=
"Polarization[k] = Polarization[k, I] represents a
polarization momentum with (incoming) momentum k.
A slashed polarization vector (e1(k) slash) has to be entered
as DiracSlash[Polarization[k]].
The internal representation for a polarization vector e1
corresponding to a boson with four momentum k is:
Momentum[ Polarization[ k, I ] ].
With this notation transversality of polarization vectors is
provided, i.e.  Pair[ Momentum[k],
Momentum[ Polarization[k, I] ] ] yields 0.
Polarization[k,-I] denotes the complex conjugate polarization 
originating from application of the ComplexConjugate function.\n
Polarization is also an option.
The setting 0 denotes the unpolarized and 1 the polarized case.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[Pair];

(* by convention *)
Polarization[k_] /;FreeQ[k,Blank|BlankSequence|BlankNullSequence] :=
  Polarization[k] = Polarization[k, I];

Polarization[k_] /;FreeQ[k,Blank|BlankSequence|BlankNullSequence] :=
  Polarization[k] = Polarization[k, I];

Polarization[-x_, I] := -Polarization[x,I];
Polarization[-x_,-I] := -Polarization[x,-I];

(* maybe this should go somewhere else *)
Unprotect[Conjugate];
Conjugate[x_Pair] := (x /. {Polarization[k_,a_,in___] :>
                            Polarization[k,Conjugate[a],in] }
                     ) /;!FreeQ[x, Polarization];
Protect[Conjugate];

Polarization /:
(* suppress color indices in the typesetting for the moment *)
   MakeBoxes[Polarization[a_,Complex[0, 1],___], TraditionalForm] :=
        Tbox["\[CurlyEpsilon]","(",a,")"];

Polarization /:
(* suppress color indices in the typesetting for the moment *)
   MakeBoxes[Polarization[a_, Complex[0, -1],___], TraditionalForm] :=
        Tbox[Superscript["\[CurlyEpsilon]", "*"], "(", a, ")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Polarization | \n "]];
Null
