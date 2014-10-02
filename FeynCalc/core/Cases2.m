(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Cases2`",{"HighEnergyPhysics`FeynCalc`"}];

Cases2::"usage"=
"Cases2[expr, f] is equivalent to \
Cases[{expr}, HoldPattern[f[___]], Infinity]//Union. \
Cases2[expr, f1, f2, ...] or \
Cases2[expr, {f1, f2, ...}] is equivalent to \
Cases[{expr}, f1[___] | f2[___] ..., Infinity]//Union.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Options[Cases2] = {Heads -> False};

Cases2[expr_, {f___}, opts:OptionsPattern[]] := Cases2 @@ Prepend[{f,opts}, expr];

Cases2[expr_, f_, opts:OptionsPattern[]] := Union[Cases[{expr}, HoldPattern[f[___]],
                                           Infinity,opts]];


Cases2[expr_, f_, g__, opts:OptionsPattern[]] := Union[
    Cases[{expr},
              Alternatives@@(#[___]&/@{f,g}),Infinity, FilterRules[{opts}, Options[Cases]]]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Cases2 | \n "]];
Null
