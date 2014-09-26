(* :Title: ILimit *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 9 December 2002 at 10:55 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`ILimit`",{"HighEnergyPhysics`FeynCalc`"}];

ILimit::"usage" = "ILimit[exp, a -> b] checks functions specified by the option \
FunctionLimits and takes the limit a->b of these functions only if it is finite.  \
For the rest of the expression exp, the limit is taken.";


FunctionLimits::"usage" = "FunctionLimits is an option of ILimit, specifying which \
functions should be checked for finiteness.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Options[ILimit] = {FunctionLimits -> {Log -> Log}};

SmallVariable = MakeContext["CoreObjects","SmallVariable"];

ILimit[exp_, lim_Rule, opts___Rule] := 
    Block[{(*limruls, m, ff, fff, out,res*)},
    limruls = 
        MapAt[(((If[FreeQ[ff[##], lim[[1]]] ||
                    !FreeQ[out = Limit[Limit[ff[##] /.
                                              SmallVariable[_?((!MatchQ[#, lim[[1]]])&)] -> 0, 
                                             SmallVariable[lim[[1]]] -> lim[[2]]], 
                                       lim], 
                           DirectedInfinity[___] | Indeterminate | _Limit], 
                     fff[##], 
                     out]&))&) /. 
                 {ff -> #[[2]], fff -> #[[1]]}, #, 2]& /@
               (FunctionLimits /. {opts} /. Options[ILimit]);
    res = exp /. limruls;
    Limit[Limit[res, SmallVariable[lim[[1]]] -> lim[[2]]], 
          lim]];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ILimit | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
