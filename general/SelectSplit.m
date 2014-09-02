(* :Title: SelectSplit *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 2 May 2001 at 17:02 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`SelectSplit`",{"HighEnergyPhysics`FeynCalc`"}];

SelectSplit::"usage"=
"SelectSplit[l, p] Construct list of mutually exclusive subsets from l in \
which every element li satisfies a criterium pj[li] with pj from p and \
appends the subset of remaining unmatched elements.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Options[SelectSplit] = {Heads -> None};

SelectSplit[ex_, p_List, opts___Rule] :=
    Block[{ii, jj, aa, res, exp = List @@ ex, h = Head[ex],
        hh = Heads /. Flatten[{opts}] /. Options[SelectSplit]}, ii = 0;
      res = (++ii;
             Select[#, (aa = #;
               And @@ ((#[aa]=!=True)& /@ Drop[p, {ii}])) &])& /@ (Select[exp, #]& /@ p);
      If[hh =!= None && hh =!= False && hh =!= {},
            If[Length[hh] < Length[#] - 1,
              hh = Join[hh, Table[hh[[-1]], {Length[#] - 1 - Length[hh]}]]];
            Append[Table[hh[[jj]][#[[jj]]], {jj, Length[#] - 1}],
              If[Length[hh] === Length[#],
                hh[[-1]][#[[-1]]], #[[-1]]]], #] &[(h @@ #) & /@
          Append[res, Complement[exp, Join @@ res]]]];

End[]; EndPackage[];


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SelectSplit | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
