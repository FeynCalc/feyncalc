(* :Title: Combinations *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 2 May 2001 at 17:04 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Combinations`",
             "HighEnergyPhysics`FeynCalc`"];

Combinations::"usage"=
"Combinations[l, n] returns a list of all possible sets containing n \
elements from the list l. (this function is probably in the combinatorics \
package, but we have enough in memory already)";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Combinations[m_List, n_Integer] :=
  Union[Select[
  Sort /@ Flatten[Outer[List, Sequence @@ Table[m, {n}]],
  n - 1], (Union[#] === #) &]];

End[]; EndPackage[];


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Combinations | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
