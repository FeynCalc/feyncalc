(* :Title: Complement1 *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 31 August 2002 at 22:16 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Complement1`",
             "HighEnergyPhysics`FeynCalc`"];

Complement1::usage=
"Complement1[l1, l2], where l1 and l2 are lists returns a list of \
elements from l1 not in l2. Multiple occurences of an element in l1 are \
kept and multiple occurences of an element in l2 are dropped multiply if \
present in l1";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

(*
If one does not need MatchQ, but just SameQ, then
Complement1[x_List, y__List] :=
 Replace[x, Dispatch[(# :> Sequence[]) & /@ Union[y]], 1]

would be sufficient
*)

Complement1[a_List, b_List] := 
    Block[{len, len1, i, alt, p, drp, ii, go}, p = 0; i = 0; drp = {}; 
      len = Length[a]; len1 = Length[b]; alt = b; 
      While[i < len && p < len1, ++i; 
        If[ii = 0; go = True; 
          While[ii < Length[alt] && go, ++ii; 
            If[MatchQ[a[[i]], alt[[ii]]], alt = Drop[alt, {ii}]; go = False]];
           Not[go], p = p + 1; drp = Append[drp, i]]]; 
      Part[a, Complement[Range[len], drp]]];

End[]; EndPackage[];


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Complement1 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
