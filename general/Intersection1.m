(* :Title: Intersection1 *)

(* :Author: Frederik Orellana *)

(* ------------------------------------------------------------------------ *)
(* :History: created 31 August 2002 at 22:16 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Intersection1`",
             "HighEnergyPhysics`FeynCalc`"];

Intersection1::usage=
"Intersection1[l1, l2], where l1 and l2 are lists returns a list of \
elements both in l1 and l2. Multiple occurences of an element are \
kept the minimum number of times it occures in l1 or l2";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Intersection1[a_List, b_List] := 
    Block[{len, len1, i, alt, p, drp, ii, go}, p = 0; i = 0; drp = {}; 
      len = Length[a]; len1 = Length[b]; alt = b; 
      While[i < len && p < len1, ++i; 
        If[ii = 0; go = True; 
          While[ii < Length[alt] && go, ++ii; 
            If[MatchQ[a[[i]], alt[[ii]]], alt = Drop[alt, {ii}]; go = False]];
           Not[go], p = p + 1; drp = Append[drp, i]]]; Part[a, drp]];

End[]; EndPackage[];


(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Intersection1 | \n "]];
Null
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
