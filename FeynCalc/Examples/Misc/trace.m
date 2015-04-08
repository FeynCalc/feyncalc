
<<FeynCalc`

(* Exercise 7.  from 2.5.6, page 59 *)
gstring[n_Integer] := Dot@@(GAD[nu[#]]&/@Range[n]);

FI;

SetAttributes[PrintTime, HoldAll];

PrintTime[a_] := Module[{ti=AbsoluteTime[]},
   tmp = a;
    Print[InputForm[tmp]];
      Print[AbsoluteTime[] - ti," seconds used"];
   tmp
];

Do[Print["\n", "n = ",n," :"];
PrintTime[
 FullSimplify /@ Factor2[
   TR[gstring[n].gstring[n]] /. D -> (d-4) ]],
{n,8,13}]

