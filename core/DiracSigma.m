(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: DiracSigma[x,y] = I/2 (x  .  y -  y . x )
              DiracSigma[DiracMatrix[x,y]] =
                I/2 (DiracMatrix[x, y] -  DiracMatrix[y, x])
*)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracSigma`",
             "HighEnergyPhysics`FeynCalc`"];

DiracSigma::"usage" =
"DiracSigma[a, b] stands for I/2*(a . b - b . a) in 4 dimensions. \
a and b must have Head DiracGamma, DiracMatrix or DiracSlash. \
Only antisymmetry is implemented.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[ DiracGamma, DiracMatrix, DiracSlash];

If[FreeQ[$NonComm, DiracSigma] && Head[$NonComm]===List,
   AppendTo[$NonComm, DiracSigma]];

(* by definition *)
DiracSigma[DOT[a_,b_]] := DiracSigma[a,b];
DiracSigma[___, 0, ___]       = 0;
DiracSigma[a_, b_] := - DiracSigma[b, a] /; !OrderedQ[{a,b}];

DiracSigma[DiracMatrix[a_, b_]] :=
   - DiracSigma[DiracMatrix[b, a]] /; !OrderedQ[{a,b}];

DiracSigma[DiracSlash[a_, b_]] :=
   - DiracSigma[DiracSlash[b, a]] /; !OrderedQ[{a,b}];

(*NEW 8/97 *)
DiracSigma[a_ DiracGamma[b__], c_. DiracGamma[d__]] :=
 a c DiracSigma[DiracGamma[b], DiracGamma[d]];

DiracSigma[a_. DiracGamma[b__], c_  DiracGamma[d__]] :=
 a c DiracSigma[DiracGamma[b], DiracGamma[d]];

   DiracSigma /:
   MakeBoxes[DiracSigma[_[x_,___], _[y_,___]], TraditionalForm] :=
   SuperscriptBox["\[Sigma]", Tbox[x,y]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSigma | \n "]];
Null
