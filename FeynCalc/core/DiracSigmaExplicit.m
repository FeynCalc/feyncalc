(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: substitute DiracSigma in terms of DiracGamma's *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`DiracSigmaExplicit`",{"HighEnergyPhysics`FeynCalc`"}];

DiracSigmaExplicit::"usage" =
"DiracSigmaExplicit[exp] inserts in exp the definition of \
DiracSigma. DiracSigmaExplict is also an option of \
DiracSimplify.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

DiracGamma = MakeContext["CoreObjects","DiracGamma"];
DiracMatrix = MakeContext["CoreObjects","DiracMatrix"];
DiracSigma = MakeContext["CoreObjects","DiracSigma"];
DiracSlash = MakeContext["CoreObjects","DiracSlash"];
fci := fci  = MakeContext["FeynCalcInternal"];

dirsigex[a_DiracGamma, b_DiracGamma] := dirsigex[a,b] =
I/2 (DOT[a, b] - DOT[b, a]);

dirsigex[DiracMatrix[a_, b_]] := dirsigex[DiracMatrix[a,b]] =
 I/2 (DiracMatrix[a, b] - DiracMatrix[b, a]);

dirsigex[DiracSlash[a_, b_]] := dirsigex[DiracSlash[a,b]] =
 I/2 (DiracSlash[a, b] - DiracSlash[b, a]);

DiracSigmaExplicit[x_] := fci[x]/. DiracSigma -> dirsigex;

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "DiracSigmaExplicit | \n "]];
Null
