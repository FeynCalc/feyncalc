(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Hill*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`Hill`",
             "HighEnergyPhysics`FeynCalc`"];

Hill::usage= "Hill[x,y] gives the Hill identity with arguments x and y.
The returned object is 0.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   SetAttributes[Hill, ReadProtected];

Factor2 = MakeContext["Factor2"];

Hill[x_, y_] := PolyLog[2,x] - PolyLog[2,y] + 
PolyLog[2,Factor2[y/x]] +
PolyLog[2, Factor2[(1-x)/(1-y)]] - 
PolyLog[2, Factor2[y/x (1-x)/(1-y)]] - Pi^2/6 +
Log[x] (Log[Factor2[1-x]] - Log[Factor2[1-y]]) + 
Log[Factor2[(1-x)/(1-y)]] ( Log[Factor2[(x-y)/(1-y)]]-Log[x]-
Log[Factor2[(x-y)/x]] + Log[Factor2[1-y]]
                          ) - 
Log[Factor2[y/x (1-x)/(1-y)]] (Log[Factor2[(x-y)/(x (1-y))]] -
Log[Factor2[(x-y)/x]] + Log[Factor2[1-y]]
                              )
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Hill | \n "]];
Null
