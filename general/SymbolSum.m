(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SymbolSum*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:01 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`general`SymbolSum`",
             "HighEnergyPhysics`FeynCalc`"];

SymbolSum::usage= "SymbolSum[] is like Sum but fixes a bug.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   


( SymbolSum[x_,y__,{iz_,jz_,kz_}] := 
  If[FreeQ[{y},iz], SymbolSum[SymbolSum[x,{iz,jz,kz}],y],
                    SymbolSum[SymbolSum[x,y],{iz,jz,kz}]
    ];

  SymbolSum[a_,b_] := Block[{myapart},
  myapart[c_]:=
  If[Head[c]===Plus,Apart/@c,Apart[c]];
  myapart[myapart[PowerExpand[Sum[a,b]]]]];
);

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SymbolSum | \n "]];
Null
