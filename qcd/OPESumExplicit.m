(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: OPESumExplicit*)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 11 March '98 at 23:33 *)
(* ------------------------------------------------------------------------ *)

(* :Summary:  the sum *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`OPESumExplicit`",
             "HighEnergyPhysics`FeynCalc`"];

OPESumExplicit::usage= "OPESumExplicit[exp] calculates OPESum's.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[OPEi, OPEj, OPESum];

sum2 = MakeContext["SymbolicSum2"];
sum3 = MakeContext["SymbolicSum3"];
Expand2 = MakeContext["Expand2"];
ExpandScalarProduct = MakeContext["ExpandScalarProduct"];
Factor2 = MakeContext["Factor2"];
Power2 = MakeContext["Power2"];
PowerSimplify = MakeContext["PowerSimplify"];

OPESumExplicit[ex_] := If[FreeQ[ex,OPESum], ex,
 Block[{symbolicsum, te},

If[$VersionNumber <=2.2, 
   If[FreeQ[$ContextPath,"Algebra`SymbolicSum`"],
      If[$VeryVerbose > 0, Print["loading Algebra`SymbolicSum`"]];
         Get["Algebra`SymbolicSum`"];
        ];
  ];

 te = ex/. OPESum->sum3 /. sum3 -> OPESum /. 
      OPESum -> sum2 /. symbolicsum -> sum3 /. sum3 -> summ;
 If[(!FreeQ[te, summ]) && !FreeQ[te, OPEi] && !FreeQ[te,OPEj],
    te = te /.(* {OPEi:>OPEj, OPEj :> OPEi} /.*)
         summ -> sum2 /. sum2 -> sum3 /. sum3 -> summ;
    If[!FreeQ[te, summ]&& !FreeQ[te, OPEi] && !FreeQ[te,OPEj],
    te = te /.summ -> sum2 /. (*{OPEi:>OPEj, OPEj :> OPEi} /.*)
         sum2 -> sum3 /.(* {OPEi:>OPEj, OPEj :> OPEi} /.*)
         sum3 -> summ;
      ];
   ];
  te = te /. summ -> symbolicsum/. symbolicsum -> Sum /.
       Sum -> OPESum;
  te = te//PowerSimplify;
  te = te /. Power[a_, b_/;Head[b] =!= Integer] :> Power2[a,b];
  te = Expand2[te, Power2] /. Power2 -> Power;
  te = te /. Power[a_, b_/;Head[b] =!= Integer] :> Power2[a,b];
te]];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "OPESumExplicit | \n "]];
Null
