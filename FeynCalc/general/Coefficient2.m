(* :Tite: Coefficient2 *)

BeginPackage["HighEnergyPhysics`general`Coefficient2`",{"HighEnergyPhysics`FeynCalc`"}];

Coefficient2::usage=
"Coefficient2
fixes a bug in Coefficient (for V2.2) concerning non-integer powers and
Coefficient2[a/x,x,0] returns 0.";

Begin["`Coefficient2`"];

Coefficient2[x_,y__] := Module[{pow},
If[!FreeQ[x, _ ^ n_ /; Head[n]=!=Integer],
   Operate[ReleaseHold,
   Operate[Hold,
   (Coefficient@@({x,y}/.w_ ^ n_ /; Head[n]=!=Integer :> pow[w,n])
   )      ] /. pow->Power
          ],
   Coefficient[x,y]
  ]
  ];

End[];
EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Coefficient2| \n "]];
Null

