(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SUND`",
             "HighEnergyPhysics`FeynCalc`"];

SUND::"usage"=
"SUND[a, b, c] is the symmetric SU(N) d_{a,b,c}.";


(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

SetAttributes[SUND, Orderless];

MakeContext[Explicit, SUNT, SUNTrace, SUNIndex, ExplicitSUNIndex];

Options[SUND] = {Explicit -> False};

(* Added check for integers - noint. F.Orellana, 24/9-2000 *)
noint[x___] :=
    Not[Or @@
        Join[IntegerQ /@ {x}, IntegerQ /@
	({x} /. {SUNIndex -> Identity, ExplicitSUNIndex -> Identity})]];

(*Added _SUNIndex to allow SUND in FeynArts couplings. F.Orellana. 4/7-2003*)

SUND[a_SUNIndex,a_SUNIndex,b_,___Rule] := 0 /; noint[a];
SUND[a_,b_,c_, opt___Rule] :=
 2 SUNTrace[SUNT[a,b,c]] + 2 SUNTrace[SUNT[b,a,c]] /;
  (Explicit /. {opt} /. Options[SUND]) === True;

SUND /:
MakeBoxes[SUND[a_, b_,c_, opt___Rule], TraditionalForm] :=
    SubscriptBox["d", Tbox[a,b,c]]

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUND | \n "]];
Null
