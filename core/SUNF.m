(* :Summary: SUNF[a, b, c] is the structure constant of SU(N) *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`SUNF`",
               "HighEnergyPhysics`FeynCalc`"];

SUNF::"usage"=
"SUNF[a, b, c] are the structure constants of SU(N).
The arguments a,b,c should be of symbolic type."

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci  := fci = MakeContext["FeynCalcInternal"];

sunindex := sunindex = MakeContext["SUNIndex"];
FreeQ2 := FreeQ2 = MakeContext["FreeQ2"];
Explicit := Explicit = MakeContext["Explicit"];
sunt     := sunt     = MakeContext["SUNT"];
suntrace := suntrace = MakeContext["SUNTrace"];

Options[SUNF] = {Explicit -> False(*, fci -> True*)};

(* IS THIS NECESSARY ?????
SUNF[a___, x_, b___, opt___Rule] := SUNF[a, sunindex[x], b] /;
 FreeQ2[x, {sunindex, Rule, Pattern, BlankSequence}] && FreeQ[{b},Rule] &&
  (FeynCalcInternal /. {opt} /. Options[SUNF])  === True;
*)

(* antisymmetry *)
(* Four arguments are now allowed. SMQCD.mod uses that. F.Orellana, 20/8-2002 *)
HoldPattern[SUNF[a___, x_, b___, x_, c___, ___Rule]] := 0 /;
         (Head[x] === sunindex) && FreeQ[x, Pattern] &&
          Length[{a,x,b,x,c}] == 3;
HoldPattern[SUNF[a___, x_, y_, b___, ___Rule]] := -SUNF[a, y, x, b] /;
FreeQ[{a,x,y,b}, Pattern] && Length[{a,x,y,b}] === 3 &&
(!OrderedQ[{x, y}]) && Head[x] === sunindex && Head[y] === sunindex;

SUNF[i_,j_,k_,Explicit -> False] := SUNF[i,j,k];
HoldPattern[SUNF[i_,j_,k_,op___Rule|op___List]]:= 2 I (suntrace[ fci[sunt[i,k,j]] ] -
                                      suntrace[ fci[sunt[i,j,k] ] ]
                                     )/;
     (Explicit/.Flatten[Join[{op},Options[SUNF]]]) === True;

   tbox[a__] := RowBox @ Map[(MakeBoxes @@ {#, TraditionalForm})&, {a}];

totr[Subscript[y_,in__Integer]] := SubscriptBox[totr[y],RowBox[{in}]];

totr[y_Symbol] := If[FormatValues[Evaluate[y]] === {},
                     ToString[y],
                     ToBoxes[y, TraditionalForm], y];
totr[y_String] := y;
totr[y_] := ToBoxes[y, TraditionalForm] /; Head[y]=!=Symbol;

Tbox[a__] :=
(RowBox @ (Insert[
  Map[totr, {a}], "\[NoBreak]",
    Array[{#}&,Length[{a}]-1,2]]));

   SUNF /:
   MakeBoxes[
             SUNF[a_,b_,c_], TraditionalForm
            ] := SubscriptBox@@{"f", Tbox[a,b,c]};

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SUNF | \n "]];
Null
