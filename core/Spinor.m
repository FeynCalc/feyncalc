(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: Spinor denotes spinors *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Spinor`",
             "HighEnergyPhysics`FeynCalc`"];

Spinor::"usage" = "Spinor[p, m] represents a Dirac spinor.
Which of the spinors u, v,u_bar or v_bar
is understood, depends on the sign of the momentum (p)
argument and the relative position of DiracSlash[p]:
Spinor[sign p, mass]  is that spinor which yields
sign*mass*Spinor[p, mass] if the Dirac equation is applied .";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[DeclareNonCommutative];
DeclareNonCommutative[Spinor];

momentum := momentum = MakeContext["Momentum"];
MomentumExpand := MomentumExpand = MakeContext["MomentumExpand"];
freeq2   := freeq2 = MakeContext["FreeQ2"];
small    := small  = MakeContext["SmallVariable"];

HoldPattern[Spinor[a__,{1}]] := Spinor[a];

frp[y___] := freeq2[{y}, {Pattern, Blank,BlankSequence,
                          BlankNullSequence,HoldForm}];

Spinor[n_. x_/; (frp[x]&&FreeQ[x, momentum]), y___/;frp[y]] :=
 (Spinor[n x, y] = Spinor[n momentum[x], y]) /;
    (frp[{n, x, y}] && (n^2)===1);

(* this is convention ... *)
Spinor[momentum[x_, di_], m_, op___] := Spinor[momentum[x], m, op];
Spinor[-momentum[x_, di_], m_, op___] := Spinor[-momentum[x], m, op];
Spinor[kk_.+ n_. momentum[ a_Plus ], m_, y___]:=
       Spinor[kk+ n momentum[a], m, y] = (
              Spinor[MomentumExpand[kk + n momentum[a]] ,m,y] );
Spinor[p_ , _. small[_], in___] := Spinor[p, 0, in] /; frp[p];
Spinor[p_ ]                     := Spinor[p,0,1] /; frp[p];
Spinor[p_, m_ /; FreeQ[m, Pattern]] := Spinor[p, m, 1] /; frp[p];

   Spinor /:
    MakeBoxes[Spinor[p_,0,___], TraditionalForm] :=
     Tbox["\[CurlyPhi]","(",p,")"];
   Spinor /:
    MakeBoxes[Spinor[p_,m_ /; m=!=0,___], TraditionalForm] :=
     Tbox["\[CurlyPhi]","(",p, ",", m, ")"];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Spinor | \n "]];
Null
