(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: ExpandScalarProduct expands scalar products *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`ExpandScalarProduct`",
             "HighEnergyPhysics`FeynCalc`"];

ExpandScalarProduct::"usage" =
"ExpandScalarProduct[expr]  expands scalar products of sums of
momenta in expr.
ExpandScalarProduct[x, y] expands ScalarProduct[x, y], where
x and y may contain sums. ExpandScalarProduct does not use Expand on
expr.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

fci  := fci                  = MakeContext["FeynCalcInternal"];
lorentzindex := lorentzindex = MakeContext["LorentzIndex"];
memset := memset             = MakeContext["MemSet"];
momentum := momentum         = MakeContext["Momentum"];
momentumexpand := momentumexpand = MakeContext["MomentumExpand"];
sCO := sCO                   = MakeContext["PairContract"];
pair := pair                 = MakeContext["Pair"];

Options[ExpandScalarProduct] = {fci -> True};

(* since one never can remember this function  ...*)
(* well, not used, so commented out. F.Orellana, 8/11-2002. *)
(*FRH = FixedPoint[ReleaseHold, #]&;*)

(*
(* schwachsinn *)
ExpandScalarProduct[x_] := If[LeafCount[x]<42,
     Expand[FixedPoint[pairexpand1,fci[x], 3]//momentumexpand ],
            FixedPoint[pairexpand1,fci[x], 3]//momentumexpand
                              ];
*)
ExpandScalarProduct[x_,ru___Rule] :=
If[(fci /. {ru} /. Options[ExpandScalarProduct]),
   FixedPoint[pairexpand1,fci[x], 3]//momentumexpand,
   FixedPoint[pairexpand1, x, 3]//momentumexpand
  ];

ExpandScalarProduct[x_, y_ /;Head[y] =!= Rule] := scev[x, y];

(* Catch Pair[LorentzIndex[mu], Momentum[a] + Momentum [b] +...].
   F.Orellana. 26/2-2003 *)
extraMomRule = pair[lorentzindex[a__],
               b : Plus[(___*momentum[__] | momentum[__]),
                        (___*momentum[__] | momentum[__]) ...]]  :>
               (pair[lorentzindex[a], #]& /@ b);

pairexpand1[x_]:=  x /. pair->scevdoit /. extraMomRule;


(* not always a good idea (IFPD)
scev[x_,y_]:= memset[ scev[x,y], scevdoit[x,y] ];
*)
scev = scevdoit;
scevdoit[x_,y_] := Distribute[ sceins@@
                              ( Expand[ momentumexpand/@{x,y} ] )
                             ]/.sceins->sczwei/.
                             sczwei(*->sCO/.sCO*)->pair;

sceins[0,_]:=0;                               (*sceinsdef*)
sceins[a_lorentzindex b_, c_] := b sceins[a, c];
sceins[a_momentum b_, c_] := b sceins[a, c];
sczwei[ _[_],_[_,_Symbol-4] ]:=0;             (*sczweidef*)
sczwei[ v_[x_,di_Symbol-4],w_[y_,di_Symbol] ]:= sczwei[v[x,di-4],w[y,di-4]];
sczwei[ w_[y_,di_Symbol],v_[x_] ]:=sczwei[ v[x],w[y] ];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "ExpandScalarProduct | \n "]];
Null
