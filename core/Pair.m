(* ------------------------------------------------------------------------ *)
(* ------------------------------------------------------------------------ *)

(* :Summary: The head of four-vectors, metric tensor and
             scalar products. *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`FeynCalc`Pair`",
             "HighEnergyPhysics`FeynCalc`"];

Pair::"usage"=
"Pair[a , b] is a special pairing used in the internal
representation: a and b may have heads LorentzIndex or Momentum.
If both a and b have head LorentzIndex, the metric tensor is
understood. If a and b have head Momentum, a scalar product is
meant. If one of a and b has head LorentzIndex and the other
Momentum, a Lorentz vector (p_mu) is understood.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[FreeQ2,LorentzIndex, Momentum, MomentumCombine, Polarization];


contract            := contract = MakeContext["Contract"];
expandscalarproduct := expandscalarproduct =
                       MakeContext["ExpandScalarProduct"];

SetAttributes[Pair, Orderless];
Pair[0,_] := 0;
Pair[n_Integer x_,y_] := n Pair[x, y];
Pair[n_ x_Momentum, y_] := n Pair[x, y];

Pair[ lom_[la_,d_Symbol], mol_[pe_]] := Pair[ lom[la], mol[pe] ] /;
  MemberQ[{LorentzIndex, Momentum}, lom] &&
     MemberQ[{LorentzIndex, Momentum}, mol] ;

Pair[Momentum[x_,___],Momentum[Polarization[x_, ___],___]] := 0;
Pair[Momentum[x_,___],Momentum[Polarization[n_?NumberQ x_, ___],___]
    ] := 0;
Pair[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= contract[expandscalarproduct[Pair[
             Momentum[x+pi, dii], Momentum[Polarization[x, ki], dii]]]
                ] /; ( pi + Last[x] ) === 0;
Pair[Momentum[pi_,___],Momentum[Polarization[x_Plus, ki___], dii___]
    ]:= contract[expandscalarproduct[Pair[
             Momentum[pi-x,dii], Momentum[Polarization[x, ki],dii]]]
                ] /; ( pi - Last[x] ) === 0;
(* by convention ... *)
Pair[Momentum[Polarization[x_,__],___],
     Momentum[Polarization[x_,__],___] ] := -1;

(* ******************************************************************** *)
Unprotect[Conjugate];
Conjugate[x_Pair] := (x /. {Polarization[k_,a_,in___] :>
                            Polarization[k,Conjugate[a],in] }
                     ) /;!FreeQ[x, Polarization];
Protect[Conjugate];
(* ******************************************************************** *)

Pair /:
   MakeBoxes[Pair[
(HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a_,d1___],
(HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[b_,d2___] ],
             TraditionalForm
            ] := If[$LorentzIndices===True,
                    SuperscriptBox["g", Tbox[LorentzIndex[a,d1], LorentzIndex[b,d2]] ],
                    SuperscriptBox["g", Tbox[a,b] ]
                   ];

MakeBoxes[Pair[a_,b_]^n_Integer?Positive, TraditionalForm] :=
 RowBox[{SuperscriptBox[Tbox[Pair[a,b]],n]}];

initialDownValues = DownValues[Pair];

Pair /:
   MakeBoxes[Pair[
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__],
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__]
                 ], TraditionalForm
            ] := SuperscriptBox[Tbox[Momentum[a]],2] /; FreeQ[{a},Plus];

MakeBoxes[Pair[
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__],
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__]
                 ]^2, TraditionalForm
            ] := SuperscriptBox[Tbox[Momentum[a]],4] /; FreeQ[{a},Plus];

MakeBoxes[Pair[
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__],
    HighEnergyPhysics`FeynCalc`Momentum`Momentum[a__]
                 ]^3, TraditionalForm
            ] := SuperscriptBox[Tbox[Momentum[a]],6];

(* Changed because of infinite recursion on
   Pair[a Momentum[k] + b Momentum[p], a Momentum[k] + b Momentum[p]]
   Frederik Orellana, 17/3-2001 *)
(*Pair/:
       MakeBoxes[Pair[a_Plus,b_],TraditionalForm] :=
        ToBoxes[Pair[MomentumCombine[a],MomentumCombine[b]],
                TraditionalForm] /; !FreeQ[a, Momentum] &&
                                    !FreeQ[b, Momentum];*)

Pair /: MakeBoxes[Pair[a_Plus, b_], TraditionalForm] :=
    RowBox[{"(", ToBoxes[TraditionalForm[a]], ")",".","(",
          ToBoxes[TraditionalForm[b]], ")"}] /; !FreeQ[a, Momentum] && 
          !FreeQ[b, Momentum];

Pair /:
        MakeBoxes[Pair[
          HighEnergyPhysics`FeynCalc`Momentum`Momentum[a_Plus,di___],
          HighEnergyPhysics`FeynCalc`Momentum`Momentum[a_Plus,dii___]],
                  TraditionalForm
                 ] := SuperscriptBox[Tbox["(", a,")"],2];

MakeBoxes[Pair[
          HighEnergyPhysics`FeynCalc`Momentum`Momentum[a_,di___],
          HighEnergyPhysics`FeynCalc`Momentum`Momentum[a_,dii___]
              ]^m_Integer,
          TraditionalForm
         ] := SuperscriptBox[Tbox["(", a,")"], #]&@@{2m};

Pair /:
        MakeBoxes[Pair[
          HighEnergyPhysics`FeynCalc`Momentum`Momentum[a_,di___],
          HighEnergyPhysics`FeynCalc`Momentum`Momentum[b_,dii___]],
                  TraditionalForm
                 ] := Which[
                       FreeQ2[{a,b},{Times,Plus}],
                       If[$PairBrackets === True,
                          Tbox["(", Momentum[a,di], "\[CenterDot]",
                                    Momentum[b,dii], ")"
                              ],
                          Tbox[Momentum[a,di], "\[CenterDot]",
                               Momentum[b,dii]]
                         ],
                       FreeQ2[{a},{Times,Plus}],
                       Tbox[Momentum[a,di],"\[CenterDot]",
                            "(",Momentum[b,dii],")"],
                       FreeQ2[{b},{Times,Plus}],
                       Tbox["(",Momentum[a,di],")","\[CenterDot]",
                            Momentum[b,dii]],
                       !FreeQ2[{a,b},{Times,Plus}],
                       Tbox["(",Momentum[a,di],")","\[CenterDot]",
                            "(",Momentum[b,dii],")"]
                           ];

Pair /:
   MakeBoxes[Pair[
      (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
      HighEnergyPhysics`FeynCalc`Momentum`Momentum[
      HighEnergyPhysics`FeynCalc`Polarization`Polarization[
                              b_,Complex[0,1]],___]
                 ], TraditionalForm
            ] := RowBox[{
        SubscriptBox["\[CurlyEpsilon]",
                     Tbox[LorentzIndex[a]]],
                     "(",Tbox[b],")"}];

Pair /:
   MakeBoxes[Pair[
      (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
      HighEnergyPhysics`FeynCalc`Momentum`Momentum[
      HighEnergyPhysics`FeynCalc`Polarization`Polarization[
                              b_,Complex[0,-1]],___]
                 ], TraditionalForm
            ] := RowBox[{
        SubsuperscriptBox["\[CurlyEpsilon]",
                          Tbox[LorentzIndex[a]], "*"
                          ], "(", Tbox[b], ")"
                        }
                       ];

Pair /:
   MakeBoxes[Pair[
              (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[
                   b_Subscripted, di___]
                 ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[b[[1,0]]],
                                   Tbox@@b[[1]],
                                    Tbox[LorentzIndex[a]]];

Pair /:
   MakeBoxes[Pair[
              (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[
                   b_Subscript,di___]
                 ], TraditionalForm
            ] := SubsuperscriptBox[Tbox[b[[1]]], Tbox@@Rest[b],
                                    Tbox[LorentzIndex[a]]];
 
Pair /:
   MakeBoxes[Pair[
              (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[b_,di___]
                 ],
             TraditionalForm
            ] := SuperscriptBox[
                    Tbox[Momentum[b,di] ], Tbox[LorentzIndex[a]] 
                   ]/;Head[b]=!=Plus;

Pair /:
   MakeBoxes[Pair[
              (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[b_Plus,di___]
                 ],
             TraditionalForm
            ] := SuperscriptBox[
                    Tbox[ "(",Momentum[b,di], ")"], Tbox[LorentzIndex[a]] ];

Pair /:
   MakeBoxes[Pair[
              (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex|
      HighEnergyPhysics`FeynCalc`ExplicitLorentzIndex`ExplicitLorentzIndex)[a__],
              HighEnergyPhysics`FeynCalc`Momentum`Momentum[b_, di___] +c_
                 ],
             TraditionalForm
            ] := SuperscriptBox[
                    Tbox[ "(",Momentum[b+c,di], ")"], Tbox[LorentzIndex[a]] ];

MakeBoxes[Pair[HighEnergyPhysics`FeynCalc`Momentum`Momentum[a_,___],
               HighEnergyPhysics`FeynCalc`Momentum`Momentum[a_,___]
              ]^n_Integer, TraditionalForm] := SuperscriptBox[TBox[a], #]&@@{2 n};
                                                                                                                        
End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Pair | \n "]];
Null
