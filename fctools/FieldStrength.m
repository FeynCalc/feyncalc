(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FieldStrength *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 16 March '98 at 11:52 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: F_{\mu \nu}^a *)

(* ------------------------------------------------------------------------ *)

MyBeginPackage["HighEnergyPhysics`fctools`FieldStrength`",
             "HighEnergyPhysics`FeynCalc`"];

FieldStrength::"usage" =
"FieldStrength[mu,nu,a] is the field strength tensor
F_{mu nu}^a = partial_mu A_nu^a - partial_nu A_mu^a +
g f^{abc} A_mu^b A_nu^c.
FieldStrength[mu,nu] is the field strength tensor
F_{mu nu}^a = partial_mu A_nu^a - partial_nu A_mu.
The name of the field (A) and the coupling constant (g)
can be set through the options or by additional arguments:
FieldStrength[mu,nu,a, A, g] or, specifying the dummy
color indices: FieldStrength[mu,nu,a, {A,b,c}, g].";

IndexPosition::"usage"=
"IndexPosition is an option for FieldStrength.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

MakeContext[
CouplingConstant,
DeclareNonCommutative,
Explicit,
FreeQ2,
GaugeField,
Gstrong,
IndexPosition,
LorentzIndex,
Momentum,
OPEDelta,
PartialD,
RightPartialD,
QuantumField,
SUNF,
SUNIndex];

DeclareNonCommutative[FieldStrength];

   Options[FieldStrength] = {CouplingConstant -> Gstrong,
                             Explicit -> False,
                             IndexPosition -> {0,0},
                             Symbol -> "F",
                             QuantumField -> GaugeField};

   FieldStrength[mu___, OPEDelta, nu___] :=
     FieldStrength[mu, Momentum[OPEDelta], nu];

   FieldStrength[mu_, nu_, a_, {aA_, b_, c_}, g_ /; Head[g] =!= Rule,
                 ru___Rule] :=
       (QuantumField[PartialD[LorentzIndex[mu]],
                     aA, LorentzIndex[nu], SUNIndex[a]] -
        QuantumField[PartialD[LorentzIndex[nu]],aA,
                      LorentzIndex[mu], SUNIndex[a]] +
    (* dat is hEEEEEl belangrijk .... *)
    g SUNF[a, b, c] DOT[QuantumField[aA, LorentzIndex[mu], SUNIndex[b]],
                        QuantumField[aA, LorentzIndex[nu], SUNIndex[c]]
                       ]
       ) /; FreeQ2[{mu,nu}, {Momentum, OPEDelta}] &&
               (Explicit /. {ru} /. Options[FieldStrength]);

   FieldStrength[mu_, Momentum[OPEDelta], a_, {aA_, b_, c_},
                 g_ /; Head[g] =!= Rule, ru___Rule] :=
       (QuantumField[PartialD[LorentzIndex[mu]],
                     aA, Momentum[OPEDelta], SUNIndex[a]] -
        QuantumField[PartialD[Momentum[OPEDelta]],aA,
                      LorentzIndex[mu], SUNIndex[a]] +
       (* dat is hEEEEEl belangrijk .... *)
      g SUNF[a, b, c] DOT[QuantumField[aA, LorentzIndex[mu], SUNIndex[b]],
                          QuantumField[aA, Momentum[OPEDelta], SUNIndex[c]]
                         ]
       ) /; FreeQ2[{mu}, {Momentum, OPEDelta}] &&
               (Explicit /. {ru} /. Options[FieldStrength]);


   FieldStrength[Momentum[OPEDelta], nu_, a_, {aA_, b_, c_},
                 g_ /; Head[g] =!= Rule, ru___Rule] :=
       (QuantumField[PartialD[Momentum[OPEDelta]],
                     aA, LorentzIndex[nu], SUNIndex[a]] -
        QuantumField[PartialD[LorentzIndex[nu]],aA,
                      Momentum[OPEDelta], SUNIndex[a]] +
       (* dat is hEEEEEl belangrijk .... *)
      g SUNF[a, b, c] DOT[QuantumField[aA, Momentum[OPEDelta], SUNIndex[b]],
                          QuantumField[aA, LorentzIndex[nu], SUNIndex[c]]
                         ]
       ) /; FreeQ2[{nu}, {Momentum, OPEDelta}] &&
               (Explicit /. {ru} /. Options[FieldStrength]);


   FieldStrength[mu_, nu_, ru___Rule] := (
   QuantumField[PartialD[mu],
                QuantumField /. {ru} /. Options[FieldStrength],
                LorentzIndex[nu]
               ] -
  QuantumField[PartialD[nu],
                QuantumField /. {ru} /. Options[FieldStrength],
                LorentzIndex[mu]
               ]                         ) /;
               (Explicit /. {ru} /. Options[FieldStrength]);

   FieldStrength[mu_, nu_, a_, ru___Rule] := Block[{g,b,c},
   b = Unique["b"]; c = Unique["c"];
   FieldStrength[mu, nu, a, {QuantumField /. {ru} /. Options[FieldStrength],
                             b, c}, CouplingConstant /. {ru} /.
                                    Options[FieldStrength],
                            ru
                ]                                  ] /;
               (Explicit /. {ru} /. Options[FieldStrength]);

   MakeBoxes[FieldStrength[mu_, nu_, a___, ru___Rule], TraditionalForm
            ] :=
 Catch[
       If[(IndexPosition /. {ru} /. Options[FieldStrength]) === {0,0},
          Throw[SubsuperscriptBox[Evaluate[Symbol /. {ru} /.
                                   Options[FieldStrength]],
                                   Tbox[mu,nu], Tbox[a] ]
               ]
         ];
       If[(IndexPosition /. {ru} /. Options[FieldStrength]) === {1,1},
          Throw[SubsuperscriptBox[Evaluate[Symbol /. {ru} /.
                                   Options[FieldStrength]],
                                  "\[Null]", Tbox[a,mu,nu]
                                  ]
               ]
         ];
       If[(IndexPosition /. {ru} /. Options[FieldStrength]) === {0,1},
          Throw[SubsuperscriptBox[Evaluate[Symbol /. {ru} /.
                                   Options[FieldStrength]],
                                   Tbox[mu], Tbox[a,nu]
                                  ]
               ]
         ];
       If[(IndexPosition /. {ru} /. Options[FieldStrength]) === {1,0},
          Throw[SubsuperscriptBox[Evaluate[Symbol /. {ru} /.
                                   Options[FieldStrength]],
                                   Tbox[nu], Tbox[a,mu]
                                  ]
               ]
         ];
   ];

End[]; MyEndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FieldStrength | \n "]];
Null