(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: QuarkPropagator *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)

(* :Summary: QuarkPropagator *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`qcd`QuarkPropagator`",{"HighEnergyPhysics`FeynCalc`"}];

QP::"usage" =
"QP is an alias for QuarkPropagator.
QP[p] is the massless quark propagator.
QP[{p,m}] gives the  quark propagator with mass m.";

QuarkPropagator::"usage" =
"QuarkPropagator[p] is the massless quark propagator.
QuarkPropagator[{p,m}] gives the  quark propagator with mass m.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

CF = MakeContext["CoreObjects","CF"];
CouplingConstant = MakeContext["CoreOptions","CouplingConstant"];
Dimension = MakeContext["CoreOptions","Dimension"];
DiracGamma = MakeContext["CoreObjects","DiracGamma"];
Epsilon = MakeContext["CoreObjects","Epsilon"];
FeynAmpDenominator = MakeContext["CoreObjects","FeynAmpDenominator"];
Gstrong = MakeContext["CoreObjects","Gstrong"];
Loop = MakeContext["CoreOptions","Loop"];
Momentum = MakeContext["CoreObjects","Momentum"];
OPE = MakeContext["CoreObjects","OPE"];
Pair = MakeContext["CoreObjects","Pair"];
Polarization = MakeContext["CoreObjects","Polarization"];
PropagatorDenominator = MakeContext["CoreObjects","PropagatorDenominator"];
ScaleMu = MakeContext["CoreObjects","ScaleMu"];
Twist2QuarkOperator := Twist2QuarkOperator = MakeContext["Twist2QuarkOperator"];

MakeContext[
    Abbreviation,
    CounterT,
    CounterTerm,
    DeclareNonCommutative,
    Explicit,
    MomentumExpand,
    Sn
    ];



DeclareNonCommutative[QuarkPropagator];

Options[QuarkPropagator] = {CounterTerm -> False,
                            CouplingConstant -> Gstrong,
                            Dimension -> D,
                            Explicit -> False,
                            Loop -> 0,
                            OPE -> False,
                            Polarization -> 0};

QP = QuarkPropagator;
Abbreviation[QuarkPropagator]=HoldForm[QP];

QuarkPropagator[a_, __, b_/;Head[b]=!=Rule, opt___Rule] :=
QuarkPropagator[a, opt];

QuarkPropagator[pi_/;(Head[pi]=!=List) && FreeQ[pi, BlankSequence], opt___Rule] :=
  QuarkPropagator[{pi,0}, opt];

QuarkPropagator[{pi_, m_}, opt___Rule] :=
Block[{dim, re, ope, pol, cou, loo},
 dim    = Dimension /. {opt} /. Options[QuarkPropagator];
 ope    = OPE       /. {opt} /. Options[QuarkPropagator];
 pol    = Polarization /. {opt} /. Options[QuarkPropagator];
 cou    = CounterTerm /. {opt} /. Options[QuarkPropagator];
 cop    = CouplingConstant /. {opt} /. Options[QuarkPropagator];
 loo    = Loop /. {opt} /. Options[QuarkPropagator];
(* one-loop expression *)
 If[loo === 1 && m === 0,
    (-(Pair[Momentum[pi], Momentum[pi]]/ScaleMu^2))^(Epsilon/2)*
    (-I*CF*(2 - Epsilon)*cop^2*Sn*DiracGamma[Momentum[pi,dim],dim]
    )/Epsilon
    ,
  If[ope =!= True,
     re = 0,
     re = OPE DOT[ QuarkPropagator[{pi, m}, OPE -> False, opt],
                   Twist2QuarkOperator[pi,Polarization -> pol],
                   QuarkPropagator[{pi, m}, OPE -> False, opt]
                 ]
    ];

  If[(cou =!= False) && m === 0,
     Which[(cou === True) || ( cou === 1),
           re = re + I cop^2 Sn CF 2/Epsilon *
                     DiracGamma[Momentum[pi,dim]],
           cou === All,
           re = re + CounterT Sn I cop^2 CF 2/Epsilon*
                     DiracGamma[Momentum[pi,dim]]
          ]
    ];

 If[cou === False,
  re = re +
   I (DiracGamma[Momentum[pi, dim], dim]+m) FeynAmpDenominator[
     MomentumExpand[
        PropagatorDenominator[Momentum[pi,dim], m]]       ]
   ];
    re ]] /; (Explicit /. {opt} /.Options[QuarkPropagator])===True;


QuarkPropagator /:
   MakeBoxes[QuarkPropagator[{p_,m_}, ___?OptionQ],
             TraditionalForm
            ] := RowBox[{SubscriptBox["\[CapitalPi]","q"],
                        "(", Tbox[p], ")"
                        }];


(*
QuarkPropagator[{pi_, m_}, opt___Rule] :=  Block[{dim},
 dim    = Dimension /. {opt} /. Options[QuarkPropagator];
   I (DiracGamma[Momentum[pi, dim], dim]+m) FeynAmpDenominator[
     MomentumExpand[
    PropagatorDenominator[Momentum[pi,dim], m]]]];
*)

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "QuarkPropagator | \n "]];
Null
