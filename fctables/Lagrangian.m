(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Lagrangian *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 22:59 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`Lagrangian`",
             "HighEnergyPhysics`FeynCalc`"];

Lagrangian::"usage"= "Lagrangian[\"oqu\"] gives the unpolarized OPE quark
operator.\n
Lagrangian[\"oqp\"] gives the polarized quark OPE operator.\n
Lagrangian[\"ogu\"] gives the unpolarized gluon OPE operator.\n
Lagrangian[\"ogp\"] gives the polarized gluon OPE operator.\n
Lagrangian[\"ogd\"] gives the sigma-term part of the QCD lagrangian.\n
Lagrangian[\"QCD\"] gives the gluon self interaction part of the QCD lagrangian.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];
   

MakeContext[AntiQuarkField, CovariantD, DiracGamma,
            FieldStrength, GluonField, LeviCivita,Momentum,
            OPEDelta, OPEm, Polarization, QuarkField, QuantumField];

Lagrangian[x_] /;Depth[x]===1:= Block[{na,lali,a,b,c,d,al,be,ga},
If[ValueQ[Global`a], a = Unique["a"], a = Global`a];
If[ValueQ[Global`b], b = Unique["b"], b = Global`b];

If[$Notebooks,
   al = ToExpression["Global`"<>"\[Alpha]"];
   be = ToExpression["Global`"<>"\[Beta]"];
   ga = ToExpression["Global`"<>"\[Gamma]"];
   mu = ToExpression["Global`"<>"\[Mu]"];
   nu = ToExpression["Global`"<>"\[Nu]"];
   la = ToExpression["Global`"<>"\[Lambda]"];
   rho = ToExpression["Global`"<>"\[Rho]"];
   ,
   al = Global`Al;
   be = Global`Be;
   mu = Global`Mu;
   nu = Global`Nu;
   la = Global`La;
   rho= Global`Rho;
   ga = Global`Ga
  ];
    
na = ToString[x];
lali = {
"oqu" :> I^(OPEm)   DOT[QuantumField[AntiQuarkField],
                       DiracGamma[Momentum[OPEDelta]],
                       (CovariantD[OPEDelta]^(OPEm-1)),
                        QuantumField[QuarkField]]
,
"oqp" :> I^OPEm   DOT[QuantumField[AntiQuarkField],
                        DiracGamma[5],
                        DiracGamma[Momentum[OPEDelta]],
                       (CovariantD[OPEDelta]^(OPEm-1)),
                       QuantumField[QuarkField]
                  ]
,
"ogu" :> I^(OPEm-1)/2 DOT[FieldStrength[al, OPEDelta,a] ,
                        (CovariantD[OPEDelta,a,b]^(OPEm-2)),
                         FieldStrength[al, OPEDelta,b]
                      ]
,
"ogp" :> I^OPEm/2 DOT[LeviCivita[al, be, ga][OPEDelta] ,
         FieldStrength[be, ga, a] ,
               (CovariantD[OPEDelta, a, b]^(OPEm-2)),
         FieldStrength[al, OPEDelta, b]
                      ]
,
"ogd" :> DOT[LeviCivita[mu,nu,la,rho] , 
         FieldStrength[mu,nu, a] ,
         FieldStrength[la,rho,a]
         ]
,
"QCD" :> -1/4 DOT[FieldStrength[al, be, a] ,
              FieldStrength[al, be, a]]
       };
na /. lali];


End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "Lagrangian | \n "]];
Null
