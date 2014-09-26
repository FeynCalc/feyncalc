(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: FourDivergence *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 7 August '97 at 14:04 *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctools`FourDivergence`",{"HighEnergyPhysics`FeynCalc`"}];

FourDivergence::"usage" =
"FourDivergence[exp, FourVector[p, mu]]
calculates the partial derivative of exp w.r.t. p(mu).
FourDivergence[exp, FourVector[p, mu], FourVector[p,nu], ...]
gives the multiple derivative.";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];


Contract     = MakeContext["Contract"];
DiracGamma   = MakeContext["CoreObjects","DiracGamma"];
Eps          = MakeContext["CoreObjects","Eps"];
EpsEvaluate  := EpsEvaluate = MakeContext["EpsEvaluate"];
ExpandScalarProduct = MakeContext["ExpandScalarProduct"];
FC2TLI := FC2TLI = MakeContext["FC2TLI"];
FeynAmpDenominator    = MakeContext["CoreObjects","FeynAmpDenominator"];
FeynAmpDenominatorCombine= MakeContext["FeynAmpDenominatorCombine"];
FeynAmpDenominatorSplit    = MakeContext["FeynAmpDenominatorSplit"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
Momentum     = MakeContext["CoreObjects","Momentum"];
MomentumCombine = MakeContext["MomentumCombine"];
Pair         = MakeContext["CoreObjects","Pair"];
PropagatorDenominator = MakeContext["CoreObjects","PropagatorDenominator"];
ScalarProductCancel := ScalarProductCancel = MakeContext["ScalarProductCancel"];
TLI    = MakeContext["TLI"];
TLI2FC = MakeContext["TLI2FC"];
fci          = MakeContext["FeynCalcInternal"];

FourDivergence[x_,a_,b__] :=
 FourDivergence[FourDivergence[x, a], b];

FourDivergence[x_, fv_] := Block[
{nx = x,ve=fci[fv],p,mu,gpa,cont, tliflag = False},
If[!FreeQ[nx, TLI], nx = TLI2FC[nx]; tliflag = True];
nx = fci[nx];
ve = MomentumCombine[ve];
p  = Select[ve, Head[#] === Momentum&][[1]];
If[!FreeQ[p,Plus], nx = MomentumCombine[nx]];
mu = Select[ve, Head[#] === LorentzIndex&][[1]];

If[!FreeQ[nx, FeynAmpDenominator],
   nx = FeynAmpDenominatorSplit[nx]
  ];

nx = D[nx, p] /. Derivative -> deriv;

nx = nx /. (deriv[1][FeynAmpDenominator][_]) :> 1 /.
           {deriv[1,0][PropagatorDenominator][pe_,b_] :>
            (-2 Pair[pe,mu] *
              FeynAmpDenominator[PropagatorDenominator[pe, b],
                                 PropagatorDenominator[pe, b]
                                ]
            ),
            deriv[1, 0][Pair][p,  a_] :> Pair[a, mu] ,
            deriv[0, 1][Pair][a_, p] :> Pair[a, mu] ,
(* deriv[1][DiracGamma][p] :> DiracGamma[mu] , *)
            deriv[1,0,0,0][Eps][p,c__] :>
          Eps[mu,c] ,
            deriv[0,1,0,0][Eps][a_,p,c__] :>
          Eps[a,mu,c] ,
            deriv[0,0,1,0][Eps][a__,p,c_] :>
          Eps[a,mu,c] ,
            deriv[0,0,0,1][Eps][c__,p] :>
          Eps[c,mu]} /. deriv -> Derivative;

If[CheckContext["CoreObjects"], nx = EpsEvaluate[nx]];

If[!FreeQ[nx, FeynAmpDenominator],
   nx = FeynAmpDenominatorCombine[nx]
  ];

If[tliflag===True && FreeQ[nx, LorentzIndex],
   nx = FC2TLI[nx, (Momentum/.Options[TLI2FC])[[1]],
                   (Momentum/.Options[TLI2FC])[[2]]
              ]
  ];

nx = Contract[nx]// ExpandScalarProduct;
If[!FreeQ[nx, FeynAmpDenominator], nx = ScalarProductCancel[nx]];
 nx];

End[]; EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "FourDivergence | \n "]];
Null
