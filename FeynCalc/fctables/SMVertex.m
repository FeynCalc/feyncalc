(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SMVertex *)

(* :Author: Rolf Mertig *)

(* ------------------------------------------------------------------------ *)
(* :History: File created on 22 June '97 at 23:00 *)
(* ------------------------------------------------------------------------ *)

(* :Summary: SMVertex *)

(* ------------------------------------------------------------------------ *)

BeginPackage["HighEnergyPhysics`fctables`SMVertex`",{"HighEnergyPhysics`FeynCalc`"}];

SMVertex::"usage" =
"SMVertex[\"AWW\", p,mu, q,nu, k,rho] gives
the photon-W-W vertex (p,mu correspond to the photon,
q,nu to the (incoming) W+ and k,rho to the (incoming) W-.
All momenta are flowing into the vertex.

SMVertex[\"HHH\", ___] give the three-higgs coupling.
";

(* ------------------------------------------------------------------------ *)

Begin["`Private`"];

Dimension = MakeContext["CoreOptions","Dimension"];
FourVector = MakeContext["CoreObjects","FourVector"];
Gauge = MakeContext["CoreOptions","Gauge"];
LorentzIndex = MakeContext["CoreObjects","LorentzIndex"];
MetricTensor = MakeContext["CoreObjects","MetricTensor"];
Momentum = MakeContext["CoreObjects","Momentum"];
Pair = MakeContext["CoreObjects","Pair"];

MakeContext[ ChangeDimension, Explicit, SMP];

Options[SMVertex] = {Dimension -> 4, Explicit -> True};

l[w_Integer] := ToExpression["Global`li"<>ToString[w]];

SMVertex[x___, i_Integer, y___] :=
SMVertex[x, l[i], y];


SMVertex["AWW", mom1_, li1_, mom2_, li2_, mom3_, li3_,
         opt___Rule] := Block[
 {dim, EL},
  EL = SMP["EL"];
  dim   = Dimension /. {opt} /. Options[SMVertex];
  re = ChangeDimension[
  -I*EL*( MetricTensor[li1, li2] * FourVector[(mom2 -mom1 ),li3]
              +MetricTensor[li2, li3] * FourVector[(mom3 -mom2 ),li1]
              +MetricTensor[li3, li1] * FourVector[(mom1 -mom3 ),li2]
        ), dim];
                           re] /;
 (Explicit /. {opt} /. Options[SMVertex]) === True;


SMVertex["HHH", ___] := Block[ {EL, MW, MH, SW},
   {EL, MW, MH, SW} = SMP /@ {"EL", "MW", "MH", "SW"};
(* directly from the SM.model file from FeynArts1.0 *)
   ((-3*I)/2*EL*MH^2)/(MW*SW)];

SMVertex["eeH", ___] := Block[ {EL, MW, MH, SW},
   {EL, MW, ME, SW} = SMP /@ {"EL", "MW", "ME", "SW"};
-((I*EL*ME)/(2*MW*SW))];

End[]; EndPackage[];
(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)
If[$VeryVerbose > 0,WriteString["stdout", "SMVertex | \n "]];
Null
