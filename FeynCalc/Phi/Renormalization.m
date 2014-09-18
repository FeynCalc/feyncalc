(* Renormalization *)


(* Dimensional regularization of loop amplitudes *)


(* Author:  Frederik Orellana, fjob@cabocomm.dk


   Creation date:  1/8-2000


   Context: HighEnergyPhysics`Phi`ChPTRenormalization` *)


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


BeginPackage[
    "HighEnergyPhysics`Phi`Renormalization`", {"HighEnergyPhysics`Phi`",
    "HighEnergyPhysics`FeynCalc`", "HighEnergyPhysics`Phi`Objects`"}];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


tmp`olddir = Directory[];
SetDirectory[HighEnergyPhysics`Phi`$HEPDir];
SetDirectory["HighEnergyPhysics"];
SetDirectory["Phi"];
Get["Renormalization.defs.m"];
SetDirectory[tmp`olddir];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


Begin["`Private`"];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* FeynCalc functions *)


fcdim := fcdim = MakeContext["Dimension"];
fcb0 := fcb0 = MakeContext["B0"];
fcc0 := fcc0 = MakeContext["C0"];
fcd0 := fcd0 = MakeContext["D0"];
fcmom := fcmom = MakeContext["Momentum"];
fcpa := fcpa = MakeContext["Pair"];
fcpol := fcpol = MakeContext["Polarization"];
fcfad := fcfad = MakeContext["FeynAmpDenominator"];
fcpd := fcpd = MakeContext["PropagatorDenominator"];
fcli := fcli = MakeContext["LorentzIndex"];
fcmomexp := fcmomexp = MakeContext["MomentumExpand"];
fcmomcomb := fcmomcomb = MakeContext["MomentumCombine"];
fccontr := fccontr = MakeContext["Contract"];
fcintx := fcintx = MakeContext["Integratedx"];
fcfpana := fcfpana = MakeContext["FeynmanParameterNames"];
fcint := fcint = MakeContext["FCIntegrate"];
fcsunn := fcsunn = MakeContext["SUNN"];
fccoupl := fccoupl = MakeContext["CouplingConstant"];
fcscmu := fcscmu = MakeContext["ScaleMu"];
fcsmeps := fcsmeps = MakeContext["SmallEpsilon"];
fca0 := fca0 = MakeContext["A0"];
fca0tob0 := fca0tob0 = MakeContext["A0ToB0"];


(* Defaults *)


Options[VeltmanB0] = {fcsmeps -> fcsmeps, B0Evaluation -> "jbar",
      ExpandGammas -> True, FixPoint -> 0, TaylorOrder -> 2,
      fcdim -> D, MassScale -> fcscmu,
      fcint -> IntegrateHeld, DimensionExpand -> SpaceTimeDimensions,
      LeutwylerJBarEvaluation -> "none", ExplicitLeutwylerJ0 -> False,
      ExplicitLeutwylerLambda -> False, ExplicitLeutwylerSigma -> False};
Options[VeltmanC0] = {fcsmeps -> fcsmeps, C0Evaluation -> "Spence",
      fcint -> NIntegrate};
Options[VeltmanD0] = {fcsmeps -> fcsmeps, D0Evaluation -> "Spence",
      fcint -> NIntegrate};
Options[FeynmanIntegral] = {fcsmeps -> fcsmeps,
      fcint -> IntegrateHeld};
Options[VeltmanDelta] := {FixPoint -> 0, TaylorOrder -> 2};
Options[DimensionExpand] = {fcdim -> SpaceTimeDimensions, FixPoint -> 4,
      TaylorOrder -> 1};
Options[ExpandGammas] = {FixPoint -> 0, TaylorOrder -> 2, fcdim -> D};
Options[VeltmanExpand] = {(*fcdim -> D, MassScale -> fcscmu,
      ExplicitLeutwylerLambda -> False, LeutwylerJBarEvaluation -> "none",
      ExplicitLeutwylerSigma -> False, ExplicitLeutwylerJ0 -> False,
      fcsmeps -> fcsmeps,
      ExpandGammas -> True, DimensionExpand -> False, FixPoint -> 0,
      TaylorOrder -> 2,*) B0Evaluation -> "jbar", C0Evaluation -> "none",
      D0Evaluation -> "none", fcint -> IntegrateHeld,
      OnMassShell -> False, MomentumVariablesString -> "p",
      Masses -> {ParticleMass[Pion, RenormalizationState[0]],
          ParticleMass[Pion, RenormalizationState[0]],
          ParticleMass[Pion, RenormalizationState[0]],
          ParticleMass[Pion, RenormalizationState[0]]}};
Options[LeutwylerLambda] = {fcdim -> D,
      MassScale -> fcscmu, ExplicitLeutwylerLambda -> False};
Options[LeutwylerJ0] = {MassScale -> fcscmu, ExplicitLeutwylerJ0 -> False,
      ExplicitLeutwylerLambda -> False};
Options[LeutwylerSigma] = {ExplicitLeutwylerSigma -> False};
Options[LeutwylerJBar] = {LeutwylerJBarEvaluation -> "none",
      ExplicitLeutwylerSigma -> False};
Options[Renormalize] = {fcsunn -> 2, InfinityFactor -> LeutwylerLambda[],
      RenormalizationCoefficientFunction[fccoupl[lag_, n_, ___]] :>
        RenormalizationCoefficients[lag][[n]]};
Options[FeynmanParameterize] = {fcdim -> D,
      fcint ->
      (DOT[HighEnergyPhysics`FeynCalc`Integratedx`Integratedx@@#2, #1] &),
      fcfpana -> Automatic
      (*Compatibility with FeynmanParametrize:*)
      (*{Global`x,Global`y,Global`z, Global`\[Alpha]1,
      Global`\[Alpha]2, Global`\[Alpha]3}*)};


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* B0 *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* Expansion of the gamma function: *)

TruncatedGamma[dd_, p_, o_] := Normal[Series[Gamma[d], {d, p, o}]] /. d :> dd;


(* The capital delta of the 't Hooft and Veltman article: *)

VeltmanDelta[d_, opts___] :=
    TruncatedGamma[
        d/2, (FixPoint /. Flatten[{opts}] /.
            Options[VeltmanDelta]), (TaylorOrder /. Flatten[{opts}] /.
            Options[VeltmanDelta])] - Log[Pi];


(* Expansion of power functions of (SpaceTimeDimensions-4): *)

DimensionExpand[a_, opts___] :=
    Block[{dim, fixp, oo, b, c, s6},
      dim = fcdim /. Flatten[{opts}] /. Options[DimensionExpand];
      fixp = FixPoint /. Flatten[{opts}] /. Options[DimensionExpand];
      oo = TaylorOrder /. Flatten[{opts}] /. Options[DimensionExpand];
      a /. b_^c_ /; !FreeQ[c, dim] :> Normal[Series[b^c, {dim, fixp, oo}]]];

(* From Donoghue, Golowich and Holstein: *)

DenominatorD[m10_, m20_, q0_, x_, opts___] :=
    m10*x + m20*(1 - x) - q0*x*(1 - x) -
      I*(fcsmeps /. Flatten[{opts}] /. Options[FeynmanIntegral]);


(* Formula (5.4a): *)

FeynmanIntegral[p10_, m10_, m20_, n1_, n2_, d_, d1_,
      opts___] := ((-1)^(n1 + n2)*I*Pi^(d1/2)*
            Gamma[n1 + n2 - d/2]/(Gamma[n1]*Gamma[n2])*
            IntegrateHeld[
              Global`x^(n1 - 1)*(1 - Global`x)^(n2 - 1)/
                  DenominatorD1[m10, m20, p10, Global`x,
                      opts]^(*-*)(n1 + n2 - d1/2), {Global`x, 0, 1}]) /.
        DenominatorD1 -> DenominatorD /.
      IntegrateHeld -> (fcint /. Flatten[{opts}] /.
            Options[FeynmanIntegral]);

VeltmanB0[p10_, m10_, m20_,
        opts___?OptionQ] /; (B0Evaluation /. Flatten[{opts}] /. Options[VeltmanB0]) ===
         "none" := fcb0[p10, m10, m20];

VeltmanB0[p10_, m10_, m20_,
        opts___?OptionQ] /; (B0Evaluation /. Flatten[{opts}] /. Options[VeltmanB0]) ===
         "direct1" := (
        bdi = fcdim /. Flatten[{opts}] /. Options[VeltmanB0];
        dedi = DimensionExpand /. Flatten[{opts}] /. Options[VeltmanB0];
        dedim = If[dedi, bdi, bdi, dedi];
        iib0 =(*-*)
          1/(I*Pi^2)*(MassScale /. Flatten[{opts}] /.
                  Options[VeltmanB0])^(4 - dedim /. Flatten[{opts}] /.
                  Options[VeltmanB0])*
            FeynmanIntegral[p10, (*Why were they swapped - not correct I think. 5/3-2001*)
            (*m20, m10,*)m10, m20, 1,
              1, bdi, dedim,
              fcint -> IntegrateHeld, opts] /.
          If[(ExpandGammas /. Flatten[{opts}] /. Options[VeltmanB0]),
            Gamma[x_] ->
              TruncatedGamma[
                x, (FixPoint /. Flatten[{opts}] /.
                    Options[VeltmanB0]), (TaylorOrder /. Flatten[{opts}] /.
                    Options[VeltmanB0])], {}];
      If[(DimensionExpand /. Flatten[{opts}] /. Options[VeltmanB0]) =!= False,
            Evaluate[DimensionExpand[iib0, fcdim->dedim, opts]], iib0] /.
          IntegrateHeld -> (fcint /. Flatten[{opts}] /.
                Options[VeltmanB0]) (*Why? 26/2-2003*)(*/.
        If[dedi =!= False && dedi =!= True, dedim -> bdi, {}]*));


(* Expansion of the Gamma functions of (D-4) around 0 starting with the term
proprotional to the infinite 1/(D-4): *)

ExpandGammas[a_, opts___] :=
Block[{fixp,oo,dim,eps,test},
  fixp = (FixPoint /. Flatten[{opts}] /. Options[ExpandGammas]);
  oo = (TaylorOrder /. Flatten[{opts}] /. Options[ExpandGammas]);
  dim = (fcdim /. Flatten[{opts}] /. Options[ExpandGammas]);
  (*a /. Gamma[x_] :> TruncatedGamma[x, fixp, oo]*)
  a /. Gamma[x_] :> If[IntegerQ[test=x/.dim->4] && test<1,
     Normal[Series[Gamma[x/.dim->4-eps], {eps, fixp, oo}]] /. eps -> 4-dim,
     Gamma[test]
     ]
];


(* Splitting up sums and getting constants out: *)

IntegrateHeld[a_ + b_, {x_, s_, e_}] :=
    IntegrateHeld[a, {x, s, e}] + IntegrateHeld[b, {x, s, e}];
IntegrateHeld[a_*b_, {x_, s_, e_}] /; FreeQ[a, x] :=
    a*IntegrateHeld[b, {x, s, e}];
IntegrateHeld[a_, {x_, s_, e_}] /; FreeQ[a, x] := a*(e - s);


(* The Leibniz rule: *)

IntegrateHeld /: HoldPattern[D[IntegrateHeld[ex_, {x_, a_, b_}], d_]] :=

    IntegrateHeld[D[ex, d], {x, a, b}];
HoldPattern[aa_*Derivative[1, _List][IntegrateHeld][a_, {b_, c_, d_}]] ^:=
    IntegrateHeld[aa, {b, c, d}];


(* The D-dimensional integral B0 from FeynCalc is related to the J function of
Gasser and Leutwyler (1983), "Chiral Perturbation Theory to One Loop", Ann.
Phys. 158, 142-210 by *)

VeltmanB0[s_, m1s_, m1s_, opts___?OptionQ] /; (B0Evaluation /. Flatten[{opts}] /.
            Options[VeltmanB0]) === "jbar" :=
     (16*Pi^2)*(
     LeutwylerJBar[s, m1s, Sequence@@OptionsSelect[LeutwylerJBar, opts]] +
     LeutwylerJ0[m1s, Sequence@@OptionsSelect[LeutwylerJ0, opts]]);


(* From J.Gasser and H. Leutwyler (1985), Nuclear Physics B250,
   465-516: *)

VeltmanB0[s_, m1s_, m2s_, opts___?OptionQ] /; (B0Evaluation /. Flatten[{opts}] /.
            Options[VeltmanB0]) === "jbar" :=
          (16*Pi^2)*(
          LeutwylerJBar[s, m1s, m2s, Sequence@@OptionsSelect[LeutwylerJBar, opts]] +
          LeutwylerJ0[m1s, m2s, Sequence@@OptionsSelect[LeutwylerJ0, opts]]);


(* Everything below is taken directly from Gasser and Leutwyler (no additional
   factors): *)

LeutwylerJBar[0, __, opts___Rule] := 0;

LeutwylerJBar[s_, m1s_, opts___Rule] /;
    (s =!= 0 && MemberQ[{"subthreshold", "physical", "general"},
     (LeutwylerJBarEvaluation /. Flatten[{opts}] /.
                               Options[LeutwylerJBar])]) :=
    1/(16*Pi^2)*(sig*Log[(sig - 1)/(sig + 1)] + 2) /.
    sig -> (LeutwylerSigma[s, m1s, ##]& @@
                OptionsSelect[LeutwylerSigma, opts, Options[LeutwylerJBar]]);

(* Added 3/3-2000. From J.Gasser and H. Leutwyler (1985), Nuclear Physics B250,
   465-516: *)

LeutwylerJBar[s_, m1s_, m2s_, opts___Rule] /;
    (s =!= 0 && ((LeutwylerJBarEvaluation /. Flatten[{opts}] /.
              Options[LeutwylerJBar]) === "subthreshold")) :=
    1/(32*Pi^2)*(2 + delta/s*Log[m2s/m1s] - sigma/delta*Log[m2s/m1s] -
    nu/s*Log[((s + nu)^2 - delta^2)/((s - nu)^2 - delta^2)]) /.
        {delta -> m1s - m2s, sigma -> m1s + m2s,
        nu -> Sqrt[(s - (Sqrt[m1s] + Sqrt[m2s])^2)(s - (Sqrt[m1s] -
                        Sqrt[m2s])^2)]};

(* JBar is defined by having the imaginary part equal to Pi/16 times the phase space,
   see e.g. Knecht&Urech 1997 *)

LeutwylerJBar[s_, m1s_, m2s_, opts___Rule] /;
    (s =!= 0 && ((LeutwylerJBarEvaluation /. Flatten[{opts}] /.
              Options[LeutwylerJBar]) === "physical")) :=
    1/(32*Pi^2)*(2 + delta/s*Log[m2s/m1s] - sigma/delta*Log[m2s/m1s] +
    nu/s*Log[((s - nu)^2 - delta^2)/((s + nu)^2 - delta^2)] +
    2*Pi*I*nu/s) /.
        {delta -> m1s - m2s, sigma -> m1s + m2s,
        nu -> Sqrt[(s - (Sqrt[m1s] + Sqrt[m2s])^2)(s - (Sqrt[m1s] -
                        Sqrt[m2s])^2)]};

(*Gasser&Leutwyler (A.7) can be integrated explicitly by Mathematica*)
(*g[m12_, m22_, x_, s_] := m12 - s x(1 - x) - (m12 - m22)x;
-(16\[Pi]^2)^(-1)Integrate[
      Log[g[m12, m22, x, s]/g[m12, m22, x, 0]], {x, 0, 1}] // FullSimplify*)

LeutwylerJBar[s_, m1s_, m2s_, opts___Rule] /;
    (s =!= 0 && ((LeutwylerJBarEvaluation /. Flatten[{opts}] /.
              Options[LeutwylerJBar]) === "general")) :=
((-(m1s - m2s)^2 + (m1s + m2s)*s)*Log[-m1s] + ((m1s - m2s)^2 - (m1s + m2s)*s)*Log[-m2s] + 
  (m1s - m2s)*(2*s + I*Sqrt[-m1s^2 - (m2s - s)^2 + 2*m1s*(m2s + s)]*
     (Log[1 - (I*(m1s - m2s - s))/Sqrt[-m1s^2 - (m2s - s)^2 + 2*m1s*(m2s + s)]] - 
      Log[1 + (I*(m1s - m2s - s))/Sqrt[-m1s^2 - (m2s - s)^2 + 2*m1s*(m2s + s)]] - 
      Log[1 - (I*(m1s - m2s + s))/Sqrt[-m1s^2 - (m2s - s)^2 + 2*m1s*(m2s + s)]] + 
      Log[1 + (I*(m1s - m2s + s))/Sqrt[-m1s^2 - (m2s - s)^2 + 2*m1s*(m2s + s)]])))/
 (32*(m1s - m2s)*Pi^2*s);


LeutwylerJ0[s_, opts___Rule] /;
    (ExplicitLeutwylerJ0 /. Flatten[{opts}] /.
          Options[LeutwylerJ0]) :=
    -2*LeutwylerLambda[##]& @@ OptionsSelect[LeutwylerLambda, opts] -
    1/(16*Pi^2)*(Log[s/(MassScale /. Flatten[{opts}] /.
                      Options[LeutwylerJ0])^2] + 1);


(* Added 3/3-2000. From J.Gasser and H. Leutwyler (1985), Nuclear Physics B250,
   465-516: *)

LeutwylerJ0[m1s_, m2s_, opts___Rule] /; (ExplicitLeutwylerJ0 /. Flatten[{opts}] /.
          Options[LeutwylerJ0]) :=
      -2*LeutwylerLambda[##]& @@ OptionsSelect[LeutwylerLambda, opts] -
      1/(16*Pi^2)*
      (m1s*Log[m1s/(MassScale /. Flatten[{opts}] /. Options[LeutwylerJ0])^2] -
       m2s*Log[m2s/(MassScale /. Flatten[{opts}] /. Options[LeutwylerJ0])^2])/
      (m1s - m2s);

LeutwylerSigma[s_, m1s_, opts___] /; (ExplicitLeutwylerSigma /. Flatten[{opts}] /.
          Options[LeutwylerSigma]) := Sqrt[1 - 4*m1s/s];

LeutwylerLambda[opts___Rule | opts___List] /; 
    (ExplicitLeutwylerLambda /. Flatten[{opts}] /.
          Options[LeutwylerLambda]) :=
    1/(16*Pi^2)*(MassScale /. Flatten[{opts}] /.
            Options[LeutwylerLambda])^((fcdim /. Flatten[{opts}] /.
                Options[LeutwylerLambda]) -
            4)*(1/((fcdim /. Flatten[{opts}] /. Options[LeutwylerLambda]) -
                4) - (Log[4*Pi] - EulerGamma + 1)/2);




(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* C0 *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


VeltmanC0[p10_, p20_, p12_, m10_, m20_, m30_,
        opts___] /; (C0Evaluation /. Flatten[{opts}] /. Options[VeltmanC0]) ===
         "none" := fcc0[p10, p20, p12, m10, m20, m30];

Spence[x_] := PolyLog[2, x];


(* VeltmanC0 is taken directly from 't Hooft and Veltman (divided with a factor
I Pi^2 as in Passarino and Veltman): *)

alpha[a_, b_, c_,
      n_] := (alpsol = Solve[a*\[Alpha]^2 + b*\[Alpha] + c == 0, \[Alpha]];
      If[alpsol === {}, (VerbosePrint[1, "Warning : no roots found, using 0"];
          0), \[Alpha] /. alpsol[[3 - 2*n]]]);

alpha1[a_, b_, c_,
      n_] := (alpsol = Solve[a*\[Alpha]^2 + b*\[Alpha] + c == 0, \[Alpha]];
      If[alpsol === {}, 0, \[Alpha] /. alpsol[[3 - 2*n]]]);


(* 't Hooft and Veltman use the metric (-1,1,1,1), so the definitions from
formulae (5.2) are changed a little: *)

veaa[p20_] := p20; vebb[p10_] := p10;
vecc[p10_, p20_, p12_] := p12 - p10 - p20;
vedd[p20_, m20_, m30_] := m20 - m30 - p20;
veee[p10_, p12_, p20_, m10_, m20_] := m10 - m20 - p10 - vecc[p10, p20, p12];
veff[m30_, eps_] := m30 - I*eps;


(* This is formula (5.2): *)

VeltmanC0[p10_, p20_, p12_, m10_, m20_, m30_,
        opts___] /; (C0Evaluation /. Flatten[{opts}] /. Options[VeltmanC0]) ===
         "direct1" :=
    Block[{a = veaa[p20], b = vebb[p10], c = vecc[p10, p20, p12],
        d = vedd[p20, m20, m30], e = veee[p10, p12, p20, m10, m20],
        eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanC0]),
        f = veff[m30, eps]},
      IntegrateHeld[(a*x^2 + b*y^2 + c*x*y + d*x + e*y + f)^(-1), {x, 0,
                1}, {y, 0, x}, ##] & @@
          Union[OptionsSelect[NIntegrate, opts],
            OptionsSelect[Integrate, opts]] /.
        IntegrateHeld -> (fcint /. Flatten[{opts}] /.
              Options[VeltmanC0])];


(* This is formula (5.3): *)

VeltmanC0[p10_, p20_, p12_, m10_, m20_, m30_,
        opts___] /; (C0Evaluation /. Flatten[{opts}] /. Options[VeltmanC0]) ===
         "direct2" :=
    Block[{a = veaa[p20], b = vebb[p10], c = vecc[p10, p20, p12],
        d = vedd[p20, m20, m30], e = veee[p10, p12, p20, m10, m20],
        eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanC0]),
        f = veff[m30, eps], alp = alpha[b, c, a, 1]},
      IntegrateHeld[(1 - alp)/((c + 2*alp*b)*(1 - alp)*y + d + e*alp)*
                  Log[(b*(1 - alp)^2*y^2 + (1 - alp)*(c + 2*alp*b + e)*y + d +
                           e*alp +
                          f)/((b*(1 - alp)^2 + (c + 2*alp*b)*(1 - alp))*
                            y^2 + (e*(1 - alp) + d + e*alp)*y + f)] +
                alp/(-(c + 2*alp*b)*alp*y + d + e*alp)*
                  Log[(b*alp^2*y^2 - alp*(c + 2*alp*b + e)*y + d + e*alp +
                          f)/((alp^2*b - (c + 2*alp*b)*alp)*
                            y^2 + (e*alp + d - e*alp)*y + f)], {y, 0,
                1}, ##] & @@
          Union[OptionsSelect[NIntegrate, opts],
            OptionsSelect[Integrate, opts]] /.
        IntegrateHeld -> (fcint /. Flatten[{opts}] /.
              Options[VeltmanC0])];


(* This is formula (5.6): *)

VeltmanC0[p10_, p20_, p12_, m10_, m20_, m30_,
        opts___] /; (C0Evaluation /. Flatten[{opts}] /. Options[VeltmanC0]) ===
         "direct3" :=
    Block[{a = veaa[p20], b = vebb[p10], c = vecc[p10, p20, p12],
        d = vedd[p20, m20, m30], e = veee[p10, p12, p20, m10, m20],
        eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanC0]),
        f = veff[m30, eps], alp = alpha[b, c, a, 1],
        y0 = -(d + e*alp)/(c + 2*alp*b), y1 = y0 + alp, y2 = y0/(1 - alp),
        y3 = -y0/alp},
      IntegrateHeld[
              1/((c + 2*alp*b)*y + d + e*alp + 2*a + c*alp)*(Log[
                        b*y^2 + (c + e)*y + a + d + f] -
                      Log[b*y1^2 + (c + e)*y1 + a + d + f]) - (1 -
                        alp)/((c + 2*alp*b)*(1 - alp)*y + d +
                        e*alp)*(Log[(a + b + c)*y^2 + (e + d)*y + f] -
                      Log[(a + b + c)*y2^2 + (e + d)*y2 + f]) -
                alp/(-(c + 2*alp*b)*alp*y + d + e*alp)*(Log[a*y^2 + d*y + f] -
                       Log[a*y3^2 + d*y3 + f]), {y, 0, 1}, ##] & @@
          Union[OptionsSelect[NIntegrate, opts],
            OptionsSelect[Integrate, opts]] /.
        IntegrateHeld -> (fcint /. Flatten[{opts}] /.
              Options[VeltmanC0])];
theta[x_] := If[x < 0, 0, 1];


(* In the 't Hooft and Veltman article, the eta (formula (2.4)) is to enforce
the cut along the negative real axis: *)

(* The alternative Im (Im1) below is to correct a Mathematica bug causing
Im[...] sometimes to have a small imginary part ... *)

Im1[x_] := Sign[Im[x]]*Abs[Im[x]];
eta[a_, b_] :=
    2*Pi*I*(theta[-Im1[a]]*theta[-Im1[b]]*theta[Im1[a*b]] -
          theta[Im1[a]]*theta[Im1[b]]*theta[-Im1[a*b]]);
rr[y0_, y1_] :=
    Spence[y0/(y0 - y1)] - Spence[(y0 - 1)/(y0 - y1)] +
      eta[-y1, 1/(y0 - y1)]*Log[y0/(y0 - y1)] -
      eta[1 - y1, 1/(y0 - y1)]*Log[(y0 - 1)/(y0 - y1)];


(* The S_3 of appendix B of 't Hooft and Veltman: *)

s3[a_, b_, c_, yy0_, opts___] := (epsi = -Sign[Im[c]];
      delta = -Sign[Im[a*yy0^2 + b*yy0 + c]];
      eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanC0]);
      yy1 = alpha[a, b, c, 1]; yy2 = alpha[a, b, c, 2];
      imzero1 = alpha1[Im[a], Im[b], Im[c], 1];
      imzero2 = alpha1[Im[a], Im[b], Im[c], 2];
      If[Im[imzero1] === 0,
        If[(imzero1 < 1 && imzero1 > 0),
          VerbosePrint[1, "Warning : Integral decomposition being used outside range of \
validity - ", imzero1]]];
      If[Im[imzero2] === 0,
        If[(imzero2 < 1 && imzero2 > 0),
          VerbosePrint[1, "Warning : Integral decomposition being used outside range of \
validity - ", imzero2]]];
      rr[yy0, yy1] +
        rr[yy0, yy2] - (eta[-yy1, -yy2] - eta[yy0 - yy1, yy0 - yy2] -
              eta[a - I*epsi*eps, 1/(a - I*delta*eps)])*Log[(yy0 - 1)/yy0]);


(* This is formula (5.6) using the above S_3: *)

VeltmanC0[p10_, p20_, p12_, m10_, m20_, m30_,
        opts___] /; (C0Evaluation /. Flatten[{opts}] /. Options[VeltmanC0]) ===
         "Spence" :=
    Block[{a = veaa[p20], b = vebb[p10], c = vecc[p10, p20, p12],
        d = vedd[p20, m20, m30], e = veee[p10, p12, p20, m10, m20],
        eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanC0]),
        f = veff[m30, eps], alp = alpha[b, c, a, 1],
        y0 = -(d + e*alp)/(c + 2*alp*b), y1 = y0 + alp, y2 = y0/(1 - alp),
        y3 = -y0/alp},
      1/(c + 2*alp*b)*
          s3[b, c + e, a + d + f, -(d + e*alp + 2*a + c*alp)/(c + 2*alp*b),
            opts] - (1 - alp)/((c + 2*alp*b)*(1 - alp))*
          s3[a + b + c, e + d, f, -(d + e*alp)/((c + 2*alp*b)*(1 - alp)),
            opts] - alp/(-(c + 2*alp*b)*alp)*
          s3[a, d, f, (d + e*alp)/((c + 2*alp*b)*alp), opts]];


(* For later use (the D0) we give the three point function in terms of the
a,..,f directly: *)

vC0[a_, b_, c_, d_, e_, f_, opts___] := (alp = alpha[b, c, a, 1];
      1/(c + 2*alp*b)*
          s3[b, c + e, a + d + f, -(d + e*alp + 2*a + c*alp)/(c + 2*alp*b),
            opts] - (1 - alp)/((c + 2*alp*b)*(1 - alp))*
          s3[a + b + c, e + d, f, -(d + e*alp)/((c + 2*alp*b)*(1 - alp)),
            opts] - alp/(-(c + 2*alp*b)*alp)*
          s3[a, d, f, (d + e*alp)/((c + 2*alp*b)*alp), opts]);


(* The S_3 of appendix B of 't Hooft and Veltman using Mathematica's symbolic
integration: *)

(* (*Integrate[
      1/(y - y0)*(Log[a*y^2 + b*y + c] - Log[a*y0^2 + b*y0 + c]), {y, 0, 1}] //
     InputForm*) *)

s33[a_, b_, c_, y0_, opts___] :=
    s33[a, b, c, y0, opts] =
      Log[a + b + c]*Log[1 - y0] - Log[(2*a + b - Sqrt[b^2 - 4*a*c])/(2*a)]*
   Log[1 - y0] - Log[(2*a + b + Sqrt[b^2 - 4*a*c])/(2*a)]*Log[1 - y0] -
  Log[c]*Log[-y0] + Log[(b - Sqrt[b^2 - 4*a*c])/(2*a)]*Log[-y0] +
  Log[(b + Sqrt[b^2 - 4*a*c])/(2*a)]*Log[-y0] -
  Log[(b - Sqrt[b^2 - 4*a*c])/(2*a)]*
   Log[(2*a*y0)/(b - Sqrt[b^2 - 4*a*c] + 2*a*y0)] +
  Log[(2*a + b - Sqrt[b^2 - 4*a*c])/(2*a)]*
   Log[(2*(-a + a*y0))/(b - Sqrt[b^2 - 4*a*c] + 2*a*y0)] -
  Log[(b + Sqrt[b^2 - 4*a*c])/(2*a)]*
   Log[(2*a*y0)/(b + Sqrt[b^2 - 4*a*c] + 2*a*y0)] +
  Log[(2*a + b + Sqrt[b^2 - 4*a*c])/(2*a)]*
   Log[(2*(-a + a*y0))/(b + Sqrt[b^2 - 4*a*c] + 2*a*y0)] -
  Log[1 - y0]*Log[c + b*y0 + a*y0^2] + Log[-y0]*Log[c + b*y0 + a*y0^2] -
  PolyLog[2, (-b + Sqrt[b^2 - 4*a*c])/(-b + Sqrt[b^2 - 4*a*c] - 2*a*y0)] +
  PolyLog[2, (-2*a - b + Sqrt[b^2 - 4*a*c])/
    (-b + Sqrt[b^2 - 4*a*c] - 2*a*y0)] -
  PolyLog[2, (b + Sqrt[b^2 - 4*a*c])/(b + Sqrt[b^2 - 4*a*c] + 2*a*y0)] +
  PolyLog[2, (2*a + b + Sqrt[b^2 - 4*a*c])/(b + Sqrt[b^2 - 4*a*c] + 2*a*y0)];


(* This is formula (5.6) using the above S_3: *)

VeltmanC0[p10_, p20_, p12_, m10_, m20_, m30_,
        opts___] /; (C0Evaluation /. Flatten[{opts}] /. Options[VeltmanC0]) ===
         "direct4" :=
    Block[{a = veaa[p20], b = vebb[p10], c = vecc[p10, p20, p12],
        d = vedd[p20, m20, m30], e = veee[p10, p12, p20, m10, m20],
        eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanC0]),
        f = veff[m30, eps], alp = alpha[b, c, a, 1],
        y0 = -(d + e*alp)/(c + 2*alp*b), y1 = y0 + alp, y2 = y0/(1 - alp),
        y3 = -y0/alp},
      1/(c + 2*alp*b)*
          s33[b, c + e, a + d + f, -(d + e*alp + 2*a + c*alp)/(c + 2*alp*b),
            opts] - (1 - alp)/((c + 2*alp*b)*(1 - alp))*
          s33[a + b + c, e + d, f, -(d + e*alp)/((c + 2*alp*b)*(1 - alp)),
            opts] - alp/(-(c + 2*alp*b)*alp)*
          s33[a, d, f, (d + e*alp)/((c + 2*alp*b)*alp), opts]];


(* This is formulas (E.1-E.3) using 2 Spence functions: *)

VeltmanC0[mmm10_, mmm30_, ss_, m10_, m20_, m30_,
        opts___] /; (C0Evaluation /. Flatten[{opts}] /. Options[VeltmanC0]) ===
         "infrared" := If[mmm10=!=m10 || mmm30=!=m30, Message[VeltmanC0::nodef],
    Block[{s,y1,y2,eps,f1,f2,res,yy1,yy2},s = -ss; 
        yy1 = alpha[-s, (s + m30 - m10), m10, 2];
        yy2 = alpha[-s, (s + m30 - m10), m10, 1];
        eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanC0]);
        f1 = (-1/(s*(y1 - y2)))*(Log[(y1 - 1)/y1] - Log[(y2 - 1)/y2]);
        f2 = (f1*Log[-s - I*eps] -
              1/(s*(y1 - y2))*(1/2*(Log[1 - y1])^2 - 1/2*(Log[-y1])^2 -
                    1/2*(Log[1 - y2])^2 + 1/2*(Log[-y2])^2 -
                    Log[1 - y2]*Log[1 - y1] + Log[-y2]*Log[-y1] +
                    2*Log[(y1 - 1)/y1]*Log[y1 - y2] -
                    2*Spence[(y1 - 1)/(y1 - y2)] +
                    2*Spence[y1/(y1 - y2)]));
       res=-1/2*(f1*Log[m20] - f2);
       (*Symmetrization seems to be necessary??!?*)
       ((res/.{y1->yy1,y2->yy2})+(res/.{y1->yy2,y2->yy1}))/2]];

(* From Knecht&Urech *) (*Notice that their dilog[x] is -PolyLog[2,1-x]*)

VeltmanC0[mm10_,mm30_,s_,m10_,m20_,m30_,
        opts___?OptionQ]/;(C0Evaluation/.Flatten[{opts}]/.Options[VeltmanC0])===
        "infrared1":=
    If[Union[{mm10,mm30,m10,m30}]=!={m10},Message[VeltmanC0::nodef1],
      Block[{sig},sig=
            LeutwylerSigma[s,m10,
              Sequence@@
                OptionsSelect[LeutwylerSigma,opts]];
           (*-*)1/(2s sig)(4(-PolyLog[
                  2,1-(1-sig)/(1+sig)])+Pi^2/3+
              Log[(sig-1)/(sig+1)]^2+2(Log[-s/m10]-
                    Log[m20/m10]+2Log[sig])Log[(sig-1)/(sig+1)])]];

VeltmanC0[mm10_,mm30_,s_,m10_,m20_,m30_,
        opts___?OptionQ]/;(C0Evaluation/.Flatten[{opts}]/.Options[VeltmanC0])===
        "infrared2":=
    If[Union[{mm10,mm30,m10,m30}]=!={m10},Message[VeltmanC0::nodef1],
      Block[{sig},sig=
            LeutwylerSigma[s,m10,
              Sequence@@
                OptionsSelect[LeutwylerSigma,opts]];
         (*-*)1/(2s sig)(4(-PolyLog[
                  2,-(1-sig)/(1+sig)])+4Pi^2/3+
              Log[(1-sig)/(1+sig)]^2+2(Log[s/m10]-
                    Log[m20/m10]+2Log[sig])(Log[(1-sig)/(1+sig)]+I Pi))]];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* D0 *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

VeltmanD0[p10_, p20_, p30_, p12_, p13_, p23_, m10_, m20_, m30_, m40_,
        opts___] /; (D0Evaluation /. Flatten[{opts}] /. Options[VeltmanD0]) ===
         "none" := fcd0[p10, p20, p30, p12, p13, p23, m10, m20, m30, m40];


(* In the following, p10,... are the p1^2,... appearing in the 't Hooft and
Veltman article (that is, with the Pauli metric (-1,-1,-1,1)). *)

(* Formulas (6.6) to 6.9): *)

(* {moms} is to be {p10,p20,p30,p12,p13,p23}. *)

pp2[i_, j_, moms__] := (ppl[1, 2] = {moms}[[4]]; ppl[1, 3] = {moms}[[5]];
      ppl[2, 3] = {moms}[[6]]; ppl[r_, r_] = 0; ppsq[1] := {moms}[[1]];
      ppsq[2] := {moms}[[2]];
      ppsq[3] := {moms}[[3]]; (Sum[ppsq[rep], {rep, i, j - 1}] +
          2*Sum[ppl[rep, rep1], {rep1, i, j - 1}, {rep, i, rep1}]));


(* {masses} is to be {m1^2,m2^2,m3^2,m4^2}. *)

ll[i_, j_, moms__, {masses__}] :=
    pp2[i, j, moms] + {masses}[[i]] + {masses}[[j]];
aa[1, moms__, {masses__}] := (res =
        aa[2, moms, {masses}]*(ll[1, 2, moms, {masses}] +
                Sqrt[ll[1, 2, moms, {masses}]^2 -
                    4*{masses}[[1]]*{masses}[[2]]])/(2*{masses}[[1]]);
      If[! NumericQ[res] || Im[res] === 0, res,
        VerbosePrint[1, "Warning: A[1] not real - ", res]; res]);


(* The final result is independent of A_2, so we just set it to some value: *)

aa[2, moms__, {masses__}] := 10000000;
aa[3, moms__, {masses__}] := (res = ({masses}[[2]]*
                aa[2, moms, {masses}]^2 - {masses}[[1]]*
                aa[1, moms, {masses}]^2)/(ll[2, 3, moms, {masses}]*
                aa[2, moms, {masses}] -
              ll[1, 3, moms, {masses}]*aa[1, moms, {masses}]);
      If[! NumericQ[res] || Im[res] === 0, res,
        VerbosePrint[1, "Warning: A[3] not real - ", res]; res]);
aa[4, moms__, {masses__}] := (res = ({masses}[[2]]*
                aa[2, moms, {masses}]^2 - {masses}[[1]]*
                aa[1, moms, {masses}]^2)/(ll[2, 4, moms, {masses}]*
                aa[2, moms, {masses}] -
              ll[1, 4, moms, {masses}]*aa[1, moms, {masses}]);
      If[! NumericQ[res] || Im[res] === 0, res,
        VerbosePrint[1, "Warning: A[4] not real - ", res]; res]);


(* This is formula (6.6): *)

qq2[i_, j_, p10_, p20_, p30_, p12_, p13_, p23_, m10_, m20_, m30_,
      m40_] := (pp2[i, j, p10, p20, p30, p12, p13,
              p23] + {m10, m20, m30, m40}[[i]] + {m10, m20, m30, m40}[[j]])*
        aa[i, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]*
        aa[j, p10, p20, p30, p12, p13,
          p23, {m10, m20, m30, m40}] - {m10, m20, m30, m40}[[i]]*
        aa[i, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2 - {m10,
            m20, m30, m40}[[j]]*
        aa[j, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2;


(* Evaluation using two three point functions: *)

(* We also put on a minus on the C0 functions due to their definition as the
negative of the 't Hooft and Veltman definition: *)

(* All in all, I didn't at all achieve to get agreement with FF - the Pi^2
factor is taken from nowhere - the minuses are not put on after all - just
seems to improve agreement with FF: *)

VeltmanD0[pf10_, pf20_, pf30_, pp10_, pp20_, pp30_, m10_, m20_, m30_, m40_,
        opts___] /; (D0Evaluation /. Flatten[{opts}] /. Options[VeltmanD0]) ===
         "C0" :=
    Block[{$MaxExtraPrecision =
          4000,(*The minus (from the Bjoerken -
                Drell metric (-1, -1, -1, 1)) is put on here :*)p10 = pf10,
        p20 = pf20, p30 = pf30, p12 = (pp20 - p10 - p20)/2,
        p23 = (pp30 - p20 - p30)/2,
        p13 = (pp10 - p10 - p20 - p30 - 2*p12 - 2*p23)/2,
        mm10 = m10*
            aa[1, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        mm20 = m20*
            aa[2, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        mm30 =
          m30*aa[3, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        mm40 = m40*
            aa[4, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        a = -qq2[3, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40],
        b = -qq2[2, 3, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40],
        c = -qq2[2, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] +
            qq2[2, 3, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] +
            qq2[3, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40],
        d = mm30 - mm40 +
            qq2[3, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40],
        e = mm20 - mm30 +
            qq2[2, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] -
            qq2[3, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40],
        eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanD0]),
        f = mm40 - I*eps,
        g = -qq2[1, 2, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40],
        h = -qq2[1, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] -
            qq2[2, 3, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] +
            qq2[1, 3, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] +
            qq2[2, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40],
        j = -qq2[1, 3, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] +
            qq2[1, 2, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] +
            qq2[2, 3, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40],
        k = mm10 - mm20 +
            qq2[1, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] -
            qq2[2, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40],
        optss = Flatten[{opts}]},
      firstc0 = -vC0[a, b, c, d, e, f, ##] & @@
          Join[optss, Options[VeltmanD0]];
      secondc0 = -vC0[a, b, c, d, (e + k), f, ##] & @@
          Join[optss, Options[VeltmanD0]]; kk = k;
      afactors = {aa[1, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}],
          aa[2, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}],
          aa[3, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}],
          aa[4, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]};(*?? ??*)
        Pi^2*(Times @@ afactors)/k*(firstc0 - secondc0)];
llaa[i_, j_,
      moms__, {masses__}] := -ll[i, j, moms, {masses}]*aa[i, moms, {masses}]*
        aa[j, moms, {masses}] + {masses}[[i]]*
        aa[i, moms, {masses}]^2 + {masses}[[j]]*aa[j, moms, {masses}]^2;
laa[i_, j_, moms__, {masses__}] :=
    ll[i, j, moms, {masses}]*aa[i, moms, {masses}]*aa[j, moms, {masses}] -
      2*{masses}[[j]]*aa[j, moms, {masses}]^2;


(* This is formula (6.15): *)

sss[i_, j_, k_, p10_, p20_, p30_, p12_, p13_, p23_, m10_, m20_, m30_, m40_,
      opts___] := (rootss =
        Solve[-qq2[i, j, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40]*
                x^2 + (qq2[i, j, p10, p20, p30, p12, p13, p23, m10, m20, m30,
                      m40] + qq2[k, j, p10, p20, p30, p12, p13, p23, m10, m20,
                       m30, m40] -
                    qq2[k, i, p10, p20, p30, p12, p13, p23, m10, m20, m30,
                      m40])*x -
              qq2[k, j, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40] -
              I*(fcsmeps /. Flatten[{opts}] /. Options[VeltmanC0]) == 0, x];
      x1 = x /. Flatten[rootss][[1]];
      x2 = x /. Flatten[rootss][[-1]]; -I*
        Pi/(-qq2[i, j, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40]*(x1 -
                   x2))*(Log[(x1 - 1)/x1] - Log[(x2 - 1)/x2]));
ss[p10_, p20_, p30_, p12_, p13_, p23_, {m10_, m20_, m30_, m40_}, opts___] :=
    Sign[(aa[1, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]/
              aa[2, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}] -
            ll[1, 2, p10, p20, p30, p12, p13,
                p23, {m10, m20, m30, m40}]/(2*m10))]*(theta[
              Re[aa[3, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]*
                  aa[4, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]]]*
            sss[3, 4, 2, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40,
              opts] - theta[
              Re[-aa[3, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]*
                  aa[4, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}]]]*(sss[2, 4, 3, p10, p20, p30,
                  p12, p13, p23, m10, m20, m30, m40, opts] +
                sss[2, 3, 4, p10, p20, p30, p12, p13, p23, m10, m20, m30, m40,
                   opts]));
yminus[i_, j_, moms__, {masses__},
      ie_] := (roots = (y /. #) & /@
          Flatten[Solve[
              llaa[i, j, moms, {masses}]*y^2 +
                  laa[i, j, moms, {masses}]*y + {masses}[[j]]^2*
                    aa[j, moms, {masses}]^2 - I*ie == 0, y]];
      If[NumericQ[roots[[1]]] && NumericQ[roots[[-1]]],
        If[Im[roots[[1]]] < 0, roots[[1]], roots[[-1]]], roots[[1]]]);
yplus[i_, j_, moms__, {masses__},
      ie_] := (roots = (y /. #) & /@
          Flatten[Solve[
              llaa[i, j, moms, {masses}]*y^2 +
                  laa[i, j, moms, {masses}]*y + {masses}[[j]]^2*
                    aa[j, moms, {masses}]^2 - I*ie == 0, y]];
      If[NumericQ[roots[[1]]] && NumericQ[roots[[-1]]],
        If[Im[roots[[1]]] > 0, roots[[1]], roots[[-1]]], roots[[-1]]]);


(* The S_3 of appendix B of 't Hooft and Veltman: *)

s3D[a_, b_, c_, yy0_, opts___] := (epsi = -Sign[Im[c]];
      delta = -Sign[Im[a*yy0^2 + b*yy0 + c]];
      eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanD0]);
      yy1 = alpha[a, b, c, 1]; yy2 = alpha[a, b, c, 2];
      imzero1 = alpha1[Im[a], Im[b], Im[c], 1];
      imzero2 = alpha1[Im[a], Im[b], Im[c], 2];
      If[Im[imzero1] === 0,
        If[(imzero1 < 1 && imzero1 > 0),
          VerbosePrint[1, "Warning : Integral decomposition being used outside range of \
validity - ", imzero1]]];
      If[Im[imzero2] === 0,
        If[(imzero2 < 1 && imzero2 > 0),
          VerbosePrint[1, "Warning : Integral decomposition being used outside range of \
validity - ", imzero2]]];
      rr[yy0, yy1] +
        rr[yy0, yy2] - (eta[-yy1, -yy2] - eta[yy0 - yy1, yy0 - yy2] -
              eta[a - I*epsi*eps, 1/(a - I*delta*eps)])*Log[(yy0 - 1)/yy0]);


(* 't Hooft and Veltman use the metric (-1,1,1,1), so the definitions are
changed a little (dot products change sign): *)

heaa[p30_] := -p30; hebb[p20_] := -p20; hecc[p23_] := -2*p23;
hedd[p30_, m30_, m40_] := m30 - m40 + p30;
heee[p20_, p23_, m20_, m30_] := m20 - m30 + 2*p23 + p20;
heff[m40_, eps_] := m40 - I*eps; hegg[p10_] := -p10; hehh[p13_] := -2*p13;
hejj[p12_] := -2*p12;
hekk[p10_, p12_, p13_, m10_, m20_] := m10 - m20 + 2*p12 + 2*p13 + p10;


(* This is formula (6.12) using the above S_3: *)

(* We have to extract the 't Hooft and Veltman external momenta from the
FeynCalc input momenta: *)

(* p12 is p1*p2 etc. *)

VeltmanD0[pf10_, pf20_, pf30_, pp10_, pp20_, pp30_, m10_, m20_, m30_, m40_,
        opts___] /; (D0Evaluation /. Flatten[{opts}] /. Options[VeltmanD0]) ===
         "Spence" :=
    Block[{(*The minus (from the Bjoerken -
                Drell metric (-1, -1, -1, 1)) is put on here :*)p10 = pf10,
        p20 = -pf20, p30 = -pf30, p12 = -(pp20 - p10 - p20)/2,
        p23 = -(pp30 - p20 - p30)/2,
        p13 = -(pp10 - p10 - p20 - p30 - 2*p12 - 2*p23)/2,
        mm10 = m10*
            aa[1, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        mm20 = m20*
            aa[2, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        mm30 = m30*
            aa[3, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        mm40 = m40*
            aa[4, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        a = heaa[p30], b = hebb[p20], c = hecc[p23],
        d = hedd[p30, mm30, mm40], e = heee[p20, p23, mm20, mm30],
        eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanD0]),
        f = heff[mm40, eps], g = hegg[p10], h = hehh[p13], j = hejj[p12],
        k = hekk[p10, p12, p13, mm10, mm20], alp = alpha[b, c, a, 1],
        y1 = -(d + e*alp)/((c + 2*alp*b)*(1 - alp)),
        y2 = (d + e*alp)/((c + 2*alp*b)*alp),
        y3 = -(d + e*alp + c*alp + 2*a)/(c + 2*alp*b),
        y4 = -(d + e*alp + k*alp)/((c + 2*alp*b)*(1 - alp)),
        y5 = (d + e*alp + k*alp)/((c + 2*alp*b)*alp),
        y6 = -(d + e*alp + k*alp + c*alp + 2*a)/(c + 2*alp*b)}, (y[1] = y1;
        y[2] = y2; y[3] = y3; y[4] = y4; y[5] = y5; y[6] = y6;
        rr[i_, j_, k_, np10_, np20_, np30_, np12_, np13_,
            np23_, {nm10_, nm20_, nm30_,
              nm40_}] := -theta[
                  Re[-aa[i, np10, np20, np30, np12, np13,
                          np23, {nm10, nm20, nm30, nm40}]*
                      aa[j, np10, np20, np30, np12, np13,
                        np23, {nm10, nm20, nm30, nm40}]]]/(c +
                  2*alp*b)*(Pi^2 +
                I*Pi*theta[
                    Im[y[k]]]*(2*
                        Log[y[k] -
                            yminus[i, j, np10, np20, np30, np12, np13,
                              np23, {nm10, nm20, nm30, nm40}, eps]] -
                      Log[(y[k] -
                              yminus[i, j, np10, np20, np30, np12, np13,
                                np23, {nm10, nm20, nm30, nm40}, eps])]*(y[k] -
                             yplus[i, j, np10, np20, np30, np12, np13,
                              np23, {nm10, nm20, nm30, nm40}, eps])) -
                I*Pi*theta[-Im[y[k]]]*(2*
                        Log[y[k] -
                            yplus[i, j, np10, np20, np30, np12, np13,
                              np23, {nm10, nm20, nm30, nm40}, eps]] -
                      Log[y[k] -
                            yminus[i, j, np10, np20, np30, np12, np13,
                              np23, {nm10, nm20, nm30, nm40}, eps]]*(y[k] -
                            yplus[i, j, np10, np20, np30, np12, np13,
                              np23, {nm10, nm20, nm30, nm40}, eps])));
        aa[1, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]*
          aa[2, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]*
          aa[3, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]*
          aa[4, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]/
            k*(-1/(c + 2*alp*b)*
                s3D[llaa[2, 4, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  laa[2, 4, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  m40*aa[4, p10, p20, p30, p12, p13,
                          p23, {m10, m20, m30, m40}]^2 - I*eps, y[1], opts] +
              1/(c + 2*alp*b)*
                s3D[llaa[3, 4, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  laa[3, 4, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  m40*aa[4, p10, p20, p30, p12, p13,
                          p23, {m10, m20, m30, m40}]^2 - I*eps, y[2], opts] +
              1/(c + 2*alp*b)*
                s3D[llaa[2, 3, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  laa[2, 3, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  m30*aa[4, p10, p20, p30, p12, p13,
                          p23, {m10, m20, m30, m40}]^2 - I*eps, y[3], opts] +
              1/(c + 2*alp*b)*
                s3D[llaa[1, 4, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  laa[1, 4, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  m40*aa[4, p10, p20, p30, p12, p13,
                          p23, {m10, m20, m30, m40}]^2 - I*eps, y[4], opts] +
              1/(-c + 2*alp*b)*
                s3D[llaa[3, 4, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  laa[3, 4, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  m40*aa[4, p10, p20, p30, p12, p13,
                          p23, {m10, m20, m30, m40}]^2 - I*eps, y[5], opts] -
              1/(c + 2*alp*b)*
                s3D[llaa[1, 3, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  laa[1, 3, p10, p20, p30, p12, p13,
                    p23, {m10, m20, m30, m40}],
                  m30*aa[4, p10, p20, p30, p12, p13,
                          p23, {m10, m20, m30, m40}]^2 - I*eps, y[6], opts] -
              rr[2, 4, 1, p10, p20, p30, p12, p13,
                p23, {m10, m20, m30, m40}] +
              rr[3, 4, 2, p10, p20, p30, p12, p13,
                p23, {m10, m20, m30, m40}] +
              rr[2, 3, 3, p10, p20, p30, p12, p13,
                p23, {m10, m20, m30, m40}] +
              rr[1, 4, 4, p10, p20, p30, p12, p13,
                p23, {m10, m20, m30, m40}] -
              rr[3, 4, 5, p10, p20, p30, p12, p13,
                p23, {m10, m20, m30, m40}] -
              rr[1, 3, 6, p10, p20, p30, p12, p13,
                p23, {m10, m20, m30, m40}] +
              theta[Re[-aa[1, p10, p20, p30, p12, p13,
                          p23, {m10, m20, m30, m40}]*
                      aa[2, p10, p20, p30, p12, p13,
                        p23, {m10, m20, m30, m40}]]]*
                ss[p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40},
                  opts]))];


(* This is formula (6.10): *)

VeltmanD0[pf10_, pf20_, pf30_, pp10_, pp20_, pp30_, m10_, m20_, m30_, m40_,
        opts___] /; (D0Evaluation /. Flatten[{opts}] /. Options[VeltmanD0]) ===
         "direct1" :=
    Block[{(*The minus (from the Bjoerken -
                Drell metric (-1, -1, -1, 1)) is put on here :*)p10 = pf10,
        p20 = -pf20, p30 = -pf30, p12 = -(pp20 - p10 - p20)/2,
        p23 = -(pp30 - p20 - p30)/2,
        p13 = -(pp10 - p10 - p20 - p30 - 2*p12 - 2*p23)/2,
        mm10 = m10*
            aa[1, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        mm20 = m20*
            aa[2, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        mm30 = m30*
            aa[3, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        mm40 = m40*
            aa[4, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]^2,
        a = heaa[p30], b = hebb[p20], c = hecc[p23],
        d = hedd[p30, mm30, mm40], e = heee[p20, p23, mm20, mm30],
        eps = (fcsmeps /. Flatten[{opts}] /. Options[VeltmanD0]),
        f = heff[mm40, eps], g = hegg[p10], h = hehh[p13], j = hejj[p12],
        k = hekk[p10, p12, p13, mm10, mm20], alp = alpha[b, c, a, 1],
        y1 = -(d + e*alp)/((c + 2*alp*b)*(1 - alp)),
        y2 = (d + e*alp)/((c + 2*alp*b)*alp),
        y3 = (d + e*alp + c*alp + 2*a)/(c + 2*alp*b),
        y4 = (d + e*alp + k*alp)/((c + 2*alp*b)*(1 - alp)),
        y5 = (d + e*alp + k*alp)/((-c + 2*alp*b)*alp),
        y6 = (d + e*alp + k*alp + c*alp + 2*a)/(c + 2*alp*b)}, (y[1] = y1;
        y[2] = y2 : y[3] = y3; y[4] = y4; y[5] = y5; y[6] = y6;
        rr[i_, j_, k_, np10_, np20_, np30_, np12_, np13_,
            np23_, {nm10_, nm20_, nm30_,
              nm40_}] := -theta[
                  Re[-aa[i, np10, np20, np30, np12, np13,
                          np23, {nm10, nm20, nm30, nm40}]*
                      aa[k, np10, np20, np30, np12, np13,
                        np23, {nm10, nm20, nm30, nm40}]]]/(c +
                  2*alp*b)*(Pi^2 +
                I*Pi*theta[
                    Re[Im[y[k]]*(2*
                              Log[y[k] -
                                  yminus[i, j, np10, np20, np30, np12, np13,
                                    np23, {nm10, nm20, nm30, nm40}, eps]] -
                            Log[(y[k] -
                                    yminus[i, j, np10, np20, np30, np12, np13,
                                       np23, {nm10, nm20, nm30, nm40},
                                      eps])*(y[k] -
                                    yplus[i, j, np10, np20, np30, np12, np13,
                                      np23, {nm10, nm20, nm30, nm40},
                                      eps])])]] -
                I*Pi*theta[
                    Re[-Im[y[k]]*(2*
                              Log[y[k] -
                                  yplus[i, j, np10, np20, np30, np12, np13,
                                    np23, {nm10, nm20, nm30, nm40}, eps]] -
                            Log[(y[k] -
                                    yminus[i, j, np10, np20, np30, np12, np13,
                                       np23, {nm10, nm20, nm30, nm40},
                                      eps])*(y[k] -
                                    yplus[i, j, np10, np20, np30, np12, np13,
                                      np23, {nm10, nm20, nm30, nm40},
                                      eps])])]]);
        aa[1, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]*
                aa[2, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]*
                aa[3, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]*
                aa[4, p10, p20, p30, p12, p13, p23, {m10, m20, m30, m40}]/k*
                IntegrateHeld[(a*Global`x^2 + b*Global`y^2 +
                          c*Global`x*Global`y + d*Global`x + e*Global`y +
                          f)^(-1) - (a*Global`x^2 + b*Global`y^2 +
                          c*Global`x*Global`y +
                          d*Global`x + (e + k)*Global`y + f)^(-1), {Global`x,
                    0, 1}, {Global`y, 0, Global`x}, ##] & @@
            Union[OptionsSelect[NIntegrate, opts],
              OptionsSelect[Integrate, opts]] /.
          IntegrateHeld -> (fcint /. Flatten[{opts}] /.
                Options[VeltmanD0]))];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Feynman parameters (taken from feynpar by Todd West) *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* feynpar is the procedure which converts integrals over momentum space into
integrals over Feynman parameters.  This procedure calls
privatefeynmanparamter once for each momentum integral to be so converted. *)

 FeynmanParameter1[input1_, input2_, input3_:Null,
      input4_:Null, (opts___Rule | opts___List)] :=
    Block[
     {integrator, intermediateresult1, intermediateresult2},
       integrator = If[FreeQ[input1, FeynmanX],
         FeynmanX, If[FreeQ[input1, FeynmanY], FeynmanY, FeynmanZ]];
       intermediateresult1 = privatefeyparamter[input1, input2,opts]/.
       wrap->Identity/.tmpfad->fcfad;
       If[input3 =!= Null,VerbosePrint[3,"First intermediate result: ", intermediateresult1]];
       integrator = If[integrator === FeynmanX, FeynmanY, FeynmanZ];
       intermediateresult2 = intermediateresult1;
       If[input3 =!= Null, intermediateresult2 =
         intermediateresult1[[0]][privatefeyparamter[
         intermediateresult1[[1]], input3,opts],
         Sequence@@Take[List@@intermediateresult1,{2,-1}]]];
       If[input4 =!= Null,
         VerbosePrint[3,"Second intermediate result: ", intermediateresult2]];
       integrator = FeynmanZ;
       intermediateresult1 = intermediateresult2;
       If[input4 =!= Null, intermediateresult1 =
         intermediateresult2[[0]][privatefeyparamter[
         intermediateresult2[[1]],input4,opts],
         Sequence@@Take[List@@intermediateresult2,{2,-1}]]];
       intermediateresult1 (*What was this good for ?? 5/3-2001*) (*/.
        vector1_[_]^2 :>
          vector1^2 /;
        (vector1 =!= m && vector1 =!= FeynmanX &&
                vector1 =!= FeynmanY && vector1 =!= FeynmanZ)*)];

 FeynmanParameter1[input1_, input2_, input3_:Null, input4_:Null] :=

    Block[
     {integrator, intermediateresult1, intermediateresult2},
       integrator = If[FreeQ[input1, FeynmanX],
         FeynmanX, If[FreeQ[input1, FeynmanY], FeynmanY, FeynmanZ]];
       intermediateresult1 = privatefeyparamter[input1, input2]/.wrap->Identity/.tmpfad->fcfad;
       If[input3 =!= Null,VerbosePrint[3,"First intermediate result: ", intermediateresult1]];
       integrator = If[integrator === FeynmanX, FeynmanY, FeynmanZ];
       intermediateresult2 = intermediateresult1;
       If[input3 =!= Null, intermediateresult2 =
         intermediateresult1[[0]][privatefeyparamter[
         intermediateresult1[[1]], input3],Sequence@@Take[List@@intermediateresult1,{2,-1}]]];
       If[input4 =!= Null,
         VerbosePrint[3,"Second intermediate result: ", intermediateresult2]];
       integrator = FeynmanZ;
       intermediateresult1 = intermediateresult2;
       If[input4 =!= Null, intermediateresult1 =
         intermediateresult2[[0]][privatefeyparamter[
         intermediateresult2[[1]],input4],Sequence@@Take[List@@intermediateresult2,{2,-1}]]];
       intermediateresult1 (*What was this good for ?? 5/3-2001*) (*/.
        vector1_[_]^2 :>
          vector1^2 /;
        (vector1 =!= m && vector1 =!= FeynmanX &&
                vector1 =!= FeynmanY && vector1 =!= FeynmanZ)*)];

 FeynmanParameterize[input1_, rest__, opts___Rule|opts___List] :=
 Block[{a,ex,ddiimm,res,momentabacklist,li,fps,len},
      ddiimm =
      fcdim /. Flatten[{opts}] /. Options[FeynmanParameterize];
      momentabacklist = {};
      res = input1 /.
        {a_*ex_HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator :>
                FeynmanParameter1[a*ex, rest, opts],
       ex_HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator :>
                FeynmanParameter1[ex, rest, opts]} /.
       IntegrateHeld -> (fcint /. {opts} /.
                  Options[FeynmanParameterize]) /.
       (((Times @@ #) -> (fcpa @@ (fcmom[#,ddiimm] & /@ #))) & /@
              Flatten[Outer[List, momentabacklist, momentabacklist],1]) /.
       ((#[li_] -> fcpa[fcli[li, ddiimm], fcmom[#, ddiimm]]) & /@
            momentabacklist)/.
        wrap->Identity/.tmpfad->fcfad//fccontr;

     If[(len=Length[fps=Union[Cases[res,_FeynmanX|_FeynmanY|_FeynmanZ,Infinity]]]) >
     Length[fcfpana /. Flatten[{opts}] /. Options[FeynmanParameterize]] ||
     len===0,res,
     res/.((Rule@@#)&)/@(Transpose[{fps,
     Take[fcfpana /. Flatten[{opts}] /. Options[FeynmanParameterize],len]}])]
];


(* The following procedure is called once for each momentum integral that we are
converting. *)

privatefeyparamter[ex_,kay_, opts___] /; !FreeQ[ex,tmpfad]:=
   privatefeyparamter[ex/.wrap->Identity/.tmpfad->fcfad, kay, opts];

privatefeyparamter[
      ex_HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator,
      kay_, opts___] := privatefeyparamter[dummy*ex, kay, opts] /. dummy -> 1;

privatefeyparamter[
    num___*ex_HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator, kay_,
    opts___] := (ddiimm =
      fcdim /. Flatten[{opts}] /. Options[FeynmanParameterize];
 Block[{g, dimension = ddiimm,(* Fix because we now use
    HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator
    in FeynArts too and FeynArts sets FeynAmpDenominator[]=1. 11/2-2001.*) dum,
    denom = fcfad[dum], nokay, count, coefofksquared,
        enn, finaldenominator,
        power, coefficientlist, ell, ellnumerator, elldenominator, msquared,
        msquaredminusellsquared, numerator, eyezero, alphaenn},

Attributes[g] = {Orderless};

g[x_, y_]*g[x_, z_] ^:= g[y, z] /;  ! NumberQ[x];

g[x_, y_]*(z_)[w___, x_, v___] ^:=
        z[w, y, v] /; z =!= m && z =!= Complex &&  ! NumberQ[x];

g /: g[x_, y_]^2 := dimension /;  ! NumberQ[x];

g[x_, x_] := dimension /;  ! NumberQ[x];


(* Pick out the FeynAmpdenominator and group identical
   PropagatorDenominators*)

      denominator = ((List @@ ex) //.
              List[a___, b_, c___, b_, d___] :> List[a, b^2, c, b, d]) /.
          fcmom[mom_, ___] :> (momentabacklist =
                Union[momentabacklist,
                  Cases[fcmomexp[fcmom[mom]], fcmom[_], Infinity,
                      Heads -> True] /. fcmom -> Sequence]; mom);

(* Pick out factors in the denominator which depend on the momentum
over which we're integrating (kay). *)

   nokay =
        Times @@
          Map[If[FreeQ[#1, kay], #1, (denom = Join[denom, fcfad[#1]] /. dum -> Sequence[]; 1)] &,
            denominator];

  VerbosePrint[3,"Denominator: ", denom];
  VerbosePrint[3,"Inverse denominators depending on the integration momentum: ",kay];
  VerbosePrint[3,"Denominators not depending on the integration momentum: ",nokay];

    enn = Length[denom];


(* Take care of terms in the denominator which are of the form
(kay^2 + 2p kay + M^2)^power , where power is not equal to 1. *)

    Block[{iter},
      Do[If[
            Head[denom[[iter]]] ===

              Power, (power[iter] = denom[[iter, 2]]; denom[[iter]] =
        denom[[iter, 1]];), power[iter] = 1
         ],
      {iter, enn}
      ]];

      VerbosePrint[3,"Denominator after power fixing: ", denom];


(* Expand each term in the denominator. *)

      denom = Map[Expand[#1 //. fcpd[a_, b_] :> a^2 - b^2] & , denom,
          2];

      VerbosePrint[3,"Denominator expanded: ", StandardForm[denom]];


(* If a term in the denominator is of the form (a kay^2 + 2p kay + M^2),
with a not equal to 1, divide this term by a. *)

  Block[{iter},
    Do[If[(coefofksquared = Coefficient[denom[[iter]], kay, 2]) =!=
              1, (nokay *=
       coefofksquared^power[iter];
       denom[[iter]] = Apart[denom[[iter]]/
       coefofksquared];)] ,
    {iter, enn}]];

      VerbosePrint[3,"Denominator after more power fixing: ", StandardForm[denom]];


(* Introduce Feynman parameters (x[i], y[i], z[i]). *)

    finaldenominator = denom[[1]] + Sum[(denom[[iter]] - denom[[iter - 1]])*
      integrator[iter - 1], {iter, 2, enn}];
    coefficientlist = CoefficientList[finaldenominator, kay];


(* The new denominator can be written in the form
(kay^2 + 2 ell kay + msquared). *)

 ell = Factor[coefficientlist[[2]]/2];
 ellnumerator = Expand[Numerator[ell]];
 elldenominator = Denominator[ell];
 msquared = coefficientlist[[1]];
 msquaredminusellsquared = msquared - Expand[ell ell];

 VerbosePrint[3,"Masses squared: ", StandardForm[msquared,msquaredminusellsquared]];

      numerator =
        Expand[fcmomcomb[Times[num]]/.
         fcpa[fcmom[mom1_,___],fcmom[mom2_,___]] :>
         (lil=Unique[li]; fcpa[fcli[lil],fcmom[mom1]] fcpa[fcli[lil],fcmom[mom2]])/.
            fcpa[fcli[li_, ___],
                fcmom[mom_, ___]] :> (momentabacklist =
                  Union[momentabacklist,
                    Cases[fcmomexp[fcmom[mom]], fcmom[_], Infinity,
                        Heads -> True] /. fcmom -> Sequence];
                fcmomexp[fcmom[mom]] /. fcmom[momm_, ___] :> momm[li])];

 VerbosePrint[3,"The numerator: ", numerator];


(* eyezero is approximately equal to the result we get if the
numerator of the expression which we are converting does
not contain any factors of kay. *)

 eyezero = (
 I(-1)^(dimension/2)/(2^dimension Pi^(dimension/2)))*
   Gamma[(alphaenn = Block[{iterator, firstletter},
     Sum[power[iterator], {iterator, enn}]]) - dimension/2]/
     Block[{iterator}, Product[(power[iterator] - 1)!, {iterator, enn}]]*
     integrator[enn - 1]^(power[enn] - 1)*
     Block[{iterator}, Product[(integrator[iterator - 1] -
             integrator[iterator])^(power[iterator] - 1),
           {iterator, 2, enn - 1}]] (1 - integrator[1])^
     (power[1] - 1) msquaredminusellsquared^(dimension/2 - alphaenn);

 VerbosePrint[3,"eyezero: ", eyezero];


(* The rest of this procedure handles any factors of kay which
may appear in the numerator (up to terms of the form kay^5). *)

numerator = numerator //. kay^2 :> kay[Unique["dumb$"]]^2;
numerator = numerator //. kay mom_Symbol xxxxxx___ :>
              kay[dumb = Unique["dumb$"]] mom[dumb] xxxxxx;
numerator =
        numerator /. kay[xx1_] kay[yy2_] kay[zz3_] kay[ww4_] kay[vv5_] :>
 -(privateel[xx1] privateel[yy2] privateel[zz3] privateel[ww4]privateel[
                      vv5] +
  (g[xx1, yy2]privateel[zz3]privateel[ww4]privateel[vv5] +
   g[xx1, zz3]privateel[yy2]privateel[ww4]privateel[vv5] +
   g[yy2, zz3]privateel[xx1]privateel[ww4]privateel[vv5] +
   g[xx1, ww4]privateel[zz3]privateel[yy2]privateel[vv5] +
   g[yy2, ww4]privateel[xx1]privateel[zz3]privateel[vv5] +
   g[zz3, ww4]privateel[xx1]privateel[yy2]privateel[vv5] +
   g[xx1, vv5]privateel[zz3]privateel[ww4]privateel[yy2] +
   g[yy2, vv5]privateel[xx1]privateel[ww4]privateel[zz3] +
   g[zz3, vv5]privateel[xx1]privateel[yy2]privateel[ww4] +
   g[ww4, vv5]privateel[xx1]privateel[yy2]privateel[zz3])*
     msquaredminusellsquared/(2 alphaenn - 2 - dimension) +
  (g[xx1, yy2]g[zz3, ww4]privateel[vv5] +
                        g[xx1, zz3]g[yy2, ww4]privateel[vv5] +
   g[xx1, ww4]g[yy2, zz3]privateel[vv5] +
                        g[xx1, yy2]g[zz3, vv5]privateel[ww4] +
   g[xx1, yy2]g[ww4, vv5]privateel[zz3] +
                        g[xx1, zz3]g[yy2, vv5]privateel[ww4] +
   g[xx1, zz3]g[ww4, vv5]privateel[yy2] +
                        g[yy2, zz3]g[xx1, vv5]privateel[ww4] +
   g[yy2, zz3]g[ww4, vv5]privateel[xx1] +
                        g[xx1, ww4]g[yy2, vv5]privateel[zz3] +
   g[xx1, ww4]g[zz3, vv5]privateel[yy2] +
                        g[yy2, ww4]g[xx1, vv5]privateel[zz3] +
   g[yy2, ww4]g[zz3, vv5]privateel[xx1] +
                        g[zz3, ww4]g[xx1, vv5]privateel[yy2] +
   g[zz3, ww4]g[yy2, vv5]privateel[xx1])msquaredminusellsquared^2/
    ((2 alphaenn - 2 - dimension)(2 alphaenn - 4 - dimension)));

numerator = numerator /. kay[xx1_] kay[yy2_] kay[zz3_] kay[ww4_]^2 :>
  -(privateel[xx1] privateel[yy2] privateel[zz3] privateel[ww4]*
    privateel[ww4] + (g[xx1, yy2]privateel[zz3]privateel[ww4]privateel[ww4] +
   g[xx1, zz3]privateel[yy2]privateel[ww4]privateel[ww4] +
   g[yy2, zz3]privateel[xx1]privateel[ww4]privateel[ww4] +
   g[xx1, ww4]privateel[zz3]privateel[yy2]privateel[ww4] +
   g[yy2, ww4]privateel[xx1]privateel[zz3]privateel[ww4] +
   g[zz3, ww4]privateel[xx1]privateel[yy2]privateel[ww4] +
   g[xx1, ww4]privateel[zz3]privateel[ww4]privateel[yy2] +
   g[yy2, ww4]privateel[xx1]privateel[ww4]privateel[zz3] +
   g[zz3, ww4]privateel[xx1]privateel[yy2]privateel[ww4] +
   g[ww4, ww4]privateel[xx1]privateel[yy2]privateel[zz3])*
     msquaredminusellsquared/(2 alphaenn - 2 - dimension) +
  (g[xx1, yy2]g[zz3, ww4]privateel[ww4] +
                        g[xx1, zz3]g[yy2, ww4]privateel[ww4] +
   g[xx1, ww4]g[yy2, zz3]privateel[ww4] +
                        g[xx1, yy2]g[zz3, ww4]privateel[ww4] +
   g[xx1, yy2]g[ww4, ww4]privateel[zz3] +
                        g[xx1, zz3]g[yy2, ww4]privateel[ww4] +
   g[xx1, zz3]g[ww4, ww4]privateel[yy2] +
                        g[yy2, zz3]g[xx1, ww4]privateel[ww4] +
   g[yy2, zz3]g[ww4, ww4]privateel[xx1] +
                        g[xx1, ww4]g[yy2, ww4]privateel[zz3] +
   g[xx1, ww4]g[zz3, ww4]privateel[yy2] +
                        g[yy2, ww4]g[xx1, ww4]privateel[zz3] +
   g[yy2, ww4]g[zz3, ww4]privateel[xx1] +
                        g[zz3, ww4]g[xx1, ww4]privateel[yy2] +
   g[zz3, ww4]g[yy2, ww4]privateel[xx1])msquaredminusellsquared^2/
      ((2 alphaenn - 2 - dimension)(2 alphaenn - 4 - dimension)));

numerator = numerator /. kay[xx1_] kay[yy2_]^2 kay[ww4_]^2 :>
-(privateel[xx1] privateel[yy2] privateel[yy2] privateel[ww4]privateel[ww4] +
  (g[xx1, yy2]privateel[yy2]privateel[ww4]privateel[ww4] +
   g[xx1, yy2]privateel[yy2]privateel[ww4]privateel[ww4] +
   g[yy2, yy2]privateel[xx1]privateel[ww4]privateel[ww4] +
   g[xx1, ww4]privateel[yy2]privateel[yy2]privateel[ww4] +
   g[yy2, ww4]privateel[xx1]privateel[yy2]privateel[ww4] +
   g[yy2, ww4]privateel[xx1]privateel[yy2]privateel[ww4] +
   g[xx1, ww4]privateel[yy2]privateel[ww4]privateel[yy2] +
   g[yy2, ww4]privateel[xx1]privateel[ww4]privateel[yy2] +
   g[yy2, ww4]privateel[xx1]privateel[yy2]privateel[ww4] +
   g[ww4, ww4]privateel[xx1]privateel[yy2]privateel[yy2])*
      msquaredminusellsquared/(2 alphaenn - 2 - dimension) +
  (g[xx1, yy2]g[yy2, ww4]privateel[ww4] +
                        g[xx1, yy2]g[yy2, ww4]privateel[ww4] +
   g[xx1, ww4]g[yy2, yy2]privateel[ww4] +
                        g[xx1, yy2]g[yy2, ww4]privateel[ww4] +
   g[xx1, yy2]g[ww4, ww4]privateel[yy2] +
                        g[xx1, yy2]g[yy2, ww4]privateel[ww4] +
   g[xx1, yy2]g[ww4, ww4]privateel[yy2] +
                        g[yy2, yy2]g[xx1, ww4]privateel[ww4] +
   g[yy2, yy2]g[ww4, ww4]privateel[xx1] +
                        g[xx1, ww4]g[yy2, ww4]privateel[yy2] +
   g[xx1, ww4]g[yy2, ww4]privateel[yy2] +
                        g[yy2, ww4]g[xx1, ww4]privateel[yy2] +
   g[yy2, ww4]g[yy2, ww4]privateel[xx1] +
                        g[yy2, ww4]g[xx1, ww4]privateel[yy2] +
   g[yy2, ww4]g[yy2, ww4]privateel[xx1])*msquaredminusellsquared^2/
       ((2 alphaenn - 2 - dimension)(2 alphaenn - 4 - dimension)));

numerator =
        numerator /.
          kay[xx1_] kay[yy2_] kay[zz3_] kay[
                ww4_] :>
(privateel[xx1] privateel[yy2] privateel[
                    zz3] privateel[ww4] +
  (g[xx1, yy2]privateel[zz3]privateel[ww4] +
   g[xx1, zz3]privateel[yy2]privateel[ww4] +
   g[yy2, zz3]privateel[xx1]privateel[ww4] +
   g[xx1, ww4]privateel[zz3]privateel[yy2] +
   g[yy2, ww4]privateel[xx1]privateel[zz3] +
   g[zz3, ww4]privateel[xx1]privateel[yy2])*
      msquaredminusellsquared/(2 alphaenn - 2 - dimension) +
  (g[xx1, yy2]g[zz3, ww4] + g[xx1, zz3]g[yy2, ww4] + g[xx1, ww4]g[yy2, zz3])*
      msquaredminusellsquared^2/((2 alphaenn - 2 - dimension)*
      (2 alphaenn - 4 - dimension)));

numerator =
        numerator /.
          kay[xx1_] kay[
                yy2_] kay[
                  zz3_]^2 :>
(privateel[xx1] privateel[yy2] privateel[
                    zz3] privateel[zz3] +
  (g[xx1, yy2]privateel[zz3]privateel[zz3] +
   g[xx1, zz3]privateel[yy2]privateel[zz3] +
   g[yy2, zz3]privateel[xx1]privateel[zz3] +
   g[xx1, zz3]privateel[zz3]privateel[yy2] +
   g[yy2, zz3]privateel[xx1]privateel[zz3] +
   g[zz3, zz3]privateel[xx1]privateel[yy2])*
      msquaredminusellsquared/(2 alphaenn - 2 - dimension) +
  (g[xx1, yy2]g[zz3, zz3] + g[xx1, zz3]g[yy2, zz3] + g[xx1, zz3]g[yy2, zz3])*
     msquaredminusellsquared^2/((2 alphaenn - 2 - dimension)*
     (2 alphaenn - 4 - dimension)));

numerator = numerator /. kay[xx1_]^2 kay[zz3_]^2 :>
(privateel[xx1] privateel[xx1] privateel[zz3]privateel[zz3] +
  (g[xx1, xx1]privateel[zz3]privateel[zz3] +
   g[xx1, zz3]privateel[xx1]privateel[zz3] +
   g[xx1, zz3]privateel[xx1]privateel[zz3] +
   g[xx1, zz3]privateel[zz3]privateel[xx1] +
   g[xx1, zz3]privateel[xx1]privateel[zz3] +
   g[zz3, zz3]privateel[xx1]privateel[xx1])msquaredminusellsquared/
      (2 alphaenn - 2 - dimension) +
  (g[xx1, xx1]g[zz3, zz3] + g[xx1, zz3]g[xx1, zz3] + g[xx1, zz3]g[xx1, zz3])*
      msquaredminusellsquared^2/((2 alphaenn - 2 - dimension)*
      (2 alphaenn - 4 - dimension)));

numerator = numerator /. kay[xx1_] kay[yy2_] kay[zz3_] :>
-(privateel[xx1] privateel[yy2] privateel[zz3] +
  (1/2)(g[xx1, yy2]privateel[zz3] + g[xx1, zz3]privateel[yy2] +
  g[yy2, zz3]privateel[xx1])msquaredminusellsquared/(alphaenn - 1 -
                          dimension/2));

numerator = numerator /. kay[xx1_]^2 kay[zz3_] :>
-(privateel[xx1]^2privateel[zz3] + (1/2)(g[xx1, xx1]privateel[zz3] +
 privateel[zz3] + privateel[zz3])msquaredminusellsquared/
    (alphaenn - 1 - dimension/2));

numerator =
        numerator /. kay[xx1_] kay[yy2_] :> (privateel[xx1] privateel[yy2] +
   g[xx1, yy2] msquaredminusellsquared/(2(alphaenn - 1 - dimension/2))) ;

numerator = numerator /. kay[xx1_]^2 :> (privateel[xx1]^2 + g[xx1, xx1]*
   msquaredminusellsquared/(2(alphaenn - 1 - dimension/2))) ;

numerator = numerator /. kay[xx1_] -> -privateel[xx1];

integrators =
        If[enn == 2, {integrator[1], 0, 1},
          Sequence @@ (Join[{{integrator[1], 0, 1}},
                Table[{integrator[iterator], 0,
                    integrator[iterator - 1]}, {iterator, 2, enn - 1}]])];

 VerbosePrint[3,"The final eyezero: ",eyezero];
 VerbosePrint[3,"The final numerator: ",numerator];
 VerbosePrint[3,"The final nokay: ",nokay];

                        IntegrateHeld[((2*Pi)^dimension)*numerator*wrap[eyezero]*
                        If[Head[nokay]===Times,tmpfad@@nokay,If[nokay===1,1,tmpfad[nokay]]],
                       integrators] /.
        g[a_, b_] :> fcpa[fcli[a], fcli[b]]]);

privateel[xxx2_] :=
  Block[{temporary},
   temporary = ellnumerator /.
       xxx3___ yyy3_Symbol :> xxx3 yyy3[xxx2];
       Map[If[Head[#] === Symbol, #[xxx2], #] &, temporary, {1}]/
        elldenominator];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Dimensional expansion of integrals  *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* The pole terms are isolated: *)

VeltmanExpand[amp_, opts___?OptionQ] := (
      a0opts = Options[fca0];
      SetOptions[fca0, fca0tob0->True];
      mv[i_] := ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MandelstamReduce]) <> ToString[i]];
      masses1[i_] := Masses[[i]];
      res = amp /. ParticleMass[a__]^2 -> ParticleMass[a]^2 /.
             {fcb0[p10_, m10_, m20_] :> VeltmanB0[p10, m10, m20, ##]& @@
                OptionsSelect[VeltmanB0, opts, Sequence @@
                                               Select[Options[VeltmanExpand], FreeQ[#,fcint]&]],
              fcc0[p10_, p12_, p20_, m10_, m20_, m30_] :>
                VeltmanC0[p10, p12, p20, m10, m20, m30, ##]& @@
                OptionsSelect[VeltmanC0, opts, Sequence @@ 
                                               Select[Options[VeltmanExpand], FreeQ[#,fcint]&]],
              fcd0[p10_, p12_, p23_, p30_, p20_, p13_, m10_, m20_, m30_, m40_] :>
                VeltmanD0[p10, p12, p23, p30, p20, p13, m10, m20, m30, m40, ##]& @@
                OptionsSelect[VeltmanD0, opts, Sequence @@ 
                                               Select[Options[VeltmanExpand], FreeQ[#,fcint]&]]} /.
              (B0Evaluation -> "jbar") :> (B0Evaluation -> "direct1") /.
              If[(OnMassShell /. Flatten[{opts}] /. Options[VeltmanExpand]),
                Table[fcpa[fcmom[mv[irep], ___], fcmom[mv[irep], ___]] ->
                masses1[irep]^2, {irep, Length[(Masses /. Flatten[{opts}] /.
                Options[VeltmanExpand])]}], {}] /.
              IntegrateHeld -> (fcint /. Flatten[{opts}] /. Options[VeltmanExpand]) /.
              LeutwylerLambda[oopp__] :> (LeutwylerLambda[##]& @@
                Complement[Flatten[{oopp}], Options[LeutwylerLambda]]);
        SetOptions[fca0,Sequence@@a0opts];
        res);


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Renormalization *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Wavefuntion and decay constant renormalization of the lowest order amplitudes
and two loop renormalization should be added.... *)

Renormalize[m_, opts___] :=
  m /. fccoupl[a_[l_], n_, aaa___, RenormalizationState[0],
        b___] :> (fccoupl[a[l], n, aaa, RenormalizationState[1],
            b] + (RenormalizationCoefficientFunction[
                    fccoupl[a[l], n]] /. Flatten[{opts}] /.
                Options[Renormalize])*(InfinityFactor /. Flatten[{opts}] /.
                Options[Renormalize]));


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


End[];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


EndPackage[];


If[$Verboseness > 0,WriteString["stdout", "Renormalization | \n "]];
