(* Utilities *)

(* Utilities for kinematics and Dirac algebra *)

(* Author:  Frederik Orellana, fjob@cabocomm.dk

   Creation date:  1/8-2000

   Context: HighEnergyPhysics`Phi`Utilities *)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

BeginPackage[
    "HighEnergyPhysics`Phi`Utilities`", {"HighEnergyPhysics`Phi`",
    "HighEnergyPhysics`FeynCalc`", "HighEnergyPhysics`Phi`Objects`"}];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

tmp`olddir = Directory[];
SetDirectory[HighEnergyPhysics`Phi`$HEPDir];
SetDirectory["HighEnergyPhysics"];
SetDirectory["Phi"];
Get["Utilities.defs.m"];
SetDirectory[tmp`olddir];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

Begin["`Private`"];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* FeynCalc functions *)

fcpa := fcpa = MakeContext["Pair"];
fcmom := fcmom = MakeContext["Momentum"];
fcsundel := fcsundel = MakeContext["SUNDelta"];
fcexpscp := fcexpscp = MakeContext["ExpandScalarProduct"];
fcfad := fcfad = MakeContext["FeynAmpDenominator"];
fcli := fcli = MakeContext["LorentzIndex"];
fcdot := fcdot = DOT;
fcdiga := fcdiga = MakeContext["DiracGamma"];
fcdtr := fcdtr = MakeContext["DiracTrace"];
fceps := fceps = MakeContext["Eps"];
fcsp := fcsp = MakeContext["ScalarProduct"];
fcprd := fcprd = MakeContext["PropagatorDenominator"];
fcmomex := fcmomex = MakeContext["MomentumExpand"];
fcmomcomb := fcmomcomb = MakeContext["MomentumCombine"];
fcpave := fcpave = MakeContext["PaVe"];
fconeloop := fconeloop = MakeContext["OneLoop"];
fcexpt := fcexpt = MakeContext["Explicit"];
fcqf := fcqf = MakeContext["QuantumField"];
fcpd := fcpd = MakeContext["PartialD"];
fcsunn := fcsunn = MakeContext["SUNN"];
fcpol := fcpol = MakeContext["Polarization"];
fccombs := fccombs = MakeContext["Combinations"];
FieldDerivative := FieldDerivative = MakeContext["FieldDerivative"];
CovariantFieldDerivative := CovariantFieldDerivative = MakeContext["CovariantFieldDerivative"];


(* Tracer functions *)

trtr := trtr = Tracer`GammaTrace; trid = Tracer`TrU; trsp :=
  trsp = Tracer`Spur; trdot := trdot = Tracer`TrS; vecd :=
  vecd = Tracer`VectorDimension; trsig := trsig = Tracer`Sigma; treps :=
  treps = Tracer`TrEps;



(* Defaults *)

Options[MandelstamReduce] = {MomentaSumLeft -> All, OnMassShell -> True,
      Cancel -> MandelstamU, MomentumVariablesString -> "p",
      MomentaSumRule -> True,
      Masses -> {ParticleMass[Pion, RenormalizationState[0]],
          ParticleMass[Pion, RenormalizationState[0]],
          ParticleMass[Pion, RenormalizationState[0]],
          ParticleMass[Pion, RenormalizationState[0]]}};
DeclareUScalar[MandelstamS]; DeclareUScalar[MandelstamT]; 
DeclareUScalar[MandelstamU];
Options[LorentzIndicesSupply] = {LorentzIndicesString -> "\[Nu]"};
Options[GammaSort] = {Gamma5AntiCommute -> False,
      OrderingFunction -> OrderedQ};
Options[DiscardOrders] = {PerturbationOrder -> 4, DiscardMomenta -> True,
      ScalarProductForm -> MomentaScalarProduct};
Options[FCToTracer] := {TracerIndicesString -> "l"};
Options[CheckF] = {Directory ->
        (*Directory[]*)
ToFileName[{HighEnergyPhysics`FeynCalc`$FeynCalcDirectory,"Phi"}, "Storage"],
ForceSave -> False, NoSave -> False};
Options[SurfaceReduce] = {DifferenceOrder -> 2, UFields -> UPerturbation};
Options[UReduce] = {SMMToMM -> False, FullReduce -> True, fcsunn -> 2, UDimension -> Automatic};
Options[SMMToMM] = {fcsunn -> 2};
Options[UPerturb] = {ExpansionOrder -> {1}, fcsunn -> 2,
UFields -> {USmall, UChiPlus, UChiMinus, UFPlus, UFMinus, MM}};
Options[CharacteristicCoefficient] = {UDimension -> 2};
Options[CayleyHamilton] = {UDimension -> 2, Eliminate -> True};
Options[CayleyHamiltonRules] = {fcsunn -> 2, UDimension -> 2,
  CommutatorReduce->True, UReduce -> False};
tmpoptscdr=Options[CovariantFieldDerivative];
tmpoptscn=Options[CovariantNabla];
tmpoptsmm=Options[MM];
tmpoptssmm=Options[SMM];
SetOptions[CovariantFieldDerivative, fcexpt -> False];
SetOptions[MM, fcexpt -> False];
Options[CayleyHamiltonTrick] = {fcsunn -> 2, UDimension -> 2,
    CommutatorReduce -> True, UReduce -> True,
    UMatrices :> {{I NM[Adjoint[CovariantFieldDerivative[MM[Global`x_],
    Global`x_, {Global`\[Rho]1_}]], MM[Global`x_]],
    I NM[Adjoint[MM[Global`x_]], CovariantFieldDerivative[MM[Global`x_],
    Global`x_, {Global`\[Rho]2_}]],
    NM[Adjoint[CovariantFieldDerivative[MM[Global`x_], Global`x_, {Global`\[Rho]1_}]],
      CovariantFieldDerivative[MM[Global`x_], Global`x_, {Global`\[Rho]2_}]]}}};
SetOptions[CovariantFieldDerivative, Sequence@@tmpoptscdr];
SetOptions[MM, Sequence@@tmpoptsmm];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Mandelstam simplification *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

mv[opts___][i_] := ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MandelstamReduce]) <> ToString[i]];
mss[opts___][i_] := fcpa[fcmom[mv[opts][i]], fcmom[mv[opts][i]]];
masses1[opts___][i_] := (Masses /. Flatten[{opts}] /.
              Options[MandelstamReduce])[[i]];

manrul[opts___] := {MandelstamS + MandelstamU + MandelstamT -> Sum[mss[opts][i], {i,1,4}],
         -MandelstamS - MandelstamU - MandelstamT -> -Sum[mss[opts][i], {i,1,4}],
          MandelstamU + MandelstamT -> -MandelstamS + Sum[mss[opts][i], {i,1,4}],
         -MandelstamU - MandelstamT -> MandelstamS - Sum[mss[opts][i], {i,1,4}],
          MandelstamS + MandelstamT -> -MandelstamU + Sum[mss[opts][i], {i,1,4}],
         -MandelstamS - MandelstamT -> MandelstamU - Sum[mss[opts][i], {i,1,4}],
          MandelstamS + MandelstamU -> -MandelstamT + Sum[mss[opts][i], {i,1,4}],
         -MandelstamS - MandelstamU -> MandelstamT - Sum[mss[opts][i], {i,1,4}],
          a_*MandelstamS + b_*MandelstamT + b_*MandelstamU ->
            (a - b)*MandelstamS + b*Sum[mss[opts][i], {i,1,4}],
          b_*MandelstamS + a_*MandelstamT + b_*MandelstamU ->
            (a - b)*MandelstamT + b*Sum[mss[opts][i], {i,1,4}],
         b_*MandelstamS + b_*MandelstamT + a_*MandelstamU ->
            (a - b)*MandelstamU + b*Sum[mss[opts][i], {i,1,4}]};

(* The convention used is:  s=(p1+p2)^2, t=(p2+p3)^2, u=(p1+p3)^2 with all \
particles incoming (All) as in FeynCalc, that is s=(p1+p2)^2, t=(p2-p3)^2, \
u=(p1-p3)^2 (FirstHalf) where particles 1 and 2 are incoming and p3 and p4 \
are outgoing: *)

sturules[opts___] /; ((MomentaSumLeft /. Flatten[{opts}] /.
              Options[MandelstamReduce]) === All) := {fcpa[
            fcmom[mv[opts][1], ___], fcmom[mv[opts][2], ___]] ->
          MandelstamS/
              2 - (fcpa[fcmom[mv[opts][1]], fcmom[mv[opts][1]]] +
                  fcpa[fcmom[mv[opts][2]], fcmom[mv[opts][2]]])/2,
        fcpa[fcmom[mv[opts][2], ___], fcmom[mv[opts][3], ___]] ->
          MandelstamT/
              2 - (fcpa[fcmom[mv[opts][2]], fcmom[mv[opts][2]]] +
                  fcpa[fcmom[mv[opts][3]], fcmom[mv[opts][3]]])/2,
        fcpa[fcmom[mv[opts][1], ___], fcmom[mv[opts][3], ___]] ->
          MandelstamU/
              2 - (fcpa[fcmom[mv[opts][1]], fcmom[mv[opts][1]]] +
                  fcpa[fcmom[mv[opts][3]], fcmom[mv[opts][3]]])/2};

sturules[opts___] /; (MomentaSumLeft /. Flatten[{opts}] /.
            Options[MandelstamReduce]) ===
        HighEnergyPhysics`Phi`Objects`FirstHalf := {fcpa[
            fcmom[mv[opts][1], ___], fcmom[mv[opts][2], ___]] ->
          MandelstamS/
              2 - (fcpa[fcmom[mv[opts][1]], fcmom[mv[opts][1]]] +
                  fcpa[fcmom[mv[opts][2]], fcmom[mv[opts][2]]])/2,
        fcpa[fcmom[mv[opts][2], ___],
            fcmom[mv[opts][3], ___]] -> -MandelstamT/
              2 + (fcpa[fcmom[mv[opts][2]], fcmom[mv[opts][2]]] +
                  fcpa[fcmom[mv[opts][3]], fcmom[mv[opts][3]]])/2,
        fcpa[fcmom[mv[opts][1], ___],
            fcmom[mv[opts][3], ___]] -> -MandelstamU/
              2 + (fcpa[fcmom[mv[opts][1]], fcmom[mv[opts][1]]] +
                  fcpa[fcmom[mv[opts][3]], fcmom[mv[opts][3]]])/2};

sturules[opts___] /; (MomentaSumLeft /. Flatten[{opts}] /.
            Options[MandelstamReduce]) ===
        HighEnergyPhysics`Phi`Objects`Odd := {fcpa[
            fcmom[mv[opts][1], ___],
            fcmom[mv[opts][2], ___]] -> -MandelstamS/
              2 + (fcpa[fcmom[mv[opts][1]], fcmom[mv[opts][1]]] +
                  fcpa[fcmom[mv[opts][2]], fcmom[mv[opts][2]]])/2,
        fcpa[fcmom[mv[opts][2], ___],
            fcmom[mv[opts][3], ___]] -> -MandelstamT/
              2 + (fcpa[fcmom[mv[opts][2]], fcmom[mv[opts][2]]] +
                  fcpa[fcmom[mv[opts][3]], fcmom[mv[opts][3]]])/2,
        fcpa[fcmom[mv[opts][1], ___], fcmom[mv[opts][3], ___]] ->
          MandelstamU/
              2 - (fcpa[fcmom[mv[opts][1]], fcmom[mv[opts][1]]] +
                  fcpa[fcmom[mv[opts][3]], fcmom[mv[opts][3]]])/2};

strules[opts___] := (Cancel /. Flatten[{opts}] /.
            Options[MandelstamReduce]) ->
        mss[opts][1] + mss[opts][2] + mss[opts][3] + mss[opts][4] -
          Complement[{MandelstamS, MandelstamT,
                MandelstamU}, {(Cancel /. Flatten[{opts}] /.
                    Options[MandelstamReduce])}][[1]] -
          Complement[{MandelstamS, MandelstamT,
                MandelstamU}, {(Cancel /. Flatten[{opts}] /.
                    Options[MandelstamReduce])}][[2]];

MandelstamReduce1[amp_, opts___Rule] := fcexpscp[
                  amp /.
                  (*We don't want a polarization vector Polarization[p1] to
                    be replaced with Polarization[-p2-p3-p4]*)
                  fcpol[a__] :> ToString/@fcpol[a] /.
                  If[MomentaSumRule /. Flatten[{opts}] /. Options[MandelstamReduce],
                     MomentaSumRule[
                      Join[Select[Flatten[{opts}],
                            (!FreeQ[#, (MomentumVariablesString -> _ |
                                        MomentaSumLeft -> _)]) &],
                        Select[Options[MandelstamReduce],
                               (!FreeQ[#, (MomentumVariablesString -> _ |
                                        MomentaSumLeft -> _)]) &]]], {}] /.
                   fcpol[a__] :> ToExpression/@fcpol[a]] /.
                sturules[opts] /.
              If[(OnMassShell /. Flatten[{opts}] /.
                    Options[MandelstamReduce]),
                Table[fcpa[fcmom[mv[opts][irep], ___], fcmom[mv[opts][irep], ___]] ->
                    masses1[opts][irep]^2, {irep, 4}], {}] /.
            If[!(Cancel /. Flatten[{opts}] /.
                      Options[MandelstamReduce]) === None,
              strules[opts], {}] /.
          If[(OnMassShell /. Flatten[{opts}] /. Options[MandelstamReduce]),
            Table[fcpa[fcmom[mv[opts][irep], ___], fcmom[mv[opts][irep], ___]] ->
                masses1[opts][irep]^2, {irep, 4}], {}];

MandelstamReduce[amp_, opts___Rule] := MandelstamReduce1[
                                         MandelstamReduce1[amp, opts] /.
                                         manrul[opts], MomentaSumRule->False, opts];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Lorentz indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Support functions for LorentzIndicesSupply.  Index numerators, increasing by \
one each time applied: *)

id[x_, opts___Rule] := x;
$LorentzIndicesCounter = 0;
lin[opts___] := (++$LorentzIndicesCounter;
      ToExpression[(LorentzIndicesString /. Flatten[{opts}] /.
              Options[LorentzIndicesSupply]) <>
          ToString[$LorentzIndicesCounter]]);



(* Step one in the supplial of the extra Lorentz index dependence: *)

indicesdotrule1[optss___] := ((fcpa | fcsp)[a_,
            b_] /; (! FreeQ[a, fcmom] && ! FreeQ[b, fcmom]) :>
        indsuppdot[a, b, lin[optss]]);
indicesdotrule2[optss___] := (fcdiga[
          a : fcmom[_, ___] | ___*fcmom[_, ___] |
              HoldPattern[Plus[((___*
           HighEnergyPhysics`FeynCalc`Momentum`Momentum[_,___]) |
           HighEnergyPhysics`FeynCalc`Momentum`Momentum[_, ___]) ..]], dim___] :>
           indsuppdot[a, fcdiga1[lorentzdummy, dim], lin[optss]]);



(* Step two in the supplial of the extra Lorentz index dependence: *)

indsuppdot[a_, b_, i_] /; (! FreeQ[a, fcmom] && ! FreeQ[b, fcmom]) :=
    a*b /. fcmom[c_, dim___] -> fcpa[fcmom[c, dim], loritemp[i, dim]];
indsuppdot[
      a : fcmom[_, ___] | ___*fcmom[_, ___] |
          HoldPattern[
            Plus[((___*HighEnergyPhysics`FeynCalc`Momentum`Momentum[_, ___]) |
            HighEnergyPhysics`FeynCalc`Momentum`Momentum[_, ___])..]],
            fcdiga1[lorentzdummy, dim___], i_] :=
    a*fcdiga[loritemp[i, dim], dim] /.
      fcmom[c_, dimm___] -> fcpa[fcmom[c, dimm], loritemp[i, dimm]];



(* The function that supplies indices to expressions involving IsoDots,
IsoCrosses and IsoSymmetricCrosses of iso-spin vectors: *)

LorentzIndicesSupply[aa_, (optss___Rule | optss___List)] :=
   (aa /.
      (fcpa | fcsp)[a_, b_]^n_ :> times1 @@ Table[fcdot[a, b], {rep, n}] /.
      Power[a_, b_ /; b > 0 && IntegerQ[b]] :>
                  times1 @@ Table[a, {ddum, 1, b}]/.
           {indicesdotrule1[optss], indicesdotrule2[optss]} /.
           loritemp -> fcli /.
           times1 -> Times);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Rank four tensor integrals *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* A four point tensor integral of rank four can not be handled by FeynCalc.  We
   reduce it to lower rank PaVe's. *)



(* The stuff below is just to have constants taken out and sums expanded: *)



(* Clear[FourPoint]; *)

FourPoint[q_, a_Plus, opts___] := (FourPoint[q, #, opts] & /@ a);
FourPoint[q_, a_*b_, opts___] /; FreeQ[a, q | fcprd] :=
    a*FourPoint[q, b, opts];


FourPoint[q_, aa_*fcfad[f___, fcprd[fcmom[q1_, d___] + fcmom[q2_,
          d___] + m___, m1_], l___], opts___] :=
    FourPoint[q, aa*fcmomcomb[fcfad[f, fcprd[fcmom[q1, d] +
       fcmom[q2, d] + m, m1], l]], opts];

FourPoint[q_,
        fcfad[fcprd[fcmom[q1_, d___], m1_], fcprd[fcmom[q2_, d___], m2_],
            fcprd[fcmom[q3_, d___], m3_], fcprd[fcmom[q4_, d___], m4_]]*
          fcpa[fcli[l1_, d___], fcmom[qq1_, d___]]*
          fcpa[fcli[l2_, d___], fcmom[qq2_, d___]]*
          fcpa[fcli[l3_, d___], fcmom[qq3_, d___]]*
          fcpa[fcli[l4_, d___], fcmom[qq4_, d___]],
        opts___] /; (! FreeQ[qq1, q] && ! FreeQ[qq2, q] && !
              FreeQ[qq3, q] && ! FreeQ[qq4, q]) && (Head[qq1] == Plus ||
            Head[qq2] == Plus || Head[qq3] == Plus || Head[qq4] == Plus) :=
    FourPoint[q,
      Expand[fcfad[fcprd[fcmom[q1, d], m1], fcprd[fcmom[q2, d], m2],
            fcprd[fcmom[q3, d], m3], fcprd[fcmom[q4, d], m4]]*
            fcmomex[fcpa[fcli[l1, d], fcmom[qq1, d]]*
                fcpa[fcli[l2, d], fcmom[qq2, d]]*
                fcpa[fcli[l3, d], fcmom[qq3, d]]*
                fcpa[fcli[l4, d], fcmom[qq4, d]]]], opts];

FourPoint[q_, aa : HoldPattern[Times[___, (_[
  HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator[
  HighEnergyPhysics`FeynCalc`Momentum`Momentum[_, d___], _] ..]), a_]], opts___] /;
   !FreeQ[{a},q] && Head[a]===Times && !FreeQ[Head/@List@@a, Plus] :=
    FourPoint[q, Times[aa]//ExpandAll, opts];



(* Formula taken from the FeynCalc1.0 manual (don't know why Rolf didn't
   implement it himself...): *)

FourPoint[q_,
      fcfad[fcprd[fcmom[q_, d___], m0_], fcprd[fcmom[q_ + q1_, d___], m1_],
          fcprd[fcmom[q_ + q2_, d___], m2_],
          fcprd[fcmom[q_ + q3_, d___], m3_]]*
        fcpa[fcli[l1_, d___], fcmom[q_, d___]]*fcpa[fcli[l2_, d___],
           fcmom[q_, d___]]*fcpa[fcli[l3_, d___],  fcmom[q_, d___]]*
        fcpa[fcli[l4_, d___], fcmom[q_, d___]], opts___] := (pp[1] = p1;
      pp[2] = p2; pp[3] = p3; pp[4] = p4;
      I*Pi^2*((fcpa[fcli[l1], fcli[l2]]*fcpa[fcli[l3], fcli[l4]] +
                  fcpa[fcli[l1], fcli[l3]]*fcpa[fcli[l2], fcli[l4]] +
                  fcpa[fcli[l1], fcli[l4]]*fcpa[fcli[l2], fcli[l3]])*
              fcpave[0, 0, 0,
                0, {fcpa[fcmom[p1, d], fcmom[p1, d]],
                  fcpa[fcmom[p1 + p2, d], fcmom[p1 + p2, d]],
                  fcpa[fcmom[p2 + p3, d], fcmom[p2 + p3, d]],
                  fcpa[fcmom[p3, d], fcmom[p3, d]],
                  fcpa[fcmom[p2, d], fcmom[p2, d]],
                  fcpa[fcmom[p1 + p3, d], fcmom[p1 + p3, d]]}, {m0^2, m1^2,
                  m2^2, m3^2}] +
            Sum[(fcpa[fcli[l1], fcli[l2]]*fcpa[fcmom[pp[i]], fcli[l3]]*
                      fcpa[fcmom[pp[j]], fcli[l4]] +
                    fcpa[fcli[l1], fcli[l3]]*fcpa[fcmom[pp[i]], fcli[l2]]*
                      fcpa[fcmom[pp[j]], fcli[l4]] +
                    fcpa[fcli[l1], fcli[l4]]*fcpa[fcmom[pp[i]], fcli[l2]]*
                      fcpa[fcmom[pp[j]], fcli[l3]] +
                    fcpa[fcli[l2], fcli[l3]]*fcpa[fcmom[pp[i]], fcli[l1]]*
                      fcpa[fcmom[pp[j]], fcli[l4]] +
                    fcpa[fcli[l2], fcli[l4]]*fcpa[fcmom[pp[i]], fcli[l1]]*
                      fcpa[fcmom[pp[j]], fcli[l3]] +
                    fcpa[fcli[l3], fcli[l4]]*fcpa[fcmom[pp[i]], fcli[l1]]*
                      fcpa[fcmom[pp[j]], fcli[l2]])*
                fcpave[0, 0, i,
                  j, {fcpa[fcmom[p1, d], fcmom[p1, d]],
                    fcpa[fcmom[p1 + p2, d], fcmom[p1 + p2, d]],
                    fcpa[fcmom[p2 + p3, d], fcmom[p2 + p3, d]],
                    fcpa[fcmom[p3, d], fcmom[p3, d]],
                    fcpa[fcmom[p2, d], fcmom[p2, d]],
                    fcpa[fcmom[p1 + p3, d], fcmom[p1 + p3, d]]}, {m0^2, m1^2,
                    m2^2, m3^2}], {i, 3}, {j, 3}] +
            Sum[(fcpa[fcmom[pp[i]], fcli[l1]]*fcpa[fcmom[pp[j]], fcli[l3]]*
                    fcpa[fcmom[pp[k]], fcli[l1]]*
                    fcpa[fcmom[pp[l]], fcli[l3]])*
                fcpave[i, j, k,
                  l, {fcpa[fcmom[p1, d], fcmom[p1, d]],
                    fcpa[fcmom[p1 + p2, d], fcmom[p1 + p2, d]],
                    fcpa[fcmom[p2 + p3, d], fcmom[p2 + p3, d]],
                    fcpa[fcmom[p3, d], fcmom[p3, d]],
                    fcpa[fcmom[p2, d], fcmom[p2, d]],
                    fcpa[fcmom[p1 + p3, d], fcmom[p1 + p3, d]]}, {m0^2, m1^2,
                    m2^2, m3^2}], {i, 3}, {j, 3}, {k, 3}, {l, 3}]));



(* Tensor integrals of rank lower than four are simply handed to OneLoop: *)

FourPoint[q_, aa : HoldPattern[Times[___, (_[
  HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator[
  HighEnergyPhysics`FeynCalc`Momentum`Momentum[_, d___], _] ..]), a___,
  b:((HighEnergyPhysics`FeynCalc`Pair`Pair[
     _[_, d___], HighEnergyPhysics`FeynCalc`Momentum`Momentum[q_,
                      d___]]|HighEnergyPhysics`FeynCalc`Pair`Pair[
     HighEnergyPhysics`FeynCalc`Momentum`Momentum[q_,
                      d___], _[_, d___]]) ..), ___]], opts___] :=
    fconeloop[q, Times[aa], opts] /;
   Length[{b}] < 4 && FreeQ[{a},q];

FourPoint[q_, aa : HoldPattern[Times[___, (_[
  HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator[
  HighEnergyPhysics`FeynCalc`Momentum`Momentum[_, d___], _] ..]), a___]], opts___] /;
   FreeQ[{a},q] :=
    fconeloop[q, Times[aa], opts];

FourPoint[q_, aa : HoldPattern[_[
  HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator[
  HighEnergyPhysics`FeynCalc`Momentum`Momentum[_, d___], _] ..]], opts___] :=
    fconeloop[q, Times[aa], opts];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Ordering gamma matrices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



gaso[fcdiga[fcli[mu_, r1___], rr1___], fcdiga[fcli[nu_, r2___], rr2___],
      opts___] := (orderfkt = (OrderingFunction /. Flatten[{opts}] /.
            Options[GammaSort]);
      If[orderfkt[{fcdiga[fcli[mu, r1], rr1], fcdiga[fcli[nu, r2], rr2]}],
        tl[fcdiga[fcli[mu, r1], rr1], fcdiga[fcli[nu, r2], rr2]],
        tl[fcdiga[fcli[nu, r2], rr2], fcdiga[fcli[mu, r1], rr1]] -
          2*fcpa[fcli[mu, r1], fcli[nu, r2]]]);
par[x_, i_] := (pp = Cases[x, fcdiga[__], Infinity, Heads -> True];
      If[pp === 0, 0, pp[[i]]]);
sortrules[
      opts___] := (orderfkt = (OrderingFunction /. Flatten[{opts}] /.
            Options[GammaSort]); {tl[f___, fi_, red[fii_], fiii___] /;
            Head[fi] =!= Plus :> (iso = gaso[fi, fii, opts];
            fiso = iso /. fcpa[__] -> 0; giso = iso - fiso;
            If[Sort[{f, fi, fii}][[-1]] =!= fii,
              tl[f, red[par[fiso, 1]], par[fiso, 2], fiii] +
                tl[f, giso, fiii], tl[f, red[fi], fii, fiii]]),
        tl[a___, b_, c__] /; FreeQ[b, fcdiga] -> b*tl[a, c],
        tl[a__, b_, c___] /; FreeQ[b, fcdiga] -> b*tl[a, c],
        tl[a___, Plus[b_, bb__], c__] :> Plus @@ (tl[a, #, c] & /@ {b, bb}),
tl[f___, fi_,
              fii_] /; (! orderfkt[{f, fi, fii}] &&
                FreeQ[{f, fi, fii}, red]) -> tl[f, fi, red[fii]],
tl[red[f_], fi___, fii_] /; (! orderfkt[{f, fi, fii}]) ->
          tl[f, fi, red[fii]]});
gammasort[xx__, opts___Rule | opts___List] :=
    tl[xx] //. sortrules[opts] /. tl -> fcdot /. red[x_] -> x;
GammaSort[fcdot[exp_, ex__],
      opts___] := (exp1 =
        If[(Gamma5AntiCommute /. Flatten[{opts}] /. Options[GammaSort]) && !
              FreeQ[tl[exp, ex], fcdiga[fcli[5, ___], ___]],
          pos = Position[
              tl[exp, ex], _?(!
                      FreeQ[#, fcdiga[fcli[5, ___], __]] &), {1}]; (-1)^pos*
            Delete[fcdot @@ Join[{tl[exp, ex][[pos]]}, {exp}], pos + 1],
          tl[exp, ex]];
      exp2 = exp1 /. {tl[aa___, a_, b__, c_,
                  cc___] /; (!
                      FreeQ[a, fcdiga[fcli[5, ___], __] | gsort] && !
                      FreeQ[c, fcdiga[fcli[5, ___], __] | gsort] &&
                    FreeQ[{b}, fcdiga[fcli[5, ___], __] | gsort]) ->
              tl[aa, a, gsort[b], c, cc],
            tl[aa___, c_,
                  cc___] /; (! FreeQ[c, fcdiga[fcli[5, ___], __] | gsort] &&
                    FreeQ[{aa}, fcdiga[fcli[5, ___], __] | gsort]) ->
              tl[gsort[aa], c, cc],
            tl[aa___, c_,
                  cc___] /; (! FreeQ[c, fcdiga[fcli[5, ___], __] | gsort] &&
                    FreeQ[{cc}, fcdiga[fcli[5, ___], __] | gsort]) ->
              tl[aa, c, gsort[cc]],
            tl[a__] /; FreeQ[{a}, fcdiga[fcli[5, ___], __] | gsort] :>
              tl[gsort[a]]};
      fcdot @@ Flatten[{exp2} /. {gsort -> gammasort, tl -> Sequence}]);
GammaSort[Plus[x_, y__], opts___] := Plus @@ GammaSort /@ {x, y};
GammaSort[x_, opts___] /; FreeQ[x, fcdiga] := x;
GammaSort[fcdiga[x__], opts___] := fcdiga[x];
GammaSort[fcdtr[x_, op___], opts___] := fcdtr[GammaSort[x, opts], op];
GammaSort[Times[x_, y__], opts___] := Times @@ GammaSort /@ {x, y};



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Gamma traces with Tracer *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* FeynCalc's \
DiracTrace[DOT[DiracGamma[momentum[p,D],D],DiracGamma[LorentzIndex[mu,D],D],..\
.]], DiracSigma[DiracGamma[LorentzIndex[mu]],DiracGamma[LorentzIndex[nu]]] \
are substituted with Tracer's GammaTrace[l1,p,{mu},...], Sigma[{mu},{nu}] \
etc. *)

$TracerIndicesCounter = 0;
tin[opts___] := (++$TracerIndicesCounter;
      ToExpression[(TracerIndicesString /. Flatten[{opts}] /.
              Options[FCToTracer]) <> ToString[$TracerIndicesCounter]]);
FCToTracer[exp_,
      opts___] := (res1 =
        exp /. fcdtr[fcdot[a__], ___] :>
              trtr[tin[opts],
                fcdot[a] /. {fcdot[fcli[mu_, ___], fcli[nu_, ___]] ->
                        trdot[{fcli[mu]}, {fcli[nu]}], fcsig -> trsig,
                      fcdiga[fcli[mu_, ___], ___] -> {fcli[mu]},
                      Plus[aa___, b_, c___] /; FreeQ[b, fcdiga] ->
                        Plus[aa, trid*b, c],
                      fceps[aa__] -> (treps[aa] /.
                            fcli[mu_] -> {fcli[mu]})} /.
                  fcdot -> Sequence] /.
          fcdiga[p_, ___] /; (! FreeQ[p, fcmom]) :> (p /.
                fcmom[pp_, ___] -> pp));
TracerToFC[
      exp_] := (If[Tracer`Private`d === 4, curdim = Sequence[],
        curdim = SpaceDimensions];
      trsp /@ Table[trl[i], {i, $TracerIndicesCounter}];
      res2 = exp /.
          Table[ToExpression[(TracerIndicesString /. Flatten[{opts}] /.
                      Options[FCToTracer]) <> ToString[i]] ->
              trl[i], {i, $TracerIndicesCounter}];
      If[! FreeQ[res2, trtr], Print["Expression not traced"]; Abort[]];
      res2 /. {fcmom[p_] -> fcmom[p, curdim], {fcli[mu_]} -> fcli[mu],
          trdot -> fcpa, trsig :> fcsig,
          treps[a__] :> (fceps[a] /. {fcli[mu_]} -> fcli[mu, curdim])});



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Discarding higher orders *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*Commented out 16/9-2002. Will look at DOT instead*)
(*SetAttributes[ditchmom, NumericFunction];*)
DiscardOrders[am_,
      opts___] := (spf = (HighEnergyPhysics`Phi`Objects`ScalarProductForm /.
              Flatten[{opts}] /. Options[DiscardOrders]);
      Cancel[ExpandAll[
              ditchmom[]^2*am /.
                        If[(DiscardMomenta /. Flatten[{opts}] /.
                              Options[DiscardOrders]), {ParticleMass[a__] ->
                              ditchmom[]*ParticleMass[a, temprec],
                            MandelstamS -> ditchmom[]^2*MandelstamS[temprec],
                            MandelstamT -> ditchmom[]^2*MandelstamT[temprec],
                            MandelstamU -> ditchmom[]^2*MandelstamU[temprec],
                            fcmom[a__] /; FreeQ[{a}, fcpol] ->
                              ditchmom[]*fcmom[a, temprec]}, {}] /. (Rule[
                              at : # /; FreeQ[at, temprec],
                              ditchmom[]*
                                at] & /@ $ExpansionQuantities) //. {fcpa[
                          ditchmom[]*a_, b_] -> ditchmom[]*fcpa[a, b],
                      spf[ditchmom[]*a_, b_] -> ditchmom[]*spf[a, b]} /.
                  ff_[temprec] -> ff /. temprec -> Sequence[]]] /.
          ditchmom[]^i_ /;
              i > (PerturbationOrder + 2 /. Flatten[{opts}] /.
                    Options[DiscardOrders]) -> 0 /. ditchmom[] -> 1);

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Evaluation using stored results *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

eliminateDoubles[s_String] :=
    Block[{str},
      str = FixedPoint[
          StringReplace[#,
              Evaluate[$PathnameSeparator <> $PathnameSeparator] ->
                Evaluate[$PathnameSeparator]] &, s];
      If[StringMatchQ[str, "*" <> $PathnameSeparator], StringDrop[str, -1],
        str]];


SetAttributes[CheckF, HoldFirst];

CheckF[ex_, fi_, opts : ((_Rule | {___Rule}) ...)] :=
    Block[{dir, file, finex, fs, ns},

      If[StringQ[fi] =!= True, Message[CheckF::nostring, fi];
        Return[]];

      Which[
         StringMatchQ[fi,"*.Gen"]===True||StringMatchQ[fi,"*.Mod"]===True,
           dir = eliminateDoubles[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory <>
	       $PathnameSeparator <>
	       "Phi" <> $PathnameSeparator <> "CouplingVectors"],
          StringMatchQ[fi,"*.Fac"]===True || StringMatchQ[fi,"*.Mass"]===True,
           dir = eliminateDoubles[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory <>
	       $PathnameSeparator <>
	       "Phi" <> $PathnameSeparator <> "Factors"],
	    True,
          dir = (Directory /. Flatten[{opts}] /. Options[CheckF])
      ];

      Which[

        (*File name given with full path*)
        DirectoryName[fi] =!= "",
        If[FileType[DirectoryName[fi]] === Directory, file = fi],

        (*Directory specified ok*)
        FileType[dir] === Directory,
        file = eliminateDoubles[dir <> $PathnameSeparator <> fi],

        (*Directory specified not ok, try Directory[]*)

        FileType[eliminateDoubles[Directory[] <> $PathnameSeparator <> dir]] ===
           Directory,
        file = eliminateDoubles[
              Directory[] <> $PathnameSeparator <> dir <> $PathnameSeparator <>
                 fi,

              True, (Message[CheckF::baddir, dir]; Return[])];

        ];

      VerbosePrint[1, "Using file name " <> file];

      fs=(ForceSave/.Flatten[{opts}]/.Options[CheckF]);
      ns=(NoSave/.Flatten[{opts}]/.Options[CheckF]);

      If[FileType[file] === None || fs === True,
      If[FileType[file] === None,
        VerbosePrint[1, "File does not exist, evaluating"],
        If[fs,VerbosePrint[1, "File exists, force evaluating"]]];
        finex = Evaluate[ReleaseHold[ex]];
        If[ns,
          VerbosePrint[1, "NoSave set to True, will evaluate but not save"],
          VerbosePrint[1, "Saving"];
          Put[finex, file]],
      VerbosePrint[1, "File exists, loading"];
        finex = Get[file]
      ];

      finex

      ];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Characteristic polynomial and Cayley-Hamilton*)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*Se Karakteristisk polynomium og Newtonformler i H.A. Nielsen*)

CharacteristicCoefficient[a_, opts___Rule][i_] /;
      HighEnergyPhysics`Phi`Objects`Private`gaugedimcheck[
          CharacteristicCoefficient, opts, a] === i := 
					(-1)^(HighEnergyPhysics`Phi`Objects`Private`gaugedimcheck[
                CharacteristicCoefficient, opts, a]) (*Bug fixed 19/2-2002*);

CharacteristicCoefficient[a_, opts___Rule][i_] :=
    Block[{n =
          HighEnergyPhysics`Phi`Objects`Private`gaugedimcheck[
            CharacteristicCoefficient, opts, a]},
      1/(i - n)Sum[
          UTrace[NMPower[a, k]]CharacteristicCoefficient[a, opts][i + k], {k,
            1, n - i}]];

CayleyHamilton[m__, opts___Rule] :=
    CayleyHamilton[m, opts] =
      Block[{n = HighEnergyPhysics`Phi`Objects`Private`gaugedimcheck[
             CharacteristicCoefficient, opts, a], coms, len = Length[{m}],
             ch, el, submat},
        If[len =!= n || (Eliminate /. {opts} /. Options[CayleyHamilton]) =!=
              True, If[len =!= n,
            Message[CayleyHamilton::baddim]];
          Sum[CharacteristicCoefficient[UMatrix[b], opts][i] NMPower[
                          UMatrix[b], i], {i, 0, n}] /.
                    UMatrix[b] -> Plus[m] // CycleUTraces // NMExpand //
              Expand // CycleUTraces,
          Do[coms = fccombs[{m}, k];
            ch[k] = Sum[
                  Sum[CharacteristicCoefficient[UMatrix[b], opts][i]NMPower[
                                UMatrix[b], i], {i, 0, n}] /.
                          UMatrix[b] -> (Plus @@ coms[[l]]) // CycleUTraces //
                       NMExpand // Expand, {l, Length[coms]}] // CycleUTraces;
             VerbosePrint[2, "Doing ", k, "-term Cayley-Hamilton"];
            el[k] = ch[k] -
                  If[k === 1, 0,
                    Plus @@
                      Table[submat =
                          NM[Sequence @@
                              Table[{m}[[1]], {dum, 1, len - sp + 1}],
                            Sequence @@ Drop[{m}, len - sp + 1]];
                        VerbosePrint[2, "Eliminating ", submat,
                          " with coefficients", " ",
                          Coefficient[ch[k], submat], " ",
                          Coefficient[el[sp], submat]];
                        Coefficient[ch[k], submat]/
                            Coefficient[el[sp], submat]el[sp], {sp, 1,
                          k - 1}]] // Expand, {k, 1, len}]; el[len]]];

(*Support function for calhamSort*)
(*Count number of adjacent Lorentz vectors*)
licount = (Count[# /. UTrace1 -> tr /. Power -> NMPower /.
           NM -> nm //. 
           {nm[a___, l_?((!FreeQ[#, fcli[__]] && FreeQ[#, _nm]) &),
              ll_?((!FreeQ[#, fcli[__]] && FreeQ[#, _nm]) &), b___] :>
           nm[a, pp[Unique[pp]], b] /;
              Cases[l, fcli[__], Infinity] === Cases[ll, fcli[__], Infinity],
           nm[l_?((!FreeQ[#, fcli[__]] && FreeQ[#, _nm]) &), a__,
              ll_?((!FreeQ[#, fcli[__]] && FreeQ[#, _nm]) &)] :>
           nm[a, pp[Unique[pp]]] /;
              Cases[l, fcli[__], Infinity] ===
              Cases[ll, fcli[__], Infinity]}, _pp, Infinity] &);

(*Function to sort matrices to find left-hand side*)
calhamSort = Block[{tmp = # . UTrace1 -> tr},
               (Count[#1, _tr, Infinity] < Count[#2, _tr, Infinity] ||
               Count[#1, _tr, Infinity] === Count[#2, _tr, Infinity] &&
               licount[#1] < licount[#2] ||
               Count[#1, _tr, Infinity] === Count[#2, _tr, Infinity] &&
               licount[#1] == licount[#2] &&
               LeafCount[#1] > LeafCount[#2]
               ) ]&;

CayleyHamiltonRules[mats_List, opts___Rule] :=
Block[{(*calham, fac, scalham, rightside, a, len, i, tr, submats, j,
calhamrules, subres*)},
   calhamrules = {};
      VerbosePrint[3, Length[mats], " sets of matrices"];
      Do[VerbosePrint[2, j];
      len = Length[mats[[j]]];
        submats = Table[UMatrix[a[i]], {i, 1, len - 1}];
        calham = (subres=(UTrace[
                      NM[CayleyHamilton[
		        If[j===1,
			VerbosePrint[2, "Calling CayleyHamilton on ", submats,
			             " (first time only)"]];
		        Sequence @@ submats,
                          Sequence @@
                            OptionsSelect[CayleyHamilton, Eliminate -> True,
                              opts, Options[CayleyHamiltonRules]]],
                        UMatrix[a[len]]]] //
	(VerbosePrint[3, "Expanding"];#)& //
	NMExpand // Expand // CycleUTraces) /.
	(VerbosePrint[3, "Substituting matrices"];
	(Rule @@ #) & /@
                  Transpose[{Append[submats, UMatrix[a[len]]], mats[[j]]}]) /.
    (*Get scalars out of UTrace1*)Pattern -> pat /. pat -> Pattern)//
	(VerbosePrint[3, "Reducing ", subres];#)& //
             If[(UReduce /. {opts} /. Options[CayleyHamiltonRules])=!=False,
						   VerbosePrint[1, "Doing UReduce"];
							 UReduce[#, Sequence@@OptionsSelect[UReduce,
               Options[CayleyHamiltonRules],opts]] // CommutatorReduce // CycleUTraces,
							If[ (CommutatorReduce/.{opts}/.Options[CayleyHamiltonRules])=!=False,
                                               # //NMExpand // CommutatorReduce // CycleUTraces, #]]&;
        VerbosePrint[2, "Finding left-hand side of ", calham];
	scalham =
          Sort[List @@ Expand[calham /. UTrace1 -> tr /.
                  Power -> NMPower], calhamSort ];
        fac = scalham[[1]] /. _tr -> 1;
        rightside = (-(Plus @@ Drop[scalham/fac, 1]) /.
              Pattern -> (({##}[[1]]) &));
        leftside = Cancel[scalham[[1]]/fac];
        VerbosePrint[2, "Left-hand side is ", leftside];
        calhamrules =
          Append[calhamrules, leftside -> rightside], {j, 1, Length[mats]}];

          If[(CommutatorReduce/.{opts}/.Options[CayleyHamiltonRules])=!=False,
          calhamrules /. tr -> UTrace // CommutatorReduce,
          calhamrules /. tr -> UTrace]];


CayleyHamiltonTrick[exp_, opts___Rule] := Block[{ruls},
  ruls=
  CayleyHamiltonRules[VerbosePrint[1, "Building Cayley-Hamilton rules..."];
     UMatrices /. {opts} /. Options[CayleyHamiltonTrick],
		   Sequence@@OptionsSelect[CayleyHamiltonRules, opts, Options[CayleyHamiltonTrick]]];
  VerbosePrint[1, "Applying rules"];
  exp /. ruls];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Reduction of lagrangians *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Here we deal symbolically with SMM[x] and MM[x], which should thus not
   be expanded *)
SetOptions[MM, fcexpt -> False];
SetOptions[SMM, fcexpt -> False];
SetOptions[CovariantFieldDerivative, fcexpt -> False];

(* With no scalar and pseudo-scalar sources, \[Chi] is self-adjoined: *)

idRules = {nm[ff___, SMM[x_], Adjoint[SMM[x_]], ll___] -> nm[ff, ll],
      nm[ff___, Adjoint[SMM[x_]], SMM[x_], ll___] -> nm[ff, ll],
      nm[ff___, MM[x_], Adjoint[MM[x_]], ll___] -> nm[ff, ll],
      nm[ff___, Adjoint[MM[x_]], MM[x_], ll___] -> nm[ff, ll],
      nm[ff___, SMM[x_], Adjoint[MM[x_]], ll___] -> nm[ff, Adjoint[SMM[x]], ll],
       nm[ff___, Adjoint[SMM[x_]], MM[x_], ll___] -> nm[ff, SMM[x], ll],
      nm[ff___, Adjoint[MM[x_]], SMM[x_], ll___] -> nm[ff, Adjoint[SMM[x]], ll],
       nm[ff___, MM[x_], Adjoint[SMM[x_]], ll___] -> nm[ff, SMM[x], ll],
      UTrace1[nm[SMM[x_], ff___, Adjoint[SMM[x_]]]] :> UTrace1[nm[ff]],
      UTrace1[nm[Adjoint[SMM[x_]], ff___, SMM[x_]]] :> UTrace1[nm[ff]],
      UTrace1[nm[MM[x_], ff___, Adjoint[MM[x_]]]] :> UTrace1[nm[ff]],
      UTrace1[nm[Adjoint[MM[x_]], ff___, MM[x_]]] :> UTrace1[nm[ff]],
      Trace1[nm[SMM[x_], ff___, Adjoint[MM[x_]]]] :>
        UTrace1[nm[ff, Adjoint[SMM[x]]]],
      Trace1[nm[MM[x_], ff___, Adjoint[SMM[x_]]]] :> UTrace1[nm[ff, SMM[x]]],
      UTrace1[nm[Adjoint[MM[x_]], ff___, SMM[x_]]] :>
        UTrace1[nm[ff, Adjoint[SMM[x]]]],
      UTrace1[nm[Adjoint[SMM[x_]], ff___, MM[x_]]] :> UTrace1[nm[ff, SMM[x]]]};


UIdTrick[expr_,opts___Rule] := expr /. NM -> nm /. idRules /. nm -> NM /.
     UTrace1[] :> (HighEnergyPhysics`Phi`Objects`Private`gaugedimcheck[UReduce,opts,expr]) ;

(* At least with the exponential representation these traces are 0. Follows also
   from det u =1. *)

traceRules0 = {UTrace1[
          NM[SMM[x_], Adjoint[FieldDerivative[SMM[x_], _, fcli[_]]]]] -> 0,
	  UTrace1[
          NM[Adjoint[SMM[x_]], FieldDerivative[SMM[x_], _, fcli[_]]]] -> 0,
	  UTrace1[
          NM[Adjoint[FieldDerivative[SMM[x_], _, fcli[_]]], SMM[x_]]] -> 0,
	  UTrace1[
          NM[FieldDerivative[SMM[x_], _, fcli[_]], Adjoint[SMM[x_]]]] -> 0,
	  UTrace1[
          NM[SMM[x_],
            Adjoint[CovariantFieldDerivative[SMM[x_], _, fcli[_]]]]] -> 0,
	    UTrace1[
          NM[Adjoint[SMM[x_]],
            CovariantFieldDerivative[SMM[x_], _, fcli[_]]]] -> 0,
      UTrace1[NM[
            Adjoint[CovariantFieldDerivative[SMM[x_], _, fcli[_]]],
            SMM[x_]]] -> 0,
      UTrace1[NM[CovariantFieldDerivative[SMM[x_], _, fcli[_]],
            Adjoint[SMM[x_]]]] -> 0,
      UTrace1[NM[
            FieldDerivative[FieldDerivative[SMM[x_], x_, fcli[li2_]],
               x_, fcli[li1_]], Adjoint[SMM[x_]]]] -> -UTrace1[
            NM[FieldDerivative[SMM[x], x, fcli[li1]],
              Adjoint[FieldDerivative[SMM[x], x, fcli[li2]]]]],
      UTrace1[NM[SMM[x_],
            Adjoint[FieldDerivative[
                FieldDerivative[SMM[x_], x_, fcli[li2_]], x_,
                fcli[li1_]]]]] -> -UTrace1[
            NM[FieldDerivative[SMM[x], x, fcli[li2]],
              Adjoint[FieldDerivative[SMM[x], x, fcli[li1]]]]]};

traceRules = Join[traceRules0, traceRules0 /. SMM -> MM];

UTraceTrick := ((# /. traceRules) &);

(* We will need to simplify using that total derivatives vanish: *)

uDagRul =
    NM[SMM[x_], Adjoint[FieldDerivative[
       FieldDerivative[SMM[x_], x_, fcli[li2_]], x_, fcli[li1_]]]] :>
    -(NM[FieldDerivative[SMM[x], x, fcli[li2]],
              Adjoint[FieldDerivative[SMM[x], x, fcli[li1]]]] +
            NM[FieldDerivative[
                FieldDerivative[SMM[x], x, fcli[li2]], x,
                fcli[li1]], Adjoint[SMM[x]]] +
            NM[FieldDerivative[SMM[x], x, fcli[li1]],
              Adjoint[FieldDerivative[SMM[x], x, fcli[li2]]]]) (*/;
      Sort[{li1,li2}]=!={li1,li2}*);

UDagRul = uDagRul /. SMM -> MM;

(* The derivative of the product of U and Adjoint[U] is 0. And the derived rules works
also for the covariant derivative.
That is, NM[CovariantFieldDerivative[U],Adjoint[U]] =
         -NM[U,Adjoint[CovariantFieldDerivative[U]]] *)

du[li1_][x_] = FieldDerivative[SMM[x], x, fcli[li1]];
dua[li1_][x_] = Adjoint[FieldDerivative[SMM[x], x, fcli[li1]]];


ddu[li1_][x_] = CovariantFieldDerivative[SMM[x], x, fcli[li1]];
ddua[li1_][x_] =
  Adjoint[CovariantFieldDerivative[SMM[x], x, fcli[li1]]];

uRules10 = {rul[nm[f___, duu[li1_][x_], Adjoint[SMM[x_]], g___],
            cond[-nm[f, SMM[x], duaa[li1][x], g], (mq[pt[{dum, f}, -1],
            SMM[_] | Adjoint[SMM[_]]] || mq[pt[{dum, dum, f}, {-2, -1}],
	    {duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]],
             duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]]}] ||
             eq[{f}, {}] && (mq[pt[{dum, g}, -1],  SMM[_] | Adjoint[SMM[_]]] ||
             mq[pt[{dum, dum, g}, {-2, -1}], {duu[_][_] |
             duaa[_][_] | SMM[_] | Adjoint[SMM[_]], duu[_][_] | duaa[_][_] | SMM[_] |
             Adjoint[SMM[_]]}]))]], rul[nm[f___, duaa[li1_][x_], SMM[x_], g___],
             cond[-nm[f, Adjoint[SMM[x]], duu[li1][x], g], (mq[pt[{dum, f}, -1],
             SMM[_] | Adjoint[SMM[_]]] || mq[pt[{dum, dum, f}, {-2, -1}],
	     {duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]],
             duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]]}] || eq[{f}, {}] &&
	     (mq[pt[{dum, g}, -1], SMM[_] | Adjoint[SMM[_]]] ||
              mq[pt[{dum, dum, g}, {-2, -1}], {duu[_][_] | duaa[_][_] | SMM[_] |
	      Adjoint[SMM[_]], duu[_][_] | duaa[_][_] | SMM[_] |
              Adjoint[SMM[_]]}]))]]} /. {{duu -> du, duaa -> dua},
	      {duu -> ddu, duaa -> ddua}} /.
              rul -> RuleDelayed /. cond -> Condition /. {mq -> MatchQ,
            eq -> SameQ} /. pt -> Part // Flatten;

uRules1 = Join[uRules10, uRules10 /. SMM -> MM];

applyuRules1 = (# /. NM -> nm /. uRules1 /. nm -> NM) &;
UOrder = (# /. NM -> nm /. (uRules1 /. Condition -> cc /. cc[a_, b_] -> a) /. nm -> NM) &;

uRules20 = {rul[nm[f___, SMM[x_], duaa[li1_][x_], g___],
            cond[-nm[f, duu[li1][x], Adjoint[SMM[x]], g],
	    (mq[pt[{g, dum}, 1], SMM[_] | Adjoint[SMM[_]]] ||
             mq[pt[{g, dum, dum}, {1, 2}], {duu[_][_] | duaa[_][_] |
             SMM[_] | Adjoint[SMM[_]], duu[_][_] | duaa[_][_] | SMM[_] |
	     Adjoint[SMM[_]]}] || eq[{g}, {}] && (mq[pt[{f, dum}, 1],
             SMM[_] | Adjoint[SMM[_]]] || mq[pt[{f, dum, dum}, {1, 2}], {duu[_][_] |
             duaa[_][_] | SMM[_] | Adjoint[SMM[_]], duu[_][_] | duaa[_][_] | SMM[_] |
             Adjoint[SMM[_]]}]))]], rul[nm[f___, Adjoint[SMM[x_]], duu[li1_][x_], g___],
             cond[-nm[f, duaa[li1][x], SMM[x], g], (mq[pt[{g, dum}, 1], SMM[_] | Adjoint[SMM[_]]] ||
             mq[pt[{g, dum, dum}, {1, 2}], {duu[_][_] | duaa[_][_] |
             SMM[_] | Adjoint[SMM[_]], duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]]}] ||
             eq[{g}, {}] && (mq[pt[{f, dum}, 1], SMM[_] | Adjoint[SMM[_]]] ||
             mq[pt[{f, dum, dum}, {1, 2}], {duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]],
             duu[_][_] | duaa[_][_] | SMM[_] | Adjoint[SMM[_]]}]))]]} /.
	     {{duu -> du,  duaa -> dua}, {duu -> ddu, duaa -> ddua}} /.
              rul -> RuleDelayed /. cond -> Condition /.
	      {mq -> MatchQ, eq -> SameQ} /. pt -> Part // Flatten;

uRules2 = Join[uRules20, uRules20 /. SMM -> MM];

applyuRules2 = (# /. NM -> nm /. uRules2 /. nm -> NM) &;
UOrder1 = (# /. NM -> nm /. (uRules2 /. Condition -> cc /. cc[a_, b_] -> a) /. nm -> NM) &;

applyuRules12 = (# /. NM -> nm /. uRules1 /. uRules2 /. nm -> NM) &;
applyuRules12n = (# /. NM -> nm /. (uRules1 /. Condition -> cc /. cc[a_, b_] -> a) /.
                    (uRules2 /. Condition -> cc /. cc[a_, b_] -> a) /. nm -> NM) &;

applyuRules21 = (# /. NM -> nm /. uRules2 /. uRules1 /. nm -> NM) &;
applyuRules21n = (# /. NM -> nm /. (uRules2 /. Condition -> cc /. cc[a_, b_] -> a) /.
                    (uRules1 /. Condition -> cc /. cc[a_, b_] -> a) /. nm -> NM) &;

(* Use this to get u's side by side: *)

usurules = {(NM[f___, SMM[x_] | Adjoint[SMM[x_]], SMM[x_] | Adjoint[SMM[x_]],
              l___] | NM[f___, MM[x_] | Adjoint[MM[x_]],
              MM[x_] | Adjoint[MM[x_]], l___]) ->
        NM[f, usu, l], (UTrace1[NM[SMM[x_] | Adjoint[SMM[x_]], f__,
                SMM[x_] | Adjoint[SMM[x_]]]] |
            UTrace1[NM[MM[x_] | Adjoint[MM[x_]], f__,
                MM[x_] | Adjoint[MM[x_]]]]) -> NM[f, usu]};

UPair[exp_, opts___Rule] := (exp /. (a : HoldPattern[NM[__]]) :> (tmpa = a // applyuRules1;
                     If[Count[a /. usurules, usu, Infinity] >
                        Count[tmpa /. usurules, usu, Infinity], a,
                      tmpa]) /. (a : HoldPattern[NM[__]]) :> (tmpa =
                    a // applyuRules2;
                  If[Count[a /. usurules, usu, Infinity] >
                      Count[tmpa /. usurules, usu, Infinity], a,
                    tmpa]) /. (a : HoldPattern[NM[__]]) :> (tmpa =
                  a // applyuRules12;
                If[Count[a /. usurules, usu, Infinity] >
                    Count[tmpa /. usurules, usu, Infinity], a, tmpa]) /. (a :
                HoldPattern[NM[__]]) :> (tmpa = a // applyuRules21;
              If[Count[a /. usurules, usu, Infinity] >
                  Count[tmpa /. usurules, usu, Infinity], a, tmpa]) //
        UIdTrick[#,opts]&);

UDrop[exp_, opts___Rule] := Block[{(*tmpexp,tmpexp1,res*)},
  tmpexp = exp // UOrder //  UIdTrick[#,opts]& // CycleUTraces;
  tmpexp1 = exp /. UTrace1 -> (UTrace[RotateLeft[#]] &) // UOrder //  UIdTrick[#,opts]& // CycleUTraces;
  res={exp,tmpexp,tmpexp1}[[Ordering[LeafCount/@{exp,tmpexp,tmpexp1}][[1]]]];
  tmpexp = res // UOrder1 //  UIdTrick[#,opts]& // CycleUTraces;
  tmpexp1 = res /. UTrace1 -> (UTrace[RotateLeft[#]] &) // UOrder1 //  UIdTrick[#,opts]& // CycleUTraces;
  res={exp,tmpexp,tmpexp1}[[Ordering[LeafCount/@{exp,tmpexp,tmpexp1}][[1]]]];
  tmpexp = res // applyuRules12n //  UIdTrick[#,opts]& // CycleUTraces;
  tmpexp1 = res /. UTrace1 -> (UTrace[RotateLeft[#]] &) // applyuRules12n //  UIdTrick[#,opts]& // CycleUTraces;
  res={exp,tmpexp,tmpexp1}[[Ordering[LeafCount/@{exp,tmpexp,tmpexp1}][[1]]]];
  tmpexp = res // applyuRules21n //  UIdTrick[#,opts]& // CycleUTraces;
  tmpexp1 = res /. UTrace1 -> (UTrace[RotateLeft[#]] &) // applyuRules21n //  UIdTrick[#,opts]& // CycleUTraces;
  res={exp,tmpexp,tmpexp1}[[Ordering[LeafCount/@{exp,tmpexp,tmpexp1}][[1]]]];
  res];


(*SU(2) rules for tr(d_mu U d_mu U^(+) d_nu U d_nu U^(+)) - not Cayley-Hamilton, but follow from
writing out the exponentials and using sigma.dphi sigma.dphi = dphi.dphi in SU(2) (no SU2D)
- or from inserting U U^(+) and decomposing the traceless matrices dU U^(+) in Pauli matrices*)
(*See Dobado,Gomez-Nicola,Maroto,Pelaez p. 149*)

SUNURules[2] := If[(fcexpt/.Options[MM])===False && (fcexpt/.Options[SMM])===False,
     {UTrace1[
        NM[Adjoint[CovariantFieldDerivative[MM[x_], x_, fcli[mu1_]]],
          CovariantFieldDerivative[MM[x_], x_, fcli[mu2_]],
          Adjoint[CovariantFieldDerivative[MM[x_], x_, fcli[mu2_]]],
          CovariantFieldDerivative[MM[x_], x_, fcli[mu1_]]]] ->
      1/2*UTrace[
          NM[Adjoint[
              CovariantFieldDerivative[MM[x], x, fcli[mu1]]],
            CovariantFieldDerivative[MM[x], x, fcli[mu1]]]]UTrace[
          NM[Adjoint[
              CovariantFieldDerivative[MM[x], x, fcli[mu2]]],
            CovariantFieldDerivative[MM[x], x, fcli[mu2]]]],
    UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x_], x_, fcli[mu1_]]],
          CovariantFieldDerivative[MM[x_], x_, fcli[mu1_]],
          Adjoint[CovariantFieldDerivative[MM[x_], x_, fcli[mu2_]]],
          CovariantFieldDerivative[MM[x_], x_, fcli[mu2_]]]] ->
      1/2*UTrace[
          NM[Adjoint[
              CovariantFieldDerivative[MM[x], x, fcli[mu1]]],
            CovariantFieldDerivative[MM[x], x, fcli[mu1]]]]UTrace[
          NM[Adjoint[
              CovariantFieldDerivative[MM[x], x, fcli[mu2]]],
            CovariantFieldDerivative[MM[x], x, fcli[mu2]]]]}, {}];


SUNURules[_] := {};


(*Going from u to U *)

(*WRONG?!*)
(*SU(2) rules like above*)
(* diffBigURules[2] :=
  {NM[CovariantFieldDerivative[SMM[x_], x_, fcli[mu1_]],
	    Adjoint[CovariantFieldDerivative[SMM[x_], x_, fcli[mu1_]]]] :>
	 1/4 NM[CovariantFieldDerivative[MM[x], x, fcli[mu1]],
	    Adjoint[CovariantFieldDerivative[MM[x], x, fcli[mu1]]]],
	 UTrace1[NM[Adjoint[CovariantFieldDerivative[SMM[x_], x_, fcli[mu1_]]],
	            m__,CovariantFieldDerivative[SMM[x_], x_, fcli[mu1_]]]] :>
	 1/4 UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x], x, fcli[mu1]]],
	    m,CovariantFieldDerivative[MM[x], x, fcli[mu1]]]],
	NM[Adjoint[CovariantFieldDerivative[SMM[x_], x_, fcli[mu1_]]],
	   CovariantFieldDerivative[SMM[x_], x_, fcli[mu1_]]] :>
	 1/4 NM[Adjoint[CovariantFieldDerivative[MM[x], x, fcli[mu1]]],
	        CovariantFieldDerivative[MM[x], x, fcli[mu1]]],
	 UTrace1[NM[CovariantFieldDerivative[SMM[x_], x_, fcli[mu1_]],
	 m__,Adjoint[CovariantFieldDerivative[SMM[x_], x_, fcli[mu1_]]]]] :>
	 1/4 UTrace1[NM[CovariantFieldDerivative[MM[x], x, fcli[mu1]],
	    m,Adjoint[CovariantFieldDerivative[MM[x], x, fcli[mu1]]]]]};	*)

diffBigURules[_] := {};


diffURules = {NM[FieldDerivative[SMM[x_], x_, fcli[li1_]], SMM[x_]] ->
      FieldDerivative[MM[x], x, fcli[li1]] -
        NM[SMM[x], FieldDerivative[SMM[x], x, fcli[li1]]],
    NM[Adjoint[FieldDerivative[SMM[x_], x_, fcli[li1_]]],
        Adjoint[SMM[x_]]] ->
      Adjoint[FieldDerivative[MM[x], x, fcli[li1]]] -
        NM[Adjoint[SMM[x]],
          Adjoint[FieldDerivative[SMM[x], x, fcli[li1]]]]};

bigURules = {nm[f___, SMM[x_], SMM[x_], l___] -> nm[f, MM[x], l],
      nm[f___, Adjoint[SMM[x_]], Adjoint[SMM[x_]], l___] ->
        nm[f, Adjoint[MM[x]], l],
				     nm[SMM[x_], m__, SMM[x_]] -> nm[MM[x], m],
             nm[Adjoint[SMM[x_]], m__, Adjoint[SMM[x_]]] ->
             nm[m, Adjoint[MM[x]]]};

SMMToMM[exp_,opts___Rule] := (max = Max[(Length /@ Cases[exp, _NM, Infinity])];
      FixedPoint[(# /. bigURules /. diffURules  /.
			                    diffBigURules[(fcsunn /. {opts} /.
			                    Options[UReduce])]// NMExpand) &,
          exp /. NM -> nm, max] /. nm -> NM);

(* Have tr(d_mu U d_mu U^(+) d_nu U d_nu U^(+)) replaced with
   tr(d_mu U^(+)  d_mu Ud_nu U^(+) d_nu U).
   Follows from writing in terms of u_mu's. If anyone should want to do higher order
   calculations than p^8, well change the 8 below accordingly :-)*)

ddURules=Table[(ex=(cut[UTrace1[
                  nm@@Table[
                      seq[idd[
                          CovariantFieldDerivative[MM[pat[x,_]],pat[x,_],
                            fcli[mu[i]]]],
                        adj[CovariantFieldDerivative[MM[pat[x,_]],pat[x,_],
                            fcli[mu[i+1]]]]],{i,1,n,2}]]]/.seq->Sequence);
          rull[(ex/.mu:>(pat[ToExpression["mu"<>ToString[#]],
                            Blank[]]&)/.pat->Pattern/.{idd->
                    Identity,adj->Adjoint}),
            condd[(ex/.mu:>(ToExpression[
                              "mu"<>ToString[#]]&)/.{idd->Adjoint,
                      adj->Identity}/.pat[xx_,yy_]->x),
              usq[sor[{mu1,mu2}],{mu1,mu2}]]]),{n,2,8,2}]/.{nm->NM,
        rull->RuleDelayed,condd->Condition,usq->SameQ,
        sor->Sort, cut->CycleUTraces};

ddURules1=
    Table[(ex=(cut[UTrace1[
                  nm@@Table[
                      seq[idd[
                          CovariantFieldDerivative[MM[pat[x,_]],pat[x,_],
                            fcli[mu[i]]]],
                        adj[CovariantFieldDerivative[MM[pat[x,_]],pat[x,_],
                            fcli[mu[i+1]]]]],{i,1,n,2}]]]/.seq->Sequence);
          rull[(ex/.mu:>(pat[ToExpression["mu"<>ToString[#]],
                            Blank[]]&)/.pat->Pattern/.{idd->Adjoint,
                  adj->Identity}),
            condd[(ex/.mu:>(ToExpression[
                              "mu"<>ToString[#]]&)/.{idd->Identity,
                      adj->Adjoint}/.pat[xx_,yy_]->x),
              usq[sor[{mu1,mu2}],{mu1,mu2}]]]),{n,2,8,2}]/.{nm->NM,
        rull->RuleDelayed,condd->Condition,usq->SameQ,
        sor->Sort, cut->CycleUTraces};

UReduce[exp_, opts___Rule] := Block[{res,opsMM,opsSMM,end},
 opsMM=Options[MM];opsSMM=Options[SMM];
 SetOptions[MM, fcexpt -> False];
 SetOptions[SMM, fcexpt -> False];
 res=If[(SMMToMM /. {opts} /. Options[UReduce]) =!= True,
(*Added inner UPair in order to force cancellation in SU(3) CayleyHamiltonRules*) 
FixedPoint[CycleUTraces[UPair[
       UTraceTrick[UIdTrick[UPair[NMExpand[# /. uDagRul /. UDagRul  /. ddURules /. ddURules1]],opts] /.
			                    SUNURules[(fcsunn /. {opts} /.
			                    Options[UReduce])]],opts]]&,
      exp, 10],
 FixedPoint[CycleUTraces[SMMToMM[UPair[
       UTraceTrick[UIdTrick[UPair[NMExpand[# /. uDagRul /. UDagRul /. ddURules /.  ddURules1]],opts] /.
			                    SUNURules[(fcsunn /. {opts} /.
			                    Options[UReduce])]],opts],opts]]&,
      exp, 10]];
 end=If[(FullReduce /. {opts} /. Options[UReduce]) === True,
 FixedPoint[CycleUTraces[UDrop[UTraceTrick[UIdTrick[NMExpand[# /. uDagRul /. UDagRul],opts]],opts]]&,
      res, 10],res];
  Options[MM]=opsMM;Options[SMM]=opsSMM;
  end];



(* Total derivatives vanish upon integration *)

(* Should be cleaned up and generalized to traces of products
   involving covariant derivatives *)

(*Disables checks for differentation orders all together*)
(*surfaceRules[0]:=(surfaceRules[1]/. Condition -> cond /.
cond[a_, __] -> (*a*) Condition[a,usq[fq[dd, ufis, Heads -> True],True]]);

surfaceRules1[0]:=(surfaceRules1[1]/. Condition -> cond /.
cond[a_, __] -> (*a*) Condition[a,usq[fq[dd, ufis, Heads -> True],True]]);*)

surfaceRules[0]:=(surfaceRules[1] /. sr0 -> True);
surfaceRules1[0]:=(surfaceRules1[1] /. sr0 -> True);

surfaceRules[n_]:={
  surdum + nm[f___,FieldDerivative[dd_,x_,fcli[li1_]],r___] :>
         surdum + idd[-nm[fdr[nm[f,r],x,fcli[li1]],dd]]/;
        (FreeQ[dd, ufis, Heads->True] =!=True)&&
				(sr0 || Length[{f,r}]>0&&((m1=Max[Depth/@
              Union[Cases[{f,r},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,Heads->True]]])<
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1)),

   surdum + nm[uo___,NM[f___,FieldDerivative[dd_,x_,fcli[li1_]],r___],
        ou___] :> surdum + idd[nm[
          nm[uo,ou,-nm[NM[fdr[NM[f],x,fcli[li1]],dd,r]]-
                    nm[NM[f,dd,fdr[NM[r],x,fcli[li1]]]]]-
          nm[fdr[nm[uo,ou],x,fcli[li1]],NM[f,dd,r]]]]/;
        (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1=Max[Depth/@
              Union[Cases[{f,r,ou},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,
                  Heads->True]]])<
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,
                    Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

    surdum + nm[uo___,
          utr[NM[f___,FieldDerivative[dd_,x_,
					HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[li1_]],
              r___]],ou___]:>
      surdum + idd[-nm[uo,ou,utr[
                NM[fdr[NM[f],x,fcli[li1]],dd,r]+
                 NM[f,dd,fdr[NM[r],x,fcli[li1]]]]]-
      nm[fdr[nm[uo,ou],x,fcli[li1]],
                utr[NM[f,dd,r]]]]/;
       ( (FreeQ[dd, ufis, Heads->True] =!=True))&& (sr0 || (m1=Max[Depth/@
              Union[Cases[{f,r,ou},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,
                  Heads->True]]])<
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,
                    Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

    surdum + nm[uo___,NM[f___,Adjoint[FieldDerivative[dd_,x_,fcli[li1_]]],r___],
        ou___] :>
        surdum + idd[nm[
          nm[uo,ou,-NM[fdr[NM[f],x,fcli[li1]],Adjoint[dd],r]-
                    NM[f,Adjoint[dd],fdr[NM[r],x,fcli[li1]]]]-
          nm[fdr[nm[uo,ou],x,fcli[li1]],NM[f,Adjoint[dd],r]]]]/;
        (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1=Max[Depth/@
              Union[Cases[{f,r,ou},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,
                  Heads->True]]])<
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,
                    Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

		surdum +
        nm[uo___,utr[NM[f___,Adjoint[FieldDerivative[dd_,x_,
							HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[li1_]]],
              r___]],ou___]:>
       surdum + idd[-nm[uo,ou,
			    utr[NM[fdr[NM[f],x,fcli[li1]],Adjoint[dd],r]
                +NM[f,Adjoint[dd],fdr[NM[r],x,fcli[li1]]]]]-
        nm[fdr[nm[uo,ou],x,fcli[li1]],
          utr[NM[f,Adjoint[dd],r]]]]/;
        (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1=Max[Depth/@
              Union[Cases[{f,r,ou},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,
                  Heads->True]]])<=
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,
                    Heads->True]]])+n-1 || m1==-Infinity&&m2==-Infinity&&n<=1)};

surfaceRules1[n_]:={
  surdum + nm[f___,dd:(fcqf[fcpd[li1_,___], 
          ff_, ___][x_] | (IsoVector | UVector | UMatrix)[
          fcqf[fcpd[li1_,___], __]][x_] | 
      IsoDot[IsoVector[fcqf[fcpd[li1_,___], __]][x_], 
        IsoVector[UMatrix[UGenerator[___], ___], ___]]),r___] :>
         surdum + idd[-nm[fdr[nm[f,r],x,fcli[li1]],dd/.fcpd[__]->Sequence[]]]/;
        (FreeQ[dd, ufis, Heads->True] =!=True)&&
				(sr0 || Length[{f,r}]>0&&((m1=Max[Depth/@
              Union[Cases[{f,r},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,Heads->True]]])<
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1)),

   surdum + nm[uo___,NM[f___,dd:(fcqf[fcpd[li1_,___], 
          ff_, ___][x_] | (IsoVector | UVector | UMatrix)[
          fcqf[fcpd[li1_,___], __]][x_] | 
      IsoDot[IsoVector[fcqf[fcpd[li1_,___], __]][x_], 
        IsoVector[UMatrix[UGenerator[___], ___], ___]]),r___],
        ou___] :> surdum + idd[nm[
          nm[uo,ou,-nm[NM[fdr[NM[f],x,fcli[li1]],dd/.fcpd[__]->Sequence[],r]]-
                    nm[NM[f,dd,fdr[NM[r],x,fcli[li1]]]]]-
          nm[fdr[nm[uo,ou],x,fcli[li1]],NM[f,dd/.fcpd[__]->Sequence[],r]]]]/;
        (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1=Max[Depth/@
              Union[Cases[{f,r,ou},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,
                  Heads->True]]])<
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,
                    Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

    surdum + nm[uo___,
          utr[NM[f___,dd:(fcqf[fcpd[li1_,___], 
          ff_, ___][x_] | (IsoVector | UVector | UMatrix)[
          fcqf[fcpd[li1_,___], __]][x_] | 
      IsoDot[IsoVector[fcqf[fcpd[li1_,___], __]][x_], 
        IsoVector[UMatrix[UGenerator[___], ___], ___]]),
              r___]],ou___]:>
      surdum + idd[-nm[uo,ou,utr[
                NM[fdr[NM[f],x,fcli[li1]],dd/.fcpd[__]->Sequence[],r]+
                 NM[f,dd/.fcpd[__]->Sequence[],fdr[NM[r],x,fcli[li1]]]]]-
      nm[fdr[nm[uo,ou],x,fcli[li1]],
                utr[NM[f,dd/.fcpd[__]->Sequence[],r]]]]/;
        (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1=Max[Depth/@
              Union[Cases[{f,r,ou},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,
                  Heads->True]]])<
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,
                    Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

    surdum + nm[uo___,NM[f___,Adjoint[dd:(fcqf[fcpd[li1_,___], 
          ff_, ___][x_] | (IsoVector | UVector | UMatrix)[
          fcqf[fcpd[li1_,___], __]][x_] | 
      IsoDot[IsoVector[fcqf[fcpd[li1_,___], __]][x_], 
        IsoVector[UMatrix[UGenerator[___], ___], ___]])],r___],
        ou___] :>
        surdum + idd[nm[
          nm[uo,ou,-NM[fdr[NM[f],x,fcli[li1]],Adjoint[dd/.fcpd[__]->Sequence[]],r]-
                    NM[f,Adjoint[dd/.fcpd[__]->Sequence[]],fdr[NM[r],x,fcli[li1]]]]-
          nm[fdr[nm[uo,ou],x,fcli[li1]],NM[f,Adjoint[dd/.fcpd[__]->Sequence[]],r]]]]/;
        (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1=Max[Depth/@
              Union[Cases[{f,r,ou},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,
                  Heads->True]]])<
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,
                    Heads->True]]])+n || m1==-Infinity&&m2==-Infinity&&n<=1),

		surdum + 
        nm[uo___,utr[NM[f___,Adjoint[dd:(fcqf[fcpd[li1_,___], 
          ff_, ___][x_] | (IsoVector | UVector | UMatrix)[
          fcqf[fcpd[li1_,___], __]][x_] | 
      IsoDot[IsoVector[fcqf[fcpd[li1_,___], __]][x_], 
        IsoVector[UMatrix[UGenerator[___], ___], ___]])],
              r___]],ou___]:>
       surdum + idd[-nm[uo,ou,
			    utr[NM[fdr[NM[f],x,fcli[li1]],Adjoint[dd/.fcpd[__]->Sequence[]],r]
                +NM[f,Adjoint[dd/.fcpd[__]->Sequence[]],fdr[NM[r],x,fcli[li1]]]]]-
        nm[fdr[nm[uo,ou],x,fcli[li1]],
          utr[NM[f,Adjoint[dd/.fcpd[__]->Sequence[]],r]]]]/;
        (FreeQ[dd, ufis, Heads->True] =!=True)&& (sr0 || (m1=Max[Depth/@
              Union[Cases[{f,r,ou},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
	      _FieldDerivative,Infinity,
                  Heads->True]]])<=
          (m2=Max[Depth/@
                Union[Cases[{dd},_HighEnergyPhysics`FeynCalc`PartialD`PartialD|
		_FieldDerivative,Infinity,
                    Heads->True]]])+n-1 || m1==-Infinity&&m2==-Infinity&&n<=1)};

SurfaceReduce[expr_,opts___Rule]:=
Block[{n=DifferenceOrder/.{opts}/.Options[SurfaceReduce],re,res,qf,pd,r,x,isod},
  ufis=UFields/.{opts}/.Options[SurfaceReduce];
        re = Expand[surdum + expr] (*//.
	{fcqf[pd:(fcpd[_]..), r__?(FreeQ[#, {fcpd}, Heads -> True] &)][x_] :>
	FieldDerivative[qf[r][x], x, (fcli @@ #) & /@ {pd}],
	IsoDot[IsoVector[
        fcqf[pd : (fcpd[_] ..), r__?(FreeQ[#, {fcpd}, Heads -> True] &)]][x_],
     l_] :> FieldDerivative[
      isod[IsoVector[qf[r]][x], l], x, (fcli @@ #) & /@ {pd}] /;
    FreeQ[l, fcpd | FieldDerivative | CovariantFieldDerivative |
        CovariantNabla, Heads -> True]}*);

	res=surdum + Expand[NMExpand[If[Head[#]===Times,nm@@#,nm[#]]&/@re]] /. UTrace1[a_]^n_ :>
	  (Sequence@@Table[utr[a],{n}])  /. UTrace1->utr //.
	(surfaceRules1[n]/.sr0->False/.fq->FreeQ/.usq->UnsameQ) //.
      (surfaceRules[n]/.sr0->False/.fq->FreeQ/.usq->UnsameQ) /. idd -> Identity /. nm -> Times /. surdum -> 0/.
	      qf -> fcqf /.isod -> IsoDot /. fdr -> FieldDerivative /.
				utr -> UTrace/. FieldDerivative[_,fcli[_]]->0 //
            UTraceTrick];

(* Return to defaults *)
SetOptions[MM, Sequence@@tmpoptsmm];
SetOptions[SMM, Sequence@@tmpoptssmm];
SetOptions[CovariantFieldDerivative, Sequence@@tmpoptscdr];


(* (7.14) and (7.20) from Gasser and Leutwyler (1985) *)

gammaRule =
  FieldDerivative[UGamma[fcli[li1_],opts___Rule][x_], x_, fcli[li2_]] /;
  Sort[{li1,li2}] =!= {li1,li2} :>
     FieldDerivative[UGamma[fcli[li2],opts][x], x,
        fcli[li1]] +
      UCommutator[UGamma[fcli[li1],opts][x],
        UGamma[fcli[li2],opts][x]] -
      1/4 UCommutator[USmall[fcli[li1],Sequence@@OptionsSelect[USmall,opts]][x],
          USmall[fcli[li2],Sequence@@OptionsSelect[USmall,opts]][x]] -
      1/2 I NM[Adjoint[SMM[x,Sequence@@OptionsSelect[SMM,opts]]],
          FieldStrengthTensorFull[{li1},
            UGeneratorMatrixIsoDotFull[
             fcqf[Particle[
              LeftComponent[0,Sequence@@OptionsSelect[RightComponent,opts]]],
	      {li2}][x]], x, I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]],
		SMM[x,Sequence@@OptionsSelect[SMM,opts]]] -
      1/2 I NM[SMM[x,Sequence@@OptionsSelect[SMM,opts]],
          FieldStrengthTensorFull[{li1},
            UGeneratorMatrixIsoDotFull[
             fcqf[Particle[
              RightComponent[0,Sequence@@OptionsSelect[LeftComponent,opts]]],
	      {li2}][x]], x, I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]],
		Adjoint[SMM[x,Sequence@@OptionsSelect[SMM,opts]]]];


UGammaTrick[exp_] := exp /. gammaRule;


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Reduction using equations of motion *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

$EOMRules :=

{UTrace1[NM[Adjoint[MM[x_]], CovariantFieldDerivative[MM[x_], x_, fcli[mu_]],
    Adjoint[UMatrix[UChi[chopts___]][x_]], CovariantFieldDerivative[MM[x_], x_,
     fcli[mu_]]]] ->
 -UTrace1[NM[MM[x], Adjoint[CovariantFieldDerivative[MM[x], x,
      fcli[mu]]], UMatrix[UChi[chopts]][x],
    Adjoint[CovariantFieldDerivative[MM[x], x, fcli[mu]]]]]-
 Det[Adjoint[UMatrix[UChi[chopts]][x]]]/2 - Det[UMatrix[UChi[chopts]][x]]/2 -
  UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x], x, fcli[mu]]],
    CovariantFieldDerivative[UMatrix[UChi[chopts]][x], x, fcli[mu]]]] -
  UTrace1[NM[Adjoint[CovariantFieldDerivative[UMatrix[UChi[chopts]][x], x,
      fcli[mu]]], CovariantFieldDerivative[MM[x], x, fcli[mu]]]] +
  UTrace1[NM[Adjoint[MM[x]], UMatrix[UChi[chopts]][x]]]^2/4 +
  (UTrace1[NM[Adjoint[MM[x]], UMatrix[UChi[chopts]][x]]]*
    UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], MM[x]]])/2 +
  UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], MM[x]]]^2/4 -
  UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], UMatrix[UChi[chopts]][x]]],

UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x_], x_, fcli[mu_]]],
CovariantFieldDerivative[MM[x_], x_, fcli[mu_]],
    Adjoint[UMatrix[UChi[chopts___]][x_]], MM[x_]]] ->
 -(-UTrace1[NM[MM[x], Adjoint[CovariantFieldDerivative[MM[x], x,
      fcli[mu]]], UMatrix[UChi[chopts]][x],
    Adjoint[CovariantFieldDerivative[MM[x], x, fcli[mu]]]]]-
 Det[Adjoint[UMatrix[UChi[chopts]][x]]]/2 - Det[UMatrix[UChi[chopts]][x]]/2 -
  UTrace1[NM[Adjoint[CovariantFieldDerivative[MM[x], x, fcli[mu]]],
    CovariantFieldDerivative[UMatrix[UChi[chopts]][x], x, fcli[mu]]]] -
  UTrace1[NM[Adjoint[CovariantFieldDerivative[UMatrix[UChi[chopts]][x], x,
      fcli[mu]]], CovariantFieldDerivative[MM[x], x, fcli[mu]]]] +
  UTrace1[NM[Adjoint[MM[x]], UMatrix[UChi[chopts]][x]]]^2/4 +
  (UTrace1[NM[Adjoint[MM[x]], UMatrix[UChi[chopts]][x]]]*
    UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], MM[x]]])/2 +
  UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], MM[x]]]^2/4 -
  UTrace1[NM[Adjoint[UMatrix[UChi[chopts]][x]], UMatrix[UChi[chopts]][x]]])};


EOMTrick[expr_] := expr /.
  Join[$EOMRules, (Expand[a_ * #[[1]]] -> a * #[[2]])& /@ $EOMRules];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Expansion of the fields around the solution to the equations of motion *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Auxiliary functions*)

xi[x_] :=
  IsoDot[IsoVector[fcqf[Particle[PseudoScalar[12]]]][x],
    IsoVector[UMatrix[UGenerator[]]]];

(*See Ecker 1992, CERN-TH-6660/92*)

uExpRight[x_,a___RenormalizationState,b___RenormalizationScheme,
      c___ExpansionState,opts___Rule]:=
    NM[SMM[x,Sequence@@OptionsSelect[SMM,opts]],
      UFieldMatrix[
        DecayConstant[UPerturbation,a,b,c]/DecayConstant[pm,a,b,c]/
          Sqrt[2],fcqf[Particle[UPerturbation,a,b,c]][x],
        Sequence@@OptionsSelect[UFieldMatrix,opts]]];

uExpLeftAdj[x_,a___RenormalizationState,b___RenormalizationScheme,
      c___ExpansionState,opts___Rule]:=
    NM[UFieldMatrix[
        DecayConstant[UPerturbation,a,b,c]/DecayConstant[pm,a,b,c]/
          Sqrt[2],fcqf[Particle[UPerturbation,a,b,c]][x],
        Sequence@@OptionsSelect[UFieldMatrix,opts]],
      SMM[x,Sequence@@OptionsSelect[SMM,opts]]];

(* The u_mu field *)

(*Keep things compact*)
SetOptions[CovariantNabla, fcexpt -> False];

UCoefficient[USmall][0][li_, x_] = USmall[li][x];
UCoefficient[USmall][1][li_, x_] =
           -Sqrt[2]/DecayConstant[pm] CovariantNabla[xi[x], x, {li}];
UCoefficient[USmall][2][li_, x_] =
  1/4/DecayConstant[pm]^2 UCommutator[xi[x],
      UCommutator[USmall[li][x], xi[x]]];
UCoefficient[USmall][do_?((# > 2) &)][li_, x_] :=
    UCoefficient[USmall][do][li, x] = (Message[UPerturb::"nocoeff", do];
        DiscardTerms[
            I*NM[Adjoint[uExpRight[x, ExpansionOrder -> do]],
                    CDr[NM[uExpRight[x, ExpansionOrder -> do],
                        uExpLeftAdj[x, ExpansionOrder -> do]], x, {li},
                      Explicit -> True],
                    Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] //
                NMExpand // Expand,
            Retain -> {Particle[UPerturbation] -> do}](* // UReduce*));

(*Return to defaults*)
SetOptions[CovariantNabla, Sequence@@tmpoptscn];


(* The chi_+ field*)

UCoefficient[UChiPlus][0][x_] = UChiPlus[x];
UCoefficient[UChiPlus][1][x_] =
   -I/Sqrt[2]/DecayConstant[pm]UAntiCommutator[xi[x],
      UChiMinus[x]];
UCoefficient[UChiPlus][2][x_] =
    -1/4/DecayConstant[pm]^2UAntiCommutator[xi[x],
      UAntiCommutator[xi[x], UChiPlus[x]]];
UCoefficient[UChiPlus][do_?((# > 2) &)][x_] :=
    UCoefficient[UChiPlus][do][x] = (Message[UPerturb::"nocoeff", do];
        DiscardTerms[
            NM[Adjoint[uExpRight[x, ExpansionOrder -> do]],
                    UMatrix[UChi[]][x],
                    Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] +
                  NM[uExpLeftAdj[x, ExpansionOrder -> do],
                    Adjoint[UMatrix[UChi[]][x]],
                    uExpRight[x, ExpansionOrder -> do]] // NMExpand // Expand,
             Retain -> {Particle[UPerturbation] -> do}] (*// UReduce*));

(* The chi_- field*)

UCoefficient[UChiMinus][0][x_] = UChiMinus[x];
UCoefficient[UChiMinus][1][x_] =
  -I/Sqrt[2]/DecayConstant[pm]UAntiCommutator[xi[x],
      UChiPlus[x]];
UCoefficient[UChiMinus][2][x_] = -1/4/DecayConstant[pm]^2 UAntiCommutator[xi[x],
      UAntiCommutator[xi[x], UChiMinus[x]]];
UCoefficient[UChiMinus][do_?((# > 2) &)][x_] :=
    UCoefficient[UChiMinus][do][x] = (Message[UPerturb::"nocoeff", do];
        DiscardTerms[
            NM[Adjoint[uExpRight[x, ExpansionOrder -> do]],
                    UMatrix[UChi[]][x],
                    Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] -
                  NM[uExpLeftAdj[x, ExpansionOrder -> do],
                    Adjoint[UMatrix[UChi[]][x]],
                    uExpRight[x, ExpansionOrder -> do]] // NMExpand // Expand,
             Retain -> {Particle[UPerturbation] -> do}] (*// UReduce*));

(* The f_+ field*)

UCoefficient[UFPlus][0][li1_, li2_, x_] = UFPlus[li1, li2][x];
UCoefficient[UFPlus][1][li1_, li2_, x_] =
  I/Sqrt[2]/DecayConstant[pm]UCommutator[xi[x], UFMinus[li1, li2][x]];
UCoefficient[UFPlus][2][li1_, li2_, x_] = -1/4/DecayConstant[pm]^2 UCommutator[xi[x],
      UCommutator[xi[x], UFPlus[li1, li2][x]]];
UCoefficient[UFPlus][do_?((# > 2) &)][li1_, li2_, x_] :=
    UCoefficient[UChiPlus][do][x, li1, li2] = (Message[UPerturb::"nocoeff", do];
        DiscardTerms[
            NM[uExpLeftAdj[x, ExpansionOrder -> do],
                    FieldStrengthTensorFull[{li1},
                      UGeneratorMatrixIsoDot[
                        fcqf[Particle[LeftComponent[0]], {li2}][x]],
                      x, -I], Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] +
                   NM[Adjoint[uExpRight[x, ExpansionOrder -> do]],
                    FieldStrengthTensorFull[{li1},
                      UGeneratorMatrixIsoDot[
                        fcqf[Particle[RightComponent[0]], {li2}][x]],
                      x, -I], uExpRight[x, ExpansionOrder -> do]] //
                NMExpand // Expand,
            Retain -> {Particle[UPerturbation] -> do}] (*// UReduce*));

(* The f_- field*)

UCoefficient[UFMinus][0][li1_, li2_, x_] = UFMinus[li1, li2][x];
UCoefficient[UFMinus][1][li1_, li2_, x_] =
  I/Sqrt[2]/DecayConstant[pm]UCommutator[xi[x], UFPlus[li1, li2][x]];
UCoefficient[UFMinus][2][li1_, li2_, x_] =
  -1/4/DecayConstant[pm]^2UCommutator[xi[x],
                    UCommutator[xi[x], UFMinus[li1, li2][x]]];
UCoefficient[UFPlus][do_?((# > 2) &)][li1_, li2_, x_] :=
    UCoefficient[UFMinus][do][x, li1, li2] = (Message[UPerturb::"nocoeff", do];
        DiscardTerms[
            NM[uExpLeftAdj[x, ExpansionOrder -> do],
                    FieldStrengthTensorFull[{li1},
                      UGeneratorMatrixIsoDot[
                        fcqf[Particle[LeftComponent[0]], {li2}][x]],
                      x, -I], Adjoint[uExpLeftAdj[x, ExpansionOrder -> do]]] -
                   NM[Adjoint[uExpRight[x, ExpansionOrder -> do]],
                    FieldStrengthTensorFull[{li1},
                      UGeneratorMatrixIsoDot[
                        fcqf[Particle[RightComponent[0]], {li2}][x]],
                      x, -I], uExpRight[x, ExpansionOrder -> do]] //
                NMExpand // Expand,
            Retain -> {Particle[UPerturbation] -> do}] (*// UReduce*));

(* The U-field*)

UCoefficient[MM][do_][x_] :=
    UCoefficient[MM][do][x] = (
        DiscardTerms[
            NM[uExpRight[x, ExpansionOrder -> do],
               uExpLeftAdj[x, ExpansionOrder -> do]]//
                NMExpand // Expand,
            Retain -> {Particle[UPerturbation] -> do}] (*// UReduce*));

UPerturb[exp_, opts___Rule] :=
    Block[{or, lim, quants, ruls, subs, a, b, i, summ, UCoeff},
      or = ExpansionOrder /. {opts} /. Options[UPerturb];
      lim = Which[NumericQ[or], {i, 0, or},
          Head[or] ===
              List && (Length[or] === 1 ||
                Length[or] === 2) && (And @@ (NumericQ /@ or)), {i,
            Sequence @@ or}, True, Message[UPerturb::"badlim", or]; Return[]];
       quants = UFields /. {opts} /. Options[UPerturb];
      subs = (#[a__][b__] :> #[a, b]) & /@ quants;
      ruls = ((#[a__] -> ((summ[UCoeff[#][i][a], lim])) & /@ quants) /.
            summ -> Sum); exp /. subs /. ruls /. UCoeff -> UCoefficient /.
            pm -> If[(fcsunn /. {opts} /. Options[UPerturb]) === 2,
            Pion, PhiMeson]];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Miscellaneous *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* The Gell-mann Okubo mass formula *)

$GellmannOkubo = {ParticleMass[EtaMeson, r___]^
        n_ -> ((-ParticleMass[PionZero, r]^2 + 4ParticleMass[KaonZero, r]^2)/
            3)^(n/2)};

GellmannOkubo[exp_] := Block[{l, s, pm}, exp /.
  (l : (HighEnergyPhysics`Phi`Renormalization`LeutwylerJBar | Log))[s__] :>
  (l[s] /. ParticleMass -> pm) /. $GellmannOkubo /. 
  pm -> ParticleMass];

$GellmannOkuboInv = {ParticleMass[PseudoScalar[2], r___]^2 - 
          4 ParticleMass[PseudoScalar[6], r___]^2 :> -3*
          ParticleMass[PseudoScalar[11], 
              r]^2, -ParticleMass[PseudoScalar[2], r___]^2 + 
          4 ParticleMass[PseudoScalar[6], r___]^2 :> 
        3*ParticleMass[PseudoScalar[11], r]^2, 
      4/3 - ParticleMass[Pion]^2/(3 ParticleMass[Kaon]^2) :> 
        ParticleMass[EtaMeson]^2/(ParticleMass[Kaon]^2)};

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

End[];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

EndPackage[];

If[$Verboseness > 0,WriteString["stdout", "Utilities | \n "]];

