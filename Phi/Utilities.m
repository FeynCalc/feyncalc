(* Utilities *)

(* Utilities for kinematics and Dirac algebra *)

(* Author:  Frederik Orellana, fjob@cabocomm.dk

   Date:  1/8-2000

   Context: HighEnergyPhysics`Phi`Channels

   Package version:  1.2

   Mathematica version:  4.0 *)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

BeginPackage[
    "HighEnergyPhysics`Phi`Utilities`", {"HighEnergyPhysics`Phi`",
      "HighEnergyPhysics`Phi`Objects`"}];

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

fcpa := fcpa = HighEnergyPhysics`FeynCalc`Pair`Pair;
fcmom := fcmom = HighEnergyPhysics`FeynCalc`Momentum`Momentum;
fcsundel := fcsundel = HighEnergyPhysics`FeynCalc`SUNDelta`SUNDelta;
fcexpscp := fcexpscp = HighEnergyPhysics`FeynCalc`ExpandScalarProduct`ExpandScalarProduct;
fcfad := fcfad = HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator;
fcli := fcli = HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
fcdot := fcdot = HighEnergyPhysics`FeynCalc`DOT`DOT;
fcdiga := fcdiga = HighEnergyPhysics`FeynCalc`DiracGamma`DiracGamma;
fcdtr := fcdtr = HighEnergyPhysics`FeynCalc`DiracTrace`DiracTrace;
fceps := fceps = HighEnergyPhysics`FeynCalc`Eps`Eps;
fcsp := fcsp = HighEnergyPhysics`FeynCalc`ScalarProduct`ScalarProduct;
fcprd := fcprd = HighEnergyPhysics`FeynCalc`PropagatorDenominator`PropagatorDenominator;
fcmomex := fcmomex = HighEnergyPhysics`FeynCalc`MomentumExpand`MomentumExpand;
fcmomcomb := fcmomcomb = HighEnergyPhysics`FeynCalc`MomentumCombine`MomentumCombine;
fcpave := fcpave = HighEnergyPhysics`fctools`PaVe`PaVe;
fconeloop := fconeloop = HighEnergyPhysics`fctools`OneLoop`OneLoop;



(* Tracer functions *)

trtr := trtr = Tracer`GammaTrace; trid = Tracer`TrU; trsp :=
  trsp = Tracer`Spur; trdot := trdot = Tracer`TrS; vecd :=
  vecd = Tracer`VectorDimension; trsig := trsig = Tracer`Sigma; treps :=
  treps = Tracer`TrEps;



(* Defaults *)

Options[MandelstamReduce] = {MomentaSumLeft -> All, OnMassShell -> True,
      MandelstamCancel -> MandelstamU, MomentumVariablesString -> "p",
      Masses -> {ParticleMass[Pion, RenormalizationState[0]],
          ParticleMass[Pion, RenormalizationState[0]],
          ParticleMass[Pion, RenormalizationState[0]],
          ParticleMass[Pion, RenormalizationState[0]]}};
DeclareUScalar[MandelstamS]; DeclareUScalar[MandelstamT]; \
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


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Mandelstam simplification *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* The convention used is:  s=(p1+p2)^2, t=(p2+p3)^2, u=(p1+p3)^2 with all \
particles incoming (All) as in FeynCalc, that is s=(p1+p2)^2, t=(p2-p3)^2, \
u=(p1-p3)^2 (FirstHalf) where particles 1 and 2 are incoming and p3 and p4 \
are outgoing: *)

sturules[opts___] /; ((MomentaSumLeft /. Flatten[{opts}] /.
              Options[MandelstamReduce]) === All) := (mv[i_] :=
        ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MandelstamReduce]) <> ToString[i]]; {fcpa[
            fcmom[mv[1], ___], fcmom[mv[2], ___]] ->
          MandelstamS/
              2 - (fcpa[fcmom[mv[1]], fcmom[mv[1]]] +
                  fcpa[fcmom[mv[2]], fcmom[mv[2]]])/2,
        fcpa[fcmom[mv[2], ___], fcmom[mv[3], ___]] ->
          MandelstamT/
              2 - (fcpa[fcmom[mv[2]], fcmom[mv[2]]] +
                  fcpa[fcmom[mv[3]], fcmom[mv[3]]])/2,
        fcpa[fcmom[mv[1], ___], fcmom[mv[3], ___]] ->
          MandelstamU/
              2 - (fcpa[fcmom[mv[1]], fcmom[mv[1]]] +
                  fcpa[fcmom[mv[3]], fcmom[mv[3]]])/2});
sturules[opts___] /; (MomentaSumLeft /. Flatten[{opts}] /.
            Options[MandelstamReduce]) ===
        HighEnergyPhysics`Phi`Objects`FirstHalf := (mv[i_] :=
        ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MandelstamReduce]) <> ToString[i]]; {fcpa[
            fcmom[mv[1], ___], fcmom[mv[2], ___]] ->
          MandelstamS/
              2 - (fcpa[fcmom[mv[1]], fcmom[mv[1]]] +
                  fcpa[fcmom[mv[2]], fcmom[mv[2]]])/2,
        fcpa[fcmom[mv[2], ___],
            fcmom[mv[3], ___]] -> -MandelstamT/
              2 + (fcpa[fcmom[mv[2]], fcmom[mv[2]]] +
                  fcpa[fcmom[mv[3]], fcmom[mv[3]]])/2,
        fcpa[fcmom[mv[1], ___],
            fcmom[mv[3], ___]] -> -MandelstamU/
              2 + (fcpa[fcmom[mv[1]], fcmom[mv[1]]] +
                  fcpa[fcmom[mv[3]], fcmom[mv[3]]])/2});
sturules[opts___] /; (MomentaSumLeft /. Flatten[{opts}] /.
            Options[MandelstamReduce]) ===
        HighEnergyPhysics`Phi`Objects`Odd := (mv[i_] :=
        ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MandelstamReduce]) <> ToString[i]]; {fcpa[
            fcmom[mv[1], ___],
            fcmom[mv[2], ___]] -> -MandelstamS/
              2 + (fcpa[fcmom[mv[1]], fcmom[mv[1]]] +
                  fcpa[fcmom[mv[2]], fcmom[mv[2]]])/2,
        fcpa[fcmom[mv[2], ___],
            fcmom[mv[3], ___]] -> -MandelstamT/
              2 + (fcpa[fcmom[mv[2]], fcmom[mv[2]]] +
                  fcpa[fcmom[mv[3]], fcmom[mv[3]]])/2,
        fcpa[fcmom[mv[1], ___], fcmom[mv[3], ___]] ->
          MandelstamU/
              2 - (fcpa[fcmom[mv[1]], fcmom[mv[1]]] +
                  fcpa[fcmom[mv[3]], fcmom[mv[3]]])/2});
strules[opts___] := (mv[i_] :=
        ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MandelstamReduce]) <> ToString[i]];
      mss[i_] :=
        fcpa[fcmom[mv[i]],
          fcmom[mv[i]]]; (MandelstamCancel /. Flatten[{opts}] /.
            Options[MandelstamReduce]) ->
        mss[1] + mss[2] + mss[3] + mss[4] -
          Complement[{MandelstamS, MandelstamT,
                MandelstamU}, {(MandelstamCancel /. Flatten[{opts}] /.
                    Options[MandelstamReduce])}][[1]] -
          Complement[{MandelstamS, MandelstamT,
                MandelstamU}, {(MandelstamCancel /. Flatten[{opts}] /.
                    Options[MandelstamReduce])}][[2]] );
MandelstamReduce[amp_,
      opts___] := (mv[i_] :=
        ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MandelstamReduce]) <> ToString[i]];
      masses1[i_] := (Masses /. Flatten[{opts}] /.
              Options[MandelstamReduce])[[i]];
      Collect[fcexpscp[
                  amp /. MomentaSumRule[
                      Join[Select[
                          Flatten[{opts}], (!
                                FreeQ[#, (MomentumVariablesString -> _ |
                                        MomentaSumLeft -> _)]) &],
                        Select[
                          Options[
                            MandelstamReduce], (!
                                FreeQ[#, (MomentumVariablesString -> _ |
                                        MomentaSumLeft -> _)]) &]]]] /.
                sturules[opts] /.
              If[(OnMassShell /. Flatten[{opts}] /.
                    Options[MandelstamReduce]),
                Table[fcpa[fcmom[mv[irep], ___], fcmom[mv[irep], ___]] ->
                    masses1[irep]^2, {irep, 4}], {}] /.
            If[! (MandelstamCancel /. Flatten[{opts}] /.
                      Options[MandelstamReduce]) === None,
              strules[opts], {}] /.
          If[(OnMassShell /. Flatten[{opts}] /. Options[MandelstamReduce]),
            Table[fcpa[fcmom[mv[irep], ___], fcmom[mv[irep], ___]] ->
                masses1[irep]^2, {irep, 4}], {}], _fcsundel]);



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
      (*Added 11/1-2001*)
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



(* A four point tensor integral of rank four can not be handled by FeynCalc.  We \
reduce it to lower rank PaVe's. *)



(* The stuff below is just to have constants taken out and sums expanded: *)



(* Clear[FourPoint]; *)

FourPoint[q_, a_Plus, opts___] := (FourPoint[q, #, opts] & /@ a);
FourPoint[q_, a_*b_, opts___] /; FreeQ[a, q | fcprd] :=
    a*FourPoint[q, b, opts];



MomentaApart[exp_] :=
   exp /.
   {
   fcpa[fcli[a__], b : HoldPattern[
      Plus[(___*HighEnergyPhysics`FeynCalc`Momentum`Momentum[__] |
      HighEnergyPhysics`FeynCalc`Momentum`Momentum[__]) ..]]]  :>
      (fcpa[fcli[a], #] & /@ b),
   fcdiga[b : HoldPattern[
      Plus[(___*HighEnergyPhysics`FeynCalc`Momentum`Momentum[__] |
      HighEnergyPhysics`FeynCalc`Momentum`Momentum[__]) ..]],dim___]  :>
      (fcdiga[#,dim] & /@ b)
   };



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
          MomentaApart[
            fcmomex[fcpa[fcli[l1, d], fcmom[qq1, d]]*
                fcpa[fcli[l2, d], fcmom[qq2, d]]*
                fcpa[fcli[l3, d], fcmom[qq3, d]]*
                fcpa[fcli[l4, d], fcmom[qq4, d]]]]], opts];



(* Formula taken from the FeynCalc1.0 manual (don't know why Rolf didn't \
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

FourPoint[q_,
        aa : HoldPattern[
            Times[___, (ffaadd_[
                  HighEnergyPhysics`FeynCalc`PropagatorDenominator`\
PropagatorDenominator[
                      HighEnergyPhysics`FeynCalc`Momentum`Momentum[_,
                        d___], _] ..]), ___, \
(HighEnergyPhysics`FeynCalc`Pair`Pair[
                    HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[_,
                      d___], HighEnergyPhysics`FeynCalc`Momentum`Momentum[q_,
                      d___]] ..), ___]], opts___] /; Length[a] < 4 :=
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

SetAttributes[ditchmom, NumericFunction];
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

      If[StringQ[fi] =!= True, Message[PutIfNotThere::nostring, fi];
        Return[]];

      Which[
         StringMatchQ[fi,"*.Gen"]===True||StringMatchQ[fi,"*.Mod"]===True,
           dir = eliminateDoubles[HighEnergyPhysics`FeynCalc`$FeynCalcDirectory <>
	       $PathnameSeparator <>
	       "Phi" <> $PathnameSeparator <> "CouplingVectors"],
	     (*Added 24/7-2001*)
	     StringMatchQ[fi,"*.Fac"]===True,
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

              True, (Message[PutIfNotThere::baddir, dir]; Return[])];

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

End[];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

EndPackage[];

If[$Verboseness > 0,WriteString["stdout", "Utilities | \n "]];
