(* Channels *)

(* Utilities for kinematics, isospin, etc. *)

(* Author:  Frederik Orellana, fjob@cabocomm.dk

   Date:  1/8-2000

   Context: HighEnergyPhysics`Phi`Channels

   Package version:  1.2

   Mathematica version:  4.0 *)



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

BeginPackage[
    "HighEnergyPhysics`Phi`Channels`", {"HighEnergyPhysics`Phi`",
      "HighEnergyPhysics`Phi`Objects`"}];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

tmp`olddir = Directory[];
SetDirectory[HighEnergyPhysics`Phi`$HEPDir];
SetDirectory["HighEnergyPhysics"];
SetDirectory["Phi"];
Get["Channels.defs.m"]; SetDirectory[tmp`olddir];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

Begin["`Private`"];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* FeynCalc functions *)

fcpa := fcpa = HighEnergyPhysics`FeynCalc`Pair`Pair;
fcmom := fcmom = HighEnergyPhysics`FeynCalc`Momentum`Momentum;
fcsundel := fcsundel = HighEnergyPhysics`FeynCalc`SUNDelta`SUNDelta;
fcsuni := fcsuni = HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex;
fcsund := fcsund = HighEnergyPhysics`FeynCalc`SUND`SUND;
fcsunf := fcsunf = HighEnergyPhysics`FeynCalc`SUNF`SUNF;
fcqf := fcqf = HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;
fcfad := fcfad = HighEnergyPhysics`FeynCalc`FeynAmpDenominator`FeynAmpDenominator;
fcli := fcli = HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
fcpd := fcpd = HighEnergyPhysics`FeynCalc`PartialD`PartialD;
fcdiga := fcdiga = HighEnergyPhysics`FeynCalc`DiracGamma`DiracGamma;



(* FeynArts functions *)

faso := faso = HighEnergyPhysics`FeynArts`SumOver;



(* Defaults *)

$IsoSpinProjectionRules = {PionPlus -> (Iso[PhiMeson, {1}] -
              I*Iso[PhiMeson, {2}])/Sqrt[2],
      PionMinus -> (Iso[PhiMeson, {1}] + I*Iso[PhiMeson, {2}])/Sqrt[2],
      PionZero -> Iso[PhiMeson, {3}],
      KaonPlus -> (Iso[PhiMeson, {4}] - I*Iso[PhiMeson, {5}])/Sqrt[2],
      KaonMinus -> (Iso[PhiMeson, {4}] + I*Iso[PhiMeson, {5}])/Sqrt[2],
      KaonZero -> (Iso[PhiMeson, {6}] - I*Iso[PhiMeson, {7}])/Sqrt[2],
      KaonZeroBar -> (Iso[PhiMeson, {6}] + I*Iso[PhiMeson, {7}])/Sqrt[2],
      EtaMeson -> Iso[PhiMeson, {8}]};
Options[FieldProjection] = {Channel -> PionPlus};
Options[AmplitudeProjection] = {Channel -> {{Pion, Pion} -> {Pion,
              Pion}, {2}}, (*CommutatorReduce -> True,*) OnMassShell -> True,
      MassArguments -> {RenormalizationState[0]},
      MomentumVariablesString -> "p", HoldSums -> False,
      SummationForm -> ExplicitSums, GaugeGroup -> 2};
Options[SUNReduce] = {HoldSums -> True, CommutatorReduce -> False,
      SummationForm -> ImplicitSums,FullReduce -> False,GaugeGroup -> 2,
      RemoveIntegerIndices -> False};
Options[IndicesCleanup] := {IsoDummys -> {"j", "k", "l"},
      LorentzDummys -> {"\[Xi]", "\[Rho]", "\[Sigma]", "\[Tau]", "\[Omega]"},
      ExtendedCleanup -> True, FCleanup -> False};



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Isospin simplification and summation *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* A summation function for multiple sums that remembers previously calculated
  sums and uses these for the ensuing summations: *)

USum[a_, b___List] := (sumlims[i_] := ({b}[[i]]); recfun[0] = a;
      Do[recfun[rep] =
          Sum1[recfun[rep - 1], sumlims[rep]] /. Sum1 -> Sum, {rep, 1,
          Length[{b}]}]; recfun[Length[{b}]]);
USum[a_, {}] := a;
USumHeld[a_] := a;



(* Finding pairs in a list of indices (ignoring integers and elements from
   $ConstantIsoIndices): *)

su3summationindices[{a__}, gennr2_] :=
      (us = (Complement[
              Union[Complement[Sort[Flatten[{a}]],
                  Sort[Flatten[{a}]] //. {cc___, b_, b_, c___} -> {cc, c}]],
              Join[$ConstantIsoIndices,
                fcsuni /@ $ConstantIsoIndices]]) //. {{cc___, _Integer,
                c___} -> {cc, c}, {cc___, fcsuni[_Integer], c___} -> {cc, c}};
       Table[{removesun[us[[icount]]], gennr2}, {icount, Length[us]}]);
su3summationindices[{}, ___] := {};



(* Support functions for SUNReduce for removing SUNIndex and UIndex heads: *)

removesun[(fcsuni | UIndex)[iii_]] := iii;
removesun[iii /; FreeQ[iii, (fcsuni | UIndex)]] := iii;



(* Support functions for SUNReduce for removing SUNIndex and UIndex heads from
   constants: *)

removesunc[(fcsuni | UIndex)[
          iii_]] /; (IntegerQ[
            iii] || (! FreeQ[$ConstantIsoIndices, iii])) :=(*Change 31/1 -
        1999*)(*iii*)tmpsuni[iii];
removesunc[
        fcsuni[iii_]] /; (!
            IntegerQ[iii] && (FreeQ[$ConstantIsoIndices, iii])) :=
    fcsuni[iii];
removesunc[
        UIndex[iii_]] /; (!
            IntegerQ[iii] && (FreeQ[$ConstantIsoIndices, iii])) :=
    UIndex[iii];
removesunc[iii_] /; FreeQ[{iii}, (fcsuni | UIndex)] := iii;




(* Adding a head to the inner index of Projection: *)

NTo3Rules1 = (Projection[pa_, ___][pb_]) :> fcsundel[pa, pb];



(* Removing heads from constants and changing from FeynCalc to Phi functions: *)

NTo3Rules2[2] = {SU2D[x1_, x2_, x3_] :> 0,
      SU2F[x1_, x2_, x3_] :>
        SU2F[removesunc[x1], removesunc[x2], removesunc[x3]],
      SU2Delta[x1_, x2_] :> SU2Delta[removesunc[x1], removesunc[x2]],
      fcsund[x1_, x2_, x3_] :> 0,
      fcsunf[x1_, x2_, x3_] :>
        SU2F[removesunc[x1], removesunc[x2], removesunc[x3]],
      fcsundel[x1_, x2_] :> SU2Delta[removesunc[x1], removesunc[x2]]};
NTo3Rules2[
      3] = {SU3D[x1_, x2_, x3_] :>
        SU3D[removesunc[x1], removesunc[x2], removesunc[x3]],
      SU3F[x1_, x2_, x3_] :>
        SU3F[removesunc[x1], removesunc[x2], removesunc[x3]],
      SU3Delta[x1_, x2_] :> SU3Delta[removesunc[x1], removesunc[x2]],
      fcsund[x1_, x2_, x3_] :>
        SU3D[removesunc[x1], removesunc[x2], removesunc[x3]],
      fcsunf[x1_, x2_, x3_] :>
        SU3F[removesunc[x1], removesunc[x2], removesunc[x3]],
      fcsundel[x1_, x2_] :> SU3Delta[removesunc[x1], removesunc[x2]]};
NTo3iRules2[2] = {fcsund[x1_, x2_, x3_] :> 0, fcsunf :> SU2F,
      fcsundel :> SU2Delta};
NTo3iRules2[3] = {fcsund :> SU3D, fcsunf :> SU3F, fcsundel :> SU3Delta};



(* A function for selecting the members of a list with head SUNIndex: *)

su3test[a_] := {};
su3test[a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] := a;
su3list[{a___}] := Flatten[su3test /@ Flatten[{a}]];



(* Distributivity: *)

(*SUNReduce1[aa_ + bb_ /; FreeQ[{bb}, Plus], opts___] :=
      SUNReduce1[aa, opts] + SUNReduce1[bb, opts];*)
SUNReduce1[aa_, opts___] /; (FreeQ[aa, fcsuni]) := aa;
SUNReduce1[aa_*bb_, opts___] /; (FreeQ[aa, fcsuni]) :=
    aa*SUNReduce1[bb, opts];
SUNReduce1[aa_Plus, opts___] := SUNReduce1[#, opts] & /@ aa;
SUNReduce1[aa_,
        opts___] /; (((SummationForm /. Flatten[{opts}] /.
                  Options[SUNReduce]) == ExplicitSums) && FreeQ[{aa}, Plus]) :=
     SUNReduce2[aa, opts];
SUNReduce1[aa_,
        opts___] /; ((SummationForm /. Flatten[{opts}] /.
              Options[SUNReduce]) == ImplicitSums) := SUNReduce2[aa, opts];



(* Getting scalars out: *)

(*SUNReduce2[aa_*bb_, opts___] /; (NumericQ[aa] || MemberQ[$UScalars, aa]) :=
      aa*SUNReduce2[bb, opts];*)
(*SUNReduce2[x_^m_*c_, opts___] /; MemberQ[$UScalars, x] :=
    x^m*SUNReduce2[c, opts];
SUNReduce2[x_*c_, opts___] /; MemberQ[$UScalars, x] := x*SUNReduce2[c, opts];
  SUNReduce2[x_^m_, opts___] /; MemberQ[$UScalars, x] := x^m;
  SUNReduce2[x_, opts___] /; MemberQ[$UScalars, x] := x;*)
(*SUNReduce2[Conjugate[x_]^m_*c_, opts___] /; MemberQ[$UScalars, x] :=
    Conjugate[x]^m*SUNReduce2[c, opts];
SUNReduce2[Conjugate[x_]*c_, opts___] /; MemberQ[$UScalars, x] :=
    Conjugate[x]*SUNReduce2[c, opts];
  SUNReduce2[Conjugate[x_]^m_, opts___] /; MemberQ[$UScalars, x] :=
    Conjugate[x]^m;
  SUNReduce2[Conjugate[x_], opts___] /; MemberQ[$UScalars, x] :=
    Conjugate[x];*)



(* Explicit summation: *)

SUNReduce2[aa_,
        opts___] /; (SummationForm /. Flatten[{opts}] /. Options[SUNReduce]) ==
         ExplicitSums := (VerbosePrint[2, "Finding indices to sum over"];
      gennr1 = If[
          FreeQ[aa, _SU3F | _SU3D | _SU3Delta] &&
            FreeQ[aa,
              GaugeGroup -> 3], (GaugeGroup^2 - 1 /. Flatten[{opts}] /.
              Options[SUNReduce]), 8];
      flatlist =
        List1[(aa /. NTo3Rules1 /.
                  NTo3Rules2[(GaugeGroup /. Flatten[{opts}] /.
                        Options[SUNReduce])]) /. {NM -> List1,
                Times ->
                  List1}(*writing out powers*)/. ((at_ /; !
                        FreeQ[at, fcsuni])^ex_Integer :>
                Table[NM[at], {ex}])];
      flatlist1 = Flatten[flatlist /. List1 -> List];
      sumlist =
        su3summationindices[
          su3list[Flatten[(flatlist1 //. {ff_[a___, fcsuni[ind_], b___][_] /;
                        FreeQ[ff, List] -> {ff[a, b], fcsuni[ind]},
                    ff_[fcsuni[ind_], b___][_] /; FreeQ[ff, List] -> {ff[b],
                        fcsuni[ind]},
                    ff_[a___, fcsuni[ind_]][_] /; FreeQ[ff, List] -> {ff[a],
                        fcsuni[ind]},
                    ff_[a___, fcsuni[ind_], b___] /;
                        FreeQ[ff, List] -> {ff[a, b], fcsuni[ind]},
                    ff_[fcsuni[ind_], b___] /; FreeQ[ff, List] -> {ff[b],
                        fcsuni[ind]},
                    ff_[a___, fcsuni[ind_]] /; FreeQ[ff, List] -> {ff[a],
                        fcsuni[ind]}})]], gennr1];
      VerbosePrint[3, "Found:\n", sumlist]; VerbosePrint[2, "Summing"];
      tmpres=USum1[(aa /. NTo3Rules1 /.
                    NTo3Rules2[(GaugeGroup /. Flatten[{opts}] /.
                          Options[SUNReduce])]), ##] & @@ sumlist /.
          If[(HoldSums /. Flatten[{opts}] /. Options[SUNReduce]),
            USum1 -> USumHeld, USum1 -> USum];
        If[(CommutatorReduce /. Flatten[{opts}] /.
              Options[SUNReduce]), VerbosePrint[2,"Applying CommutatorReduce"];
	      CommutatorReduce[tmpres,opts], tmpres]);



(* To determine if there are identical indices in the two f-functions of a
product, we have to compare them all pairwise, that is,  construct all
possible combinations with Outer: *)



twoselect[{a_, b_, c_}, {d_, e_, f_}] :=
    twoselect[{a, b, c}, {d, e, f}] =
      Select[Join @@
          Outer[List, {a, b, c}, {d, e, f}], (#[[1]] == #[[2]] &&
              Head[#[[1]]] === fcsuni && Head[#[[2]]] === fcsuni) &];



(* This function gives the signature of the permutations needed to turn one e.g.
{a,b,cc},{c,a,b} into {a,b,cc},{a,b,c}: *)

sig1[{___, a_, mm___, b_, ___}, {___, a_, m___,
        b_, ___}] := (-1)^(Length[{mm}] - Length[{m}]);



(* This function gives the signature of the permutations needed to turn one e.g.
{a,b,c},{c,aa,bb} into {a,b,c},{aa,bb,c}: *)

sig2[{a___, m_, b___}, {aa___, m_,
        bb___}] := (-1)^(Length[{a}] - Length[{aa}]);



(* Implicit summation: *)

(* (*SU3Delta[i_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
        j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]*
      SU3D[a___, j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, zz___] :>
    SU3D[a, i, zz],
  SU2Delta[i_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
        j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]*
      SU2F[a___, j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, zz___] :>
    SU2F[a, i, zz],
  SU3Delta[i_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
        j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]*
      SU3F[a___, j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, zz___] :>
    SU3F[a, i, zz],*) *)

$SUNDeltaRules =(*the delta functions are orderless,
      so we need not be carefull with the order*){SU3Delta[fcsuni[i_],
          fcsuni[i_]] :> 8,
      SU3Delta[i_, fcsuni[j_]]*SU3Delta[fcsuni[j_], k_] :> SU3Delta[i, k],
      SU3Delta[i_Integer, i_Integer] :> 1,
      SU3Delta[tmpsuni[i_], fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU3Delta[fcsuni[i_], tmpsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU3Delta[i_Integer, fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU3Delta[fcsuni[i_], j_Integer]^n_ /; EvenQ[n] :> 1,(*Added 22/6 - 2000*)
        SU3Delta[i_?((IntegerQ[#] || Head[#] === tmpsuni) &),
            j_?((IntegerQ[#] || Head[#] === tmpsuni) &)]^n_ :> SU3Delta[i, j],
       SU3Delta[fcsuni[i_], fcsuni[j_]]^n_ /; EvenQ[n] :> 8^(n/2),
      SU3Delta[i_, j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]*
            expr_ /; (!
                FreeQ[expr,
                  j] && (FreeQ[expr,
                    fcqf[___, j, ___] | fcqf[___, j, ___][_]] || !
                    IntegerQ[i])) :> (expr /. j -> i),
      SU2Delta[fcsuni[i_], fcsuni[i_]] :> 3,
      SU2Delta[i_, fcsuni[j_]]*SU2Delta[fcsuni[j_], k_] :> SU2Delta[i, k],
      SU2Delta[i_Integer, i_Integer] :> 1,
      SU3Delta[i_Integer, i_Integer] :> 1,
      SU2Delta[tmpsuni[i_], fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU2Delta[fcsuni[i_], tmpsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU2Delta[i_Integer, fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU2Delta[fcsuni[i_], j_Integer]^n_ /; EvenQ[n] :> 1,(*Added 22/6 - 2000*)
        SU2Delta[i_?(IntegerQ[#] || Head[#] === tmpsuni),
            j_?(IntegerQ[#] || Head[#] === tmpsuni)]^n_ :> SU2Delta[i, j],
      SU2Delta[fcsuni[i_], fcsuni[j_]]^n_ /; EvenQ[n] :> 3^(n/2),
      SU2Delta[i_, j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]*
            expr_ /; (!
                FreeQ[expr,
                  j] && (FreeQ[expr,
                    fcqf[___, j, ___] | fcqf[___, j, ___][_]] || !
                    IntegerQ[i])) :> (expr /. j -> i),
      Projection[i_Integer][fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      Projection[i_Integer][j_Integer] /; (i =!= j) :> 0,
      Projection[i_Integer][fcsuni[j_Integer]] /; (i =!= j) :> 0};
$SUNDFRules = {(*contraction of three indices*)
      SU2F[a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
          c_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
          d_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]^2 :> 6,
    SU3F[a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
          c_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
          d_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]^2 :> 24,
    SU3D[a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
          c_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
          d_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]^2 :>
      40/3,(*contraction of two indices*)
      SU2F[a_, b_,
            c_]^2 /; (Sort[Head /@ {a, b, c}] == {Integer, fcsuni, fcsuni}) :>
       2, SU3F[a_, b_,
            c_]^2 /; (Sort[Head /@ {a, b, c}] == {Integer, fcsuni, fcsuni}) :>
       3(*Bug fix 12/1 - 2000, was 6*),
    SU3D[a_, b_,
            c_]^2 /; (Sort[Head /@ {a, b, c}] == {Integer, fcsuni, fcsuni}) :>
       5/3, (SU2F[a_, b_, c_]*SU2F[d_, e_, f_]*
            rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] == 2) :>
      2*(SU2Delta @@
            Complement[{a, b, c, d, e,
                f}, (Join @@ twoselect[{a, b, c}, {d, e, f}])])*(sig1[{a, b,
              c}, {d, e, f}])*
        rest, (SU3F[a_, b_, c_]*SU3F[d_, e_, f_]*
            rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] == 2) :>
      3*(SU3Delta @@
            Complement[{a, b, c, d, e,
                f}, (Join @@ twoselect[{a, b, c}, {d, e, f}])])*(sig1[{a, b,
              c}, {d, e, f}])*
        rest, (SU3D[a_, b_, c_]*SU3D[d_, e_, f_]*
            rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] == 2) :>
      rest*5/3*SU3Delta @@
          Complement[{a, b, c, d, e,
              f}, (Join @@ twoselect[{a, b, c}, {d, e, f}])],(*Added 15/2 -
        2000*)(SU3D[a_, b_, c_]*SU3F[d_, e_, f_]*
            rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] == 2) :>
      0,(*contraction of one index -
        is this formula true for SU(3)?*)(*NO its not - fixed,
      but correct formula no really usefull,
      16/12/1999*)(SU2F[a_, b_, c_]*SU2F[d_, e_, f_]*
            rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] ==
            1) :> (((SU2Delta[#1, #3]*SU2Delta[#2, #4] -
                      SU2Delta[#2, #3]*SU2Delta[#1, #4]) & @@
                Join[Complement[{a, b,
                      c}, {twoselect[{a, b, c}, {d, e, f}][[1, 1]]}],
                  Complement[{d, e,
                      f}, {twoselect[{a, b, c}, {d, e, f}][[1, 1]]}]])*
            sig2[{a, b, c}, {d, e, f}])*
        rest,(*(SU3F[a_, b_, c_]*SU3F[d_, e_, f_]*
              rest___) /; (Length[twoselect[{a, b, c}, {d, e, f}]] ==
              1) :> (twonum = twoselect[{a, b, c}, {d, e, f}][[1, 1]];
          morenums =
            Join[Complement[{a, b, c}, {twonum}],
              Complement[{d, e,
                  f}, {twonum}]]; ((2/
                        3((SU3Delta[#1, #3]*SU3Delta[#2, #4] -
                                SU3Delta[#2, #3]*SU3Delta[#1, #4]) & @@
                          morenums) + ((SU3D[#1, #3, twonum]*
                                SU3D[#2, #4, twonum] -
                              SU3D[#2, #3, twonum]*SU3D[#1, #4, twonum]) & @@
                        morenums))*sig2[{a, b, c}, {d, e, f}])*
            rest),*)(*integers should already be sorted and put first -
        the following should also take care of SU2F[a_integer, c_integer,
              d_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]^2, ...*)
      SU2F[a_Integer, b_Integer,
        c_] :> (-1)^(Complement[{1, 2, 3}, {a, b}][[1]] +
              1)*(SU2Delta[#, c] & @@ (Complement[{1, 2, 3}, {a, b}]))}
$SUNRules =
    Join[$SUNDeltaRules, $SUNDFRules, $SU3FReduceList, $SU3DReduceList];

SUNReduce2[aa_,
        opts___] /; ((SummationForm /. Flatten[{opts}] /.
                Options[SUNReduce]) ==
            ImplicitSums) := (VerbosePrint[2,
        "Applying reduction rules"];
      VerbosePrint[3, "Reduction rules are:\n", $SUNRules];
      aa /. If[(RemoveIntegerIndices /. Flatten[{opts}] /.
                    Options[SUNReduce]), {}, NTo3Rules1] /.
            NTo3Rules2[(GaugeGroup /. Flatten[{opts}] /.
                  Options[SUNReduce])] /.
          NTo3iRules2[(GaugeGroup /. Flatten[{opts}] /.
                Options[SUNReduce])] /. $SUNRules);

SUNReduce[aa_,
        opts___] /; (((SummationForm /. Flatten[{opts}] /.
                Options[SUNReduce]) ==
            ImplicitSums) && (FullReduce /. Flatten[{opts}] /.
                    Options[SUNReduce]) === True) :=
Block[{op},
   op=Join[{FullReduce->False},
   Select[Flatten[{opts}],FreeQ[#,FullReduce,Infinity,Heads->True]]];
   VerbosePrint[2, "Iterating with options ", op];
   FixedPoint[(SUNReduce[ExpandAll[#],Sequence@@op]&),aa]
];

SUNReduce[aa_Plus, opts___] := SUNReduce[#, opts] & /@ aa;

SUNReduce[aa_,
        opts___] /; ((SummationForm /. Flatten[{opts}] /.
              Options[SUNReduce]) == ImplicitSums && (FullReduce /. Flatten[{opts}] /.
                    Options[SUNReduce]) =!= True) := (VerbosePrint[2,
        "Will use reduction rules"];
      Collect[VerbosePrint[2, "Collecting"];
            SUNReduce1[
              Expand[aa](*Change 5/3 - 1999 -
                    FA likes to use indices more than twice in a
product*)(*Changed again 16/12/1999*)(*fcsundel[fcsuni[i_?! NumberQ],
                    fcsuni[i_!NumberQ]]*)/.
                fcsundel[
                    fcsuni[i_?((NumberQ[#] == False &&
                                FreeQ[$ConstantIsoIndices, #]) &)],
                    fcsuni[i_?((NumberQ[#] == False &&
                                FreeQ[$ConstantIsoIndices, #]) &)]] :> \
(GaugeGroup /. Flatten[{opts}] /. Options[SUNReduce])^2 - 1,
              opts], {_fcfad, _SU2Delta, _SU2F, _SU2D}] /.(*Change 31/1 -
              1999*)tmpsuni -> fcsuni /.(*Added 9/1 - 2000*){faso[_Integer] ->
             1, faso[fcsuni[_Integer]] -> 1});

SUNReduce[aa_,
        opts___] /; ((SummationForm /. Flatten[{opts}] /.
              Options[SUNReduce]) == ExplicitSums) := (VerbosePrint[2,
        "Will sum explicitly"];
      Collect[VerbosePrint[2, "Collecting"];
            SUNReduce1[
                      Expand[aa /.
                          Plus[bb_, bbb__] /; FreeQ[{bb, bbb}, fcsuni] ->
                            tempplus[ttt[Plus[bb, bbb]] /. Plus -> ppl]],
                      opts] /. Plus -> ppl /. tempplus[cc___] -> cc /.
                ppl -> Plus /.
              ttt -> Together, {_fcfad, _SU2Delta, _SU3Delta, _SU2F, _SU3F, \
_SU3D}] /.(*Change 31/1 - 1999*)tmpsuni -> fcsuni /.(*Added 9/1 - 2000*)
          faso[_Integer](*/; (!
                    FreeQ[i, Alternatives @@ $ConstantIsoIndices] ||
                  IntegerQ[i])*)-> 1);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Cleaning up indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* These rules substitute "real" index names instead of the internal code names: *)

dummynames[li_List,
      ll_List] := {fcsuni[IsoDummy][n_] :>
        fcsuni[ToExpression[li[[1]] <> ToString[n]]],
fcsuni[IsoExternDummy][n_] :> fcsuni[ToExpression[li[[2]] <> ToString[
n]]], fcsuni[IsoInternDummy][n_] :> fcsuni[ToExpression[li[[3]] <> ToString[
n]]], fcli[LorentzDummy][n_] :> fcli[ToExpression[ll[[1]] <> ToString[
n]]],
fcli[LorentzExternDummy][n_] :> fcli[ToExpression[ll[[2]] <> ToString[
n]]], fcli[LorentzInternDummy][n_] :>
        fcli[ToExpression[ll[[3]] <> ToString[n]]],
fcli[DerivativeExternDummy][n_] :> fcli[ToExpression[ll[[4]] <> ToString[
n]]], fcli[DerivativeInternDummy][n_] :>
        fcli[ToExpression[ll[[5]] <> ToString[n]]]};



(* The aim is to make a unique dummy substitution ending with as few dummys as
possible.  The uniqueness should then make terms cancel... *)


(* We have to be carefull with the iso f function, since it is not orderless: *)

dummyrulesf3 = {ff_[a_, b_, c_]*
            facc_  :> (res =
            ff[a, b, c]*facc /.
              Map[Apply[Rule, #] &,
                Transpose[{Select[{a, b, c}, ! FreeQ[facc, #] &],
                    Table[fcsuni[IsoDummy][rep], {rep, isodummycounter,
                        isodummycounter + 2}]}]]; isodummycounter += 3; res)/;
			(Count[{! FreeQ[facc, a], ! FreeQ[facc, b],
	      !FreeQ[facc, c]}, True] == 3 &&
              Count[Select[{a, b, c}, ! FreeQ[facc, #] &],
_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] == 3 &&
              FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &], _Integer] &&
              FreeQ[{a, b, c}, IsoDummy] &&
              FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &],
                Alternatives @@ $ConstantIsoIndices])};

dummyrulesf2 = {ff_[a_, b_, c_]*
            facc_ :> (res =
            ff[a, b, c]*facc /.
              Map[Apply[Rule, #] &,
                Transpose[{Select[{a, b, c}, ! FreeQ[facc, #] &],
                    Table[fcsuni[IsoDummy][rep], {rep, isodummycounter,
                        isodummycounter + 1}]}]]; isodummycounter += 2; res)/;
			(Count[{! FreeQ[facc, a], ! FreeQ[facc, b],
	      !FreeQ[facc, c]}, True] == 2 &&
              Count[Select[{a, b, c}, ! FreeQ[facc, #] &],
_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] == 2 &&
              FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &], _Integer] &&
              FreeQ[{a, b, c}, IsoDummy] &&
              FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &],
                Alternatives @@ $ConstantIsoIndices]) };

dummyrulesf1 = {ff_[a_, b_, c_]*
            facc_ :> (res =
            ff[a, b, c]*facc /.
              Map[Apply[Rule, #] &,
                Transpose[{Select[{a, b, c}, ! FreeQ[facc, #] &],
                    Table[fcsuni[IsoDummy][rep], {rep, isodummycounter,
                        isodummycounter + 0}]}]]; isodummycounter += 1;
          res)/; (Count[{! FreeQ[facc, a], ! FreeQ[facc, b],
	      !FreeQ[facc, c]}, True] == 1 &&
              Count[Select[{a, b,c}, ! FreeQ[facc, #] &],
_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] == 1 &&
              FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &], _Integer] &&
              FreeQ[{a, b, c}, IsoDummy] &&
              FreeQ[Select[{a, b, c}, ! FreeQ[facc, #] &],
                Alternatives @@ $ConstantIsoIndices])};



(* Now we can deal with general functions, which are assumed to be orderless in
iso-indices, derivative indices and Lorentz indices.  This last point means
that we may miss cancellation of e.g. some space-time tensors... Again we
device a unique substitution of dummys. *)

(*freemult = NM | NM1 | NM2;*)

(* Comparison between factors: *)

(*Iso - indices*)(*Bug fixed, split in two, 21/12/1999*)
  dummyrulesiso1a = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___][
              x_], facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc(*, multdum*)] /.
            a -> fcsuni[IsoExternDummy][1]) /; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, IsoExternDummy] &&(*Added 21/12/1999*)(*Changed 26/9-2000*)
                FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(*(NM | NM1 | NM2)[___?(FreeQ[{##},isomult]&)]*)(*Changed 17/1-2000*)
		(NM | NM1 | NM2)[___, _?((FreeQ[{##}, isomult] &&
                   FreeQ[{##}, _HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] ===
                   False) &), ___]
		])};
(*Iso - indices*)
  dummyrulesiso1b = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___],
             facc___] :> (nm1[fac,
              gg[fi, a, la], facc(*, multdum*)] /.
            a -> fcsuni[IsoExternDummy][1]) /; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, IsoExternDummy] &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(*(NM | NM1 | NM2)[___?(FreeQ[{##},isomult]&)]*)(*Changed 17/1-2000*)
		(NM | NM1 | NM2)[___, _?((FreeQ[{##}, isomult] &&
                   FreeQ[{##}, _HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] ===
                   False) &), ___]

		])};
(*Iso - indices*)
  dummyrulesiso2 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___][
              x_], facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc(*, multdum*)] /.
            a ->
              fcsuni[IsoExternDummy][
                Max[Union[{0},Cases[{fac, facc}, fcsuni[IsoExternDummy][_], Infinity,
                        Heads -> True]] /.
                      fcsuni[IsoExternDummy][na_] -> na] + 1]) /;
		(! FreeQ[{fac, facc}, a] (*&& !FreeQ[{fac, facc}, IsoExternDummy]*) &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},isomult]&)]]),
	      (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___],
             facc___] :> (nm1[fac,
              gg[fi, a, la], facc(*, multdum*)] /.
            a -> fcsuni[IsoExternDummy][
                Max[Union[{0},Cases[{fac, facc}, fcsuni[IsoExternDummy][_], Infinity,
                        Heads -> True]] /.
                      fcsuni[IsoExternDummy][na_] -> na] + 1]) /;
		(! FreeQ[{fac, facc}, a] (*&& !FreeQ[{fac, facc}, IsoExternDummy]*) &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},isomult]&)]]) };

(*Derivative indices*)
  dummyrulesder1a = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
                fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
                la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, fcpd[a], la][x], facc(*, multdum*)] /.
            a -> fcli[DerivativeExternDummy][1]) /; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, DerivativeExternDummy] &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Derivative indices*)
  dummyrulesder1b = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
              fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
              la___], facc___] :> (nm1[fac,
              gg[fi, fcpd[a], la], facc(*, multdum*)] /.
            a -> fcli[DerivativeExternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, DerivativeExternDummy] &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Derivative indices*)
  dummyrulesder2 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
                fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
                la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, fcpd[a], la][x], facc(*, multdum*)] /.
            a -> fcli[DerivativeExternDummy][
                Max[Cases[{fac, facc}, fcli[DerivativeExternDummy][_],
                        Infinity, Heads -> True] /.
                      fcli[DerivativeExternDummy][na_] -> na] + 1])/;
		      (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fac, facc}, DerivativeExternDummy] &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
	      (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
              fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
              la___], facc___] :> (nm1[fac,
              gg[fi, fcpd[a], la], facc(*, multdum*)] /.
            a -> fcli[DerivativeExternDummy][
                Max[Cases[{fac, facc}, fcli[DerivativeExternDummy][_],
                        Infinity, Heads -> True] /.
                      fcli[DerivativeExternDummy][na_] -> na] + 1])/;
		      (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fac, facc}, DerivativeExternDummy] &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Lorentz indices*)
  dummyruleslor1 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                 la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc(*, multdum*)] /.
            a -> fcli[LorentzExternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, LorentzExternDummy] &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
	      (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
               la___],
            facc___] :> (nm1[fac,
              gg[fi, a, la], facc(*, multdum*)] /.
            a -> fcli[LorentzExternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, LorentzExternDummy] &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Lorentz indices*)
  dummyruleslor2 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                 la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc(*, multdum*)] /.
            a -> fcli[LorentzExternDummy][
                Max[Cases[{fac, facc}, fcli[LorentzExternDummy][_], Infinity,
                        Heads -> True] /.
                      fcli[LorentzExternDummy][na_] -> na] + 1])/;
		      (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fac, facc}, LorentzExternDummy] &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
	      (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
               la___],
            facc___] :> (nm1[fac,
              gg[fi, a, la], facc(*, multdum*)] /.
            a ->
              fcli[LorentzExternDummy][
                Max[Cases[{fac, facc}, fcli[LorentzExternDummy][_], Infinity,
                        Heads -> True] /.
                      fcli[LorentzExternDummy][na_] -> na] + 1])/;
		      (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fac, facc}, LorentzExternDummy] &&
              FreeQ[{fac, facc}, (*freemult[___, multdum]*)
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};



(* Comparison internally in functions: *)

(*Iso - indices*)
  dummyrulesisoint1 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___][
              x_], facc___] :> (nm1[fac, gg[fi, a, la][x],
              facc] /.
            a -> fcsuni[IsoInternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fi, la}, IsoInternDummy]), (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___],
             facc___] :> (nm1[fac, gg[fi, a, la],
              facc] /. a -> fcsuni[IsoInternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fi, la}, IsoInternDummy])};
(*Iso - indices*)
  dummyrulesisoint2 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___][
              x_], facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc] /.
            a -> fcsuni[IsoInternDummy][
                Max[Cases[{fi, la}, fcsuni[IsoInternDummy][_], Infinity,
                        Heads -> True] /.
                      fcsuni[IsoInternDummy][na_] -> na] + 1])/; (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fi, la}, IsoInternDummy]), (nm1 :
                NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___],
             facc___] :> (nm1[fac, gg[fi, a, la],
              facc] /.
            a -> fcsuni[IsoInternDummy][
                Max[Cases[{fi, la}, fcsuni[IsoInternDummy][_], Infinity,
                        Heads -> True] /.
                      fcsuni[IsoInternDummy][na_] -> na] + 1])/; (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fi, la}, IsoInternDummy])};
(*Derivative indices*)
  dummyrulesderint1 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
                fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
                la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc] /.
            a -> fcli[DerivativeInternDummy][1]) /; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fi, la}, DerivativeInternDummy]), (nm1 : NM | NM1 | NM2)[
            fac___, gg_[fi___,
              fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
              la___],
            facc___] :> (nm1[fac,
              gg[fi, a, la], facc] /. a -> fcli[DerivativeInternDummy][1]) /;
	      (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fi, la}, DerivativeInternDummy])};
(*Derivative indices*)
  dummyrulesderint2 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
                fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
                la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc] /.
            a -> fcli[DerivativeInternDummy][
                Max[Cases[{fi, la}, fcli[DerivativeInternDummy][_], Infinity,
                        Heads -> True] /.
                      fcli[DerivativeInternDummy][na_] -> na] + 1]) /;
		      (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fi, la}, DerivativeInternDummy]), (nm1 :
                NM | NM1 | NM2)[fac___,
            gg_[fi___,
              fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
              la___], facc___] :> (nm1[fac,
              gg[fi, a, la], facc] /.
            a -> fcli[DerivativeInternDummy][
                fcli[DerivativeInternDummy][
                    Max[Cases[{fi, la}, fcli[DerivativeInternDummy][_],
                            Infinity, Heads -> True] /.
                          fcli[DerivativeInternDummy][na_] -> na] + 1] +
                  1])/; (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fi, la}, DerivativeInternDummy])};
(*Lorentz indices*)
  dummyruleslorint1 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                 la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc] /.
            a -> fcli[LorentzInternDummy][1]) /; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fi, la}, LorentzInternDummy]), (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
               la___],
            facc___] :> (nm1[fac, gg[fi, a, la],
               facc] /. a -> fcli[LorentzInternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fi, la}, LorentzInternDummy])};
(*Lorentz indices*)
  dummyruleslorint2 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                 la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc] /.
            a ->
              fcli[LorentzInternDummy][
                Max[Cases[{fi, la}, fcli[LorentzInternDummy][_], Infinity,
                        Heads -> True] /.
                      fcli[LorentzInternDummy][na_] -> na] + 1])/;
		      (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fi, la}, LorentzInternDummy]), (nm1 :
                NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
               la___],
            facc___] :> (nm1[fac,
              gg[fi, a, la], facc] /.
            a -> fcli[LorentzInternDummy][
                Max[Cases[{fi, la}, fcli[LorentzInternDummy][_], Infinity,
                        Heads -> True] /.
                      fcli[LorentzInternDummy][na_] -> na] + 1])/; (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fi, la}, LorentzInternDummy])};



(* Squared objects will not be catched by the previous rules (and we don't have
higher powers with one iso-spin index), so we need a special set of rules: *)

dummyrulessq1 = {(ff_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
                  la___][x_])^2 :> (res = (ff[fi, fcsuni[IsoDummy][isodummycounter],
                    la][x])^2; isodummycounter += 1;
          res) /; (FreeQ[a,
              IsoDummy]), (ff_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
                la___])^2 :> (res = (ff[fi, fcsuni[IsoDummy][isodummycounter],
                  la])^2; isodummycounter += 1;
          res)/; (FreeQ[a,
              IsoDummy]), (ff_[fi___,
                  a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                  la___][x_])^2 :> (ff[fi, fcli[LorentzDummy][1], la][
              x])^2 /; (FreeQ[a,
              LorentzDummy]), (ff_[fi___,
                a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                la___])^2 :> (ff[fi,
              fcli[LorentzDummy][1],
              la])^2/; (FreeQ[a, LorentzDummy]) , (ff_[fcpd[fcli[a_]], fi__][x_])^2 :> (ff[
                fcpd[fcli[DerivativeExternDummy][1]], fi][
              x])^2/; (FreeQ[a,
              DerivativeExternDummy]), (ff_[fcpd[fcli[a_]], fi__])^2 :> (ff[
              fcpd[fcli[DerivativeExternDummy][1]], fi])^2 /; (FreeQ[a,
              DerivativeExternDummy])};
dummyrulessq2 = {(ff_[fi___, fcsuni[ld_][ll_], fcsuni[ln_], la___][
              x_])^2 :> (ff[fi, fcsuni[ld][ll], fcsuni[ln][ll + 1], la][
              x])^2, (ff_[fi___, fcsuni[ld_][ll_], fcsuni[ln_],
              la___])^2 :> (ff[fi, fcsuni[ld][ll], fcsuni[ln][ll + 1],
              la])^2, (ff_[fi___, fcli[ld_][ll_], fcli[ln_], la___][
              x_])^2 :> (ff[fi, fcli[ld][ll], fcli[ln][ll + 1], la][
              x])^2, (ff_[fi___, fcli[ld_][ll_], fcli[ln_], la___])^2 :> (ff[
              fi, fcli[ld][ll], fcli[ln][ll + 1],
              la])^2, (ff_[fcpd[fcli[ld_][ll_]], fcpd[fcli[ln_]], fi__][
              x_])^2 :> (ff[fcpd[fcli[ld][ll]], fcpd[fcli[ln][ll + 1]], fi][
              x])^2, (ff_[fcpd[fcli[ld_][ll_]], fcpd[fcli[ln_]],
              fi__])^2 :> (ff[fcpd[fcli[ld][ll]], fcpd[fcli[ln][ll + 1]],
              fi])^2};
allpatterns = (Blank | BlankSequence | BlankNullSequence | Pattern);



(* The final cleanup function *)

(*Change 26/9-2000: Commented out multdum in the substitutions above;
added it below*)

IndicesCleanup1[w_, opts___] :=
    w /. dummyrulesiso1a /. dummyrulesiso1b /. dummyrulesiso2 /.
              dummyrulesder1a /. dummyrulesder1b /. dummyrulesder2 /.
        dummyruleslor1 /. dummyruleslor2(*change 19/1 -
      1999*)(*/. dummyrulessq1 /. dummyrulessq2*)/.
      (*Added 26/9-2000*)
      {(nm1:NM|NM1|NM2)[fac___] :> nm1[fac,isomult] /;
         FreeQ[{fac},fcsuni[_?((FreeQ[#,IsoDummy|IsoExternDummy|IsoInternDummy]&&!UScalarQ[#])&)]] &&
         FreeQ[{fac},isomult],
       (nm1:NM|NM1|NM2)[fac___] :> nm1[fac,lorentzmult] /;
         FreeQ[{fac},fcsuni[_?((FreeQ[#,LorentzDummy|LorentzExternDummy|LorentzInternDummy|
         DerivativeExternDummy]&&!UScalarQ[#])&)]] && FreeQ[{fac},lorentzmult]};


SetAttributes[NM1, Flat]; SetAttributes[NM2, Flat];


IndicesCleanup[w_, opts___] := (

      larul = {{}, {}, {}};

      Which[
      !FreeQ[w,fcli[_, SpaceTimeDimensions] | fcmom[_, SpaceTimeDimensions]],

        If[ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
          VerbosePrint[1,
            "Found occurence of SpaceTimeDimensions, using this as number of \
space-time dimensions for all momenta, Dirac gamma matrices and Lorentz \
indices"];
           larul = {fcli[llii_] -> fcli[llii, SpaceTimeDimensions],
            dg[dig_] -> dg[dig, SpaceTimeDimensions],
            fcmom[mo_] -> fcmom[mo, SpaceTimeDimensions]}];,


        !FreeQ[w, fcli[_, D] | fcmom[_, D]],
         If[ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],VerbosePrint[1,
            "Found occurence of D, using this as number of space-time \
dimensions for all momenta, Dirac gamma matrices and Lorentz indices"];
          larul = {fcli[llii_] -> fcli[llii, D], dg[dig_] -> dg[dig, D],
            fcmom[mo_] -> fcmom[mo, D]}];
	];

      isodummycounter = 1;
      w1 = If[ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
              VerbosePrint[1, "Using ExtendedCleanup->True"];
              w /. {SU2Delta -> su2delta1, SU2F -> su2f1,
                  SU3Delta -> su3delta1, SU3F -> su3f1, SU3D -> su3d1,
                  SUNDelta -> sundelta1, SUNF -> sunf1,
                  SUND -> sund1}(*Change 30/12 - 1999*)(*/. Times -> NM*),
              VerbosePrint[1,
                  "Using ExtendedCleanup->False\nWill not work if mixed Times \
and NM products are present"]w] /.
       (*Added 14/2 - 2000*)(((fcsuni[#] ->
                        protectisoconstant[#]) &) /@ (((Sequence @@ #) &) /@
                    Cases[w, faso[_], Infinity])) /.

          faso[fcsuni[a_]] -> protectisoconstant[a];

      (*freemult = NM | NM1 | NM2;*)

      subres = FixedPoint[(VerbosePrint[2, "Applying renaming rules"];
              IndicesCleanup1[#, opts]) &,
          If[FCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],

	    VerbosePrint[2, "Using FCleanup->True"];
            VerbosePrint[2,
              "Renaming product functions and protecting constants"];
            w1 /. {fcsuni[a_] :> protectisoconstant[a]/; (UScalarQ[a] || !
                                  FreeQ[$ConstantIsoIndices, a]),
                        fcli[a_] :> protectliconstant[a] /; UScalarQ[a]} /.
                    dummyrulesf3 /. dummyrulesf2 /.
                dummyrulesf1 /. {NM -> NM1(*Added NM 30/12 - 1999*),
                Times -> NM1,
                Dot -> NM2,(*change 19/1 - 1999*)
                  Power[a_, b_ /; b > 0 && IntegerQ[b]] :>
                  NM1 @@ Table[a, {ddum, 1, b}]},

            VerbosePrint[2,
              "Renaming product functions and protecting constants"];
            w1 /. {fcsuni[a_/; (UScalarQ[a] || !
                            FreeQ[$ConstantIsoIndices, a])]  ->
                    protectisoconstant[a],
                  fcli[a_] :> protectliconstant[a] /; UScalarQ[a] } /. {NM ->
                  NM1, Times -> NM1,
                Dot -> NM2,(*change 19/1 - 1999*)
                  Power[a_, b_ /; b > 0 && IntegerQ[b]] :>
                  NM1 @@ Table[a, {ddum, 1, b}]}]];

		  (*freemult = {};
        FixedPoint[IndicesCleanup1[#, opts] &, subres]*)

	VerbosePrint[2,
        "Putting back product function names and constants"];
      subres /. {(*multdum -> Sequence[],*)isomult->Sequence[],lorentzmult->Sequence[]} /.
      {NM1 ->(*Times*)(*Change 30/12 -
                                  1999*)NM,
                            NM2 -> Dot} /. {protectisoconstant -> fcsuni,
                          protectliconstant -> fcli} /.
                      faso[fcsuni[ii_]] -> faso[ii] /. {su2delta1 -> SU2Delta,
                       su2f1 -> SU2F, su3delta1 -> SU3Delta, su3f1 -> SU3F,
                      su3d1 -> SU3D, sundelta1 -> SUNDelta, sunf1 -> SUNF,
                      sund1 -> SUND} /.
                  dummynames[
                    IsoDummys /. Flatten[{opts}] /. Options[IndicesCleanup],
                    LorentzDummys /. Flatten[{opts}] /.
                      Options[IndicesCleanup]](* //.
                If[ExtendedCleanup /. Flatten[{opts}] /.
                    Options[IndicesCleanup],
                  VerbosePrint[2,
                    "Applying $CommutatorRules"]; $CommutatorRules, {}]*) /.
              fcdiga -> dg /. Flatten[{larul[[1]], larul[[3]]}] /.
          larul[[2]] /. dg -> fcdiga(*Added 26/9-2000*)/.Null->Sequence[]);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Isospin projection *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Projecting out IsoVectors in particle channels: *)

FieldProjection[
      IsoVector[
        fcqf[ders___, Particle[p : Alternatives @@ $UParticles, pp___],
          la___], oo___],
      opts___] := (Select[$IsoSpinProjectionRules, (!
                  FreeQ[#, (Channel /. Flatten[{opts}] /.
                          Options[FieldProjection]) -> _] &)])[[1, 2]] /.
      Iso[par_, {i_}] -> fcqf[ders, Particle[par, pp], fcsuni[i], la];
FieldProjection[
      IsoVector[
          fcqf[ders___, Particle[p : Alternatives @@ $UParticles, pp___],
            la___], oo___][x_],
      opts___] := (Select[$IsoSpinProjectionRules, (!
                  FreeQ[#, (Channel /. Flatten[{opts}] /.
                          Options[FieldProjection]) -> _] &)])[[1, 2]] /.
      Iso[par_, {i_}] -> fcqf[ders, Particle[par, pp], fcsuni[i], la][x];



(* Support functions for AmplitudeProjection: *)



(* The in and out particles are arranged as {tou,in}: *)

processfieldproduct[a_List -> b_List] :=
    Join[ChargeConjugate /@ Reverse[b], a];



(* On-mass-shell rules: *)

processmasses[a_List -> b_List, opts___][
      i_] := (ParticleMass[#,
              Sequence @@ (MassArguments /. Flatten[{opts}] /.
                    Options[AmplitudeProjection])] & /@ Join[a, b])[[i]];
momentarules[
      opts___] := (mv[i_] :=
        ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[AmplitudeProjection]) <> ToString[i]];
      Flatten[Join[{(*MomentaSumRule[
                OptionsSelect[opts, Options[AmplitudeProjection]]],*)
              If[(OnMassShell /. Flatten[{opts}] /.
                  Options[AmplitudeProjection]),
              Table[fcpa[fcmom[mv[irep], ___],
                    fcmom[mv[
                        irep], ___]] -> (processmasses[(Channel /.
                                  Flatten[{opts}] /.
                                Options[AmplitudeProjection])[[1]]][
                        irep])^2, {irep,
                  Length[(Channel /. Flatten[{opts}] /.
                            Options[AmplitudeProjection])[[1, 1]]] +
                    Length[(Channel /. Flatten[{opts}] /.
                            Options[AmplitudeProjection])[[1,
                          2]]]}], {}]}]]);



(* Projecting amplitudes out in particle channels: *)

AmplitudeProjection[amp_,
      opts___Rule] := (Expand[
                  NMExpand[
                    NM @@ (processfieldproduct[(Channel /. Flatten[{opts}] /.
                                    Options[
                                      AmplitudeProjection])[[1]]] /. \
($IsoSpinProjectionRules /. Iso -> iso1)) /.
                      Conjugate[Plus[a_, b___]] :>
                        Plus @@ Conjugate /@ {a, b}]] /.
                NM -> amp1) /. {iso1[PhiMeson, {aa_Integer}] -> aa,
              iso1[Pion, {aa_Integer}] -> aa} /.
          iso1[_, {aa_Integer}] -> aa /. momentarules[opts] /. amp1 -> amp;



(* Pion-Kaon-IsoSpin->3/2: *)

AmplitudeProjection[amp_,
        opts___Rule] /; ((Channel /. Flatten[{opts}] /.
              Options[AmplitudeProjection]) === {{Pion, Kaon} -> {Kaon, Pion},
             3/2}) :=
    AmplitudeProjection[amp,
      Channel -> {{PionPlus, KaonPlus} -> {KaonPlus, PionPlus}},
      Sequence[opts]];



(* PionKaonIsoSpin->1/2: *)

AmplitudeProjection[amp_,
        opts___Rule] /; ((Channel /. Flatten[{opts}] /.
              Options[AmplitudeProjection]) === {{Pion, Kaon} -> {Kaon, Pion},
             1/2}) := -1/2*
        AmplitudeProjection[amp,
          Channel -> {{PionPlus, KaonPlus} -> {KaonPlus, PionPlus}},
          Sequence[opts]] +
      3/2*AmplitudeProjection[amp,
          Channel -> {{KaonPlus, PionMinus} -> {KaonPlus, PionMinus}},
          Sequence[opts]];



(* PionPionIsoSpin->2: *)

AmplitudeProjection[amp_,
        opts___Rule] /; ((Channel /. Flatten[{opts}] /.
              Options[AmplitudeProjection])) === {{Pion, Pion} -> {Pion,
              Pion}, 2} :=
    AmplitudeProjection[amp,
      Channel -> {{PionPlus, PionPlus} -> {PionPlus, PionPlus}},
      Sequence[opts]];



(* PionPionIsoSpin->1: *)

AmplitudeProjection[amp_,
        opts___Rule] /; ((Channel /. Flatten[{opts}] /.
              Options[AmplitudeProjection]) === {{Pion, Pion} -> {Pion, Pion},
             1}) := 2*
        AmplitudeProjection[amp,
          Channel -> {{PionPlus, PionZero} -> {PionPlus, PionZero}},
          Sequence[opts]] -
      AmplitudeProjection[amp,
        Channel -> {{PionPlus, PionPlus} -> {PionPlus, PionPlus}},
        Sequence[opts]];



(* PionPionIsoSpin->0: *)

AmplitudeProjection[amp_,
        opts___Rule] /; ((Channel /. Flatten[{opts}] /.
              Options[AmplitudeProjection]) === {{Pion, Pion} -> {Pion, Pion},
             0}) := 3*
        AmplitudeProjection[amp,
          Channel -> {{PionZero, PionZero} -> {PionZero, PionZero}},
          Sequence[opts]] -
      2*AmplitudeProjection[amp,
          Channel -> {{PionPlus, PionPlus} -> {PionPlus, PionPlus}},
          Sequence[opts]];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

End[];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

EndPackage[];

If[$Verboseness > 0,WriteString["stdout", "Channels | \n "]];
