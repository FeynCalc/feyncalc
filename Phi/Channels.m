(* Channels *)

(* Utilities for kinematics, isospin, etc. *)

(* Author:  Frederik Orellana, fjob@cabocomm.dk

   Creation date:  1/8-2000

   Context: HighEnergyPhysics`Phi`Channels *)



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

BeginPackage[
    "HighEnergyPhysics`Phi`Channels`", {"HighEnergyPhysics`Phi`",
    "HighEnergyPhysics`FeynCalc`", "HighEnergyPhysics`Phi`Objects`"}];

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

fcpa := fcpa = MakeContext["Pair"];
fcmom := fcmom = MakeContext["Momentum"];
fcsundel := fcsundel = MakeContext["SUNDelta"];
fcsuni := fcsuni = MakeContext["SUNIndex"];
fcsund := fcsund = MakeContext["SUND"];
fcsunf := fcsunf = MakeContext["SUNF"];
fcsunn := fcsunn = MakeContext["SUNN"];
fcqf := fcqf = MakeContext["QuantumField"];
fcfad := fcfad = MakeContext["FeynAmpDenominator"];
fcli := fcli = MakeContext["LorentzIndex"];
fcpd := fcpd = MakeContext["PartialD"];
fcdiga := fcdiga = MakeContext["DiracGamma"];
fcexpt := fcexpt = MakeContext["Explicit"];
fcexsuni := fcexsuni = MakeContext["ExplicitSUNIndex"];
fccombs := fccombs = MakeContext["Combinations"];
FieldDerivative := FieldDerivative = MakeContext["FieldDerivative"];



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
              Pion}, 2}, (*CommutatorReduce -> True,*) OnMassShell -> True,
      MassArguments -> {RenormalizationState[0]},
      MomentumVariablesString -> "p"(*, HoldSums -> False,
      fcexpt -> True, fcsunn -> 2*)(*Dunno what these options were doing here. 5.4.2002*)};
Options[SUNReduce] = {HoldSums -> True, CommutatorReduce -> False,
      fcexpt -> False, FullReduce -> False, fcsunn -> 2,
      UDimension -> Automatic(*Commented out 11/5-2003*)(*, RemoveIntegerIndices -> False*)};
Options[IndicesCleanup] := {IsoDummys -> {"j", "k", "l"},
      LorentzDummys -> {"\[Xi]", "\[Rho]", "\[Sigma]", "\[Tau]", "\[Omega]"},
      ExtendedCleanup -> True, FCleanup -> False, CommutatorReduce -> True};
Options[CNM] = {IsoDummys -> {"p", "q", "r", "s"},
    LorentzDummys -> {"\[Zeta]", "\[Eta]", "\[Theta]", "\[Kappa]"}}



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
                c___} -> {cc, c}, {cc___, (fcsuni|fcexsuni)[_Integer], c___} -> {cc, c}};
       Table[{removesun[us[[icount]]], gennr2}, {icount, Length[us]}]);
su3summationindices[{}, ___] := {};



(* Support functions for SUNReduce for removing SUNIndex and UIndex heads: *)

removesun[(fcsuni | fcexsuni | UIndex)[iii_]] := iii;
removesun[iii /; FreeQ[iii, (fcsuni | fcexsuni | UIndex)]] := iii;



(* Support functions for SUNReduce for removing SUNIndex and UIndex heads from
   constants: *)

removesunc[(fcsuni | fcexsuni | UIndex)[iii_]] /;
     (IntegerQ[iii] || (!FreeQ[$ConstantIsoIndices, iii])) :=
    tmpsuni[iii];
removesunc[(fcsuni | fcexsuni)[iii_]] /;
     (!IntegerQ[iii] && (FreeQ[$ConstantIsoIndices, iii])) :=
    fcsuni[iii];
removesunc[UIndex[iii_]] /;
     (!IntegerQ[iii] && (FreeQ[$ConstantIsoIndices, iii])) :=
    UIndex[iii];
removesunc[iii_] /; FreeQ[{iii}, (fcsuni | fcexsuni | UIndex)] := iii;




(* Adding a head to the inner index of Projection: *)
(* This Projection business is not necessary. Dropped, 11/5-2003 *)
(*NTo3Rules1 = (Projection[pa_, ___][pb_]) :> fcsundel[pa, pb];*)
NTo3Rules3 = {(fcsundel|SU2Delta|SU3Delta)[i_, j:((fcsuni(*|fcexsuni*)(*Bug fix, 11/5-2003*))[_])]*
            HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[a___, j_, b___][x_]  ->
            HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[a, fcsuni[i], b][x]};



(* Removing heads from constants and changing from FeynCalc to PHI functions: *)

NTo3Rules2[2] = {SU2D[x1_, x2_, x3_] :> 0,
      SU2F[x1_, x2_, x3_] :>
        SU2F[removesunc[x1], removesunc[x2], removesunc[x3]],
      SU2Delta[x1_, x2_] :> SU2Delta[removesunc[x1], removesunc[x2]],
      fcsund[x1_, x2_, x3_] :> 0,
      fcsunf[x1_, x2_, x3_] :>
        SU2F[removesunc[x1], removesunc[x2], removesunc[x3]],
      fcsundel[x1_, x2_] :> SU2Delta[removesunc[x1], removesunc[x2]]};
NTo3Rules2[3] = {SU3D[x1_, x2_, x3_] :>
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

NTo3Rules2[_] = {};
NTo3iRules2[_] = {};



(* A function for selecting the members of a list with head SUNIndex: *)

su3test[a_] := {};
su3test[a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] := a;
su3list[{a___}] := Flatten[su3test /@ Flatten[{a}]];



(* Distributivity: *)

SUNReduce1[aa_, opts___] /; (FreeQ[aa, fcsuni | fcexsuni]) := aa;
SUNReduce1[aa_*bb_, opts___] /; (FreeQ[aa, fcsuni | fcexsuni]) :=
    aa*SUNReduce1[bb, opts];
SUNReduce1[aa_Plus, opts___] := SUNReduce1[#, opts] & /@ aa;
SUNReduce1[aa_, opts___] /;
    (((fcexpt /. Flatten[{opts}] /.
    Options[SUNReduce]) == True) && FreeQ[{aa}, Plus]) :=
    SUNReduce2[aa, opts];
SUNReduce1[aa_, opts___] /;
    ((fcexpt /. Flatten[{opts}] /. Options[SUNReduce]) == False) :=
    SUNReduce2[aa, opts];


(* Explicit summation: *)

SUNReduce2[aa_,
        opts___] /; (fcexpt /. Flatten[{opts}] /. Options[SUNReduce]) ==
         True := (VerbosePrint[2, "Finding indices to sum over"];
      gennr1 = If[
          FreeQ[aa, _SU3F | _SU3D | _SU3Delta] &&
            FreeQ[aa,
              fcsunn -> 3], (fcsunn^2 - 1 /. Flatten[{opts}] /.
              Options[SUNReduce]), 8];
      flatlist =
        List1[(aa (*Commented out 11/5-2003*)(*/. NTo3Rules1*) /.
                  NTo3Rules2[(fcsunn /. Flatten[{opts}] /.
                        Options[SUNReduce])]) /. {NM -> List1,
                Times ->
                  List1}(*writing out powers*)/.
                  ((at_ /; !FreeQ[at, fcsuni])^ex_Integer :>
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
      tmpres=USum1[(aa (*Commented out 11/5-2003*)(*/. NTo3Rules1*) /.
                    NTo3Rules2[(fcsunn /. Flatten[{opts}] /.
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

sig1[{___, a_, mm___, b_, ___}, {___, a_, m___, b_, ___}] :=
  (-1)^(Length[{mm}] - Length[{m}]);
sig1[{___, a_, mm___, b_, ___}, {___, b_, m___, a_, ___}] :=
  (-1)^(Length[{mm}] - Length[{m}] + 1);



(* This function gives the signature of the permutations needed to turn one e.g.
{a,b,c},{c,aa,bb} into {a,b,c},{aa,bb,c}: *)
(*Bug fixed 25/5-2001: There might be an identical constant index in
  the two argument lists besides the index to be permuted*)

sig2[{a___, m_?((!IntegerQ[#] && Head[#] =!= tmpsuni)&), b___},
{aa___, m_?((!IntegerQ[#] && Head[#] =!= tmpsuni)&), bb___}] :=
(-1)^(Length[{a}] - Length[{aa}]);



(* Implicit summation: *)

$SUNDeltaRules =(*the delta functions are orderless,
      so we need not be carefull with the order*){SU3Delta[fcsuni[i_],
          fcsuni[i_]] :> 8,
      SU3Delta[i_, fcsuni[j_]]*SU3Delta[fcsuni[j_], k_] :> SU3Delta[i, k],
      SU3Delta[i_Integer|(i:tmpsuni[_Integer]), i_Integer|(i:tmpsuni[_Integer])] :> 1,
      SU3Delta[i:tmpsuni[_]|_Integer, j:tmpsuni[_]|_Integer]^_ :> SU3Delta[i, j],
      SU3Delta[i:tmpsuni[_Integer]|_Integer, k_]*
        SU3Delta[j:tmpsuni[_Integer]|_Integer, k_] :> 0/;
            ((i/.tmpsuni->Identity) =!= (j/.tmpsuni->Identity)),
      SU3Delta[tmpsuni[i_], fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU3Delta[fcsuni[i_], tmpsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU3Delta[i_Integer, fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU3Delta[fcsuni[i_], j_Integer]^n_ /; EvenQ[n] :> 1,
        SU3Delta[i_?((IntegerQ[#] || Head[#] === tmpsuni) &),
            j_?((IntegerQ[#] || Head[#] === tmpsuni) &)]^n_ :> SU3Delta[i, j],
       SU3Delta[fcsuni[i_], fcsuni[j_]]^n_ /; EvenQ[n] :> 8^(n/2),
      SU3Delta[i_, j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]*
            expr_ :> (expr /. j -> i) /; (!FreeQ[expr, j] &&
            (FreeQ[expr, HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[___, j, ___]] || !(IntegerQ[i]||Head[i]===tmpsuni&&IntegerQ[i[[1]]]))),
      SU2Delta[fcsuni[i_], fcsuni[i_]] :> 3,
      SU2Delta[i_, fcsuni[j_]]*SU2Delta[fcsuni[j_], k_] :> SU2Delta[i, k],
      SU2Delta[i_Integer|(i:tmpsuni[_Integer]), i_Integer|(i:tmpsuni[_Integer])] :> 1,
      SU2Delta[i:tmpsuni[_]|_Integer, j:tmpsuni[_]|_Integer]^_ :> SU2Delta[i, j],
      SU2Delta[i:tmpsuni[_Integer]|_Integer, k_]*
        SU2Delta[j:tmpsuni[_Integer]|_Integer, k_] :> 0 /;
                ((i/.tmpsuni->Identity) =!= (j/.tmpsuni->Identity)),
      SU2Delta[tmpsuni[i_], fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU2Delta[fcsuni[i_], tmpsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU2Delta[i_Integer, fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      SU2Delta[fcsuni[i_], j_Integer]^n_ /; EvenQ[n] :> 1,
        SU2Delta[i_?(IntegerQ[#] || Head[#] === tmpsuni),
            j_?(IntegerQ[#] || Head[#] === tmpsuni)]^n_ :> SU2Delta[i, j],
      SU2Delta[fcsuni[i_], fcsuni[j_]]^n_ /; EvenQ[n] :> 3^(n/2),
      SU2Delta[i_, j_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]*
            expr_ :> (expr /. j -> i) /; (!FreeQ[expr, j] &&
                (FreeQ[expr, HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[___, j, ___]] || !(IntegerQ[i]||Head[i]===tmpsuni&&IntegerQ[i[[1]]])))(*Commented out 11/5-2003*)(*,
      Projection[i_Integer][fcsuni[j_]]^n_ /; EvenQ[n] :> 1,
      Projection[i_Integer][j_Integer] /; (i =!= j) :> 0,
      Projection[i_Integer][(fcsuni|fcexsuni)[j_Integer]]/;(*fixed problem when missing one head 9/10-2001*)
                                ((i/.tmpsuni->Identity) =!= (j/.tmpsuni->Identity)) :> 0*)};

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
      SU2F[a_, b_,              (*Added tmpsuni->Identity to catch ExplicitSUNIndex. 16/9-2002*)
            c_]^2 /; (Sort[Head /@ ({a, b, c}/.tmpsuni->Identity)] == {Integer, fcsuni, fcsuni}) :>
       2, SU3F[a_, b_,
            c_]^2 /; (Sort[Head /@ ({a, b, c}/.tmpsuni->Identity)] == {Integer, fcsuni, fcsuni}) :>
       3(*Bug fix 12/1 - 2000, was 6*),
    SU3D[a_, b_,
            c_]^2 /; (Sort[Head /@ ({a, b, c}/.tmpsuni->Identity)] == {Integer, fcsuni, fcsuni}) :>
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
              f}, (Join @@ twoselect[{a, b, c}, {d, e, f}])],
         (SU3D[a_, b_, c_]*SU3F[d_, e_, f_]*rest___) /;
         (Length[twoselect[{a, b, c}, {d, e, f}]] == 2) :> 0,
    (*contraction of one index - is this formula true for SU(3)?*)
    (*NO it's not - fixed, but correct formula not really useful, 16/12/1999*)
      (SU2F[a_, b_, c_]*SU2F[d_, e_, f_]*rest___) /;
      (Length[twoselect[{a, b, c}, {d, e, f}]] == 1) :>
      (((SU2Delta[#1, #3]*SU2Delta[#2, #4] - SU2Delta[#2, #3]*SU2Delta[#1, #4])& @@
      Join[Complement[{a, b, c}, {twoselect[{a, b, c}, {d, e, f}][[1, 1]]}],
           Complement[{d, e, f}, {twoselect[{a, b, c}, {d, e, f}][[1, 1]]}]])*
            sig2[{a, b, c}, {d, e, f}])*rest,
      (SU2F[a_, b_, c_]^2) /;
      (Length[twoselect[{a, b, c}, {a, b, c}]] == 1) :>
      (((SU2Delta[#1, #3]*SU2Delta[#2, #4] - SU2Delta[#2, #3]*SU2Delta[#1, #4])& @@
      Join[Complement[{a, b, c}, {twoselect[{a, b, c}, {a, b, c}][[1, 1]]}],
           Complement[{a, b, c}, {twoselect[{a, b, c}, {a, b, c}][[1, 1]]}]])*
            sig2[{a, b, c}, {a, b, c}]),
      (*Integers should already be sorted and put first -
        the following should also take care of SU2F[a_integer, c_integer,
              d_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex]^2, ...*)
      SU2F[a_, b_Integer, c_Integer] :>
      (-1)^(Complement[{1, 2, 3}, {b, c}][[1]] + 1)*
      (SU2Delta[#, a] & @@ (Complement[{1, 2, 3}, {b, c}])),
      SU2F[a_Integer, b_Integer, c_] :>
      (-1)^(Complement[{1, 2, 3}, {a, b}][[1]] + 1)*
      (SU2Delta[#, c] & @@ (Complement[{1, 2, 3}, {a, b}])),
      SU2F[a_, (b:tmpsuni[_Integer]), (c:tmpsuni[_Integer])] :>
      (-1)^(Complement[{1, 2, 3}, {b[[1]], c[[1]]}][[1]] + 1)*
      (SU2Delta[#, a] & @@ (Complement[tmpsuni/@{1, 2, 3}, {b, c}])),
      SU2F[(a:tmpsuni[_Integer]), (b:tmpsuni[_Integer]), c_] :>
      (-1)^(Complement[{1, 2, 3}, {a[[1]], b[[1]]}][[1]] + 1)*
      (SU2Delta[#, c] & @@ (Complement[tmpsuni/@{1, 2, 3}, {a, b}])) };

      
$SUNRules =
    Join[$SUNDeltaRules, $SUNDFRules, $SU3FReduceList, $SU3DReduceList];

(*Completeness of SU(N) - see Gasser & Leutwyler 1985*)
$SUNCompletenessRules = {HoldPattern[
        NM[UTrace1[NM[f___,
	UMatrix[UGenerator[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[j_]]], a___]], 
          UTrace1[NM[g___,
	  UMatrix[UGenerator[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[j_]]], b___]]]] :> 
      2 UTrace[NM[a, f, b, g]] - 2/fcsunn NM[UTrace[NM[a, f]], UTrace[NM[b, g]]], 
    HoldPattern[
        Times[UTrace1[NM[f___,
	UMatrix[UGenerator[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[j_]]], a___]], 
          UTrace1[NM[g___,
	  UMatrix[UGenerator[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[j_]]], b___]]]] :> 
      2 UTrace[NM[a, f, b, g]] + -2/fcsunn NM[UTrace[NM[a, f]], UTrace[NM[b, g]]], 
    HoldPattern[
        UTrace1[NM[f___,
	UMatrix[UGenerator[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[j_]]], a___]]^2] :> 
      2 UTrace[NM[a, f, a, f]] - 2/fcsunn NM[UTrace[NM[a, f]], UTrace[NM[a, f]]], 
    HoldPattern[
        UTrace1[NM[f___,
	UMatrix[UGenerator[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[j_]]], a___, 
            UMatrix[UGenerator[HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex[j_]]], b___]]] :>
	    -2/fcsunn UTrace[
            NM[a, b, f]] + 2NM[UTrace[NM[a]], UTrace[NM[b, f]]]};

applyCompletenessRules[expr_, opts___Rule] := expr /. $SUNCompletenessRules /.
  fcsunn -> (fcsunn /. Flatten[{opts}] /. Options[SUNReduce]) /.
(UTrace1[]|UTrace[]) ->
  HighEnergyPhysics`Phi`Objects`Private`gaugedimcheck[SUNReduce, opts, expr];

SUNReduce2[aa_, opts___] /;
((fcexpt /. Flatten[{opts}] /. Options[SUNReduce]) == False) :=
Block[{bb, cc},VerbosePrint[2, "Applying reduction rules on ", StandardForm[aa]];
 VerbosePrint[3, "Reduction rules are:\n", $SUNRules];
      bb =
      (*Commented out 11/5-2003*)(*If[(RemoveIntegerIndices /. Flatten[{opts}] /.  Options[SUNReduce]),
          aa,*)
          aa (*/. NTo3Rules1*) (*7/4.2002*) /. NTo3Rules3(*]*) /.

      NTo3Rules2[(fcsunn /. Flatten[{opts}] /.Options[SUNReduce])];

      VerbosePrint[2, "Entered SUNReduce2; expression is:\n", StandardForm[bb]];

      cc = bb /. NTo3iRules2[(fcsunn /. Flatten[{opts}] /. Options[SUNReduce])];

      VerbosePrint[2, "After NTo3iRules2, expression is:\n", StandardForm[cc]];
  
      (*Changed below, 13/5-2003, to have $SU3FReduceList, $SU3DReduceList from
        $SUNRules catch ExplicitSUNIndex[1], ... *)
      cc /. ($SUNRules/.(fcsuni | fcexsuni | UIndex)[iii_Integer]->tmpsuni[iii])];

SUNReduce[aa_, opts___] /;
    (((fcexpt /. Flatten[{opts}] /. Options[SUNReduce]) == False) &&
    (FullReduce /. Flatten[{opts}] /. Options[SUNReduce]) === True) :=
Block[{op},
   op=Join[{FullReduce->False},
   Select[Flatten[{opts}],FreeQ[#,FullReduce,Infinity,Heads->True]]];
   VerbosePrint[2, "Iterating with options ", op];
   FixedPoint[(SUNReduce[applyCompletenessRules[
	 ExpandAll[#],opts],
               Sequence@@op]&),aa]
];

SUNReduce[aa_Plus, opts___] := SUNReduce[#, opts] & /@ aa;

   
(*Added 7/6-2001 - might slow down too much ? ... - well, yes it did*)      
(*SUNReduce[Times[
aa:((_?((FreeQ[#,fcsuni|SU2Delta|SU3Delta|fcsundel|SU2F|SU3F|SU3D|fcsunf|fcsund]===True)&))..),
bb:((_?((FreeQ[#,fcsuni|SU2Delta|SU3Delta|fcsundel|SU2F|SU3F|SU3D|fcsunf|fcsund]===False)&))..)],
opts___] :=
Times[aa]*SUNReduce[Times[bb]];*)

SUNReduce[aa_, opts___] /; ((fcexpt /. Flatten[{opts}] /.
              Options[SUNReduce]) == False && (FullReduce /. Flatten[{opts}] /.
                    Options[SUNReduce]) =!= True) := (VerbosePrint[2,
        "Will use reduction rules"];
      Collect[VerbosePrint[2, "Collecting"];
            SUNReduce1[
              Expand[aa](*Change 5/3 - 1999 -
             FA likes to use indices more than twice in a product*)
 (*Changed again 16/12/1999*)(*fcsundel[fcsuni[i_?! NumberQ], fcsuni[i_!NumberQ]]*)/.
                fcsundel[
                    fcsuni[i_?((NumberQ[#] == False &&
                                FreeQ[$ConstantIsoIndices, #]) &)],
                    fcsuni[i_?((NumberQ[#] == False &&
                                FreeQ[$ConstantIsoIndices, #]) &)]] :>
(fcsunn /. Flatten[{opts}] /. Options[SUNReduce])^2 - 1,
              opts], {_fcfad, _SU2Delta, _SU2F, _SU2D}] /.
             tmpsuni -> fcsuni /.{faso[_Integer,___] ->
             1, faso[(fcsuni|fcexsuni)[_Integer],___] -> 1});

SUNReduce[aa_,
        opts___] /; ((fcexpt /. Flatten[{opts}] /.
              Options[SUNReduce]) == True) := (VerbosePrint[2,
        "Will sum explicitly"];
      Collect[VerbosePrint[2, "Collecting"];
            SUNReduce1[
                      Expand[aa /.
                          Plus[bb_, bbb__] /; FreeQ[{bb, bbb}, fcsuni] ->
                            tempplus[ttt[Plus[bb, bbb]] /. Plus -> ppl]],
                      opts] /. Plus -> ppl /. tempplus[cc___] -> cc /.
                ppl -> Plus /.
    ttt -> Together, {_fcfad, _SU2Delta, _SU3Delta, _SU2F, _SU3F, _SU3D}] /.
          tmpsuni -> fcsuni /.
          faso[_Integer,___]-> 1);
                  

$SymSUNDFRules1 = {
   Times[dd___, (sud:(fcsund|SU3D))[a_, b_, c_], d___] :>
   Block[{inds0, originds, originds0, originds1, inds},
    originds0 = {a, b, c} /. fcsuni -> Identity;
    inds0 =(*Select instead of Intersection is used to preserve the order*)
        Select[Sort[Cases[{dd, d}, fcqf[___, fcsuni[_], ___], Infinity,
          Heads -> True]] /. fcqf[___, fcsuni[e_], ___] -> e, (MemberQ[originds0, #]) &];
    originds1 = Select[originds0, (MemberQ[inds0, #]) &];
    originds = Join[Complement[originds0, originds1], originds1];
    inds = Join[Complement[originds, inds0], inds0];
    If[Length[inds0] > 1 && Length[inds] < 4,
       sud[a, b, c]*(Times[dd, d] /. ((Rule @@ #) & /@ Transpose[{originds, inds}])),
       sud[a, b, c]*Times[dd, d]]],
   Times[dd___, (suf:(fcsunf|SU2F|SU3F))[a_, b_, c_], d___] :>
   Block[{inds0, originds, originds0, originds1, inds},
    originds0 = {a, b, c} /. fcsuni -> Identity;
    inds0 =(*Select instead of Intersection is used to preserve the order*)
        Select[Sort[Cases[{dd, d}, fcqf[___, fcsuni[_], ___], Infinity,
          Heads -> True]] /. fcqf[___, fcsuni[e_], ___] -> e, (MemberQ[originds0, #]) &];
    originds1 = Select[originds0, (MemberQ[inds0, #]) &];
    originds = Join[Complement[originds0, originds1], originds1];
    inds = Join[Complement[originds, inds0], inds0];
    If[Length[inds0] > 1 && Length[inds] < 4,
      sig1[originds, inds]*
       suf[a, b, c]*
         (Times[dd, d] /. ((Rule @@ #) & /@ Transpose[{originds, inds}])),
       suf[a, b, c]*Times[dd, d]]]
        };

$SymSUNDFRules2 = {
   Times[dd___, (sud:(fcsund|SU3D))[a_, b_, c_], d___] :>
   Block[{inds0, originds, originds0, originds1, inds},
    originds0 = {a, b, c} /. fcsuni -> Identity;
    inds0 =(*Select instead of Intersection is used to preserve the order*)
        Select[Cases[{dd, d}, UMatrix[UGenerator[fcsuni[_]]], Infinity,
        Heads -> True] /.
      UMatrix[UGenerator[fcsuni[e_]]] -> e, (MemberQ[originds0, #]) &];
    originds1 = Select[originds0, (MemberQ[inds0, #]) &];
    originds = Join[Complement[originds0, originds1], originds1];
    inds = Join[Complement[originds, inds0], inds0];
    If[Length[inds0] > 1 && Length[inds] < 4,
       sud[a, b, c]*(Times[dd, d] /. ((Rule @@ #) & /@ Transpose[{originds, inds}])),
       sud[a, b, c]*Times[dd, d]]],
   Times[dd___, (suf:(fcsunf|SU2F|SU3F))[a_, b_, c_], d___] :>
   Block[{inds0, originds, originds0, originds1, inds},
    originds0 = {a, b, c} /. fcsuni -> Identity;
    inds0 =(*Select instead of Intersection is used to preserve the order*)
        Select[Cases[{dd, d}, UMatrix[UGenerator[fcsuni[_]]], Infinity,
        Heads -> True] /.
      UMatrix[UGenerator[fcsuni[e_]]] -> e, (MemberQ[originds0, #]) &];
    originds1 = Select[originds0, (MemberQ[inds0, #]) &];
    originds = Join[Complement[originds0, originds1], originds1];
    inds = Join[Complement[originds, inds0], inds0];
    If[Length[inds0] > 1 && Length[inds] < 4,
      sig1[originds, inds]*
       suf[a, b, c]*
         (Times[dd, d] /. ((Rule @@ #) & /@ Transpose[{originds, inds}])),
       suf[a, b, c]*Times[dd, d]]]
        };


SUDFSymmetrize[exp_] := exp /. $SymSUNDFRules1 /. $SymSUNDFRules2;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Cleaning up indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Sorting indices *)

(*Clean up contracted indices*)
sortRules1 = {(nm : (Times | NM))[a__, b__] :>
  ((nm[a, b] /. ((Rule @@ #) & /@ 
                  Transpose[{Cases[{a}, fcli[__], Infinity, 
                        Heads -> True], 
                      Sort[Cases[{a}, fcli[__], Infinity, 
                          Heads -> True]]}]))) /; (Sort[
                Cases[{a}, fcli[__], Infinity, Heads -> True]] === 
              Sort[Cases[{b}, fcli[__], Infinity, Heads -> True]] && 
            Cases[{a}, fcli[__], Infinity, Heads -> True] =!= 
              Sort[Cases[{a}, fcli[__], Infinity, 
                  Heads -> True]]), (nm : (Times | NM))[a__, 
        b__] :> (ablors = 
            Intersection[
              alors = Cases[{a}, fcli[__], Infinity, Heads -> True], 
              blors = Cases[{b}, fcli[__], Infinity, Heads -> True]]; 
          test = (alors =!= {} && blors =!= {} && ablors === {} && 
                Join[alors, blors] =!= Sort[Join[alors, blors]] && 
                Sort[alors] === Sort[Join[Union[alors], Union[alors]]] && 
                Sort[blors] === Sort[Join[Union[blors], Union[blors]]]); 
          If[test, 
            nm[a, b] /. ((Rule @@ #) & /@ 
                  Transpose[{Join[alors, blors], Sort[Join[alors, blors]]}]), 
            nm[a, b]]) /; (Cases[{a}, fcli[__], Infinity, 
                Heads -> True] =!= {} && 
            Cases[{b}, fcli[__], Infinity, Heads -> True] =!= {} && 
            Join[Cases[{a}, fcli[__], Infinity, Heads -> True], 
                Cases[{b}, fcli[__], Infinity, Heads -> True]] =!= 
              Sort[Join[Cases[{a}, fcli[__], Infinity, Heads -> True], 
                  Cases[{b}, fcli[__], Infinity, 
                    Heads -> True]]] && ((Sort[#] === 
                      Sort[Join[Union[#], Union[#]]]) &[
                Cases[{a}, fcli[__], Infinity, 
                  Heads -> 
                    True]]) && ((Sort[#] === 
                      Sort[Join[Union[#], Union[#]]]) &[
                Cases[{b}, fcli[__], Infinity, Heads -> True]]))};

sortRules2 = sortRules1 //. {HoldPattern[fcli] -> fcsuni, fcli -> fcsuni};

sortRules3 = sortRules1 //. {HoldPattern[fcli] -> UIndex, fcli -> UIndex};

(* Cleaning up contracted Lorentz indices. co is the number of factors that will be
   considered; in e.g. u[mu2] gamma[mu1] gamma[mu1] u[mu2], two will suffice. *)

lorentzCleanup[exp_] := (exp1 = exp; 
      Do[ders = 
         fccombs[
         Union[Cases[exp, fcli[__], Infinity, Heads -> True]], co];
         ruls = ((RuleDelayed1[Power1[PatternTest1[a_, func[
         And @@ Table[Count1[yo, #[[i]], Infinity, Heads -> True] == 1, {i, co}]]], 2], 
         Condition1[ReplaceAll1[a^2, Map1[(Rule1 @@ #) &, 
         Transpose1[{Cases1[{a}, fcli[__], Infinity, Heads -> True], Sort[#]}]]], 
         SameQ1[Sort1[Cases1[{a}, fcli[__], Infinity, Heads -> True]], Sort1[#]] && 
         UnsameQ1[Cases1[{a}, fcli[__], Infinity, Heads -> True],
	 Sort1[#]]]]) & /@ ders) /. PatternTest1 -> PatternTest /. 
         Count1 -> Count /. yo -> # /. func -> Function /. 
         RuleDelayed1 -> RuleDelayed /. ReplaceAll1 -> ReplaceAll /. 
         Condition1 -> Condition /. UnsameQ1 -> UnsameQ /. SameQ1 -> SameQ /. 
         Cases1 -> Cases /. Sort1 -> Sort /. ReplaceAll1 -> ReplaceAll /.
	 Transpose1 -> Transpose /. Rule1 -> Rule /. Map1 -> Map /. Power1 -> Power; 
         exp1 = exp1 /. ruls, {co, 2, 2}]; exp1);

(* In case we would want to do the same for NM products*)

(*lorentzCleanupNM[exp_] := (exp1 = exp;
      Do[ders =
          fccombs[
            Union[Cases[exp, fcli[__], Infinity, Heads -> True]], co];
        ruls = ((RuleDelayed1[
         NM[PatternTest1[a_,
         func[And @@ Table[Count1[yo, #[[i]],
         Infinity, Heads -> True] == 1, {i,
         co}]]], PatternTest1[b_,
         func[And @@ Table[Count1[yo, #[[i]],
         Infinity, Heads -> True] == 1, {i,
         co}]]]], Condition1[
         ReplaceAll1[NM[a, b],
         Map1[(Rule1 @@ #) &,
         Transpose1[{Cases1[{b}, fcli[__],
         Infinity, Heads -> True], Sort[#]}]]],
         SameQ1[Sort1[Cases1[{b}, fcli[__],
         Infinity, Heads -> True]], Sort1[#]] &&
         UnsameQ1[Cases1[{b}, fcli[__],
         Infinity, Heads -> True],
         Sort1[#]]]]) & /@ ders) /.
         PatternTest1 -> PatternTest /.
         Count1 -> Count /. yo -> # /.
                                    func -> Function /.
                                  RuleDelayed1 -> RuleDelayed /.
                                ReplaceAll1 -> ReplaceAll /.
                              Condition1 -> Condition /.
                            UnsameQ1 -> UnsameQ /. SameQ1 -> SameQ /.
                        Cases1 -> Cases /. Sort1 -> Sort /.
                    ReplaceAll1 -> ReplaceAll /. Transpose1 -> Transpose /.
                Rule1 -> Rule /. Map1 -> Map /. Power1 -> Power;
        exp1 = exp1 /. ruls, {co, 2, 2}]; exp1);*)

(* Cleaning up contracted SU(N) indices. co is the number of factors that will be
   considered; in e.g. u[mu2] gamma[mu1] gamma[mu1] u[mu2], two will suffice. *)

sunCleanup[exp_] := (exp1 = exp; 
      Do[ders = 
         fccombs[
         Union[Cases[exp, fcsuni[__], Infinity, Heads -> True]], co];
         ruls = ((RuleDelayed1[Power1[PatternTest1[a_, func[
         And @@ Table[Count1[yo, #[[i]], Infinity, Heads -> True] == 1, {i, co}]]], 2], 
         Condition1[ReplaceAll1[a^2, Map1[(Rule1 @@ #) &, 
         Transpose1[{Cases1[{a}, fcsuni[__], Infinity, Heads -> True], Sort[#]}]]], 
         SameQ1[Sort1[Cases1[{a}, fcsuni[__], Infinity, Heads -> True]], Sort1[#]] && 
         UnsameQ1[Cases1[{a}, fcsuni[__], Infinity, Heads -> True],
	 Sort1[#]]]]) & /@ ders) /. PatternTest1 -> PatternTest /. 
         Count1 -> Count /. yo -> # /. func -> Function /. 
         RuleDelayed1 -> RuleDelayed /. ReplaceAll1 -> ReplaceAll /. 
         Condition1 -> Condition /. UnsameQ1 -> UnsameQ /. SameQ1 -> SameQ /. 
         Cases1 -> Cases /. Sort1 -> Sort /. ReplaceAll1 -> ReplaceAll /.
	 Transpose1 -> Transpose /. Rule1 -> Rule /. Map1 -> Map /. Power1 -> Power; 
         exp1 = exp1 /. ruls, {co, 2, 2}]; exp1);

(* Sort differentation Lorentz indices. *)

SetAttributes[nmm, Flat];

sortDiff := (# //. 
                FieldDerivative[f_, x_, fcli[li_]] :> 
                  nmm[Derivativex[x, fcli[li]], f] /. 
              nmm[dl : (Derivativex[x_, fcli[_]] ..)] :> 
                nmm @@ Sort[{dl}] //. 
            nmm[Derivativex[x_, fcli[li_]], f_] :> 
              FieldDerivative[f, x, fcli[li]] /; 
                FreeQ[f, Derivativex, Infinity] /. nmm -> NM)&;

(* Change order of differentation if appropriate. *)

lorentzDerCleanupRules = {HoldPattern[
        a__ UTrace1[
            NM[b___, 
              FieldDerivative[
                fd : (FieldDerivative[f_, x_, fcli[li2_]] | 
                      Adjoint[
                        FieldDerivative[f_, x_, fcli[li2_]]]), x_, 
                fcli[li1_]], 
              c___]]] :> (a UTrace1[
                NM[b, FieldDerivative[
                    fd /. {li1 -> li2, li2 -> li1}, x, fcli[li2]], 
                  c]] /. {li1 -> li2, li2 -> li1}) /; 
        Cases[{a, b, c}, fcli[__], Infinity, 
            Heads -> True] === {fcli[li1], fcli[li2]}, 
    HoldPattern[
        a__ UTrace1[
            NM[b___, 
              Adjoint[FieldDerivative[
                  fd : (FieldDerivative[f_, x_, fcli[li2_]] | 
                        Adjoint[
                          FieldDerivative[f_, x_, fcli[li2_]]]), 
                  x_, fcli[li1_]]], 
              c___]]] :> (a UTrace1[
                NM[b, Adjoint[
                    FieldDerivative[
                      fd /. {li1 -> li2, li2 -> li1}, x, fcli[li2]]], 
                  c]] /. {li1 -> li2, li2 -> li1}) /; 
        Cases[{a, b, c}, fcli[__], Infinity, 
            Heads -> True] === {fcli[li1], fcli[li2]}, 
    HoldPattern[
        NM[a__, UTrace1[
            NM[b___, 
              FieldDerivative[
                fd : (FieldDerivative[f_, x_, fcli[li2_]] | 
                      Adjoint[
                        FieldDerivative[f_, x_, fcli[li2_]]]), x_, 
                fcli[li1_]], c___]]]] :> (NM[a, 
              UTrace1[NM[b, 
                  FieldDerivative[
                    fd /. {li1 -> li2, li2 -> li1}, x, fcli[li2]], 
                  c]]] /. {li1 -> li2, li2 -> li1}) /; 
        Cases[{a, b, c}, fcli[__], Infinity, 
            Heads -> True] === {fcli[li1], fcli[li2]}, 
    HoldPattern[
        NM[a__, UTrace1[
            NM[b___, 
              Adjoint[FieldDerivative[
                  fd : (FieldDerivative[f_, x_, fcli[li2_]] | 
                        Adjoint[
                          FieldDerivative[f_, x_, fcli[li2_]]]), 
                  x_, fcli[li1_]]], c___]]]] :> (NM[a, 
              UTrace1[NM[b, 
                  Adjoint[
                    FieldDerivative[
                      fd /. {li1 -> li2, li2 -> li1}, x, fcli[li2]]], c]]] /.
          {li1 -> li2, li2 -> li1}) /; 
        Cases[{a, b, c}, fcli[__], Infinity, 
            Heads -> True] === {fcli[li1], fcli[li2]}}

lorentzDerCleanup := ((# /. lorentzDerCleanupRules) &);

(* Field strength tensors *)

fstrules = {n : 
          (FieldStrengthTensorFull|FieldStrengthTensor)[fcli[mu1_], 
            HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[p__, 
                HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[mu2_]][
              x_], x_, f___] :> (-n /. {mu1 -> mu2, mu2 -> mu1}) /; 
          Sort[HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex /@ {mu1, 
                  mu2}] =!= \
(HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex /@ {mu1, mu2}), 
      n : NM[a___, 
            (FieldStrengthTensorFull|FieldStrengthTensor)[fcli[mu1_], 
              HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[p__, 
                  HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[mu2_]][
                x_], x_, f___], b___] :> (n /. {mu1 -> mu2, mu2 -> mu1}) /; 
          Sort[Cases[{a, b}, 
                  HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[
                    mu1 | mu2], Infinity, Heads -> True]] == 
              Sort[HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex /@ \
{mu1, mu2}] && 
            Cases[{a, b}, 
                HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[
                  mu1 | mu2], Infinity, Heads -> True] =!= 
              Sort[HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex /@ \
{mu1, mu2}]};

fstCleanup := ((# //. fstrules) &);

(* Final sorting function *)

SortIndices = ((Expand[NMExpand[#]] /. sortRules1 /. sortRules2 /. sortRules3 // 
                sortDiff// lorentzCleanup // sunCleanup // lorentzDerCleanup // fstCleanup)&);

(*------------------------------------------------------------------------------*)


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

(* Label factors in nested structures like NM[NM[..]+NM[..], NM[..]+NM[..]].
   The problem is that we cannot use identical indices on both sides. Thus,
   we label the NM's sequentially and take one label out on each run. *)

(*Put on an isowait[..] tag if an NM product is of the above form*)
(*Applied only once*)
isowaitrules1 =
(aa : HoldPattern[
(NM | NM1 | NM2)[(Plus(*|UTrace1*))[
  (___*(NM | NM1 | NM2)[__?(FreeQ[#, _(NM | NM1 | NM2)] &)] |
(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &)] |
  _?(FreeQ[#, (NM | NM1 | NM2)[__]] &)) ..] ..]]) :>
(waitcounter = 0; ((waitcounter++;
 Replace[#, (nm1 : NM | NM1 | NM2)[b__] :>
 nm1[b, isowait[waitcounter]] /; FreeQ[{b}, isowait[__]], {1, 3}]) &) /@ aa)/;
 !FreeQ[aa,fcsuni[_]];
                  

(*remove the highest tag*)
(*Applied only once*)
isowaitrules0 =
aa : HoldPattern[
(NM | NM1 | NM2)[(Plus(*|UTrace1*))[(___*(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &), isowait[__]] |
(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &), isowait[__]] |
  _?(FreeQ[#, (NM | NM1 | NM2)[__]] &)) ..] ..,___]] :>
(aa /. (isowait[(Max[(#[[1]]) & /@ Cases[aa, isowait[__], {3, 4}]])] ->
                                    (seq[])) /. seq -> Sequence)/;
 !FreeQ[aa,fcsuni[_]];

(*If an NM product is of the above form, already has a tag and ends with
a factor that has no NM's, remove the highest tag and increment isoexterndummycounter by one*)
(*Applied repeatdly (with IndicesCleanup1)*)
isowaitrules2 =
aa : HoldPattern[
(NM | NM1 | NM2)[___,
(Plus(*|UTrace1*))[(___*(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &), isowait[__],___] |
(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &),
  isowait[__],___] | _?(FreeQ[#, (NM | NM1 | NM2)[__]] &)) ..] ..,
  (_?((FreeQ[#,isowait[__]] &&
  FreeQ[#,fcsuni[_?(FreeQ[#,IsoDummy|IsoExternDummy|IsoInternDummy]&)]])&))..]] :>
(aa /. (isowait[(Max[(#[[1]]) & /@ Cases[aa, isowait[__], {3, 4}]])] ->
(isoexterndummycounter = Max[Union[{0},Cases[{aa}, fcsuni[IsoExternDummy][_], Infinity,
                        Heads -> True]] /.
                      fcsuni[IsoExternDummy][na_] -> na]+1;seq[])) /. seq -> Sequence)/;
 !FreeQ[aa,fcsuni[_]];

(*Same as above but no check fo last factor NM's*)
(*Hmm, the way it is applied, it seems to be redundant. 26/8-2001*)
isowaitrules3 =
aa : HoldPattern[
(NM | NM1 | NM2)[___,
(Plus(*|UTrace1*))[(___*(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &), isowait[__],___] |
(NM | NM1 | NM2)[__?(FreeQ[#, (NM | NM1 | NM2)[__]] &),
  isowait[__],___] | _?(FreeQ[#, (NM | NM1 | NM2)[__]] &)) ..] ..,
  (_?((FreeQ[#,isowait[__]])&))..]] :>
(aa /. (isowait[(Max[(#[[1]]) & /@ Cases[aa, isowait[__], {3, 4}]])] ->
(isoexterndummycounter = Max[Union[{0},Cases[{aa}, fcsuni[IsoExternDummy][_], Infinity,
                        Heads -> True]] /.
                      fcsuni[IsoExternDummy][na_] -> na]+1;seq[])) /. seq -> Sequence)/;
 !FreeQ[aa,fcsuni[_]];

(* Rules for UTraces. This is the one exception to only allowing one level of nesting.
   The strategy is to take out SUNIndices in UTraces via SUNDeltas, leaving a dummy
   index without SUNIndex head in the UTrace and then later substitute back in the index *)

tracerule1 = {

NM[a___, UTrace1[b_?(!FreeQ[#,fcli]&)], c___]*d_ :>
  (Times @@ (inxlist=(fcsundel[#, fcsuni[suninx[Unique["dum"]]]] & /@
           Select[
             Cases[Cases[{a,UTrace1[b],c,d}, UTrace1[_], Infinity, Heads->True],
						   fcsuni[_], Infinity, Heads->True],
           FreeQ[#,suninx]&])))*
  (*Lorentz stuff added 23/9-2001*)
  (Times @@ (linxlist=(fcpa[#, fcli[linx[Unique["dum"]]]] & /@
           Select[
             Cases[Cases[{a,UTrace1[b],c,d}, UTrace1[_], Infinity, Heads->True],
						   fcli[_], Infinity, Heads->True],
           FreeQ[#,linx]&])))(**)*
      (NM[a, UTrace1[b], c] /.
			(UTrace1[e_] :>
        UTrace1[e/.((Rule[#[[1]], suninx0@@#[[2,1]]])& /@ inxlist)/.
                   (*PartialD automatically put on head LorentzIndex which screws up things*)
                   ((Rule[fcpd[#[[1]]],partd[linx0@@#[[2,1]]]])& /@ linxlist)/.
                   ((Rule[#[[1]],linx0@@#[[2,1]]])& /@ linxlist)]))*(d),

(nm:(NM|Times))[a___, UTrace1[b_?(!FreeQ[#,fcli]&)], c___] :>
  (Times @@ (inxlist=(fcsundel[#, fcsuni[suninx[Unique["dum"]]]] & /@
           Select[
             Cases[Cases[{a,UTrace1[b],c}, UTrace1[_], Infinity, Heads->True],
						   fcsuni[_], Infinity, Heads -> True],
           FreeQ[#,suninx]&])))*
  (Times @@ (linxlist=(fcpa[#, fcli[linx[Unique["dum"]]]] & /@
           Select[
             Cases[Cases[{a,UTrace1[b],c}, UTrace1[_], Infinity, Heads->True],
						   fcli[_], Infinity, Heads -> True],
           FreeQ[#,linx]&])))*
      (nm[a, UTrace1[b], c]/.
			UTrace1[e_]:>
        UTrace1[e/.((Rule[#[[1]],suninx0@@#[[2,1]]])& /@ inxlist)/.
                   ((Rule[fcpd[#[[1]]],partd[linx0@@#[[2,1]]]])& /@ linxlist)/.
                   ((Rule[#[[1]],linx0@@#[[2,1]]])& /@ linxlist)])};

tracerule2 = {fcsundel[fcsuni[suninx[f_]], j_]*
    a_?((!FreeQ[#, suninx0[f_]])&) :> (a /. suninx0[f] -> j),
    fcpa[fcli[linx[f_]], j_]*a_?((!FreeQ[#, linx0[f_]])&) :>
    (a /. linx0[f] -> j)};

tracerule3 = {fcsundel[fcsuni[suninx[_]], _]->1, fcpa[fcli[linx[_]], _]->1, partd->fcpd};

(* Comparison between factors: *)

(*Iso - indices*)
  dummyrulesiso1a =
{(nm1 : NM | NM1 | NM2)[
   fac___, gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___][x_],
   facc___] :>
    (( (nm1[fac, gg[fi, a, la][x], facc] /.
            a -> fcsuni[IsoExternDummy][isoexterndummycounter]) /;
              (!FreeQ[{fac, facc}, a] &&  FreeQ[{fac, facc}, IsoExternDummy] &&
              FreeQ[{fac, facc}, _isowait] &&
              FreeQ[{fac, facc},
                (NM | NM1 | NM2)[___, _?((FreeQ[{##}, isomult] &&
                               FreeQ[{##}, _HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] ===
                               False)&), ___]
		           ]
		       )  ) )
		 };
(*Iso - indices*)
  dummyrulesiso1b = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___],
             facc___] :> (((nm1[fac,
              gg[fi, a, la], facc] /.
            a -> fcsuni[IsoExternDummy][isoexterndummycounter]) /; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, IsoExternDummy] && FreeQ[{fac, facc}, _isowait] &&
              FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___, _?((FreeQ[{##}, isomult] &&
                   FreeQ[{##}, _HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] ===
                   False) &), ___]

		]) ))};
(*Iso - indices*)
  dummyrulesiso2 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___][
              x_], facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc] /.
              a -> fcsuni[IsoExternDummy][
              Max[Union[{0},Cases[{fac, facc}, fcsuni[IsoExternDummy][_], Infinity,
                        Heads -> True]] /.
                      fcsuni[IsoExternDummy][na_] -> na] + 1]) /;
		(! FreeQ[{fac, facc}, a]  &&
           FreeQ[{fac, facc}, _isowait] && FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___?(FreeQ[{##},isomult]&)]]),
	      (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex, la___],
             facc___] :> (nm1[fac,
              gg[fi, a, la], facc] /.
            a -> fcsuni[IsoExternDummy][
                Max[Union[{0},Cases[{fac, facc}, fcsuni[IsoExternDummy][_], Infinity,
                        Heads -> True]] /.
                      fcsuni[IsoExternDummy][na_] -> na] + 1]) /;
		(! FreeQ[{fac, facc}, a]  &&
           FreeQ[{fac, facc}, _isowait] && FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___?(FreeQ[{##},isomult]&)]]) };

(*Derivative indices*)
  dummyrulesder1a = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
                fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
                la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, fcpd[a], la][x], facc] /.
            a -> fcli[DerivativeExternDummy][1]) /; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, DerivativeExternDummy] &&
              FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
   (ff_[fcpd[fcli[a_]], fi___,fcli[a_],if___][x_]) :> (ff[
    fcpd[fcli[DerivativeInternDummy][0]],
     fi,fcli[DerivativeInternDummy][0],if][x])/;
    (FreeQ[a, DerivativeInternDummy])};
(*Derivative indices*)
  dummyrulesder1b = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
              fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
              la___], facc___] :> (nm1[fac,
              gg[fi, fcpd[a], la], facc] /.
            a -> fcli[DerivativeExternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, DerivativeExternDummy] &&
              FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Derivative indices*)
  dummyrulesder2 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
                fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
                la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, fcpd[a], la][x], facc] /.
            a -> fcli[DerivativeExternDummy][
                Max[Cases[{fac, facc}, fcli[DerivativeExternDummy][_],
                        Infinity, Heads -> True] /.
                      fcli[DerivativeExternDummy][na_] -> na] + 1])/;
		      (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fac, facc}, DerivativeExternDummy] &&
              FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
	      (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___,
              fcpd[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex],
              la___], facc___] :> (nm1[fac,
              gg[fi, fcpd[a], la], facc] /.
            a -> fcli[DerivativeExternDummy][
                Max[Cases[{fac, facc}, fcli[DerivativeExternDummy][_],
                        Infinity, Heads -> True] /.
                      fcli[DerivativeExternDummy][na_] -> na] + 1])/;
		      (! FreeQ[{fac, facc}, a] && !
                FreeQ[{fac, facc}, DerivativeExternDummy] &&
              FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Lorentz indices*)
  dummyruleslor1 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                 la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc] /.
            a -> fcli[LorentzExternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, LorentzExternDummy] &&
              FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
	      (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
               la___], facc___] :> (nm1[fac,
              gg[fi, a, la], facc] /.
            a -> fcli[LorentzExternDummy][1])/; (! FreeQ[{fac, facc}, a] &&
              FreeQ[{fac, facc}, LorentzExternDummy] &&
              FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]])};
(*Lorentz indices*)
  dummyruleslor2 = {(nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                 la___][x_],
            facc___] :> (nm1[fac,
              gg[fi, a, la][x], facc] /.
            a -> fcli[LorentzExternDummy][
                Max[Cases[{fac, facc}, fcli[LorentzExternDummy][_], Infinity,
                        Heads -> True] /.
                      fcli[LorentzExternDummy][na_] -> na] + 1])/;
		      (!FreeQ[{fac, facc}, a] && !FreeQ[{fac, facc}, LorentzExternDummy] &&
              FreeQ[{fac, facc}, 
		(NM | NM1 | NM2)[___?(FreeQ[{##},lorentzmult]&)]]),
	      (nm1 : NM | NM1 | NM2)[fac___,
            gg_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
               la___],
            facc___] :> (nm1[fac,
              gg[fi, a, la], facc] /.
            a ->
              fcli[LorentzExternDummy][
                Max[Cases[{fac, facc}, fcli[LorentzExternDummy][_], Infinity,
                        Heads -> True] /.
                      fcli[LorentzExternDummy][na_] -> na] + 1])/;
		      (! FreeQ[{fac, facc}, a] && !FreeQ[{fac, facc}, LorentzExternDummy] &&
              FreeQ[{fac, facc}, 
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



(* Squared objects will not be caught by the previous rules (and we don't have
higher powers with one iso-spin index), so we need a special set of rules: *)

dummyrulessq1 = {
  (ff_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
   la___][x_])^2 :> (res = (ff[fi, fcsuni[IsoDummy][isodummycounter], la][x])^2;
   isodummycounter += 1; res) /; (FreeQ[a, IsoDummy]),
   
   (ff_[fi___, a_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
   la___])^2 :> (res = (ff[fi, fcsuni[IsoDummy][isodummycounter], la])^2;
   isodummycounter += 1; res)/; (FreeQ[a, IsoDummy]),
   
   (ff_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
   la___][x_])^2 :> (ff[fi, fcli[LorentzDummy][1], la][x])^2 /; (FreeQ[a, LorentzDummy]),
   
   (ff_[fi___, a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
   la___])^2 :> (ff[fi, fcli[LorentzDummy][1], la])^2/; (FreeQ[a, LorentzDummy]),
   
   (ff_[fcpd[fcli[a_]], fi__][x_])^2 :> (ff[
    fcpd[fcli[DerivativeExternDummy][1]], fi][x])^2/; (FreeQ[a, DerivativeExternDummy]),
    
   (ff_[fcpd[fcli[a_]], fi__])^2 :>
   (ff[fcpd[fcli[DerivativeExternDummy][1]], fi])^2 /; (FreeQ[a, DerivativeExternDummy])};
   
dummyrulessq2 = {
  (ff_[fi___, fcsuni[ld_][ll_], fcsuni[ln_], la___][x_])^2 :>
  (ff[fi, fcsuni[ld][ll], fcsuni[ln][ll + 1], la][x])^2,
  (ff_[fi___, fcsuni[ld_][ll_], fcsuni[ln_], la___])^2 :>
  (ff[fi, fcsuni[ld][ll], fcsuni[ln][ll + 1], la])^2,
  (ff_[fi___, fcli[ld_][ll_], fcli[ln_], la___][x_])^2 :>
  (ff[fi, fcli[ld][ll], fcli[ln][ll + 1], la][x])^2,
  (ff_[fi___, fcli[ld_][ll_], fcli[ln_], la___])^2 :>
  (ff[fi, fcli[ld][ll], fcli[ln][ll + 1], la])^2,
  (ff_[fcpd[fcli[ld_][ll_]], fcpd[fcli[ln_]], fi__][x_])^2 :>
  (ff[fcpd[fcli[ld][ll]], fcpd[fcli[ln][ll + 1]], fi][x])^2,
  (ff_[fcpd[fcli[ld_][ll_]], fcpd[fcli[ln_]], fi__])^2 :>
  (ff[fcpd[fcli[ld][ll]], fcpd[fcli[ln][ll + 1]], fi])^2};
  
(* This is to clean up products of QuantumField[
    PartialD[LorentzIndex[DerivativeInternDummy][0]], p, 
    LorentzIndex[DerivativeInternDummy][0]][x] *)
fixderindices1 = {NM->nmm1, NM1->nmm1}; 
fixderindices2 = {nmm1[a__]?((FreeQ[#, nmm1[__?(!FreeQ[#,
    fcli[DerivativeInternDummy][0]]&)]] && !FreeQ[#, DerivativeInternDummy])&) :>
    (derindcounter = Max[Union[{0},
    Cases[{a}, fcli[DerivativeInternDummy][_], Infinity, Heads->True] /. 
      fcli[DerivativeInternDummy] -> Identity]]; 
    nmm1 @@ ((# /. fcqf[
    fcpd[fcli[DerivativeInternDummy][0]], p__, 
    fcli[DerivativeInternDummy][0], r___] :>
    (++derindcounter; fcqf[fcpd[fcli[
    DerivativeInternDummy][derindcounter]], p, fcli[
    DerivativeInternDummy][derindcounter], r]))& /@ {a}))};
fixderindices3 = {nmm1->NM1}; 
  
allpatterns = (Blank | BlankSequence | BlankNullSequence | Pattern);

(*Expand traces of products of sums and products of sums of traces*)
fixtraceplus = {aa:HoldPattern[UTrace1[NM[___,
  Plus[___,_?(!FreeQ[#,HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex,
  Heads->True]&),___],___]]] :>
(VerbosePrint[1,"Found UTrace of NM Product of sums. Applying NMExpand"];
NMExpand[aa]),
aa:(NM[___, _?((!FreeQ[#, UTrace1, Heads -> True] &&
   !FreeQ[#,HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] &&
      !MatchQ[#, UTrace1[_] | _*UTrace1[_]]) &), ___]) :>
  (VerbosePrint[1,"Found NM Product of sums of UTraces. Applying NMExpand"];NMExpand[aa]),
aa:(Times[___, _?((!FreeQ[#, UTrace1, Heads -> True] &&
   !FreeQ[#,HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] &&
      !MatchQ[#, UTrace1[_] | _*UTrace1[_]]) &), ___]) :>
  (VerbosePrint[1,"Found Product of sums of UTraces. Applying Expand"];Expand[aa])};

(*Very cheap hacks to catch a few nested indices*)
cheapnesthackrule = {f_[fcli[li__]][x_] :> f[x, lihusk][fcli[li]] /; !MatchQ[x,fcli[__]],
HoldPattern[FieldStrengthTensor[HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[li1__],
  HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[p__, l:(HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[__]..)][x_], x_]] :>
  FieldStrengthTensor[fcli[li1], l, HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[p][x], x]};
cheapnesthackruleback = {f_[x_, lihusk][fcli[li__]] :> f[fcli[li]][x],
HoldPattern[FieldStrengthTensor[HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[li1_], l:(HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[__]..),
HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[p__][x_], x_]] :> FieldStrengthTensor[fcli[li1],
  HighEnergyPhysics`FeynCalc`QuantumField`QuantumField[p, l][x], x]
};

(* The final cleanup function *)

IndicesCleanup1[w_, opts___] :=
    w /.
		    dummyrulesiso1a /. dummyrulesiso1b /. dummyrulesiso2 /.
              dummyrulesder1a /. dummyrulesder1b /. dummyrulesder2 /.
    dummyruleslor1 /. dummyruleslor2 //.
      fixderindices1 //. fixderindices2 //. fixderindices3 /.
      {(nm1:NM|NM1|NM2)[fac___] :> nm1[fac,isomult] /;
         FreeQ[{fac},fcsuni[_?((FreeQ[#,IsoDummy|IsoExternDummy|IsoInternDummy]&&!UScalarQ[#])&)]] &&
         FreeQ[{fac},isomult],
       (nm1:NM|NM1|NM2)[fac___] :> nm1[fac,lorentzmult] /;
         FreeQ[{fac},fcsuni[_?((FreeQ[#,LorentzDummy|LorentzExternDummy|LorentzInternDummy|
         DerivativeExternDummy]&&!UScalarQ[#])&)]] && FreeQ[{fac},lorentzmult]}  /. isowaitrules2 ;


SetAttributes[NM1, Flat]; SetAttributes[NM2, Flat];

IndicesCleanup[ww_, opts___] := (

      declutr = False;
      
      w = ww /.
      
      (* If UTrace1 has been declared  UScalar, NMPower's of it will be Power's
         and contracted indices woun't be seen. Hack to fix this *)
      If[ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
        If[UScalarQ[UTrace1],declutr = True;
	VerbosePrint[2,"UndeclareUScalar[UTrace1]"];
	UndeclareUScalar[UTrace1]];
	Power[utr_UTrace1, po_] :> NM@@Table[utr,{po}],
	Power[utr_UTrace1, po_] :> NM@@Table[utr,{po}]] /.
      
      cheapnesthackrule/.fixtraceplus//.tracerule1/.isowaitrules1/.isowaitrules0;

      larul = {{}, {}, {}};

      Which[
      !FreeQ[w,fcli[_, D] | fcmom[_, D]],

        If[ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
          VerbosePrint[1,
            "Found occurence of D, using this as number of \
space-time dimensions for all momenta, Dirac gamma matrices and Lorentz \
indices"];
           larul = {fcli[llii_] -> fcli[llii, D],
            dg[dig_] -> dg[dig, D],
            fcmom[mo_] -> fcmom[mo, D]}];,


        !FreeQ[w, fcli[_, D] | fcmom[_, D]],
         If[ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],VerbosePrint[1,
            "Found occurence of D, using this as number of space-time \
dimensions for all momenta, Dirac gamma matrices and Lorentz indices"];
          larul = {fcli[llii_] -> fcli[llii, D], dg[dig_] -> dg[dig, D],
            fcmom[mo_] -> fcmom[mo, D]}];
	];

      isodummycounter = 1;
      isoexterndummycounter = 1;
      w1 = If[ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],
              VerbosePrint[1, "Using ExtendedCleanup->True"];
              w /. {SU2Delta -> su2delta1, SU2F -> su2f1,
                  SU3Delta -> su3delta1, SU3F -> su3f1, SU3D -> su3d1,
                  SUNDelta -> sundelta1, SUNF -> sunf1,
                  SUND -> sund1},
              VerbosePrint[1,
                  "Using ExtendedCleanup->False\nWill not work if mixed Times \
and NM products are present"]w] /.
       (*Cleaned up a bit below. 20/6-2003*)
       (((fcsuni[#] -> protectisoconstant[#])&) /@ (((#[[1]])&) /@
                    Cases[w, faso[_?((Head[#]=!=fcsuni)&),___], Infinity])) /.

          faso[fcsuni[a_], b___] -> faso[protectisoconstant[a], b];
      
      VerbosePrint[3,"Doing reduction on ", w1//StandardForm];            
      
      subres = FixedPoint[
      (If[FreeQ[#,isowait],w2=#/.isowaitrules3,w2=#];
          FixedPoint[(VerbosePrint[2, "Applying renaming rules"];
              IndicesCleanup1[#, opts]) &,
          If[FCleanup /. Flatten[{opts}] /. Options[IndicesCleanup],

	    VerbosePrint[2, "Using FCleanup->True"];
            VerbosePrint[2,
              "Renaming product functions and protecting constants"];
            w2 /. {fcsuni[a_] :> protectisoconstant[a]/; (UScalarQ[a] || !
                                  FreeQ[$ConstantIsoIndices, a]),
                        fcli[a_] :> protectliconstant[a] /; UScalarQ[a]} /.
                    dummyrulesf3 /. dummyrulesf2 /.
                dummyrulesf1 /. {NM -> NM1,
                Times -> NM1,
                DOT -> NM2,(*change 19/1 - 1999*)
                  Power[a_, b_ /; b > 0 && IntegerQ[b]] :>
                  NM1 @@ Table[a, {ddum, 1, b}]},

            VerbosePrint[2,
              "Renaming product functions and protecting constants"];
            w2 /. {fcsuni[a_/; (UScalarQ[a] || !FreeQ[$ConstantIsoIndices, a])]  ->
                    protectisoconstant[a],
                  fcli[a_] :> protectliconstant[a] /; UScalarQ[a] } /. {NM ->
                  NM1, Times -> NM1, DOT -> NM2,
                  Power[a_, b_ /; b > 0 && IntegerQ[b]] :>
                  NM1 @@ Table[a, {ddum, 1, b}]}]])&,w1];

	VerbosePrint[2,
        "Putting back product function names and constants"];
      subres /. {isomult->Sequence[],lorentzmult->Sequence[]} /.
      {NM1 ->NM, NM2 -> DOT} /. {protectisoconstant -> fcsuni,
                          protectliconstant -> fcli} /.
  (*Commented out 20/6-2003*)(*faso[fcsuni[ii_]] -> faso[ii] /.*) {su2delta1 -> SU2Delta,
                       su2f1 -> SU2F, su3delta1 -> SU3Delta, su3f1 -> SU3F,
                      su3d1 -> SU3D, sundelta1 -> SUNDelta, sunf1 -> SUNF,
                      sund1 -> SUND} /.
                  dummynames[
                    IsoDummys /. Flatten[{opts}] /. Options[IndicesCleanup],
                    LorentzDummys /. Flatten[{opts}] /.
                      Options[IndicesCleanup]] /.
              fcdiga -> dg /. Flatten[{larul[[1]], larul[[3]]}] /.
          larul[[2]] /. dg -> fcdiga /. Null->Sequence[] //.
          tracerule2 /. tracerule3 /. cheapnesthackruleback //
          (If[declutr, VerbosePrint[2, "DeclareUScalar[UTrace1]"]; DeclareUScalar[UTrace1]];
	   If[(CommutatorReduce /. Flatten[{opts}] /. Options[IndicesCleanup]),
        VerbosePrint[2, "Applying CommutatorReduce"];
        # // ( CommutatorReduce[#,opts])&, #])& //
          If[(ExtendedCleanup /. Flatten[{opts}] /. Options[IndicesCleanup]),
        VerbosePrint[2, "Applying SortIndices"];
        # // ( SortIndices[#,opts])&, #]&);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Multiplication of factors containing contracted indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Replace NM with CNM and contracted indices in factors will be renamed *)

CNM[a_, b_, opts___Rule] :=
    Block[{ais = {}, bis = {}, ais1 = {}, bis1 = {}, isos,
        lors, is, dum, j, jj, nm, nma, nmb},
        siCNM = 0; liCNM = 0;
      a /. ((NM | Times)[___,
                j_?(! FreeQ[#, fcsuni[__] | fcli[__]] &), ___,
                jj_?(! FreeQ[#, fcsuni[__] | fcli[__]] &), ___]) :>
           dum /; ((ais =
                    Intersection[
                      Cases[j, fcsuni[__] | fcli[__], Infinity,
                        Heads -> True],
                      Cases[jj, fcsuni[__] | fcli[__], Infinity,
                        Heads -> True]]) =!= {});
      ais1 = Cases[
          Cases[{a}, ((_?(! FreeQ[#, fcsuni[__] | fcli[__]] &))^2),
            Infinity, Heads -> True], fcsuni[_], Infinity, Heads -> True];
      b /. ((NM | Times)[___,
                j_?(! FreeQ[#, fcsuni[__] | fcli[__]] &), ___,
                jj_?(! FreeQ[#, fcsuni[__] | fcli[__]] &), ___]) :>
           dum /; ((bis =
                    Intersection[
                      Cases[j, fcsuni[__] | fcli[__], Infinity,
                        Heads -> True],
                      Cases[jj, fcsuni[__] | fcli[__], Infinity,
                        Heads -> True]]) =!= {});
      bis1 = Cases[
          Cases[{b}, ((_?(! FreeQ[#, fcsuni[__] | fcli[__]] &))^2),
            Infinity, Heads -> True], fcsuni[__] | fcli[__],
          Infinity, Heads -> True];
      If[(is = Intersection[Union[ais, ais1], Union[bis, bis1]]) =!= {},
        isos = IsoDummys /. {opts} /. Options[CNM];
        lors = LorentzDummys /. {opts} /. Options[CNM];
        NM[a, b /. (Rule[#,
                    If[Head[#] === fcsuni, ++siCNM; fcsuni[isos[[siCNM]]],
                     ++liCNM; fcli[lors[[liCNM]]]]] &/@ is)], NM[a, b]]
      ];

CNM[aa_, bb_, cc__?(!MatchQ[#,Rule]&),opts___Rule] :=
  Block[{isos = IsoDummys /. {opts} /. Options[CNM],
   lors = LorentzDummys /. {opts} /. Options[CNM]},
   CNM[CNM[aa, bb, opts], cc, IsoDummys -> Drop[isos, siCNM],
                        LorentzDummys -> Drop[lors, siCNM]]];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Isospin projection *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(*!!This whole section should be reworked if it's to be of any use,
   e.g. using ClebschGordan. 
   Think it even might be wrong!!*)

(* Projecting out IsoVectors in particle channels: *)

FieldProjection[
      IsoVector[
        fcqf[ders___, Particle[p : Alternatives @@ $Particles, pp___],
          la___], oo___],
      opts___] := (Select[$IsoSpinProjectionRules, (!
                  FreeQ[#, (Channel /. Flatten[{opts}] /.
                          Options[FieldProjection]) -> _] &)])[[1, 2]] /.
      Iso[par_, {i_}] -> fcqf[ders, Particle[par, pp], fcsuni[i], la];
FieldProjection[
      IsoVector[
          fcqf[ders___, Particle[p : Alternatives @@ $Particles, pp___],
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
      Flatten[Join[{
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
                        ChargeConjugate[Plus[a_, b___]] :>
                        Plus @@ ChargeConjugate /@ {a, b}]] /.
                NM -> amp1) /. {iso1[PhiMeson, {aa_Integer|(aa:fcexsuni[_Integer])}] -> aa,
              iso1[Pion, {aa_Integer|(aa:fcexsuni[_Integer])}] -> aa} /.
          iso1[_, {aa_Integer|(aa:fcexsuni[_Integer])}] -> aa /. momentarules[opts] /. amp1 -> amp;



(* PionKaonIsoSpin->3/2: *)

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

(* After doing loops with FeynArts, particle masses with SU(N) indices come out.
   They can be projected out in charged masses with IsoToChargedMasses *)

IsoToChargedMasses[exp_] := 
    Block[{part, rul, tmppart, parts, subpar, seq}, parts = {}; 
      subpar = (part = #[[1]]; (rul = (tmppart = 
                                ParticleMass[#[[1]], #[[2, 1]] | 
                                    fcsuni[#[[2, 1]]], r___]; tmppart) -> 
                            ParticleMass[part, r]; 
                        If[FreeQ[parts /. Alternatives :> (({##}[[1]]) &), 
                            tmppart], parts = Append[parts, tmppart]; rul, 
                          seq[]]) & /@ 
                    Cases[{#[[2]]}, _Iso, 
                      Infinity]) & /@ $IsoSpinProjectionRules /. 
            seq -> Sequence // Flatten; exp /. subpar];

(*Implementation from notebook:*)
(*subpar = (((alt @@ ((ParticleMass[##, ___] & @@ #) & /@ (MapAt[(SUNIndex @@ #)&,
#, {-1}] & /@ Cases[#[[2]], Iso[Alternatives @@ $ParticlesInUse,
{_Integer}], {0, 3}]))) -> (ParticleMass[#[[1]]])) & /@
$IsoSpinProjectionRules) /. alt[a_] :> a /. alt :> Alternatives*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

End[];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

EndPackage[];

If[$Verboseness > 0,WriteString["stdout", "Channels | \n "]];
