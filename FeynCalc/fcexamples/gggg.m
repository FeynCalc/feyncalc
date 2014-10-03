(* gluon gluon --> gluon gluon *)

(* Author: Rolf Mertig, Mertig Consulting, October 2003 *)

(* This is an example batch file *)

(* Minimal version of FeynCalc to use is 5.0 *)

starttime = AbsoluteTime[];

$LoadFeynArts=True;
$LoadPhi=False;
$LoadTARCER=False;
<<HighEnergyPhysics`FeynCalc`;
Comment=WriteString["stdout",##]&;

(* Using the FCQCD and FCQCDLorentz models from the HighEnergyPhysics\Models directory *)
(* V[5] is the gluon. Visit www.feynarts.de for documentation of FeynArts *)
tmp1 = InsertFields[CreateTopologies[0, 2 -> 2], {V[5], V[5]} -> {V[5], V[5]},
                   InsertionLevel -> Classes, GenericModel -> "FCQCDLorentz", Model -> "FCQCD"
];

(* paint only if there is a notebook Front End *)
If[$Notebooks,
   Paint[tmp1, ColumnsXRows -> {4, 1}, PaintLevel -> {Classes},
      SheetHeader -> False, Numbering -> False]
];

(* Create the amplitudes and adjust some FeynArts notation *)
tmp2 = Apply[List, CreateFeynAmp[tmp1, Truncated -> False] ] /.
       FeynAmp[_, _, am_, _] :> (am /. RelativeCF -> 1) /. Polarization[a_,b_,_]:>Polarization[a,b];
Comment[Length[tmp2]," amplitudes created.  Time spent so far = ",
        Round[(AbsoluteTime[]-starttime) 10]/10., " seconds \n"];

(* The calculation becomes easier if energy momentum conservation is applied *)
enmomcon = k2 -> p1 + p2 - k1;

(* Here we specify the gauge (general covariant gauge) and the dimension *)
tmp3 = Explicit[Plus @@ tmp2, Gauge -> (1 - \[Alpha]), Dimension -> n] /. enmomcon;

(* This introduces the usual Mandelstam variables, keeping in mind that p1+p2=k1+k2 as specified in the Model file *)
SetMandelstam[s, t, -s - t, p1, p2, -k1, -k2, 0, 0, 0, 0, Dimension -> n];

(* This replaces PropagatorDenominators by scalar products and simplifies the SU(N) algebra.
   With the setting Explicit->True all SUNF's are replaced by traces over SUNT's
*)
Timing[ tmp4 = Collect2[
              SUNSimplify[PropagatorDenominatorExplicit[tmp3],
              Explicit -> True] /. enmomcon, SUNIndex, Factoring->False] ;
]

(* Here we simplify the Lorentz structure of the amplitudes. *)
Timing[ tmp5= Sum[WriteString["stdout", i, " "];
          Contract[tmp4[[i]]], {i, Length[tmp4]}];
]

(* We can readily factorize all coefficients of the SU(N) traces *)
tmp6 = Factor2/@tmp5;

(* Take the complex conjugate of the sum of all amplitudes *)
tmp7 = ComplexConjugate[tmp6];

(* This calculates the color factors in terms of SUNN's (i.e. the N of SU(N) ) *)
Timing[
tmp8 = Sum[
        SUNSimplify[tmp6[[i]] tmp7[[j]], SUNNToCACF -> False], {i, Length[tmp6]},
            {j, Length[tmp7]}]
];

(* Factorize the squared matrix element and resubstitute k2 for -p1-p2-k1 (in the polarization vectors) *)
Timing[tmp9 = Factor2[tmp8] /. Reverse[enmomcon];]

(* In order to do the sum over all polarization it is simplest to substitute the polarization Momentum[]'s
   of the amplitude and the complex conjugated amplitude by LorentzIndex[]'s
*)
polmomenta = SelectNotFree[Cases2[tmp9, Momentum], Polarization];
sub = Thread[ polmomenta -> Map[LorentzIndex[#, n] &, {nu3, mu3, nu4, mu4, nu1, mu1, nu2, mu2}]]

(* the product of the four polarization sums for gluons *)
polsu = Times @@
    Apply[PolarizationSum[#1, #2, #3, #4, Dimension -> n] &, {{mu1, nu1, p1,
          p2}, {mu2, nu2, p2, p1}, {mu3, nu3, k1, k2}, {mu4, nu4, k2,
          k1}}, {1}];

(* do the contraction *)
Timing[tmp10 = Factor2[Contract[(tmp9 /. sub) polsu]];];

(* average over initial spin and color states by hand:  (this could be automatized, but well ... ) *)
Print["Result = ", InputForm[result = tmp10/4/8/8]];

(* Compare with the result as given in
"Applications of Perturbative QCD", Richard D. Field, Addison-Wesley, 1995*)
fieldresult = (9/2)*(3 - (u*t)/s^2 - (u*s)/t^2 - (s*t)/u^2);

Print["check with Field, p. 276, (7.2.34) ",
      If[Together[(fieldresult/. (u->-s-t)) - result /. Gstrong->1/.SUNN->3/.n->4 ]===0, " OK", "Not OK"]];
Print["seconds of wall-clock time needed = ",AbsoluteTime[]-starttime]
