(* Couplings *)

(* Utilities for generating coupling definitions for FeynArts 2 and using them
   for calculating amplitudes *)

(* Author:  Frederik Orellana, fjob@cabocomm.dk

   Date:  1/8-2000

   Context: HighEnergyPhysics`Phi`Couplings`

   Package version:  1.2

   Mathematica version:  4.0 *)



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

BeginPackage[
    "HighEnergyPhysics`Phi`Couplings`", {"HighEnergyPhysics`Phi`",
     "HighEnergyPhysics`FeynCalc`", "HighEnergyPhysics`Phi`Objects`"}];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

tmp`olddir = Directory[];
SetDirectory[HighEnergyPhysics`Phi`$HEPDir];
SetDirectory["HighEnergyPhysics"];
SetDirectory["Phi"];
Get["Couplings.defs.m"];
SetDirectory[tmp`olddir];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

Begin["`Private`"];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* FeynCalc functions *)

fcdot := fcdot = MakeContext["DOT"];
fcsuni := fcsuni = MakeContext["SUNIndex"];
fcsundel := fcsundel = MakeContext["SUNDelta"];
fcsunf := fcsunf = MakeContext["SUNF"];
fcsund := fcsund = MakeContext["SUND"];
fcsunn := fcsunn = MakeContext["SUNN"];
fcca := fcca = MakeContext["CA"];
fccf := fccf = MakeContext["CF"];
fcmom := fcmom = MakeContext["Momentum"];
fcpa := fcpa = MakeContext["Pair"];
fcscp := fcscp = MakeContext["ScalarProduct"];
fcprd := fcprd = MakeContext["PropagatorDenominator"];
fcmomexp := fcmomexp = MakeContext["MomentumExpand"];
fcdim := fcdim = MakeContext["Dimension"];
fcli := fcli = MakeContext["LorentzIndex"];
fcmt := fcmt = MakeContext["MetricTensor"];
fcfad := fcfad = MakeContext["FeynAmpDenominator"];
fcpol := fcpol = MakeContext["Polarization"];
fcpd :=fcpd = MakeContext["PartialD"];
fcqf := fcqf = MakeContext["QuantumField"];
fcdiga := fcdiga = MakeContext["DiracGamma"];
fcdigaex := fcdigaex = MakeContext["DiracGammaExpand"];
fcchiralp := fcchiralp = MakeContext["ChiralityProjector"];
fcspi := fcspi = MakeContext["Spinor"];
fads := fads = MakeContext["DiracSpinor"];
fcdtr := fcdtr = MakeContext["DiracTrace"];
fcdtrev := fcdtrev = MakeContext["DiracTraceEvaluate"];
fcmomc := fcmomc = MakeContext["MomentumCombine"];
fccombs := fccombs = MakeContext["Combinations"];
fcexli := fcexli = MakeContext["ExplicitLorentzIndex"];
fcexpt := fcexpt = MakeContext["Explicit"];
fcsunt := fcsunt = MakeContext["SUNT"];
fcexsuni := fcexsuni = MakeContext["ExplicitSUNIndex"];




(* FeynArts functions *)

faid := faid = HighEnergyPhysics`FeynArts`IndexDelta;
facl := facl = HighEnergyPhysics`FeynArts`Classes;
fafm := fafm = HighEnergyPhysics`FeynArts`FourMomentum;
faint := faint = HighEnergyPhysics`FeynArts`Internal;
faintg := faintg = HighEnergyPhysics`FeynArts`Integral;
fafad := fafad = HighEnergyPhysics`FeynArts`FAFeynAmpDenominator;
faind := faind = HighEnergyPhysics`FeynArts`Index;
fapl := fapl = HighEnergyPhysics`FeynArts`PickLevel;
fainc := fainc = HighEnergyPhysics`FeynArts`Incoming;
faout := faout = HighEnergyPhysics`FeynArts`Outgoing;
(*fapd := fapd = HighEnergyPhysics`FeynArts`FAPropagatorDenominator;*)
fafv := fafv = Global`FAFourVector;(*We keep FAFourVector for FAToFC to work
with old coupling files*)
famt := famt = Global`FAMetricTensor;
falo := falo = Global`Lorentz;
facrfa := facrfa = HighEnergyPhysics`FeynArts`CreateFeynAmp;
faal := faal = HighEnergyPhysics`FeynArts`AmplitudeLevel;
fapolv := fapolv = Global`FAPolarizationVector;
faext := faext = HighEnergyPhysics`FeynArts`External;
fanoncom := fanoncom = HighEnergyPhysics`FeynArts`FANonCommutative;
(*fadm := fadm = Global`FADiracMatrix;*)
(*fachiralp := fachiralp = Global`FAChiralityProjector;*)
(*fads := fads = Global`FADiracSpinor;*)
faferch := faferch = HighEnergyPhysics`FeynArts`FermionChain;
fagen := fagen = System`Generic;
fains := fains = HighEnergyPhysics`FeynArts`Insertions;
fatop := fatop = HighEnergyPhysics`FeynArts`Topology;
fagr := fagr = HighEnergyPhysics`FeynArts`Graph;
(*faloop := faloop = HighEnergyPhysics`FeynArts`FALoop;*)
faloop := faloop = MakeContext["Loop"];
faprop := faprop = HighEnergyPhysics`FeynArts`Propagator;
(*fadsl := fadsl = Global`FADiracSlash;*)
famatr := famatr = HighEnergyPhysics`FeynArts`MatrixTrace;
favert := favert = HighEnergyPhysics`FeynArts`Vertex;
faso := faso = HighEnergyPhysics`FeynArts`SumOver;
fatopl := fatopl = HighEnergyPhysics`FeynArts`TopologyList;
faverti := faverti = HighEnergyPhysics`FeynArts`Vertices;
facoupmat := facoupmat = HighEnergyPhysics`FeynArts`M$CouplingMatrices;
faps := faps = HighEnergyPhysics`FeynArts`PSort;
faseens := faseens = HighEnergyPhysics`FeynArts`SelfEnergies;
fawfcr := fawfcr = HighEnergyPhysics`FeynArts`WFCorrections;
fafi := fafi = HighEnergyPhysics`FeynArts`Field;
fatrru := fatrru = HighEnergyPhysics`FeynArts`M$TruncationRules;
facol := facol = Global`Colour;


(* Defaults *)

$VerticesSpecifications = {{VertexFields -> {Pion[0], Pion[0], Pion[0],
            Pion[0]}, PerturbationOrder -> {2}, CouplingSign -> 1,
        PhiModel -> HighEnergyPhysics`Phi`Objects`ChPT2,
        XFileName ->
          Automatic}, {VertexFields -> {Pion[0], Pion[0], Pion[0], Pion[0],
            Pion[0], Pion[0]}, PerturbationOrder -> {2}, CouplingSign -> 1,
        PhiModel -> HighEnergyPhysics`Phi`Objects`ChPT2,
        XFileName -> Automatic}};
$PropagatorMassesStates = {Pion[0] -> {RenormalizationState[0]},
      Kaon[0] -> {RenormalizationState[0]}};
$CouplingLorentzIndicesString = "\[Mu]";
$CouplingMomentumVariablesString = "p";
$CouplingIsoIndicesSpecifications = {Pion[
        0] -> {{IsoRange -> {1, 2, 3}, IsoIndicesString -> "I"}},
    Kaon[0] -> {{IsoRange -> {1, 2}, IsoIndicesString -> "J"}}};
$MixingFields = {};
$LastModelRules = {};
$InsertOnly = {Vector[0][0] -> {fainc, faout, faext},
    AxialVector[0][0] -> {fainc, faout, faext}};
$FADelta = fcsundel;
SetAttributes[MomentaScalarProduct, Orderless];
Options[FCToFA] = {ScalarProductForm -> MomentaScalarProduct,
      MomentumVariablesString -> "p", ParticlesNumber -> 4,
      IsoIndicesString -> "I", FADeltas -> True, IsoCollect -> False};
Options[MomentaCollect] = {ParticlesNumber -> 4, PerturbationOrder -> 2,
      ScalarProductForm -> (MomentaScalarProduct|fcpa),
      fcmt -> Global`FAMetricTensor,
      MomentumVariablesString -> "p", ExtendedCollect -> True,
      HoldMinuses -> False};
Options[GenericCoupling] = {ScalarProductForm -> MomentaScalarProduct,
FCToFA->True};
Options[ClassesCoupling] = {ScalarProductForm -> MomentaScalarProduct,
FCToFA->True};
Options[XName] = {VertexFields -> {Pion[0], Pion[0], Pion[0], Pion[0]},
      PerturbationOrder -> 2, PhiModel -> HighEnergyPhysics`Phi`Objects`ChPT2,
       XFileName -> Automatic};
Options[CouplingFilesGenerate] = {VertexFields -> {Pion[0], Pion[0], Pion[0],
          Pion[0]}, PerturbationOrder -> 2,
      PhiModel -> HighEnergyPhysics`Phi`Objects`ChPT2,
      XFileName -> Automatic};
Options[FAToFC] = {EqualMasses -> True, ParticlesNumber -> 4,
      ScalarProductForm -> MomentaScalarProduct,
      MomentumVariablesString -> "p", InternalMomentumVariablesString -> "q",
      MomentaSumLeft -> All, fcdtrev -> False, Sum -> True, FADeltas -> False};
Options[DeltaFunctionsCollect] = {ParticlesNumber -> 4,
      IsoIndicesString -> "I"};
Options[DeltaFunctionProducts] = {ParticlesNumber -> 4,
      IsoIndicesString -> "I", FADeltas -> False};
Options[AddExternalLegs] = {ExternalPropagators -> 1, faseens -> False};
Options[DiscardTopologies] = {PerturbationOrder -> 2,
      OrderingPatterns -> {}};
Options[WFRenormalize] = {PerturbationOrder -> 2};
Options[PMRenormalize] = {PerturbationOrder -> 2};
Options[DCRenormalize] = {PerturbationOrder -> 2};
Options[CreateFCAmp] = {WFRenormalize -> False, PerturbationOrder -> 2,
                          DropOrder -> 4, Method -> Plus};


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Change to FeynArts 2 notation *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* ****CRAP****Before calculating the Feynman rule with FeynRule,
LorentzIFactors should be used on the lagrangian.  This function supplies a
factor I (and a sign) for each QuantumField which has both a Lorentz index
and a derivative.  I'm not sure about this, but it seems necessary in order
to get the right Feynman rules. *)

(**)(*LorentzIFactors[l_] :=
      l /. {fcqf[a_, b___][
                x_] :> (I)^(Min[{Length[
                          select1[{a, b}, (Head[#] === fcli) &] /.
                            select1 -> Select],
                        Length[
                          select1[{a, b}, (Head[#] === fcpd) &] /.
                            select1 -> Select]}])*fcqf1[a, b][x],
            fcqf[a_,
                b___] :> (I)^(Min[{Length[Select[{a, b}, fcli[__]]],
                        Length[Select[{a, b}, fcpd[__]]]}])*fcqf1[a, b]} /.
        fcqf1 -> fcqf;*)
(*LorentzIFactors[l_] :=
      Collect[l /. {fcqf[a_, b___][
                      x_] :> (po =
                        Min[{Length[
                              select1[{a, b}, (Head[#] === fcli) &] /.
                                select1 -> Select],
                            Length[
                              select1[{a, b}, (Head[#] === fcpd) &] /.
                                select1 -> Select]}]; (DecayConstant[
                              si])^(po)*(I)^(po)*fcqf1[a, b][x]),
                  fcqf[a_,
                      b___] :> (po =
                        Min[{Length[
                              select1[{a, b}, (Head[#] === fcli) &] /.
                                select1 -> Select],
                            Length[
                              select1[{a, b}, (Head[#] === fcpd) &] /.
                                select1 -> Select]}]; (DecayConstant[
                              si])^(po)*(I)^(po)*fcqf1[a, b])} /.
              fcqf1 -> fcqf, DecayConstant[si]^_] /.
          DecayConstant[si]^nn_ :> (-1)^(nn + 1) /. DecayConstant[si] :> 1;*)



(* Terms with the same delta-function factor are collected, and the result is
expressed in FeynArts notation: *)

SetAttributes[pp, {Orderless}];

SetAttributes[pll, {Orderless}];

indexsub1 = pp[a__] :> pl /@ {a};

indexsub2 = pl[{a__}] :> pll[a];

indexsub3[opts___] :=
    ToExpression[(IsoIndicesString /. Flatten[{opts}] /.
              Options[DeltaFunctionProducts]) <>
          ToString[(ParticlesNumber + (1 - (-1)^ParticlesNumber)/2) /.
                Flatten[{opts}] /. Options[DeltaFunctionProducts]]] ->
      ToExpression[(IsoIndicesString /. Flatten[{opts}] /.
              Options[DeltaFunctionProducts]) <>
          ToString[(ParticlesNumber /. Flatten[{opts}] /.
                Options[DeltaFunctionProducts])]];

indii[opts___] := {Permutations[
        Table[ToExpression[(IsoIndicesString /. Flatten[{opts}] /.
                  Options[DeltaFunctionProducts]) <>
              ToString[
                j]], {j, (ParticlesNumber + (1 - (-1)^ParticlesNumber)/2) /.
                Flatten[{opts}] /. Options[DeltaFunctionProducts]}]]};

indicestemp[opts___] :=
    Union[Union[MapThread[pp @@ Partition[#1, 2] &, indii[opts] , 1  ]] /.
            indexsub1 /. indexsub2 /. indexsub3[opts]];

DeltaFunctionProducts[opts___] :=
    Union[indicestemp[opts] /. (pll -> faid) /. ({faid[a__], b___} ->
              faid[a]*b)] /.
      If[(FADeltas /. Flatten[{opts}] /. Options[DeltaFunctionProducts]), faid -> $FADelta,
        faid[a_, b_] :> fcsundel[fcsuni[a], fcsuni[b]] /;
                        !FreeQ[{a,b}, IsoSpin, Heads -> True]];



(* The vertex in FeynArts notation: *)

melfeynartstemp[melsimplified_, opts___] :=(**)
 (fcdigaex[melsimplified](*/. {fcsunn -> Sqrt[2], fcca -> Sqrt[2],
                  fccf -> 1/(2*Sqrt[2])}*)/.

(*Added 28/6-2002*)
fcdiga[fcmom[p_,___],___] :> fcdiga[fcli[ToExpression[$CouplingLorentzIndicesString<>ToString[p]]]]*
fcpa[fcli[ToExpression[$CouplingLorentzIndicesString<>ToString[p]]],fcmom[p]] /.

            If[(FADeltas /. Flatten[{opts}] /.
                  Options[FCToFA]), {fcsundel -> $FADelta,
                SU2Delta -> $FADelta, SU3Delta -> $FADelta}, {}] /. {Dot ->
              fanoncom, fcdot -> fanoncom, (fcsuni|fcexsuni)[a_] -> a,
            fcpa[a_HighEnergyPhysics`FeynCalc`Momentum`Momentum,
                b_HighEnergyPhysics`FeynCalc`Momentum`Momentum] ->
(ScalarProductForm /. Flatten[{opts}] /. Options[FCToFA])[a, b],
            fcpa[a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                b_HighEnergyPhysics`FeynCalc`Momentum`Momentum] -> fafv[b, a],
             fcpa[fcli[li1_, ___], fcli[li2_, ___]] ->
              famt[li1, li2]} /. {fcli[a_] -> a, fcmom[a_] -> a(*,
          fcdiga -> fadm, fcchiralp -> fachiralp*)} /. (*{a__*fadm[b__] ->
          a*fanoncom[fadm[b]]}*){a__*fcdiga[b__] ->
          a*fanoncom[fcdiga[b]]} /.
    Flatten[Table[{ToExpression[(MomentumVariablesString /.
                      Flatten[{opts}] /. Options[FCToFA]) <>
                ToString[
                  b + (ParticlesNumber/2 /. Flatten[{opts}] /.
                        Options[FCToFA])]] ->(*-*)
              ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                    Options[FCToFA]) <>
                ToString[
                  b + (ParticlesNumber/2 /. Flatten[{opts}] /.
                        Options[FCToFA])]]}, {b, (ParticlesNumber/2 /.
                Flatten[{opts}] /. Options[FCToFA])}]]) ;

SetAttributes[nonc, Flat];

(*Added IsoCollect 14/4 - 2000*)
  FCToFA[melsimplified_, opts___] :=
    If[(IsoCollect /. Flatten[{opts}] /. Options[FCToFA]),
      Collect[ExpandAll[
          melfeynartstemp[
            melsimplified /. (Projection[pa_, ___][pb_]) :> fcsundel[pa, pb],
            opts]], Join[
          DeltaFunctionProducts @@
            Join[Flatten[{opts}], Options[FCToFA]],(*Added 14/4 -
              2000*){SU2F[___], SU3F[___], SU3D[___], fcsunf[___],
            fcsund[___]}, {UGenerator[__][__]}]],
      melfeynartstemp[
        melsimplified /. (Projection[pa_, ___][pb_]) :> fcsundel[pa, pb],
        opts]]  /. fanoncom -> nonc /. nonc -> fanoncom (*Added 28/6-2002*) ;

negtest[b_] := Re[b] < 0 || Im[b] < 0;
plus1[a : ((_?negtest*_) ..)] := -1*plus1 @@ ((-1*#) & /@ {a});
momprod[pta_, ptab__] := Union[Flatten[Union @@ Outer[Times, pta, ptab]]];
momprod[pta_] := Flatten[pta];
Head1[Power[a_, b_]] := Head1[a, b];
Head1[a_] := Head[a];



(* Stuff commented out 6/3-2000 to accomodate no longer relying on the
NumericFunction Attribute (got tired of the error messages from Mathematica 4
trying to evaluate things numerically). *)

(*numericheads :=
      Union[((If[MemberQ[Attributes[#], NumericFunction], #,
                    dummy]) &) /@ (Head /@ $ExpansionQuantities)] /.
        dummy -> Sequence[];*)



(* Mathematica 4 treats numerical functions differently than Mathematica 3 (a
bug?). So we replace numerical functions temporarily. Alternatively, we could
unset the numerical attribute temporarily. 18/12-1999. Nope, doesn't work. *)

(*dropnumrules := ((# -> ToExpression["tmp`Tmp" <> ToString[#]]) &) /@
        Union[$UScalars, numericheads, $Particles];*)
(*setnumrules := ((ToExpression["tmp`Tmp" <> ToString[#]]) -> # &) /@
        Union[$UScalars, numericheads, $Particles];*)
MomentaCollect[m_, opts___] :=
 (*(pc = (ScalarProductForm /. Flatten[{opts}] /. Options[MomentaCollect]);*)
 Block[{mt = (fcmt /. Flatten[{opts}] /. Options[MomentaCollect]),
  pc = (ScalarProductForm /. Flatten[{opts}] /. Options[MomentaCollect]),
  pcc, a, b, c, d, e, j, pp, xf, xff, xfff, n1,
       n2, p, l, r, tminus, var, Head1, momentaslist, momentaslist1, momentaslist2,
  momentaslist3},
  pcc = If[Head[pc]===Alternatives, Blank /@ pc, Blank[pc]];
  VerbosePrint[2, "Scalar product is ", pcc];
    VerbosePrint[1, "Building list of Collect patterns from momenta"];
    momentaslist1 =
      Join[If[pc === None, {},
          Join[{Times @@
            Table[(var = ToExpression["a" <> ToString[j]];
                Pattern[Evaluate[var], pcc]), {j,
                Ceiling[(PerturbationOrder /. Flatten[{opts}] /.
                        Options[MomentaCollect])/2]}]},
                        {pcc^(Ceiling[(PerturbationOrder /. Flatten[{opts}] /.
                      Options[MomentaCollect])/2])}]],
        If[(ExtendedCollect /. Flatten[{opts}] /. Options[MomentaCollect]),
          VerbosePrint[1,
            "Building list of Collect patterns from $ExpansionQuantities"];
          VerbosePrint[3, " ", $ExpansionQuantities];
          Select[momprod @@
          Table[Join[$ExpansionQuantities, 
                    If[pc === None, {}, {Sqrt[pc[a___]]}]](*/.dropnumrules*)/.
	     {Blank -> xf, BlankSequence -> xff, BlankNullSequence -> xfff} /.
             Pattern -> pp /.
	    {pp[b_, xf[]] -> pp[b, Blank[]],
             pp[b_, xff[]] -> pp[b, BlankSequence[]],
             pp[b_, xfff[]] -> pp[b, BlankNullSequence[]]} /.pp -> Pattern /.
	     {xf[] -> ToExpression["a" <> ToString[j] <> "_"],
              xff[] -> ToExpression["a" <> ToString[j] <> "__"],
              xfff[] -> ToExpression["a" <> ToString[j] <> "___"]},
	  {j, (Ceiling[(PerturbationOrder /. Flatten[{opts}] /. Options[MomentaCollect])])}],
                    FreeQ[#, ___*Power[_, Rational[_, 2]]] &] //.
		    {a___, b_Times, c___, d_Times, e___} /;
                      Head1 /@ (b) == Head1 /@ (d) -> {a, b, c, e} //.
		      {a___, b_, c___} /; OddQ[Count[b, Sqrt[_]]] -> {a, c} //.
              Power[a_, Rational[n1_, 2]]*Power[b_, Rational[n2_, 2]] ->
                Power[a, Rational[n1 + n2, 2]]*(1 + (-1)^(n1 + n2))/2 /.
            Sqrt[_]*__ -> Sequence[], {}](*, $ExpansionQuantities*)];
    momentaslist2 = If[mt === None, {}, mt[__]*momentaslist1];
    (*momentaslist2 = fcmt[__]*momentaslist1;*)
    momentaslist3 = Join[momentaslist2, momentaslist1](*/. dropnumrules*);
    (*Use only patterns that actually occur in the expressions. Added 24/9-2002*)
    momentaslist = Select[momentaslist3, !FreeQ[m, #]&];
    VerbosePrint[3, "I will collect ", StandardForm[momentaslist]];
    VerbosePrint[1,
      "Collecting"];(*We  replace numerical functions temporarily*)
      Collect[m(*/. dropnumrules*)/.
                  fafv[-p_, l___]*r_ :> fafv[p, l]*tminus*r, momentaslist] /.
              Plus -> plus1 /. plus1 -> Plus /.
          fafv[-p_, l___]*r_ :> fafv[p, l]*tminus*r /. tminus -> -1 /.
      If[(HoldMinuses /. Flatten[{opts}] /. Options[MomentaCollect]), -1*a__*
            Plus[b_, bb___] -> a*HoldMinus*Plus[b, bb], {}](*/.
        setnumrules*)];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Generation of FeynArts 2 couplings *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Here is the kinematical coupling vector to be used in a generic model file
for FeynArts 2: *)

expansionpatterns[opts___] :=
    Alternatives @@
      Join[$ExpansionQuantities, {(*_Global`FAMetricTensor,*)
_HighEnergyPhysics`FeynCalc`MetricTensor`MetricTensor, (*_Global`FADiracMatrix,*)
(*_Global`FAChiralityProjector,*)
_HighEnergyPhysics`FeynCalc`DiracGamma`DiracGamma,
_HighEnergyPhysics`FeynCalc`ChiralityProjector`ChiralityProjector,
          Blank[ScalarProductForm /. Flatten[{opts}] /.
              Options[GenericCoupling]]}];



GenericCoupling1[a_*m_, opts___] /;
(!FreeQ[a, expansionpatterns[opts]] && FreeQ[m, expansionpatterns[opts]]) :=
Flatten[{Select[a*m, !FreeQ[#, Alternatives @@ expansionpatterns[opts]]&] /.
          HoldMinus -> 1}];



llist[a__][opts___] :=
    Join[{Plus @@ Select[{a}, FreeQ[#, expansionpatterns[opts]] &]},
        Select[{a}, !FreeQ[#, expansionpatterns[opts]]&]] /.
	{f___, 0, l___} -> {f, l};



GenericCoupling1[a_*Plus[m_, n__], opts___] /;
      FreeQ[a, expansionpatterns[opts]] :=
    a*((Plus[m, n] + tempp) /. Plus[aa__, tempp] -> List[aa]) /.
    Plus[aa__, tempp] ->
    llistt[aa][opts] /. {a1_* m1_ /; (! FreeQ[a1, expansionpatterns[opts]] &&
    FreeQ[m1, expansionpatterns[opts]]) :>
    Sequence @@ Flatten[{Select[
    a1*m1, !FreeQ[#, Alternatives @@
    expansionpatterns[opts]] &]}]} /.
             {Plus[_*$FADelta[__], _*$FADelta[__] ..] -> 1,
              Plus[$FADelta[__], $FADelta[__] ..] -> 1,
              Plus[_*fcsundel[__], _*fcsundel[__] ..] -> 1,
              Plus[fcsundel[__], fcsundel[__] ..] ->
                1,(*Plus[_*Projection[__][_], _*Projection[__][_] ..] -> 1,
                Plus[Projection[__][_], Projection[__][_] ..] -> 1,*)
                Plus[_*UGenerator[__][_], _*UGenerator[__][_] ..] -> 1,
              Plus[UGenerator[__][_], UGenerator[__][_] ..] -> 1,
              Plus[_*fcsund[__], _*fcsund[__] ..] -> 1,
              Plus[fcsund[__], fcsund[__] ..] -> 1,
              Plus[_*fcsunf[__], _*fcsunf[__] ..] -> 1,
              Plus[fcsunf[__], fcsunf[__] ..] -> 1} //. {fac_*$FADelta[__] ->
              fac, fac_*fcsundel[__] -> fac,(*fac_*Projection[__][_] -> fac,*)
              fac_*fcsund[__] -> fac, fac_*fcsunf[__] -> fac} /.
        HoldMinus -> 1 /. llistt -> llist;



GenericCoupling1[Plus[m_, mm___],opts___] := ((m + mm + tempp) /.
   Plus[aa__, tempp] -> llistt[aa][opts]) /.
   {a1_ * m1_ /; (!FreeQ[a1, expansionpatterns[opts]] &&
                        FreeQ[m1, expansionpatterns[opts]]) :>
    Sequence @@ Flatten[{Select[a1*m1,
    !FreeQ[#,Alternatives @@ expansionpatterns[opts]]&]}]} /.
             {Plus[_*$FADelta[__], _*$FADelta[__] ..] -> 1,
              Plus[$FADelta[__], $FADelta[__] ..] -> 1,
              Plus[_*fcsundel[__], _*fcsundel[__] ..] -> 1,
              Plus[fcsundel[__], fcsundel[__] ..] -> 1,
	      (*Plus[_*Projection[__][_], _*Projection[__][_] ..] -> 1,
                Plus[Projection[__][_], Projection[__][_] ..] -> 1,*)
                Plus[_*UGenerator[__][_], _*UGenerator[__][_] ..] -> 1,
              Plus[UGenerator[__][_], UGenerator[__][_] ..] -> 1,
              Plus[_*fcsund[__], _*fcsund[__] ..] -> 1,
              Plus[fcsund[__], fcsund[__] ..] -> 1,
              Plus[_*fcsunf[__], _*fcsunf[__] ..] -> 1,
              Plus[fcsunf[__], fcsunf[__] ..] -> 1} //. {fac_*$FADelta[__] ->
              fac, fac_*fcsundel[__] -> fac,(*fac_*Projection[__][_] -> fac,*)
              fac_*fcsund[__] -> fac, fac_*fcsunf[__] -> fac} /.
        HoldMinus -> 1 /. llistt -> llist;



(* Here is the coupling vector to be used in a classes model file for FeynArts 2: *)

ClassesCoupling1[m_, opts___] := (numerator = (m + tempp) /.
              Plus[aa__, tempp] -> llistt[aa][opts] /. HoldMinus -> -1 /.
          llistt -> llist; classcoupl = numerator/GenericCoupling2[m, opts];
      Table[{classcoupl[[rep]]}, {rep, Length[numerator]}]);



(* Should FCToFA be applied first? *)

GenericCoupling2[m_,opts___] := Block[
   {fctofa = FCToFA /. Flatten[{opts}] /.Options[GenericCoupling],tmpres,
   gencoup1 = GenericCoupling1},
   tmpres =
   If[fctofa,
      VerbosePrint[2,"Applying FCToFA to the expression"];
      GenericCoupling1[FCToFA[m],opts],
      VerbosePrint[1,"FCToFA will not be applied to the expression!  Make sure the expression is in FeynArts notation"];
      GenericCoupling1[m,opts]
   ];
   If[Head[tmpres] =!= gencoup1,
      tmpres,
      Message[GenericCoupling::"nores"];
   ]
];

GenericCoupling[m_,opts___] := GenericCoupling2[m,opts] //. 
           (*Added 28/6-2002*) {a_*fanoncom[b_] :> fanoncom[a*b],
                                              a_*fanoncom[b_,c__] :> fanoncom[a*b,c]} /.
           fanoncom -> nonc /. nonc -> fanoncom ;

ClassesCoupling[m_,opts___] := Block[
   {fctofa = FCToFA /. Flatten[{opts}] /.Options[ClassesCoupling],tmpres,
   classcoup1 = ClassesCoupling1},
   tmpres =
   If[fctofa,
      VerbosePrint[2,"Applying FCToFA to the expression"];
      ClassesCoupling1[FCToFA[m],opts],
      VerbosePrint[1,"FCToFA will not be applied to the expression!  Make sure the expression is in FeynArts notation"];
      ClassesCoupling1[m,opts]
   ];
   If[Head[tmpres] =!= classcoup1,
      tmpres,
      Message[ClassesCoupling::"nores"];
   ]
];

(* A  name is generated for the coupling files: *)

xnamerule[opts___] :=
    XFileName ->
      ToString[PhiModel /. Flatten[{opts}] /.
            Options[XName]] <> ((VertexFields /. Flatten[{opts}] /.
                Options[XName]) /. (p : $ParticleHeads)[ii_] :>
              StringTake[ToString[p], {1}] <> StringTake[ToString[p], {-1}] <>
                 ToString[ii]) <> "o" <>
        ToString[PerturbationOrder /. Flatten[{opts}] /. Options[XName]];
XName[opts___] :=
    If[((XFileName /. Flatten[{opts}] /. Options[XName]) ===
          Automatic), (XFileName /.
          xnamerule[opts]), (XFileName /. Flatten[{opts}] /.
          Options[XName])];



(* The complete automatized transformation toFeynArts notations, reduction of
delta-functions, generation of FeynArts 2 couplings and saving of the two
couplings. The two saved coupling vectors can be used for constructing
coupling model files for FeynArts 2.  The name is a contraction of all the
option settings in the ordering given by Options[CouplingFilesGenerate] and
with the extension Gen or Mod: *)



(*Changed 7/1 - 2000*)
  CouplingFilesGenerate[melsimplified_, opts___] := (tmp`olddir = Directory[];
       SetDirectory[HighEnergyPhysics`Phi`$HEPDir];
      SetDirectory["HighEnergyPhysics"]; SetDirectory["Phi"];
      SetDirectory["CouplingVectors"];
    Put[GenericCoupling[MomentaCollect[FCToFA[melsimplified, opts], opts]],
      StringTake[XName[opts],
Min[30, StringLength[XName[opts]]]] <> ".Gen"];
    Put[ClassesCoupling[MomentaCollect[FCToFA[melsimplified, opts], opts]],
     StringTake[XName[opts],
Min[30, StringLength[XName[opts]]]] <> ".Mod"];
    Print["Two coupling vectors have been saved in files  " <>
          StringTake[XName[opts],
Min[30, StringLength[XName[opts]]]] <> ".Gen" <> " and " <>
          StringTake[XName[opts],
Min[30, StringLength[XName[opts]]]] <> ".Mod"];
    SetDirectory[tmp`olddir];);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Symmetry factors for Feynman graphs *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Below follow two patches to FeynArts which I am not at all sure about: *)



(* For n absolutely identical (including signs - e.g. -Fermion7[0]!=Fermion7[0])
vertices, we get a factor n!: *)


(* Added 11/7-2001.
   To deal with multiple-graph topologies.
   Splits in separate one-graph topologies. *)

VerticesExtract[top:(fatop[_][__] -> fains[fagen][
                 fagr[__][__] -> fains[facl][fagr[__][__]],
                (fagr[__][__] -> fains[facl][fagr[__][__]]) ..])]:=
(*Union @@*) (VerticesExtract /@ (top /.
             ((t : (fatop[_][__])) -> (i:(fains[fagen][
                fagr[__][__] -> fains[facl][fagr[__][__]],
                (fagr[__][__] -> fains[facl][fagr[__][__]]) ..]))) :>
             ((Rule[t, fains[fagen][#]])& /@ (List @@ i))));

(* 9/12-1998: Changed t_fatop into t:(fatop[_][__]) below. I think I changed the
pattern in the other functions some time ago, but must have forgotten the one
below. *)

(*Bug fix : For a vertex with a tadpole loop propagator,
  VerticesExtract would see only one leg from the loop, 17/6 - 2000*)
  VerticesExtract[t : (fatop[_][__]) -> fains[fagen][fagr[n_, r___][genins__] ->
            fains[facl][fagr[nn_, rr___][classins__]]]] := (verts =
        Union[Cases[t, favert[_][_], Infinity, Heads -> True]];
      vertsfull = (FullVertex @@
                Cases[t(**)/. p : faprop[_][v_, v_, _] -> Sequence[p, p](**),
                  faprop[_][___, #, ___], Infinity, Heads -> True]) & /@
          verts;
      vertsfull /. {classins} /.
          faprop[_][_, _, fi_] -> fi /. (pa : $ParticleHeads)[i_, ___] :>
          pa[i]);
          
splitinequals[l_] :=
    Union[Table[Select[l, MatchQ[#, l[[i]]] &], {i, Length[l]}]];
VerticesSymmetryFactor[
      t : (fatop[_][__]) ->
        fains[fagen][
          fagr[n_, r___][genins__] ->
            fains[facl][fagr[nn_, rr___][classins__]]]] := (verts =
        Select[VerticesExtract[
            t -> fains[fagen][
                fagr[n, r][genins] ->
                  fains[facl][fagr[nn, rr][classins]]]], (Length[#] > 1) &];
      symfac = Times @@ (Factorial /@ (Length /@
                  Select[splitinequals[verts], (Length[#] > 1) &])); (t ->
          fains[fagen][
            fagr[n/symfac, r][genins] ->
              fains[facl][fagr[nn/symfac, rr][classins]]]));



(* Each set of identical progators with scalar, pseudoscalar, vector or
axial-vector fields gives a factor n!, where n is the number of propagators
in the set: *)



(* 9/12-1998: Changed t_fatop into t:(fatop[_][__]) below. I think I changed the
pattern in the other functions some time ago, but must have forgotten the one
below. It produced wrong large expressions... *)

equalprops[
      t : (fatop[_][__]) ->
        fains[fagen][
          fagr[__][
              genins__] -> __]] := ((*Propagators are grouped with head equals[prop]*)
	sortedt =
        Sort[t] /.
              fatop[tn_][f___, faprop[faloop[n_]][vert1_, vert2_, field_],
                  faprop[faloop[n_]][vert1_, vert2_, fieldd_], l___] :>
                fatop[tn][
                  f, (equals[faprop[faloop[n]][vert1, vert2, field]][field,
                      fieldd]), l] //.
            fatop[tn_][f___,
                equals[faprop[faloop[n_]][vert1_, vert2_, field_]][eqs__],
                faprop[faloop[n_]][vert1_, vert2_, fieldd_], l___] :>
              fatop[tn][
                f, (equals[faprop[faloop[n]][vert1, vert2, field]][eqs,
                    fieldd]),
                l] /. {genins};(*Finally we select the equals[__][__] and replace equals[prop][
            fields] with the number of identical scalars or vectors in fields*)
        Cases[sortedt, equals[__][__], Infinity,
          Heads ->
            True] /. {equals[faprop[_][_, _, fi_]][
                ff__] /; (! FreeQ[fi, $ScalarHeads | $VectorHeads]) :>
            equals[Length[{ff}]],
          equals[faprop[_][_, _, fi_]][
                ff__] /; (FreeQ[fi, $ScalarHeads | $VectorHeads]) :>
            equals[1]});
LoopsSymmetryFactor[
      t : (fatop[_][__]) ->
        fains[fagen][
          fagr[n_, r___][genins__] ->
            fains[facl][fagr[nn_, rr___][classins__]]]] := (symfac =
        Times @@ (equalprops[
                t -> fains[fagen][
                    fagr[n, r][genins] ->
                      fains[facl][fagr[nn, rr][classins]]]] /.
              equals -> Factorial); (t ->
          fains[fagen][
            fagr[n*symfac, r][genins] ->
              fains[facl][fagr[nn*symfac, rr][classins]]]));



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Feynman graphs with external sources *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Support functions for DiscardCT, taken from ../FeynArts/Utilities.m: *)

Renumber[top_] := Block[{s}, s = Sort[(faprop @@ faps[#]) & /@ top];
      s /. MapIndexed[#1 -> Head[#1] @@ #2 &, faverti[s]]];
TopPermute[fatop[_][props__]] :=
    Block[{perm}, perm = Union[Cases[{props}, favert[_][_?Negative], {2}]];
      If[Length[perm] === 0, Return[Renumber[{props}]]];
      Sort[
          Renumber[{props} /. Thread[perm -> #]] & /@
            Permutations[perm]][[1]]];
Compare1[tops : _[]] = tops;
Compare1[tops_] :=
    Block[{perm, p},
      perm = TopPermute /@ (tops/. favert[n_?((# > 1) &), nn___][i_?Positive] :> favert[n,nn][-i]
        (*Make all vertices permutable. Added 22/6 - 2000*)
		(*Was commented out. Uncommented again. 12/10-2000*)
		(*Changed to make only non-external vertices permutable, 28/5-2001*));
      (p = Position[perm, #, 1];
     tops[[p[[1,1]]]](*/. fatop[s_][rest__] -> fatop[s/Length[p]][rest]*)
     (*Commented out to avoid changing the symmetry factors*))& /@
        Union[perm]];



(* DiscardCT converts first order conterterm topologies to non-counterterm
topologies: *)

DiscardCT[
      tops : fatopl[__]] := (maxv =
        Max[(#[[1]]) & /@
              Cases[tops, favert[_][_], Infinity, Heads -> True]] + 1;
      Compare1[(RemoveCT[#, maxv] & /@
              Select[tops, ((Length[Union[Cases[#, favert[_, _][_], {2}]]] <
                          2 && FreeQ[#,
                          favert[_, _?((# > 1) &)][_]]) &)])(*Added 24/5 - 2000*)(*/.
		 favert[a__][b_?((# > 99) &)] -> favert[a][-b]*)
		(*Gave problems with Compare1. Changed 12/10-2000*) ]);
RemoveCT[top_, n_] := top /. favert[i_, 1][_] -> favert[i][n];



(* AddCT generates diagrams with one of the vertices of the original diagrams
replaced by a counterterm vertex: *)
(*Changed 27/5-2001 because FeynArts 3 no longer uses the convention of
        numbering internal vertices higher than 99*)
(*Also, FeynArts 3 wants to have Internal propagators last*)
gs[faprop[fainc][__], faprop[faout][__]] := True;
gs[faprop[faout][__], faprop[fainc][__]] := False;
gs[faprop[faout][__], faprop[faint][__]] := True;
gs[faprop[faint][__], faprop[faout][__]] := False;
AddCT[t : fatopl[__]] := Sort[#, gs] & /@ ((Sequence @@ (# /. (
List /@ (Rule[#, MapAt[ss[#, 1] &, #, {0, 1}]] & /@
Union[Union[Cases[Cases[#, faprop[faint | faloop[__]][__],
  Infinity], favert[_][_], Infinity, Heads -> True],
Cases[#, favert[_?((Abs[#] > 1) &)][_], Infinity, Heads -> True]]])))) & /@ t /.
      ss -> Sequence);


(* DropInternalSelfEnergies drops topologies with selfenergy loops on internal legs.
   Added 30/5-2001. *)
   
(*Select elements that occur more than once in a list*)
SelectRepeated[s_List]:=
(*Added union to deal with more than 2 occurences*)Union[
set = {};
((If[FreeQ[set, #], set = Union[set, {#}]; seq[], #]) & /@ s) /.
seq -> Sequence];

SelectInternalSelfEnergies[fatopl[mesonstop__]] :=
Select[fatopl[mesonstop], (selfprops = {};
      (*Added ctverts to account also for CT's on internal lines. 10/10-2001*)
      ctverts = Alternatives @@ SelectRepeated[Cases[Cases[#,
                faprop[faint][_, _], Infinity], favert[2, ___][_], Infinity]];
      loopprops =
        List @@ Select[#, MatchQ[#, faprop[faloop[_]][v1_, v2_]]&];
      selfprops =
        Union[SelectRepeated[loopprops],
              Select[loopprops, MatchQ[#, faprop[faloop[_]][v_, v_]]&]];
      selfverts = Alternatives @@ Union[Flatten[(List @@ #) & /@ selfprops]];
      (Length[Complement[
                Select[List @@ #,
                MatchQ[#, faprop[faint][___, selfverts, ___]]&],
                selfprops]] === 2 ||
       FreeQ[#, ctverts] =!= True) &&
             FreeQ[List @@ #,
                faprop[faext|faout|fainc][___, selfverts, ___]])&];
                
DropInternalSelfEnergies[fatopl[mesonstop__]] :=
Complement[fatopl[mesonstop],SelectInternalSelfEnergies[fatopl[mesonstop]]];



(* AddExternalLegs puts on an extra propagator on all external legs (modified
versions of functions from FeynArts): *)

AddToLeg[faprop[h : (faext | fainc | faout)][from : (favert[1][_]), to_],
      n_] := seq[faprop[h][from, favert[2][n]],
      faprop[faint][favert[2][n], to]];
AddToLeg[faprop[h : (faint | faloop[__])][
  from : (favert[__(*Added _ to allow CTs. 10/10-2001*)][_]), to_], n_] :=
    faprop[discard][from, to];
(*AddLeg[top_, n_] :=
      fatopl @@ Array[MapAt[AddToLeg1[#, n] &, top, #] &, Length[top]] /.
            fatop[s_][p1___, fatop[snew_][pnew__], p2___] :>
              fatop[s snew][p1, pnew, p2] /. AddToLeg1 -> AddToLeg /.
        seq -> Sequence;*)
(*Changed 27/4 - 2000 to allow more than one external source*)
  AddLeg[top_, n_, opts___Rule] :=
  (exprops = (ExternalPropagators /. Flatten[{opts}] /.
            Options[AddExternalLegs]);
      posis = fccombs[Range[Length[top]], exprops];
      Map[If[MatchQ[#, {_, _}], #[[2]], #] &,
        fatopl @@ (MapAt[AddToLeg1[#[[2]], n + #[[1]] - 1] &,
                        top[[0]] @@ Table[{i, top[[i]]}, {i, Length[top]}],
                        List /@ #] & /@ posis) /.
              fatop[s_][p1___, fatop[snew_][pnew__], p2___] :>
                fatop[s snew][p1, pnew, p2] /. AddToLeg1 -> AddToLeg /.
          seq -> Sequence, {2}]);
          
AddExternalLegs[tops : fatopl[__], opts___Rule] := 
If[faseens /. Flatten[{opts}] /. Options[AddExternalLegs],
  #,DropInternalSelfEnergies[#]]&[
  (maxv = Max[(#[[1]]) & /@
              Cases[tops, favert[__(*Added _ to allow CTs. 10/10-2001*)][_], Infinity, Heads -> True]] +
          1;(*Added Compare1, 22/6 - 2000*)Compare1[
        Select[AddLeg[#, maxv, opts] & /@ tops,
          FreeQ[#, discard, Infinity, Heads -> True] &]])];



(* DiscardTopologies discards topologies with more than a specified number of a
specified type of vertex. *)

DiscardTopology[
      t : (fatop[_][__] -> fains[fagen][__[__] -> fains[facl][__[__]]]),
      opts___] :=
    Block[{ops = (OrderingPatterns /. Flatten[{opts}] /.
              Options[DiscardTopologies])},
      Plus @@ (VerticesExtract[
                  t /.(*Replace external vertex in a propagator with a dummy vertex*)
		  {faprop[p : (faext | fainc | faout)][a_, favert[1, b___][c_], f__] ->
                        faprop[p][a, favert[dummy], f],
                      faprop[p : (faext | fainc | faout)][favert[1, b___][c_],
                           a_, f__] ->
                        faprop[p][favert[dummy], a,
                          f]}] /.(*Replace vertices mathing one of the patterns with a 1*)
			  Alternatives @@ ops -> 1 /.
              FullVertex[__] ->
                0(*and the rest with 0*)) <= (PerturbationOrder /.
              Flatten[{opts}] /. Options[DiscardTopologies])];
DiscardTopologies[tops : (fatopl[___][__])] :=
    Block[{seq}, (If[DiscardTopology[#], #, seq[]] & /@ tops) /.
        seq -> Sequence];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Fixing M$CouplingMatrices with dummy indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Sometimes classes couplings have dummy indices (to be summed over). If two
such couplings are in the same M$CouplingMatrices, a graph with two vertices
with these two couplings will produce wrong results after application of e.g.
SUNReduce or IndicesCleanup. This function addresses this problem. *)

(* Added 29/12-2000: Another problem addressed is that for tadpole loops,
e.g. SUND[i1,I1,I1] can occur, where I1 is meant to be summed over explicitly,
but SUND[i1,I1,I1] is set to zero. The solution is to have SUND replaced with
SU3D and have I1,I2,... be in $ConstantIsoIndices. This of only works as long as
FeynArts sticks to using I1,I2,... as dummy indices in the loops. So, in the
coupling vectors, use SU2F or SU3F etc., not SUNF etc. This is easily achieved
by applying SUNReduce *)

FixCouplingIndices :=
  Block[{newinds, tmpcouplmatr, rep, repp, reppp, i, is,
      wrap = HighEnergyPhysics`Phi`Couplings`Wrap},
    Do[VerbosePrint[2, "Checking M$CouplingMatrices[[", rep, ",", 2, ",",
        repp, ",", reppp, "]]"];
      newinds =
        Complement[
          Select[Union[
                Flatten[
                  Cases[HighEnergyPhysics`FeynArts`M$CouplingMatrices[[rep, 2, repp,
                          reppp]], (SU2Delta | SU2F | SU3Delta | SU3F | SU3D |
                             SUNDelta | fasunf | fasund)[__], Infinity,
                      Heads ->
                        True] /. (SU2Delta | SU2F | SU3Delta | SU3F | SU3D |
                          SUNDelta | fasunf | fasund) -> List]], (!
                    NumberQ[#]) &] /. (fcsuni|fcexsuni)[i_] -> i,
          Union[Flatten[
              HighEnergyPhysics`FeynArts`M$CouplingMatrices[[rep,
                      1]] /. (Alternatives @@ $FAParticlesInUse)[__,
                      is__List] -> is /. C -> List]]];
      If[newinds =!= {},
        VerbosePrint[1, "You're using dummy indices, ", newinds,
          " in M$CouplingMatrices[[", rep, ",", 2, ",", repp, ",", reppp,
          "]]. Wrapping them and the coupling in Wrap. FAToFC will adjust"];
        tmpcouplmatr[rep, repp, reppp] =
          wrap[HighEnergyPhysics`FeynArts`M$CouplingMatrices[[rep, 2, repp, reppp]] /.
              Evaluate[(# -> wrap[#]) & /@ newinds]],
        tmpcouplmatr[rep, repp, reppp] =
          HighEnergyPhysics`FeynArts`M$CouplingMatrices[[rep, 2, repp, reppp]]], {rep,
        Length[HighEnergyPhysics`FeynArts`M$CouplingMatrices]}, {repp,
        Length[HighEnergyPhysics`FeynArts`M$CouplingMatrices[[rep, 2]]]}, {reppp,
        Length[HighEnergyPhysics`FeynArts`M$CouplingMatrices[[rep, 2, repp]]]}];
    HighEnergyPhysics`FeynArts`M$CouplingMatrices =
      Table[HighEnergyPhysics`FeynArts`M$CouplingMatrices[[rep, 1]] ==
          Table[tmpcouplmatr[rep, repp, reppp], {repp,
              Length[HighEnergyPhysics`FeynArts`M$CouplingMatrices[[rep, 2]]]}, {reppp,
              Length[HighEnergyPhysics`FeynArts`M$CouplingMatrices[[rep, 2, repp]]]}], {rep,
          Length[HighEnergyPhysics`FeynArts`M$CouplingMatrices]}];];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Change from FeynArts 2 to FeynCalc 3 notation *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Picking out the classes amplitude, substituting some FeynArts heads with
FeynCalc heads, and adding a Hold to FAFeynAmpDenominator to avoid having it
written out (FeynCalc does not accept it when written out): *)


(* A raw amplitude: *)
classesamplitude[amm__ /; FreeQ[amm, facl]][opts___] :=
(amm /.PropagatorDenominator1->fcprd(*Added 19/7-2001*) //.
{famatr[a___, b_, c___] /; UScalarQ[b] -> b*famatr[a, c],
faferch[a___, b_, c___] /; UScalarQ[b] -> b*faferch[a, c]} /.
fafm[faint, a_] :> fcmom[ToExpression[
(InternalMomentumVariablesString /. Flatten[{opts}] /. Options[FAToFC]) <> ToString[a]],
   D] /.
faintg[a_] -> a /.
fafad[a__] -> Hold[fafad[a]] /.(*Change 15/3 - 1999*)
If[(EqualMasses /. Flatten[{opts}] /. Options[FAToFC]),
          ParticleMass[a_, b___][c_] -> ParticleMass[a, b],
          ParticleMass[a_, b___][c_] -> ParticleMass[a, fcsuni[c], b]]) /.
fcsunt[faind[a__], faind[b__], faind[c__]] :>
UGenerator[fcsuni[faind[a]], fcsunn -> 3][UIndex[faind[b]], UIndex[faind[c]]]/.
{faind[(IsoSpin|facol)[i_], b_] :> ToExpression[ToString[i] <> ToString[b]],
faind[falo, b_] :> fcli[ToExpression[$CouplingLorentzIndicesString <> ToString[b]],
   D]};



(* An amplitude in FeynArts syntax: *)
classesamplitude[amm__ /; ! FreeQ[amm, facl]][opts___] :=
(**)(fapl[facl][amm] /.PropagatorDenominator1->fcprd(*Added 19/7-2001*) //.
{famatr[a___, b_, c___] /; UScalarQ[b] -> b*famatr[a, c],
faferch[a___, b_, c___] /; UScalarQ[b] -> b*faferch[a, c]} /.
fafm[faint, a_] :> fcmom[ToExpression[(InternalMomentumVariablesString /.
Flatten[{opts}] /. Options[FAToFC]) <> ToString[a]], D] /.
faintg[a_] -> a /.
FeynAmpDenominator[a__] -> Hold[FeynAmpDenominator[a]] /.(*Change 15/3 - 1999*)
If[(EqualMasses /. Flatten[{opts}] /. Options[FAToFC]),
          ParticleMass[a_, b___][c_] -> ParticleMass[a, b],
          ParticleMass[a_, b___][c_] -> ParticleMass[a, fcsuni[c], b]]) /.
fcsunt[faind[a__], faind[b__], faind[c__]] :>
UGenerator[fcsuni[faind[a]], fcsunn -> 3][UIndex[faind[b]], UIndex[faind[c]]]/.
{faind[(IsoSpin|facol)[i_], b_] :> ToExpression[ToString[i] <> ToString[b]],
faind[falo, b_] :> fcli[ToExpression[$CouplingLorentzIndicesString <> ToString[b]],
          D]};



(* The amlitude in standard (FeynCalc) notation. The momenta of outgoing
particles are multiplied by -1, and the particles are thus redefined as
incoming: *)

amptablefunc[a__ /; FreeQ[a, facl], j_Integer] :=
    ReleaseHold[Flatten[{a}][[j]]];
amptablefunc[a__ /; ! FreeQ[a, facl], i_Integer] := ReleaseHold[a[[i, 3]]];
amptable[a_] := Table[amptablefunc[a, i], {i, Length[a]}];



(* wrap = HighEnergyPhysics`Phi`Couplings`Wrap *)

fixindices[ampta_List] :=
  Block[{common, rep, repp, wraps, rules, l, inds, newinds, tmpinds, tmpwraps,
       amptab1 = ampta},
      (*Products of identical couplings. Added 26/7-2001*)
      amptab=ampta /.
            HighEnergyPhysics`Phi`Couplings`Wrap[a_]^n_ :>
            (Times @@ (HighEnergyPhysics`Phi`Couplings`Wrap[dumf[#]*a]& /@ Table[ii, {ii, 1, n}]));(**)
    Do[VerbosePrint[2, "Checking amplitude ", repp];
      If[(l = Length[wraps = Cases[amptab[[repp]],
  HoldPattern[Plus[_HighEnergyPhysics`Phi`Couplings`Wrap*__ ..]|
              _HighEnergyPhysics`Phi`Couplings`Wrap], {1}]]) > 1 (*Added 26/7-2001*) &&             
      Head[amptab[[repp]]] === Times,
        VerbosePrint[1, "Amplitude contains ", l,
          " factors with dummy indices. Renaming"]; inds = {};
        Do[newinds = (#[[1]])& /@
              Union[Cases[wraps[[l]],
                  HighEnergyPhysics`Phi`Couplings`Wrap[_?AtomQ], Infinity]];
          tmpinds = newinds;
          While[(common = Intersection[inds, tmpinds]) =!= {},
            tmpinds = (If[FreeQ[common, #], #,
                      ToExpression[
                        StringTake[ToString[#], 1] <> ToString[#]]] &) /@
                tmpinds];
          rules = (((Rule @@ #) &) /@
                Select[Transpose[{newinds, tmpinds}], (#[[1]] =!= #[[2]]) &]);
           VerbosePrint[2, StandardForm[rules]];
          tmpwraps[rep] = (wraps[[rep]] /. rules);
          inds = Join[inds, tmpinds];, {rep, l}];
        amptab1 =
          ReplacePart[
            amptab1, (((Times @@ (tmpwraps /@ Range[l]))*(amptab1[[repp]] /.
            HoldPattern[Plus[_HighEnergyPhysics`Phi`Couplings`Wrap*__ ..]] -> 1 /.
  HighEnergyPhysics`Phi`Couplings`Wrap[_?(!FreeQ[#,HighEnergyPhysics`Phi`Couplings`Wrap]&)] -> 1))) /.
            HighEnergyPhysics`Phi`Couplings`Wrap -> Identity,
            repp]], {repp, Length[amptab1]}]; amptab1 /. _dumf -> 1 /. Wrap -> Identity];

(* While[(common = Intersection[inds, tmpinds]) =!= {},
  tmpinds = (If[FreeQ[common, #], #,
            ToExpression[StringTake[ToString[#], 1] <> ToString[#]]] &) /@
      tmpinds]; rules = (((Rule @@ #) &) /@
      Select[Transpose[{newinds, tmpinds}], (#[[1]] =!= #[[2]]) &]); *)

FAToFC[amm_, opts___] := (traceev = (fcdtrev /. Flatten[{opts}] /. Options[FAToFC]);
    (**)(*selecting the classes amplitude and transforming
    the four momenta and indices*)

    VerbosePrint[2, "Extracting Classes amplitudes\n"];
    tmptable =
      amptable[classesamplitude[amm(*Added 14/3 - 2000*)/.
         fafv[0, _] -> 0/.(*Workaround because DiracGamma strips
	 D if not in both DiracGamma and Momentum. 6/8-2000*)
	 fcdiga->tmpdiga][opts]];
         tmptable1 = fixindices[tmptable];

    VerbosePrint[2, "\nApplying translation rules"];
  tmptable1 /.
(*Momenta*){
   fafm[fainc, b_] :> fcmom[ToExpression[(MomentumVariablesString /.
      Flatten[{opts}] /. Options[FAToFC]) <> ToString[b]], D],
   If[(MomentaSumLeft /.Flatten[{opts}] /. Options[FAToFC]) === All,
      fafm[faout,b_] :>(**)-fcmom[ToExpression[(MomentumVariablesString /.
         Flatten[{opts}] /. Options[FAToFC]) <> ToString[b + ((ParticlesNumber/2) /.
         Flatten[{opts}] /. Options[FAToFC])]],D],
      fafm[faout, b_] :>(**)fcmom[ToExpression[(MomentumVariablesString /.
         Flatten[{opts}] /. Options[FAToFC]) <> ToString[b + ((ParticlesNumber/2) /.
         Flatten[{opts}] /. Options[FAToFC])]],D]]
} /.
(*SU(N) delta function*) If[(FADeltas /. Flatten[{opts}] /. Options[DeltaFunctionProducts]),
                         $FADelta[aa_, bb_] :> fcsundel[fcsuni[aa],fcsuni[bb]] /;
                           !FreeQ[{aa,bb}, IsoSpin, Heads -> True], {}] /.
(*Four-vectors and scalar products*){
   (*Changed 6/1 - 2000*)(*fafv -> fcpa*)
   fafv[a_?((!FreeQ[#, fafm | fcmom])&), b_?((Head[#] =!= fcli)&)] -> fcpa[a, fcli[b]],
   fafv[a_?((!FreeQ[#, fafm | fcmom])&), b_?((Head[#] === fcli)&)] -> fcpa[a, b],
(*Added 25/9-2002 in order to have Amplitude work*)
   fafv[a_?((FreeQ[#, fafm | fcmom])&), b_?((Head[#] =!= fcli)&)] -> fcpa[fcli[b], fcmom[a]],
   famt[li1_, li2_] /; FreeQ[{li1, li2}, fcli] ->
      fcpa[fcli[li1, D],fcli[li2, D]],
   famt[li1_, li2_] /; !FreeQ[{li1, li2}, fcli] -> fcpa[li1, li2],

   If[(FADeltas /. Flatten[{opts}] /. Options[DeltaFunctionProducts]),
   $FADelta[aa_, bb_] :> fcsundel[fcsuni[aa], fcsuni[bb]] /;
                         !FreeQ[{aa,bb}, IsoSpin, Heads -> True], dum->dum],

   UGenerator[I5_, op___][J3_, J1_] :>
      UGenerator[fcsuni[I5], op][UIndex[J3],UIndex[J1]] /;
      FreeQ[{I5}, fcsuni|fcexsuni, Heads->True] &&
      FreeQ[{J3,J1}, fcsuni|fcexsuni|UIndex, Heads->True], (*fapd -> fcprd, *)
   fafad -> fcfad,
   (ScalarProductForm /. Flatten[{opts}] /. Options[FAToFC])[
      a_, b_] /; FreeQ[{a, b}, fcmom] -> fcscp[a, b, fcdim -> D],
   (ScalarProductForm /. Flatten[{opts}] /.
      Options[FAToFC])[a_, b_] /; !FreeQ[{a, b}, fcmom] -> fcpa[a, b]
} /.
(*SU(N) stuff*)(*Changed 22/2 - 2000*)(*{SU3D :> fcsund, SU2F :> fcsunf, SU3F :> fcsunf} /.*)
{
   fcsund[ii__] :> fcsund @@ (fcsuni /@ {ii}),
   fcsunf[ii__] :> fcsunf @@ (fcsuni /@ {ii}),(*Added*)
   fcsundel[ii__] :> fcsundel @@ (fcsuni /@ {ii}),
   SU2F[ii__] :> SU2F @@ (fcsuni /@ {ii}),
   SU3F[ii__] :> SU3F @@ (fcsuni /@ {ii}),
   SU3D[ii__] :> SU3D @@ (fcsuni /@ {ii}),
   SU2Delta[ii__] :>
   SU2Delta @@ (fcsuni /@ {ii}),
   SU3Delta[ii__] :>
   SU3Delta @@ (fcsuni /@ {ii}),(*End add*)
   Projection[i_Integer][j_] :> Projection[i][fcsuni[j]]
} /.
(*Added 13/10-2000*)
UGenerator[ii_,op___] :> UGenerator[fcsuni[ii],op] /.
(*Polarization vectors*)
fcpa[a_,b : Plus[-1*_, __]] :> -fcpa[a, -b] /.
(*polarization vectors*){
   Conjugate[fapolv][_, f_*fcmom[m_, d___],
   fcli[l_, ___]] -> fcpa[fcli[l], f*fcmom[fcpol[m, -I], d]],
  Conjugate[fapolv][_, fcmom[m_, d___], fcli[l_, ___]] ->
     fcpa[fcli[l], fcmom[fcpol[m, -I], d]],
  fapolv[_, f_*fcmom[m_, d___], fcli[l_, ___]] ->
    fcpa[fcli[l], f*fcmom[fcpol[m, I], d]],
  fapolv[_, fcmom[m_, d___], fcli[l_, ___]] ->
    fcpa[fcli[l], fcmom[fcpol[m, I],d]]
 }/.
(*Dirac stuff*){
   (*fadm[p_] -> fcdiga[p, D],*)
   (*Change 28/6-2002*)
   tmpdiga[p_?((!FreeQ[{#},fcmom|fcli])&)] :> fcdiga[p, D],
   tmpdiga[p_?(FreeQ[{#},fcmom|fcli]&)] :>(fcdiga[fcli[p,D], D])(*,
   fachiralp -> fcchiralp, fadsl[a_] :> fcdiga[a, D]*)
} /.
{fanoncom[a_, b__] :> (fcdot[a, b]/.fanoncom->Identity), 
fanoncom[a_] :> (a/.fanoncom->Identity(*Changed 31/1-2002. I have no idea* why mma suddenly screws up here*))} /.
famatr[mat__] -> fcdtr[fcdot[mat], fcdtrev -> False] /.
(*fixing the last momenta without D*)
{fcpa -> fcpa1, fcmom -> fcmom1} /.
{fcmom1[a_] -> fcmom1[a, D],fcli[a_] -> fcli[a, D]} /.
{fcmom1 -> fcmom, fcpa1 -> fcpa} /.
(*Fermions*){fads -> fcspi, faferch -> fcdot} /.
(fcdtrev -> False) -> (fcdtrev -> traceev) /.
        If[(Sum /. Flatten[{opts}] /. Options[FAToFC]) === False,
          faso[___] -> 1,faso[i_, r_, ___] :>
            faso[fcsuni[i], r] /; !FreeQ[{i}, IsoSpin, Heads -> True]]
	  (*Changed 9/1 - 2000 (was empty). Changed 17/7-2002 (keeping r)*)
     (*Changed 21/8-2002. We might need other index types.*) //.
      If[(Sum /. Flatten[{opts}] /. Options[FAToFC]) === fcexpt,
           Times[f__, faso[i_, r_, ___]] :>
         (VerbosePrint[2, "Summing ", i, " from 1 to ", r];
          Sum[Times[f], {i, 1, r}]), {}] /.(*Changed 11/1 - 2000, added SUND and SUNF*)
	{fcsundel[a_, b_] /; FreeQ[{a, b}, fcsuni|fcexsuni] ->
          fcsundel[fcsuni[a], fcsuni[b]],
        fcsund[a_, b_, c_] /; FreeQ[{a, b, c}, fcsuni|fcexsuni] ->
          fcsund[fcsuni[a], fcsuni[b], fcsuni[c]],
        fcsunf[a_, b_, c_] /; FreeQ[{a, b, c}, fcsuni|fcexsuni] ->
          fcsunf[fcsuni[a], fcsuni[b], fcsuni[c]],
        SU2F[a_, b_, c_] /; FreeQ[{a, b, c}, fcsuni|fcexsuni] ->
          SU2F[fcsuni[a], fcsuni[b], fcsuni[c]],
        SU3D[a_, b_, c_] /; FreeQ[{a, b, c}, fcsuni|fcexsuni] ->
          SU3D[fcsuni[a], fcsuni[b], fcsuni[c]],
        SU3F[a_, b_, c_] /; FreeQ[{a, b, c}, fcsuni|fcexsuni] ->
          SU3F[fcsuni[a], fcsuni[b], fcsuni[c]]} /.
   (*Added 29/6-2002*) fcexli[a_, ___] -> a) //.
   (*Added 13/8-2002 because patching FA model files gives structures like
     DiracMatrix[Index[Lorentz, 1]] which is auto-expanded by FC into
     DiracGamma[LorentzIndex[Index[Lorentz, 1]]]*)
     fcli[fcli[mu_, d___], dd___] :> fcli[mu,Sequence@@Union[{d},{dd}]];

DoSumOver[exp_] := exp //. Times[f__, faso[i_, r_, ___]] :>
       (VerbosePrint[2, "Summing ", i, " from 1 to ", r];Sum[Times[f], {i, 1, r}]);

DeltaFunctionsCollect[ampf__, opts___] :=
    Collect[ampf,
      Union[Flatten[((DeltaFunctionProducts @@
                  Join[Flatten[{opts}], Options[DeltaFunctionsCollect]]) /.
              Times -> List)],
        Flatten[((DeltaFunctionProducts @@
                    Join[Flatten[{opts}], Options[DeltaFunctionsCollect]]) /.
                Times -> List)] /. $FADelta -> fcsundel,
        Flatten[((DeltaFunctionProducts @@
                    Join[Flatten[{opts}], Options[DeltaFunctionsCollect]]) /.
                Times -> List)] /. $FADelta[aa_, bb_] :>
            fcsundel[fcsuni[aa], fcsuni[bb]] /;
            !FreeQ[{aa,bb}, IsoSpin, Heads -> True]]];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* CreateFCAmp *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* Some reshuffling of code to other files might be in place, in order 
   to minimize the use of giving explicit contexts like
   HighEnergyPhysics`Phi`Utilities` . Some other time...*)

(* Help functions for wave function renormalization *)

genFac[prop_, opts___Rule] := 
    Block[{nam, dum}, 
      nam = XName[VertexFields -> {prop[[-1]]}, 
            PerturbationOrder -> (PerturbationOrder /. {opts} /. 
                  Options[WFRenormalize]), 
            PhiModel -> Global`$Configuration, XFileName -> Automatic] <> ".Fac"; 
      Which[MatchQ[prop[[0, 1]], 
          fainc | faout | faext], (3 - 
              HighEnergyPhysics`Phi`Utilities`CheckF[WFFactor[nam], nam,
              HighEnergyPhysics`Phi`Utilities`NoSave -> True,
              HighEnergyPhysics`Phi`Utilities`ForceSave -> False])/2 - 1, 
        MatchQ[prop[[0, 1]], faint | faloop], 
        3 - HighEnergyPhysics`Phi`Utilities`CheckF[WFFactor[nam], nam,
            HighEnergyPhysics`Phi`Utilities`NoSave -> True,
            HighEnergyPhysics`Phi`Utilities`ForceSave -> False] - 2, True, 
        Message[WFRenormalize::"noprop", prop[[0, 1]]]; Return[]]];

WFRenormalize[exp : fatopl[oo___][tt___], opts___Rule] := 
    Block[{props, facs, cruls, parts, part, rul, tmppart}, 
      cruls = (parts = {}; (part = #[[1]]; (rul = (tmppart = #[[1]][0,
                 {#[[2, 1]] | fcsuni[#[[2, 1]]] | fcexsuni[#[[2, 1]]]}]; 
                  tmppart) -> part[0];
                  If[FreeQ[parts /. Alternatives :> (({##}[[1]]) &), 
                   tmppart], parts = Append[parts, tmppart]; rul, seq[]]) & /@ 
                      Cases[{#[[2]]}, _Iso, Infinity]) & /@
      HighEnergyPhysics`Phi`Channels`$IsoSpinProjectionRules /. 
              seq -> Sequence // Flatten); 
      props = List @@ ((List @@ (#[[1]]/. 
                          List @@ #[[2, 1, 2, 1]] /. cruls)) & /@ exp); 
      VerbosePrint[3, "Renormalizing propagators ", props]; 
      facs = (Rule[#, genFac[#, opts]] & /@ #) & /@ props; 
      VerbosePrint[3, "with factors ", facs]; 
      fatopl[oo, fawfcr -> facs][tt]];

appendMoms[toplist : fatopl[oo___][tt___]] := 
    Block[{fieldsubs, momtop, oldmom, imom,
        HighEnergyPhysics`FeynArts`Analytic`mc, 
        HighEnergyPhysics`FeynArts`Analytic`next, 
        info = List @@ toplist [[0]], 
        HighEnergyPhysics`FeynArts`Analytic`gaugeru = 
          GaugeRules /. Options[facrfa],
HighEnergyPhysics`FeynArts`Analytic`truncru = 
          If[TrueQ[Truncated /. Options[facrfa]], 
            fatrru, {}], 
        HighEnergyPhysics`FeynArts`Analytic`pref = 
          PreFactor /. Options[facrfa]}, 
      fieldsubs = (List @@ Take[#, {1, 2}][[2, 1, 1]]) & /@ List @@ toplist; 
      HighEnergyPhysics`FeynArts`Analytic`next = 
        Plus @@ Length /@ (Process /. {info}); 
      momtop = (Clear[HighEnergyPhysics`FeynArts`Analytic`c, 
                HighEnergyPhysics`FeynArts`Analytic`mc]; 
              HighEnergyPhysics`FeynArts`Analytic`c[_] = 0; 
              HighEnergyPhysics`FeynArts`Analytic`mc = 0; 
              HighEnergyPhysics`FeynArts`Analytic`AppendMomentum /@ #[[1]])& /@
(fapl[facl][toplist] /. (_ -> fains[_][]) :> 
                  Seq[] /. (fafi[i_] -> fi_?AtomQ) -> (fafi[i] -> 
                    fi[faind[fagen, i]])); 
      oldmom = Union[
          Cases[momtop, fafm[_HighEnergyPhysics`FeynArts`Analytic`ZZZ, _], 
            Infinity]];
imom = Apply[HighEnergyPhysics`FeynArts`Analytic`RenumberMom, oldmom, 1];
momtop[[0]] @@ ((#[[1]] /. #[[2]]) & /@ 
            Transpose[{(List @@ momtop /. Thread[oldmom -> imom]), 
                fieldsubs}])];

(**)

(* Help functions for mass renormalization *)

genMass2[mass_, opts___Rule] := 
    Block[{nam, dum}, 
      nam = XName[VertexFields -> {mass[[1]][0]}, 
            PerturbationOrder -> (PerturbationOrder /. {opts} /. 
                  Options[PMRenormalize]), PhiModel -> Global`$Configuration, 
            XFileName -> Automatic] <> ".Mass";
      HighEnergyPhysics`Phi`Utilities`CheckF[Shift[nam], nam, 
        HighEnergyPhysics`Phi`Utilities`NoSave -> True, 
        HighEnergyPhysics`Phi`Utilities`ForceSave -> False]];

PMRenormalize[amp_, opts___Rule] := 
   Block[{i, p, m, r, rr, pm, por, drru},
       por = (PerturbationOrder /. {opts} /. Options[PMRenormalize]);
      (*Should be safe for corrections...*)
      drru = {ParticleMass[pp_, RenormalizationState[0]] -> 
                   ParticleMass[pp, RenormalizationState[1]],
              (x : (Alternatives @@ $ExpansionQuantities)) :>
              (x /. RenormalizationState[0] -> RenormalizationState[1])};

      amp /. (l : (HighEnergyPhysics`Phi`Renormalization`LeutwylerJBar | Log))[s__] :>
      (l[s] /. ParticleMass -> pm) /.
      {fcprd[p_, 
              ParticleMass[m_, r___, RenormalizationState[0], rr___]] -> 
         fcprd[p, 
              ParticleMass[m, r, RenormalizationState[1], rr]], 
       ParticleMass[p_, RenormalizationState[0]]^i_ :>
         (ParticleMass[p, RenormalizationState[1]]^2 - 
                  genMass2[ParticleMass[p, RenormalizationState[0]], opts]/.
               drru)^(i/2)} /.
      pm -> ((ParticleMass[##]/.drru)&)];

(**)

(* Help functions for decay constant renormalization *)

genF2[ff_, opts___Rule] := 
    Block[{nam, dum}, 
      nam = XName[VertexFields -> {AxialVector[0][0], ff[[1]][0]}, 
            PerturbationOrder -> (PerturbationOrder /. {opts} /. 
                  Options[DCRenormalize]), PhiModel -> Global`$Configuration, 
            XFileName -> Automatic] <> ".Fac";
      HighEnergyPhysics`Phi`Utilities`CheckF[Shift[nam], nam, 
        HighEnergyPhysics`Phi`Utilities`NoSave -> True, 
        HighEnergyPhysics`Phi`Utilities`ForceSave -> False]];

DCRenormalize[amp_, opts___Rule] := 
    Block[{po, ca, xxs, ex,
           $ExpansionQuantities = {ParticleMass[b__], CouplingConstant[QED[1], c___]},
           mms, cou, ruls, ruls1, f, ff, g, x, fac, facinv, pop, len, por},
   por = (PerturbationOrder /. {opts} /. Options[DCRenormalize]);
   pop = HighEnergyPhysics`Phi`Utilities`DiscardOrders[#,
                     PerturbationOrder -> por, DiscardMomenta -> False]&;

   (*Should be safe for corrections...*)
   drru = DecayConstant[pp_, RenormalizationState[0]] -> 
                   DecayConstant[pp, RenormalizationState[1]];

   Plus@@(
   Which[(len=Length[ca=Cases[{#}, DecayConstant[_, RenormalizationState[0], ___],
                          Infinity, Heads->True]])>1,
       Message[DCRenormalize::"nores"]; #,

       len === 0, #,

       len === 1,

       po = Exponent[#, ca[[1]]];

       Which[po==0, Message[DCRenormalize::"nores"]; #,
             po>0, fac = genF2[ca[[1]], opts]^po // pop;
                   (fac/.drru)*(# /. DecayConstant[pp_, RenormalizationState[0]] -> 
                   DecayConstant[pp, RenormalizationState[1]]),
             po<0, VerbosePrint[2, "Negative power of decay constant ", ca[[1]]," ", po,
                                   ". Doing Taylor expansion"];
                   facinv = genF2[ca[[1]], opts];
                   mms = Union[Cases[facinv,
                         Alternatives @@ $ExpansionQuantities, Infinity]];
                   (*Dummy variables x[1], x[2], ... One for each expansion quantity*)
                   cou = 0; xxs = (++cou; x[cou]) & /@ mms;
                   (*Substitution rules for switching between x[1], x[2], ...
                     and the expansion quantities*)
                   cou = 0; ruls = ((#^i_ -> (++cou; x[cou]^(i/2))) & /@ mms);
                   cou = 0; ruls1 = (((++cou; x[cou]) -> #^2) & /@ mms);
                   f[x_] = facinv /. _Log -> 0 /. ruls;
                   (*Define the function g[x1_, x2_, ...]*)
                   Evaluate[g @@ xxs /. 
                     x[a_] :>
                     Pattern[Evaluate[ToExpression[ToString[x] <> ToString[a]]], 
                     Blank[]]] = f[x] - 1 /.
                                 x[a_] :> ToExpression[ToString[x] <> ToString[a]];
                   (*Multi-dimensional Taylor expansion of the inverse
                     renormalization factor*)
                   fac = ((Series[1/(1 + (ff @@ xxs)), Sequence @@ ({#, 0, 1}& /@ xxs)] /. 
                     ff[_?((# === 0)&) ..] -> 0 // Normal) /. ff :> g /. ruls1)^(-po) // 
                     pop;
                   (fac/.drru)*(# /. DecayConstant[pp_, RenormalizationState[0]] -> 
                     DecayConstant[pp, RenormalizationState[1]])
             ]

      ]& /@ (ex=Expand[amp];If[Head[ex]===Plus,List@@ex,{ex}]))
   ];

(**)

CreateFCAmp[amp_, opts___] := Block[{me, propmoms, pprops, wffacs, wffac},

   (*Wave function renormalization. 8/8-2002*)

   If[WFRenormalize /. Flatten[{opts}] /. Options[CreateFCAmp],
     VerbosePrint[2, "Doing wave function renormalization\n"];

   propmoms = 
     FAToFC /@ ((List @@ #) & /@ 
        List @@ appendMoms[amp] /. 
          faprop[a_][b__] :> ({b}[[-1]]));

   pprops = (((#[[2]]) & /@ #) & /@ (fawfcr /. 
          List @@ WFRenormalize[amp, opts][[0]]));

   wffacs = ((#[[1]] /. (Append[Cases[#, fcmom[___], Infinity], 
    classesamplitude[{fafm[fainc, 1]}][opts][[1]]][[1]] /. 
    D :> BlankNullSequence[]) -> #[[2]])& /@ #)& /@
    (Transpose /@ Transpose[{pprops, propmoms}]);

   wffac = Which[(me=Method/.Flatten[{opts}]/.Options[CreateFCAmp]) ===
        HighEnergyPhysics`Phi`Utilities`DiscardOrders,
        HighEnergyPhysics`Phi`Utilities`DiscardOrders[
         Times @@ ((1+#)&/@#), Sequence@@OptionsSelect[HighEnergyPhysics`Phi`Utilities`DiscardOrders,
                      Flatten[{opts}/.Rule[PerturbationOrder,_]:>Sequence[]/.
                              DropOrder->PerturbationOrder]]]& /@ wffacs,
       me === Plus,
                                ((1 + Plus @@ #) & /@ wffacs),
      True, Message[CreateFCAmp::"nomethod", me]];

    (Times@@#)&/@ Transpose[{wffac,FAToFC[facrfa[amp, faal -> facl], opts]}],

    FAToFC[facrfa[amp, faal -> facl], opts]]
   (**)

];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Generation of FeynArts 2 model files *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



$GenObjects = {"M$GenericCouplings", "M$FermionFlipRule",
      "M$LastGenericRules"};
GenSave[modelname_String] :=
    (
      t = Date[];
      dd =
        ToString[t[[3]]] <> "/" <> ToString[t[[2]]] <> "-" <>
          ToString[t[[1]]] <> ", " <> ToString[t[[4]]] <> ":" <>
          ToString[t[[5]]] <> ":" <> ToString[t[[6]]];

      tmp`olddir = Directory[];
      SetDirectory[HighEnergyPhysics`Phi`$HEPDir];
      SetDirectory["HighEnergyPhysics"];
      SetDirectory["Models"];

      strm = OpenWrite[modelname <> ".gen"];
      WriteString[strm,
        "(* ****************************************************************** \ *)\n\n"];
      WriteString[strm, "(* " <> modelname <> ".gen *)\n\n"];
      WriteString[strm,
        "(* Coupling definitions generated by Phi, " <> dd <> ". *)\n\n"];
      WriteString[strm,
        "(* ****************************************************************** \ *)"];
      Close[strm];

      VerbosePrint[2, "Will save definitions to file "modelname <> ".gen"];

      strm = OpenAppend[modelname <> ".gen"];
      WriteString[strm,
        "\n\n\n(* ****************************************************************** \ *)\n"];
      WriteString[strm,
        "(* Definitions for PropagatorType,*)\n(* PropagatorArrow and KinematicIndices *)\n"];
      WriteString[strm,
        "(* ****************************************************************** \ *)\n\n"];
      Save[
        modelname <> ".gen", {HighEnergyPhysics`FeynArts`PropagatorType,
          HighEnergyPhysics`FeynArts`PropagatorArrow, HighEnergyPhysics`FeynArts`KinematicIndices}];
      WriteString[strm, "\n\n"];
      Close[strm];

      VerbosePrint[2,
        "Saving definition of AnalyticalPropagator to temporary file dum"];

      (*Save definition of AnalyticalPropagator for reloading later*)

      Save["dum", HighEnergyPhysics`FeynArts`AnalyticalPropagator];
      Clear[HighEnergyPhysics`FeynArts`AnalyticalPropagator];

      VerbosePrint[2, "Saving definitions to file "modelname <> ".gen"];

      (*Now generate the definition lines for M$GenericPropagators*)

      genprops =
        "M$GenericPropagators = " <>
          ToString[
            InputForm[
              Flatten[HighEnergyPhysics`Phi`Couplings`GenProps /@ \
(Join[Flatten[{List @@ $ScalarHeads} /. None -> Sequence[]],
                        Flatten[{List @@ $VectorHeads} /. None -> Sequence[]],
                         Flatten[{List @@ $FermionHeads} /.
                            None -> Sequence[]]] /. None -> Sequence[])]]];

      (*Append the lines to the model file*)

      strm = OpenAppend[modelname <> ".gen"];
      WriteString[strm,
        "(* ****************************************************************** \ *)\n"];
      WriteString[strm, "(* Definition of M$GenericPropagators *)\n"];
      WriteString[strm,
        "(* ****************************************************************** \ *)\n\n"];
      WriteString[strm, genprops <> "\n\n"];

      (*Append the rest of the definitions*)
      Do[
        WriteString[strm,
          "(* ****************************************************************** \ *)\n"];
        WriteString[strm,
          "(* Definition of " <> $GenObjects[[i]] <> " *)\n"];
        WriteString[strm,
          "(* ****************************************************************** \ *)\n\n"];

        WriteString[strm, $GenObjects[[i]] <> " =\n"];
        Write[strm, ToExpression[$GenObjects[[i]]]];
        WriteString[strm, "\n\n"];,
        {i, Length[$GenObjects]}];

      Close[strm];

      VerbosePrint[2,
        "Reloading definition of AnalyticalPropagator from - and deleting - file dum"];

      (*Reestablish defintion of AnalyticalPropagator*)

      strm = OpenRead["dum"];
      str = ""; str1 = ""; stop = 0;
      While[str != "EndOfFile", str = Read[strm, String];
        If[stop < 2,
          If[StringMatchQ[str, "*AnalyticalPropagator/:*"], stop = 2]];
        If[stop < 2,
          If[str == " ", stop = stop + 1; str1 = str1 <> ";\n\n",
            str1 = str1 <> str]]]; Close[strm];
      DeleteFile["dum"];
      (*str1 =
            StringReplace[str1,
              "AnalyticalPropagator" -> "HighEnergyPhysics`FeynArts`AnalyticalPropagator"];*)

          VerbosePrint[3, "The definition of AnalyticalPropagator: ", str1];
      ToExpression[str1];

      SetDirectory[tmp`olddir];

      );
$ModObjects = {"$ScreenSymbolFont", "M$ClassesDescription",
      "M$CouplingMatrices", "M$LastModelRules"};
ModSave[modelname_String] :=
    (
      t = Date[];
      dd =
        ToString[t[[3]]] <> "/" <> ToString[t[[2]]] <> "-" <>
          ToString[t[[1]]] <> ", " <> ToString[t[[4]]] <> ":" <>
          ToString[t[[5]]] <> ":" <> ToString[t[[6]]];

      tmp`olddir = Directory[];
      SetDirectory[HighEnergyPhysics`Phi`$HEPDir];
      SetDirectory["HighEnergyPhysics"];
      SetDirectory["Models"];

      VerbosePrint[2, "Will save definitions to file "modelname <> ".mod"];

      strm = OpenWrite[modelname <> ".mod"];
      WriteString[strm,
        "(* ****************************************************************** \ *)\n\n"];
      WriteString[strm, "(* " <> modelname <> ".mod *)\n\n"];
      WriteString[strm,
        "(* Coupling definitions generated by Phi, " <> dd <> ". *)\n\n"];
      WriteString[strm,
        "(* ****************************************************************** \ *)"];
      Close[strm];

      VerbosePrint[2,
        "Using temporary file dum for generating definition of IndexRange"];

      strm = OpenWrite["dum"]; Save[strm, HighEnergyPhysics`FeynArts`IndexRange]; Close[strm];

      strm = OpenRead["dum"];
      str = ""; str1 = ""; stop = False;

      While[str != "EndOfFile", str = Read[strm, String];
        If[stop == False,
          If[StringMatchQ[str, "*MakeBoxes*"] ||
              StringMatchQ[str, "*Attributes*"], stop = True]];
        If[stop == False,
          If[str == " ", str1 = str1 <> ";\n\n", str1 = str1 <> str]]];
      Close[strm]; DeleteFile["dum"];

      str1 = FixedPoint[StringReplace[#, "  " -> " "] &, str1];

      strm = OpenWrite["dum"]; Save[strm, HighEnergyPhysics`Phi`Couplings`FAParticleMass];
      Close[strm];

      strm = OpenRead["dum"];
      str = ""; str2 = ""; stop = False;

      While[str != "EndOfFile", str = Read[strm, String];
        If[stop == False,
          If[StringMatchQ[str, "*MakeBoxes*"] ||
              StringMatchQ[str, "*Attributes*"], stop = True]];
        If[stop == False,
          If[str == " ", str2 = str2 <> ";\n\n", str2 = str2 <> str]]];
      Close[strm];

      VerbosePrint[2, "Deleting file dum"];
      DeleteFile["dum"];

      str2 = FixedPoint[StringReplace[#, "  " -> " "] &, str2];
      str2 = str2 <> "\n";

      VerbosePrint[2, "Saving definitions to file "modelname <> ".mod"];

      strm = OpenAppend[modelname <> ".mod"];
      WriteString[strm,
        "\n\n\n(* ****************************************************************** \ *)\n"];
      WriteString[strm,
        "(* Definitions for IndexRange and Mass *)\n"];
      WriteString[strm,
        "(* ****************************************************************** \ *)\n\n"];

      WriteString[strm, str1];
      WriteString[strm, str2];
      Close[strm];

      strm = OpenAppend[modelname <> ".mod"];
      Do[
        WriteString[strm,
          "(* ****************************************************************** \ *)\n"];
        WriteString[strm,
          "(* Definition of " <> $ModObjects[[i]] <> " *)\n"];
        WriteString[strm,
          "(* ****************************************************************** \ *)\n\n"];

        WriteString[strm, $ModObjects[[i]] <> " =\n"];
        Write[strm, ToExpression[$ModObjects[[i]]]];
        WriteString[strm, "\n\n"];,
        {i, Length[$ModObjects]}];
      Close[strm];

      SetDirectory[tmp`olddir];

      );

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

End[];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

EndPackage[];

If[$Verboseness > 0,WriteString["stdout", "Couplings | \n "]];
