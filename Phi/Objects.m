(* Objects *)

(* Definitions for basic objects and operations of Phi *)

(* Author:  Frederik Orellana, fjob@cabocomm.dk

   Date:  1/8-2000

   Context: HighEnergyPhysics`Phi`Objects`

   Package version:  1.2

   Mathematica version:  4.0 *)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* SECTIONS:

   1)  Non-commutative multiplication
   2)  Iso-vector products
   3)  Power functions
   4)  Scalars
   5)  Matrices
   6)  Iso-vectors
   7)  Explicit objects
   8)  Field matrices
   9)  Derivatives
   10) Adjoints and conjugates
   11) Traces
   12) SU(2) and SU(3) structure constants
   13) Supplying iso-indices
   14) Transformation to FC notation
   15) Discarding terms
   16) Easy entering of lagrangians
   17) Commutation rules

*)

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

BeginPackage["HighEnergyPhysics`Phi`Objects`", {"HighEnergyPhysics`Phi`"}];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

tmp`olddir = Directory[];
SetDirectory[HighEnergyPhysics`Phi`$HEPDir];
SetDirectory["HighEnergyPhysics"];
SetDirectory["Phi"];
Get["Objects.defs.m"];
SetDirectory[tmp`olddir];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

Begin["`Private`"];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

(* FeynCalc functions *)

fcpd := fcpd = HighEnergyPhysics`FeynCalc`PartialD`PartialD;
fcli := fcli = HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex;
fcsuni := fcsuni = HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex;
fcqf := fcqf = HighEnergyPhysics`FeynCalc`QuantumField`QuantumField;
fcsunn := fcsunn = HighEnergyPhysics`FeynCalc`SUNN`SUNN;
fcsunt := fcsunt = HighEnergyPhysics`FeynCalc`SUNT`SUNT;
fcsund := fcsund = HighEnergyPhysics`FeynCalc`SUND`SUND;
fcsunf := fcsunf = HighEnergyPhysics`FeynCalc`SUNF`SUNF;
fcsundel := fcsundel = HighEnergyPhysics`FeynCalc`SUNDelta`SUNDelta;
fctr := fctr = HighEnergyPhysics`FeynCalc`SUNTrace`SUNTrace;
fcdot := fcdot = HighEnergyPhysics`FeynCalc`DOT`DOT;
fcexpt := fcexpt = HighEnergyPhysics`fctools`Explicit`Explicit;
fcdots := fcdots = HighEnergyPhysics`FeynCalc`DotSimplify`DotSimplify;
fcexsuni := fcexsuni = HighEnergyPhysics`qcd`ExplicitSUNIndex`ExplicitSUNIndex;
fcpa := fcpa = HighEnergyPhysics`FeynCalc`Pair`Pair;
fccc := fccc = HighEnergyPhysics`fctools`ComplexConjugate`ComplexConjugate;
fclag := fclag = HighEnergyPhysics`fctables`Lagrangian`Lagrangian;
fccoupl := fccoupl = HighEnergyPhysics`FeynCalc`CouplingConstant`CouplingConstant;



(* Defaults *)

$Substitutions = {};
$PreSubstitutions[x_?((# =!= 0) &), ar___] :=
  ($PreSubstitutions[0] /. {SubX -> x, SubArgs :> Sequence[ar]});
$PostSubstitutions[x_?((# =!= 0) &), ar___] :=
  ($PostSubstitutions[0] /. {SubX -> x, SubArgs :> Sequence[ar]});
$PreSubstitutions[0] = {};
$PostSubstitutions[0] = {};
$StandardSUNBasis = True;
$ConstantIsoIndices =
{Global`I1,Global`I2,Global`I3,Global`I4,Global`I5,Global`I6};
$ExpansionQuantities = {Global`FAFourVector[__], ParticleMass[Pion, a___],
      fccoupl[QED[1], c___]};
UQuarkMassMatrix[st___RenormalizationState, sc___RenormalizationScheme,
      qs___ExpansionState, (opts___Rule | opts___List)] :=
    UMatrix[UQuarkMass[st, sc, qs, ##] & @@
            OptionsSelect[UQuarkMass, opts], ##] & @@
      OptionsSelect[UMatrix, opts];
UQuarkChargeMatrix[st___RenormalizationState, sc___RenormalizationScheme,
      qs___ExpansionState, (opts___Rule | opts___List)] :=
    UMatrix[UQuarkCharge[st, sc, qs, ##] & @@
            OptionsSelect[UQuarkCharge, opts], ##] & @@
      OptionsSelect[UMatrix, opts];
UChiralSpurionMatrix[x_,st___RenormalizationState, sc___RenormalizationScheme,
      qs___ExpansionState, (opts___Rule | opts___List)] :=
    (UMatrix[UChiralSpurion[st, sc, qs, ##] & @@
            OptionsSelect[UChiralSpurion, opts], ##] & @@
      OptionsSelect[UMatrix, opts])[x];
UChiralSpurionRightMatrix[x_,st___RenormalizationState, sc___RenormalizationScheme,
      qs___ExpansionState, (opts___Rule | opts___List)] :=
    (UMatrix[UChiralSpurionRight[st, sc, qs, ##] & @@
            OptionsSelect[UChiralSpurionRight, opts], ##] & @@
      OptionsSelect[UMatrix, opts])[x];
UChiralSpurionLeftMatrix[x_,st___RenormalizationState, sc___RenormalizationScheme,
      qs___ExpansionState, (opts___Rule | opts___List)] :=
    (UMatrix[UChiralSpurionLeft[st, sc, qs, ##] & @@
            OptionsSelect[UChiralSpurionLeft, opts], ##] & @@
      OptionsSelect[UMatrix, opts])[x];
UChiMatrix[x_, st___RenormalizationState, sc___RenormalizationScheme,
      qs___ExpansionState, opts___] :=(*Change 19/12/1999,
      from UChi to UChi[options]*)
      UMatrix[UChi[st, sc, qs, opts],
        Sequence @@ (OptionsSelect[UMatrix, opts])][x];
(*UGeneratorMatrix[i_, opts___Rule | opts___List] :=
      UMatrix[UGenerator[i], opts];*)
(*Changed 2/1 - 2000*)
  UGeneratorMatrix[i_, opts___] :=
    UMatrix[UGenerator[i, Sequence @@ OptionsSelect[UGenerator, opts]],
      Sequence @@ OptionsSelect[UMatrix, opts]];
(*UGeneratorMatrix[opts___Rule | opts___List] := UMatrix[UGenerator, opts];*)
(*Changed 2/1 - 2000*)
  UGeneratorMatrix[opts___Rule | opts___List] :=
    UMatrix[UGenerator[Sequence @@ OptionsSelect[UGenerator, opts]],
      Sequence @@ OptionsSelect[UMatrix, opts]];
UIdentityMatrix[opts___] := UMatrix[UIdentity, opts];
PhiMesonIsoVector[x_, opts___] :=
    IsoVector[fcqf[Particle[PhiMeson, RenormalizationState[0]]], opts][x];
PionIsoVector[x_, opts___] :=
    IsoVector[fcqf[Particle[Pion, RenormalizationState[0]]], opts][x];
UGeneratorMatrixIsoVector[opts___] :=
    IsoVector[
      UGeneratorMatrix[
        Sequence @@
          Union[OptionsSelect[UMatrix, opts],
            OptionsSelect[UGenerator, opts]]],
      Sequence @@ OptionsSelect[IsoVector, opts]];
ProjectionIsoVector[i_, opts___] := IsoVector[Projection[i], opts];
(*Added 15/12/1999*)$UMatrices=
MM|SMM|UChiMatrix|USmall|UFPlus|UFMinus|UChiPlus|UChiMinus|UGamma;
$ParticleTypes = {Scalar, PseudoScalar, Vector, AxialVector, LeftComponent,
      RightComponent, Fermion};


(* When a particle-field like e.g. Scalar[3] is fed to FeynArts 2, it is given
like e.g. Scalar[3][1], that is the first kind of Scalar[3] fields.
Automatically Scalar[3][1] is set to Scalar3[1].  This is to make the pattern
matching of FeynArts 2 work. *)

(*SetFAField[a_] :=
      a[aa_][bb__] := (Begin["HighEnergyPhysics`Phi`Objects`"];
          Global`ParticleName = (ToExpression[ToString[a] <> ToString[aa]])[
              bb]; End[]; Global`ParticleName);*)
(*Change 18/3 - 1999*)
  SetFAField[a_] :=
    a[aa_][bb__] := (Begin["HighEnergyPhysics`Phi`Objects`"];
        HighEnergyPhysics`Phi`Objects`Private`ParticleName = (ToExpression[
                ToString[a] <> ToString[aa]])[bb]; End[];
        HighEnergyPhysics`Phi`Objects`Private`ParticleName);
SetFAField /@ $ParticleTypes;
$Particles = {PhiMeson, Pion, PionPlus, PionMinus, PionZero, Kaon, KaonPlus,
      KaonZero, KaonZeroBar, KaonMinus, EtaMeson, DownQuark, UpQuark,
      StrangeQuark, CharmQuark, BottomQuark, TopQuark, BBaryon, Nucleon,
      Proton, Neutron, LambdaBaryon, SigmaPlusBaryon, SigmaZeroBaryon,
      XiZeroBaryon, SigmaMinusBaryon, XiMinusBaryon, Photon};
(*Dynamic definition*)(*$UAllParticleHeads :=
      Alternatives @@ Union[Head /@ $Particles];*)
(*Static definition*)$UAllParticleHeads :=
    Scalar | PseudoScalar | Vector | PseudoVector;
PhiMeson := PseudoScalar[1]; Pion := PseudoScalar[2]; PionPlus :=
  PseudoScalar[3]; PionMinus := PseudoScalar[5]; PionZero =
  PseudoScalar[4]; Kaon := PseudoScalar[6]; KaonPlus :=
  PseudoScalar[7]; KaonZero := PseudoScalar[8]; KaonZeroBar :=
  PseudoScalar[9]; KaonMinus := PseudoScalar[10]; EtaMeson :=
  PseudoScalar[11]; UPerturbation := PseudoScalar[12];
Photon := Vector[1];
HiggsBoson := Scalar[1];
Lepton := Fermion[1]; Neutrino := Fermion[2]; ElectronNeutrino :=
  Fermion[3]; MuonNeutrino := Fermion[4]; TauonNeutrino :=
  Fermion[5]; MassiveLepton := Fermion[6]; Electron := Fermion[7]; Muon :=
  Fermion[8]; Tauon := Fermion[9]; Quark := Fermion[10]; LightQuark2 :=
  Fermion[11]; LightQuark3 := Fermion[12]; DownQuark := Fermion[13]; UpQuark :=
   Fermion[14]; StrangeQuark := Fermion[15]; CharmQuark =
  Fermion[16]; BottomQuark = Fermion[17]; TopQuark = Fermion[18]; BBaryon :=
  Fermion[19]; Nucleon := Fermion[20]; Proton := Fermion[21]; Neutron :=
  Fermion[33]; LambdaBaryon := Fermion[23]; SigmaPlusBaryon :=
  Fermion[24]; SigmaZeroBaryon := Fermion[25]; SigmaMinusBaryon :=
  Fermion[26]; XiZeroBaryon := Fermion[27]; XiMinusBaryon := Fermion[28];



(* Interpretation of particlei[0] as particle[i]: *)

$ParticlesInUse = {PhiMeson, Pion, Kaon, Vector[0], AxialVector[0],
      Scalar[0], PseudoScalar[0], Photon, Electron, ElectronNeutrino, BBaryon,
       Nucleon};
$FAParticlesInUse := Head /@ (#[1] & /@ Evaluate[$ParticlesInUse]);
$ParticleHeads := $FAParticlesInUse /. {{} :> None, {a_} -> a} /.
      List :> Alternatives;
$FermionHeads :=
    Head /@ (#[1] & /@
              Select[$ParticlesInUse, (Head[#] == Fermion) &]) /. {{} :>
            None, {a_} -> a} /. List :> Alternatives;
$VectorHeads :=
    Head /@ (#[1] & /@
              Select[$ParticlesInUse, (Head[#] == Vector ||
                      Head[#] == AxialVector) &]) /. {{} :> None, {a_} ->
            a} /. List :> Alternatives;
$ScalarHeads :=
    Complement[
        Flatten[{$ParticleHeads /. {{Alternatives -> List, None -> {}}}}],
        Flatten[{$VectorHeads /. {Alternatives -> List, None -> {}}}],
        Flatten[{$FermionHeads /. {Alternatives -> List, None -> {}}}]] /.
      List -> Alternatives;
HighEnergyPhysics`FeynArts`P$Generic := (HighEnergyPhysics`FeynArts`F | HighEnergyPhysics`FeynArts`S | HighEnergyPhysics`FeynArts`V | HighEnergyPhysics`FeynArts`U |
        HighEnergyPhysics`FeynArts`VS | HighEnergyPhysics`FeynArts`SV | $ParticleHeads);
dropnumberr[phia_] := If[NumberQ[phia], numr, phia];
takenumberr[phia_] := If[! NumberQ[phia], numr, phia];
dropstringnumbers[phia_] :=
    ToString /@ (dropnumberr /@
              Flatten[Table[
                  ToExpression[StringTake[phia, {phii}]], {phii,
                    StringLength[phia]}]] /. numr -> Sequence[]) /.
      List -> StringJoin;
takestringnumbers[phia_] :=
    ToString /@ (takenumberr /@
              Flatten[Table[
                  ToExpression[StringTake[phia, {phii}]], {phii,
                    StringLength[phia]}]] /. numr -> Sequence[]) /.
      List -> StringJoin;



(* ParticleMass, DecayConstant and Particle recognize e.g. PseudoScalar2[0] as
PseudoScalar[2]: *)

(*And PseudoScalar2[1] as PseudoScalar[2,1]. Added 6/6-2002*)

ParticleMass[(parti0 : $ParticleHeads)[0], rrrest___] :=
    ParticleMass[
      ToExpression[dropstringnumbers[ToString[parti0]]][
        ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];
DecayConstant[(parti0 : $ParticleHeads)[0], rrrest___] :=
    DecayConstant[
      ToExpression[dropstringnumbers[ToString[parti0]]][
        ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];
Particle[(parti0 : $ParticleHeads)[0], rrrest___] :=
    Particle[ToExpression[dropstringnumbers[ToString[parti0]]][
        ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];
ParticleMass[(parti0 : $ParticleHeads)[i_], rrrest___] :=
    ParticleMass[
      ToExpression[dropstringnumbers[ToString[parti0]]][
        ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];
DecayConstant[(parti0 : $ParticleHeads)[i_], rrrest___] :=
    DecayConstant[
      ToExpression[dropstringnumbers[ToString[parti0]]][
        ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];
Particle[(parti0 : $ParticleHeads)[i_], rrrest___] :=
    Particle[ToExpression[dropstringnumbers[ToString[parti0]]][
        ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];


FAUpdate := (ParticleMass[(parti0 : $ParticleHeads)[0], rrrest___] :=
        ParticleMass[
          ToExpression[dropstringnumbers[ToString[parti0]]][
            ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];
DecayConstant[(parti0 : $ParticleHeads)[0], rrrest___] :=
        DecayConstant[
          ToExpression[dropstringnumbers[ToString[parti0]]][
            ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];
Particle[(parti0 : $ParticleHeads)[0], rrrest___] :=
        Particle[
          ToExpression[dropstringnumbers[ToString[parti0]]][
            ToExpression[takestringnumbers[ToString[parti0]]]], rrrest];
ParticleMass[(parti0 : $ParticleHeads)[i_], rrrest___] :=
    ParticleMass[
      ToExpression[dropstringnumbers[ToString[parti0]]][
        ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];
DecayConstant[(parti0 : $ParticleHeads)[i_], rrrest___] :=
    DecayConstant[
      ToExpression[dropstringnumbers[ToString[parti0]]][
        ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest];
Particle[(parti0 : $ParticleHeads)[i_], rrrest___] :=
    Particle[ToExpression[dropstringnumbers[ToString[parti0]]][
        ToExpression[takestringnumbers[ToString[parti0]]],i], rrrest]);

(*SetAttributes[QED, {NumericFunction, NHoldAll}];*)
If[ValueQ[Global`$Lagrangians] =!= True, Global`$Lagrangians = {}];
fclag[l_[ll___]] := Message[Lagrangian::noload, {l[ll]}[[1]]];
$RenormalizationSuperscripts = {"", "r"};
$RSSuperscripts = {"", ""};
$ExpansionSuperscripts = {"", ""};
$UScalars = {QuarkCondensate, ParticleMass, DecayConstant, fccoupl,
      SU3D, SU3F, SU3Delta, Projection, fcsuni, fcsunf, fcsund, fcsundel, fcpa,
			(*Added 24/2-2002*)fcsunn};
$UMultiplications = {Times, NM, IsoDot, IsoCross, IsoSymmetricCross};
$UDistributiveFunctions = {Conjugate,
                           (*added 17/6-2001*) fccc,
                           Transpose, Adjoint, UTrace, UTrace1, Iso};
$QuarkToPionMassesRules = {ParticleMass[UpQuark,
          rest___] -> (ParticleMass[Pion, rest])^2/(2*QuarkCondensate[rest]),
      ParticleMass[DownQuark,
          rest___] -> (ParticleMass[Pion, rest])^2/(2*
              QuarkCondensate[rest])};
$PionToQuarkMassesRule =
    ParticleMass[Pion, rest___] ->
      Sqrt[QuarkCondensate[
            rest]*(ParticleMass[UpQuark, rest] +
              ParticleMass[DownQuark, rest])];
$QuarkToMesonMassesRules = {ParticleMass[UpQuark,
          rest___] -> -(ParticleMass[KaonZero, rest]^2 -
                ParticleMass[KaonPlus, rest]^2 -
                ParticleMass[Pion, rest]^2)/(2*QuarkCondensate[rest]),
      ParticleMass[DownQuark,
          rest___] -> -(-ParticleMass[KaonZero, rest]^2 +
                ParticleMass[KaonPlus, rest]^2 -
                ParticleMass[Pion, rest]^2)/(2*QuarkCondensate[rest]),
      ParticleMass[StrangeQuark,
          rest___] -> -(-ParticleMass[KaonZero, rest]^2 -
                ParticleMass[KaonPlus, rest]^2 +
                ParticleMass[Pion, rest]^2)/(2*QuarkCondensate[rest])};
$MesonToQuarkMassesRules = {ParticleMass[Pion, rest___]^2 ->
        QuarkCondensate[
            rest]*(ParticleMass[UpQuark, rest] +
              ParticleMass[DownQuark, rest]),
      ParticleMass[KaonPlus, rest___]^2 ->
        QuarkCondensate[
            rest]*(ParticleMass[UpQuark, rest] +
              ParticleMass[StrangeQuark, rest]),
      ParticleMass[KaonZero, rest___]^2 ->
        QuarkCondensate[
            rest]*(ParticleMass[DownQuark, rest] +
              ParticleMass[StrangeQuark, rest]),
      ParticleMass[EtaMeson, rest___]^2 ->
        QuarkCondensate[rest]/
            3*(4*ParticleMass[StrangeQuark, rest] +
              ParticleMass[UpQuark, rest] + ParticleMass[DownQuark, rest])};
$UExpansionCoefficients = Table[1/(i!), {i, 0, 10}];



(* SU(2) matrices: *)



(* For general j, T(1) (XMatricesSpherical) acting on x=(x_-j,...,x_j) and
spanning SU(2) are defined (H. F. Jones, Groups, Representations and Physics,
(6.33), but transformed by SUNDelta[m,m']*(-1)^m, that is, Condon-Shortley
convention transformed by SUNDelta[m,m']*(-1)^m). Transforming with
{{-1/Sqrt[2],0,1/Sqrt[2]},{I/Sqrt[2],0,i/Sqrt[2]},{0,1,0}}, one gets the
matrices defined below. *)

$SUNBasis[2, 1/2] = {{{0, -1}, {-1, 0}}, {{0, -I}, {I, 0}}, {{-1, 0}, {0, 1}}};



(* SU(3) matrices: *)



(* The 3-dimensional matrices acting on x and spanning SU(3) are defined
   (J. F. Donoghue, E. Golowich and B. R. Holstein, Dynamics of the Standard Model,
   (2.4) multiplied by 1/2)
   (the first 3 refers to the group SU(3), the second to the dimension: *)

$SUNBasis[3, 1] = {{{0, 1, 0}, {1, 0, 0}, {0, 0, 0}}, {{0, -I, 0}, {I, 0, 0}, {0, 0,
          0}}, {{1, 0, 0}, {0, -1, 0}, {0, 0, 0}}, {{0, 0, 1}, {0, 0, 0}, {1,
          0, 0}}, {{0, 0, -I}, {0, 0, 0}, {I, 0, 0}}, {{0, 0, 0}, {0, 0,
          1}, {0, 1, 0}}, {{0, 0, 0}, {0, 0, -I}, {0, I, 0}}, {{1/Sqrt[3],
           0, 0}, {0, 1/Sqrt[3], 0}, {0, 0, -2/Sqrt[3]}}};
SelfConjugation[(a : $VectorHeads)[i_]] := True;
SelfConjugation[(a : $FermionHeads)[i_]] := False;
SelfConjugation[(a : $ScalarHeads)[i_]] := True;
(*OptionsSelect[function_, opts___] :=
    Select[(Flatten[{opts}] //. ({a___, b_ -> c_, d___, b_ -> e_, f___} -> {a,
                 b -> c, d, f})), (!
            FreeQ[#, (Options[function] /. (a_ -> b_) -> a -> _ /.
                  List -> Alternatives)]) &]*)
(*Changed 16/2 - 2000*)

OptionsSelect[function_, opts___] :=
  Select[(Cases[{opts}, _Rule,
          Infinity] //. ({a___, b_ -> c_, d___, b_ -> e_, f___} -> {a, b -> c,
               d, f})), (!
          FreeQ[#, (Options[function] /. (a_ -> b_) -> a -> _ /.
                List -> Alternatives)]) &]
Options[UGenerator] = { fcsunn -> 2, UDimension -> Automatic};
Options[UIdentity] = { fcsunn -> 2, UDimension -> Automatic};
Options[ExpandU] = { fcsunn -> 2, UDimension -> Automatic,
      CommutatorReduce -> True, RemoveIntegerIndices -> False};
Options[ExpandUGenerators] = { fcsunn -> 2, UDimension -> Automatic,
      IsoIndicesString -> "i", CommutatorReduce -> False};
Options[UNMSplit] = { DropOrder -> 4};
Options[UMatrix] = { fcsunn -> 2, UDimension -> Automatic};
Options[UVector] = { fcsunn -> 2, UDimension -> Automatic};
Options[IsoVector] = { fcsunn -> 2};
Options[UTrace] = {fcsunn -> 2, UDimension -> Automatic,
      TraceSimplify -> True, HoldUTrace -> False};
Options[UTraceToFCTrace] = {fcsunn -> 2, UDimension -> Automatic};
Options[DiscardTerms] = {Method -> Coefficient,
      Retain -> {Particle[Pion , RenormalizationState[0]] -> 4},
      CommutatorReduce -> False, NoDrop -> {}};
Options[IsoIndicesSupply] = {IsoIndicesString -> "i",
      FreeIsoIndexString -> "k", FreeIsoIndicesString -> "I",
      NumerateFree -> False};
Options[UIndicesSupply] = {UIndicesString -> "n", UIndexToSUNIndex -> False};
Options[IsoIndicesList] = {IsoIndicesNumber -> $IsoIndicesCounter,
      IsoIndicesString -> "i"};
Options[MomentumVariables] = {ParticlesNumber -> 4,
      MomentumVariablesString -> "p"};
Options[FieldsSet] = {ParticlesNumber -> 4, MomentumVariablesString -> "p",
      IsoIndicesString -> "I", LorentzIndicesString -> None};
Options[MomentaSumRule] = {ParticlesNumber -> 4, MomentaSumLeft -> All,
      MomentumVariablesString -> "p"};
Options[UQuarkMass] = {fcexpt -> True, QuarkToMesonMasses -> True, DiagonalToU -> False,
      RemoveIntegerIndices -> False, fcsunn -> 2,
      UDimension -> Automatic};
Options[UQuarkCharge] = {fcexpt -> True, DiagonalToU -> False, RemoveIntegerIndices -> False,
      fcsunn -> 2, UDimension -> Automatic};
(*Added 10/1 - 2000*)
Options[UChi] = {fcexpt -> True, DiagonalToU -> False, fcsunn -> 2,
      QuarkToMesonMasses -> True, RemoveIntegerIndices -> False,
      UDimension -> Automatic};
(*Changed the strings below from "k", "k" to "l", "l" in order to avoid problems with IndicesCleanup.
  27/2-2002*)
Options[PhiToFC] = {RemoveIntegerIndices -> True, FreeIsoIndexString -> "l",
      FreeIsoIndicesString -> "l", NumerateFree -> True};
Options[MM] = {fcexpt -> True};
Options[SMM] = {fcexpt -> True};
Options[MMS] = {fcexpt -> True};
Options[FieldStrengthTensor] = {fcexpt -> True};
Options[FieldStrengthTensorFull] = {fcexpt -> True};
Options[UFieldMatrix] = { ExpansionOrder -> 4, DropOrder -> Infinity, Constant -> Automatic};
Options[UFieldMatrixSeries] = { ExpansionOrder -> 4, Constant -> Automatic};
Options[WriteOutUMatrices] = {fcsunn -> 2, UDimension -> Automatic,
      QuarkToMesonMasses -> True, DiagonalToU -> False,
      RemoveIntegerIndices -> False};
Options[WriteOutIsoVectors] = {fcsunn -> 2};
Options[VariableBoxes] = {ParticlesNumber -> 4};
patterns = (BlankSequence | BlankNullSequence | Pattern);
allpatterns = (Blank | BlankSequence | BlankNullSequence | Pattern);
bti[c__] := (! FreeQ[{c}, UIdentity]);
Options[CommutatorReduce] = {FullReduce -> True};
Options[LeftComponent] = {fcexpt -> True};
Options[RightComponent] = {fcexpt -> True};
Options[CovariantFieldDerivative]=
  {fcexpt->True, DiagonalToU->True,
   RemoveIntegerIndices->True,
   fcsunn->2, UDimension->Automatic};
Options[CovariantNabla] = {fcexpt -> True};


(* Check if c contains a matrix. Added Infinity 26/2-2000. *)

(*btu[c__] := (!
          FreeQ[{c}, UMatrix[m__ /; FreeQ[{m}, UIdentity]], Infinity]);*)
(*btui[c__] := (! FreeQ[{c}, UMatrix, Infinity]);*)
(*nbtui[c__] := FreeQ[{c}, UMatrix, Infinity];*)



(* Added $UMatrices 13/3-2000. *)
(* Added the trace stuff 16/6-2001. *)
(* Added extra trace stuff $UMatrices->um, 24/2-2002 *)

btu[c__] := (!FreeQ[{c} /. (UTrace1|tr)[ccc_] :> (UTrace1[ccc/.UMatrix->um/.$UMatrices->um]),
                UMatrix[m__ /; FreeQ[{m}, UIdentity]] | $UMatrices,
                Infinity]);
btui[c__] := (!FreeQ[{c} /. (UTrace1|tr)[ccc_] :> UTrace1[ccc/.UMatrix->um/.$UMatrices->um],
                UMatrix | $UMatrices, Infinity]);
nbtui[c__] := FreeQ[{c} /. (UTrace1|tr)[ccc_] :> UTrace1[ccc/.UMatrix->um/.$UMatrices->um],
               UMatrix | $UMatrices, Infinity];


(* Check if a is a scalar: *)

UScalarQ[a_] := (MemberQ[$UScalars, a] || NumericQ[a] ||
          MemberQ[$UScalars, Head[a]] ||
          MatchQ[a, (Alternatives @@ $UScalars)] ||
          MatchQ[a, (Alternatives @@ $UScalars)^_] ||
          MatchQ[a, (Alternatives @@ $UScalars)[___]^_] ||
          MemberQ[$UScalars, Head[Head[a]]](*Commented out 18/3 - 2000,
          think it's redundant*)(*||
            MatchQ[a,
              fcqf[___, Particle[(Alternatives @@ $UScalars), ___], ___][_]] ||
             MatchQ[a,
              fcqf[___, Particle[(Alternatives @@ $UScalars), ___], ___]]*));
							(*Commented out 21/2-2002, think it's redundant*)(* &&FreeQ[a, allpatterns]*)


btsbin[a_] := If[UScalarQ[a]&&FreeQ[a, allpatterns], 1, 0];
btsbin1[a_] := If[UScalarQ[a], 1, 0];

(* btss[a__] is True if {a} contains at least one scalar: *)

btss[a__] := Plus @@ btsbin /@ {a} > 0;
btss1[a__] := Plus @@ btsbin1 /@ {a} > 0;
nbts[a_] := ((! (NumericQ[a] || MemberQ[$UScalars, Head[a]] ||
                MatchQ[a, (Alternatives @@ $UScalars)^_] ||
                MatchQ[a, (Alternatives @@ $UScalars)[___]^_] ||
                MemberQ[$UScalars, Head[Head[a]]] || MemberQ[$UScalars, a] ||
                MatchQ[a,
                  fcqf[___,
                      Particle[(Alternatives @@ $UScalars), ___], ___][_]] ||
                MatchQ[a,
                  fcqf[___,
                    Particle[(Alternatives @@ $UScalars), ___], ___]])) &&
        FreeQ[a, allpatterns]);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Non-commutative multiplication *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* As an experiment - to gain speed, some of the definitions below have been
comented out. 5/3-2000. *)



(* Zero and one-elements, etc.: *)

(*NM[1, a_] := a;
NM[a_, 1] := a;
NM[0, a_] := 0;
NM[a_, 0] := 0;*)
NM /: NM[] := Sequence[];
(*This pattern business should not be necessary. Commented out 5/3 - 2000*)
  NM[a_](*/; FreeQ[{a}, patterns]*):= a;
(*Added 15/6-2001*) (*And removed again 18/1-2002 - yakk - stupid*)
(*NM[f___, Adjoint[a_], a_, l___] := NM[f, a, Adjoint[a], l];*)
(*Hmm, not very general. Commented out 15/8-2001*)
(*NM[f___, UMatrix[a__], Adjoint[UMatrix[a__]], l___] := NM[f, UIdentityMatrix[
Sequence@@OptionsSelect[UMatrix,
            List@@Union@@Cases[{a}, _UMatrix, Infinity, Heads->True]]], l];
NM[f___, UMatrix[a__][x_], Adjoint[UMatrix[a__][x_]], l___] := NM[f, UIdentityMatrix[
Sequence@@OptionsSelect[UMatrix,
            List@@Union@@Cases[{a}, _UMatrix, Infinity, Heads->True]]], l];*)
(**)
(*Added 6/11-2001*)
NM[f___,a_,Inverse[a_],l___]:=
NM[f,UIdentityMatrix[If[FreeQ[a,fcsunn],Sequence[],fcsunn->gaugedimcheck[UMatrix,a]]],l];
NM[f___,Inverse[a_],a_,l___]:=
NM[f,UIdentityMatrix[If[FreeQ[a,fcsunn],Sequence[],fcsunn->gaugedimcheck[UMatrix,a]]],l];

UCommutator[a_,b_]:=NM[a,b]-NM[b,a];
UAntiCommutator[a_,b_]:=NM[a,b]+NM[b,a];


(* Non-commutative product of explicit matrices: *)

NM[m_ /; MatrixQ[m], n_ /; MatrixQ[n]] /; Length[m] == Length[n] :=
    Table[Sum[NM[m[[i, k]], n[[k, j]]], {k, 1, Length[m]}], {i, 1,
        Length[m]}, {j, 1, Length[m]}];
(*NM[m__ /; Length[{m}] > 1, n_ /; MatrixQ[n]] := NM[NM[m], n];*)
(*Changed 28/5-2002 to avoid infinite recursion*)
NM[m__, n_] := 
    NM[NM[m], n] /; (Length[{m}] > 1 && And @@ (MatrixQ /@ {m, n}));
(* Getting scalars out *)
(*NM[m_ /; (! MatrixQ[m] && nbtui[m]), n_ /; MatrixQ[n]] :=
      Map[NM[m, #] &, n, {2}];*)
(*Added 10/1 -
    2000*)(*Gives infinite recursion if NM is not Cleared on reload. 15/2 -
    2000*)(*NM[n_ /; MatrixQ[n], m_ /; (! MatrixQ[m] && nbtui[m])] :=
      Map[NM[#, m] &, n, {2}];*)
(*Changed 13/3 - 2000*)
  NM[m___, n_,
        mm___] /; (MatrixQ[n] =!= True && nbtui[n] &&
          MemberQ[MatrixQ /@ {m, mm}, True]) :=
    NM[m, n*IdentityMatrix[Length[Cases[{m, mm}, _?MatrixQ][[1]]]], mm];



(* Getting factors out *)

NM[a___, b_Times,
      c___](*This pattern business should not be necessary. Commented out 5/
            3 - 2000*)(*/; FreeQ[b, allpatterns]*):= NM[a, Sequence @@ b, c];



(* Non-commutative product of non-explicit matrices: *)



(* Identity matrices are brought left: *)

NM[aa_, a__] /; MemberQ[{a}, UMatrix[UIdentity, ___]] :=
    NM[UIdentityMatrix[
        Sequence @@
          OptionsSelect[UMatrix,
            List @@ Union @@ Cases[{a}, _UMatrix, Infinity, Heads -> True]]],
      Sequence @@
        Select[{aa, a}, ! MemberQ[{#}, UMatrix[UIdentity, ___]] &]];



(* The identity matrix is redundant when other matrices are in a product: *)

NM[a___] /; (MemberQ[{a}, UMatrix[UIdentity, ___]] && btu[a]) :=
    NM[Sequence @@ Select[{a}, ! MemberQ[{#}, UMatrix[UIdentity, ___]] &]];



(* Non-commutative product of two series.  Products are cut off at the lowest
order of the factors.  To have the ordinary handling of maximum power order,
simply change the last limitorder to startingpoint+maxorder: *)

(*NM[mm_SeriesData,
          nn_SeriesData] /; (mm[[1]] == nn[[1]] && mm[[2]] == nn[[2]] &&
            mm[[6]] == nn[[6]]) := (listmm0 =
          Take[mm[[3]], mm[[5]] - mm[[4]]];
        listnn0 = Take[nn[[3]], nn[[5]] - nn[[4]]];
        Which[mm[[5]] > nn[[5]], (listmm = listmm0;
            listnn = Join[listnn0, Table[0, {mm[[5]] - nn[[5]]}]]),
          mm[[5]] < nn[[5]], (listnn = listnn0;
            listmm = Join[listmm0, Table[0, {nn[[5]] - mm[[5]]}]]),
          mm[[5]] == nn[[5]], (listmm = listmm0; listnn = listnn0)];
        Which[mm[[4]] < nn[[4]], (listmm1 = listmm;
            listnn1 = Join[Table[0, {nn[[4]] - mm[[4]]}], listnn]),
          mm[[4]] > nn[[4]], (listnn1 = listnn;
            listmm1 = Join[Table[0, {mm[[4]] - nn[[4]]}], listmm]),
          mm[[4]] == nn[[4]], (listmm1 = listmm; listnn1 = listnn)];
        limitorder = Min[{mm[[5]], nn[[5]]}];
        startingpoint = 2*Min[{mm[[4]], nn[[4]]}]; maxorder = Length[listmm1];
         SeriesData[mm[[1]], mm[[2]],
          Table[IsoDot[Iso @@ Take[listmm1, dummy],
              Iso @@ Reverse[Take[listnn1, dummy]]], {dummy, 1, maxorder}],
          startingpoint, limitorder, mm[[6]]]);*)
SetAttributes[NM, Flat];



(* Recursive generalization: *)

(*NM[mm_SeriesData, nn_SeriesData, oo__SeriesData] := NM[NM[mm, nn], oo];*)



(* Getting scalars out: *)

(*NM[a__] /;
        btss[a] := (Times @@ Select[{a}, UScalarQ])*(NM[##] & @@
            Select[{a}, nbts]);*)
(*Changed 6/3 - 2000*)
  NM[a__] /;
      btss[a] := (Times @@ Select[{a}, ((UScalarQ[#]&&(*Added the pattern stuff
  because it's commented out above in UScalarQ*)FreeQ[#,allpatterns])&)])*(NM[##] & @@
          Select[{a}, (! UScalarQ[#] &)]);



(* Expand using distributivity and commutation of identical objects: *)

NMSeriesExpand[expr_NM] :=
    NM @@ (Flatten[
                List @@ (expr /. Plus -> pplus)] //. {List[a___,
                    b_ /; ! FreeQ[b, SeriesData], c_ /; FreeQ[c, SeriesData],
                    d___] :> List[a, NMExpand[NM[b, c]], d]} /. {List[a___,
                  b_ /; FreeQ[b, SeriesData], c_ /; ! FreeQ[c, SeriesData],
                  d___] :> List[a, NMExpand[NM[b, c]], d]}) /. pplus -> Plus;
NMSeriesExpand[expr_ /; Head[expr] =!= NM] :=
    expr /. (Rule[#, NMSeriesExpand[#]] & /@ Cases[expr, _NM, Infinity]);
(*Added to speed up DiscardTerms. 17/2 - 2000*)
  NMSeriesExpand[expr_ /; FreeQ[expr, SeriesData, Infinity, Heads -> True]] :=
     expr;



(* NMExpand changed 5/3-2000. *)

NMExpand[expr_] :=
    expr //. NM[a___, b_ + c_, d___] -> Distribute[NM[a, b + c, d]];
(*NMExpand[expr_] :=
      expr //. {NM[a___, b_ + c_, d__] -> NM[a, b, d] + NM[a, c, d],
NM[a__, b_ + c_, d___] ->
                NM[a, b, d] +
                  NM[a, c,
                    d],(*NM[
                      a__] /; (MemberQ[
                        Length /@ Union /@ Partition[{a}, 2, 1], 1]) :>
                  NM @@ ({a} //. {{aa___, b_, b_, c___} -> {aa, b^2,
                              c}, {aa___, b_^n_, b_, c___} -> {aa, b^(n + 1),
                              c}}),*)NM[
                    ll__] /; (Count[{ll}, _SeriesData] > 0 &&
                      Length[Union[
                            Transpose[(List @@ (Cases[
                                        NM[ll], _SeriesData] /. {SeriesData ->
                                         sdd, List -> llist} /.
                                        sdd -> List))][[1]]]] ==
                        1) :> ((UScalarsOut[
                        Normal[
                          NM[ll] /.
                            Cases[{ll}, _SeriesData][[1, 1]] :>
                              SU3F[Cases[{ll}, _SeriesData][[1, 1]]]]]) +
                    ot[Cases[{ll}, _SeriesData][[1,
                            1]]]^(Cases[{ll}, _SeriesData][[1, 5]]))} /.
          SU3F[inh_] -> inh /. ot -> O;*)



(* DotExpand added 3/8-2000. *)
(* Added UScalarQ stuff 18/5-2002 *)

DotExpand[expr_] :=
    expr //. {fcdot[a___, b_ + c_, d___] :> Distribute[fcdot[a, b + c, d]], 
  fcdot[a___, b_*c_, d___] :> b*fcdot[a, c, d] /; UScalarQ[b],
  fcdot[a___, b_, d___] :> b*fcdot[a, d] /; UScalarQ[b]};
    
(* NMFactor added 15/5-2001 *)

NMFactor[ex_] :=
    ex /. {HoldPattern[Plus[r : (((___*NM[___, a_]) | NM[___, a_]) ..)]] :>
            NM[Replace[#, b_NM -> dr[b, -1], {0, 1}] & /@ Plus[r], a] /;
              Length[{r}] > 1,
          HoldPattern[Plus[r : (((___*NM[a_, ___]) | NM[a_, ___]) ..)]] :>
            NM[a, Replace[#, b_NM -> dr[b, 1], {0, 1}] & /@ Plus[r]] /;
              Length[{r}] > 1} /. dr -> Drop;

NMFactor[ex_, a_] :=
    ex /. {HoldPattern[Plus[r : (((___*NM[___, a]) | NM[___, a]) ..)]] :>
            NM[Replace[#, b_NM -> dr[b, -1], {0, 1}] & /@ Plus[r], a] /;
              Length[{r}] > 1,
          HoldPattern[Plus[r : (((___*NM[a, ___]) | NM[a, ___]) ..)]] :>
            NM[a, Replace[#, b_NM -> dr[b, 1], {0, 1}] & /@ Plus[r]] /;
              Length[{r}] > 1} /. dr -> Drop;


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Iso-vector products *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* The dot product: *)



(* Notice: The dot product is defined as a.b=Conjugate[a1]b1+Conjugate[a2]b2+...
. This is different from in versions before 1.2, where it was assumed that
the first argument was real. This is the case for the meson fields when
working in the Gell-Mann basis, but not generally. This implies the
redifinition of some definitions to accomodate complex first argument (which
results when working in a different basis than the Gell-Mann matrices).
1/2-2000. *)



(* Zero and one-elements, etc.: *)

IsoDot[0, a_] := 0;
IsoDot[a_, 0] := 0;
IsoDot[a_] /; FreeQ[a, patterns] := a;
IsoDot[1, UMatrix[UIdentity, opts___]] := UMatrix[UIdentity, opts];
IsoDot[UMatrix[UIdentity, opts___], 1] := UMatrix[UIdentity, opts];



(* Distributivity: *)

$DotDistribute = True;
IsoDot[b__ + c_, d__] /; $DotDistribute := IsoDot[b, d] + IsoDot[c, d];
IsoDot[a__, b__ + c_] /; $DotDistribute := IsoDot[a, b] + IsoDot[a, c];



(* Commutativity of the iso-vector of matrices UGeneratorMatrixIsoVector[opts]
and any other non-matrix iso-vector (generator iso-vectors are brought to the
right): *)

(*IsoDot[IsoVector[UMatrix[UGenerator[a___], c___], cc___],
        b_ /; FreeQ[b, UMatrix, Infinity, Heads -> True]] :=
      IsoDot[Conjugate[b],
        Conjugate[IsoVector[UMatrix[UGenerator[a], c], cc]]];*)
IsoDot[a :
        IsoVector[(UMatrix[UGenerator[___], ___] |
              Conjugate[UMatrix[UGenerator[___], ___]]), ___],
      b_ /; FreeQ[b, UMatrix, Infinity, Heads -> True]] :=
    IsoDot[Conjugate[b], Conjugate[a]];



(* The reduction formula for products of IsoDots with Pauli and
Gell-Mann matrix iso-vectors:
Modified to incorporate the new iso-products, 1/2-2000. *)

sigrules[2] = {NM[a___,
            IsoDot[aa_,
              IsoVector[UMatrix[UGenerator[optsg___Rule], optsm___],
                optsv___]], mm___,
            IsoDot[cc_, IsoVector[UMatrix[UGenerator[___], ___], ___]],
            b___] /;
          FreeQ[{a, mm}, UMatrix(*Bug fixed 26/2 - 2000*)(*UGenerator*),
            Infinity, Heads -> True] :>
        2/2*NM[a, IsoDot[aa, NM[mm, Conjugate[cc]]], UIdentityMatrix[optsm],
              b] + I*NM[a,
              IsoDot[IsoCross[Conjugate[aa], NM[mm, cc]],
                IsoVector[UMatrix[UGenerator[optsg], optsm], optsv]], b]};
sigrules[3] = {NM[a___,
            IsoDot[aa_,
              IsoVector[UMatrix[UGenerator[optsg___Rule], optsm___],
                optsv___]], mm___,
            IsoDot[cc_, IsoVector[UMatrix[UGenerator[___], ___], ___]],
            b___] /;
          FreeQ[{a, mm}, UMatrix(*Bug fixed 26/2 - 2000*)(*UGenerator*),
            Infinity, Heads -> True] :>
        2/3*NM[a, IsoDot[aa, NM[mm, Conjugate[cc]]], UIdentityMatrix[optsm],
              b] + I*NM[a,
              IsoDot[IsoCross[Conjugate[aa], NM[mm, cc]],
                IsoVector[UMatrix[UGenerator[optsg], optsm], optsv]], b] +
          NM[a, IsoDot[IsoSymmetricCross[Conjugate[aa], NM[mm, cc]],
              IsoVector[UMatrix[UGenerator[optsg], optsm], optsv]], b]};



(* Check which dimension is to be used for the representation (e.g. SU(2) can be
represented by 2x2 matrices, 3x3 matrices, etc.). *)

gaugedimcheck[f_, expr___] :=
    UDimension /.
        Flatten[{Cases[{expr}, _[UDimension, _], Infinity,
                Heads -> True]} /. (UDimension ->
                  Automatic) :> (UDimension -> (fcsunn /.
                      Join[Flatten[
                          Cases[{expr}, _[fcsunn, _], Infinity,
                            Heads -> True]], Options[f]]))] /.
      Flatten[Options[
            f] /. (UDimension ->
                Automatic) -> (UDimension -> (fcsunn /.
                    Join[Flatten[
                        Cases[{expr}, _[fcsunn, _], Infinity,
                          Heads -> True]], Options[f]]))];



(* sigrules2 and sigrules3 could probably be generalized to other than SU(2) and
SU(3) and the standard representations.  This will have to wait till some
other time... *)

removeints =
    UMatrix[UGenerator[fcsuni[i_Integer], opts___],
        optst___] :> (IsoDot[
          ProjectionIsoVector[i,
            Sequence @@ OptionsSelect[IsoVector, opts, optst]],
          UGeneratorMatrixIsoVector[
            Sequence @@
              Union[OptionsSelect[UMatrix, opts, optst],
                OptionsSelect[IsoVector, opts, optst],
                OptionsSelect[UGenerator, opts, optst]]]]);
putints =
    IsoDot[IsoVector[Projection[i_Integer], opts___],
        IsoVector[UMatrix[UGenerator[ops___], ___], optst___]] :> (UMatrix[
          UGenerator[fcsuni[i],
            Sequence @@ OptionsSelect[UGenerator, ops, opts, optst]],
          Sequence @@ OptionsSelect[UMatrix, ops, opts, optst]]);
ExpandU[a_,
      opts___] := (gg = (fcsunn /. Flatten[{opts}] /. Options[ExpandU]);
      gd = gaugedimcheck[ExpandU, opts, a];
      If[gg == 2 && gd == 2 || gg == 3 && gd == 3,
        VerbosePrint[2, "The gauge group is SU(", gg,
          "); the dimension of the representation is ", gd];
        If[(CommutatorReduce /. Flatten[{opts}] /. Options[ExpandU]),
            VerbosePrint[2, "Expanding the NM products"];
            NMExpand[
                  a /. Power -> NMPower /. removeints] //. (VerbosePrint[2,
                    "Applying expansion rules"];
                  sigrules[gg]) // (VerbosePrint[2,
                  "Applying CommutatorReduce"]; CommutatorReduce[#,opts])&,
            VerbosePrint[2, "Expanding the NM products"];
            NMExpand[
                a /. Power -> NMPower /. removeints] //. (VerbosePrint[2,
                  "Applying expansion rules"]; sigrules[gg])] /.(*
            Added 16/12/1999 *)
            If[(RemoveIntegerIndices /. Flatten[{opts}] /. Options[ExpandU]),
            VerbosePrint[2, "Removing integer indices"]; removeints,
            VerbosePrint[2, "Putting (back) on integer indices"]; putints],
        Message[ExpandU::baddim, gg, gd]]);



(* Added a new function to expand NM products of generator matrices. Did not
include this functionality in ExpandU, since it's rarely needed. *)

iixint[opts___] := (++$IsoIndicesCounter;
      ToExpression[(IsoIndicesString /. Flatten[{opts}] /.
              Options[ExpandUGenerators]) <> ToString[$IsoIndicesCounter]]);
surules[2, opts___] :=
    NM[a___, UMatrix[UGenerator[fcsuni[i_], optsg___Rule], optsm___], mm___,
          UMatrix[UGenerator[fcsuni[j_], ___], ___], b___] /;
        FreeQ[{a, mm}, UMatrix, Infinity, Heads -> True] :> (fi =
          fcsuni[iixint[opts]];
        2/2*SU2Delta[fcsuni[i], fcsuni[j]]*
            NM[a, mm, UIdentityMatrix[optsm], b] +
          I*SU2F[fcsuni[i], fcsuni[j], fi]*
            NM[a, mm, UMatrix[UGenerator[fi, optsg], optsm], b]);
surules[3, opts___] :=
    NM[a___, UMatrix[UGenerator[fcsuni[i_], optsg___Rule], optsm___], mm___,
          UMatrix[UGenerator[fcsuni[j_], ___], ___], b___] /;
        FreeQ[{a, mm}, UMatrix, Infinity, Heads -> True] :> (fi =
          fcsuni[iixint[opts]];
        2/3*SU3Delta[fcsuni[i], fcsuni[j]]*
            NM[a, mm, UIdentityMatrix[optsm], b] +
          I*SU3F[fcsuni[i], fcsuni[j], fcsuni[fi]]*
            NM[a, mm, UMatrix[UGenerator[fcsuni[fi], optsg], optsm], b] +
          SU3D[fcsuni[i], fcsuni[j], fi]*
            NM[a, mm, UMatrix[UGenerator[fi, optsg], optsm], b]);

ExpandUGenerators[a_,
      opts___] := (gg = (fcsunn /. Flatten[{opts}] /. Options[ExpandU]);
      gd = gaugedimcheck[ExpandU, opts, a];
      If[gg == 2 && gd == 2 || gg == 3 && gd == 3,
        VerbosePrint[2, "The gauge group is SU(", gg,
          "); the dimension of the representation is ", gd];
        If[(CommutatorReduce /. Flatten[{opts}] /.
              Options[ExpandUGenerators]),
          VerbosePrint[2, "Expanding the NM products"];
          NMExpand[
                a /. Power -> NMPower] //. (VerbosePrint[2,
                  "Applying expansion rules"];
                surules[gg, opts]) // (VerbosePrint[2,
                  "Applying CommutatorReduce"]; CommutatorReduce[#,opts])&,
          VerbosePrint[2, "Expanding the NM products"];
          NMExpand[
              a /. Power -> NMPower] //. (VerbosePrint[2,
                "Applying expansion rules"]; surules[gg, opts])],
        Message[ExpandU::baddim, gg, gd]]);



(* Getting numbers out: *)

IsoDot[a_*b_, c_] /; UScalarQ[a]&&FreeQ[a,allpatterns] := Conjugate[a]*IsoDot[b, c];
IsoDot[a_, b_*c_] /; UScalarQ[b]&&FreeQ[b,allpatterns] := b*IsoDot[a, c];



(* Just in case someone would try to IsoDot two scalars... might come in handy
though, - when abusing notation in a mixture of iso-vector and iso-index
notation: *)

IsoDot[a_, b_] /; btss[a, b] := Conjugate[a]*b;



(* Getting IsoDots out: *)

IsoDot[NM[IsoDot[a_, aa_], b_], c_] :=
    NM[Conjugate[IsoDot[a, aa]], IsoDot[b, c]];
IsoDot[c_, NM[b_, IsoDot[a_, aa_]]] := NM[IsoDot[c, b], IsoDot[a, aa]];



(* Squaring the generator iso-vector: *)



(* These rules could probably be generalized to other than SU(2) and SU(3) and
the standard representations.  This will have to wait till some other time... *)

IsoDot[IsoVector[UMatrix[UGenerator, optsm___], optsv___],
        IsoVector[
          UMatrix[UGenerator, ___], ___]] /; ((fcsunn /.
                  Flatten[{optsm, optsv}] /. Options[IsoVector]) == 2 &&
          gaugedimcheck[UMatrix, optsm, optsv] == 2) :=(*3**)(*Changed 1/2 -
        2000; the conjugation on the first argument changes things*)
      UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, optsm, optsv]];
IsoDot[IsoVector[UMatrix[UGenerator, optsm___], optsv___],
        IsoVector[
          UMatrix[UGenerator, ___], ___]] /; ((fcsunn /.
                  Flatten[{optsm, optsv}] /. Options[IsoVector]) == 3 &&
          gaugedimcheck[UMatrix, optsm, optsv] == 3) :=(*16/3*)4/3*
      UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, optsm, optsv]];



(* The projection function (changed 16/12/1999): *)

Projection[i_Integer][fcsuni[j_Integer]] := If[i == j, 1, 0];
Projection[i_Integer][j_Integer] := If[i == j, 1, 0];



(* IsoDot of projection iso-vectors: *)

IsoDot[IsoVector[Projection[i_], opts1___],
      IsoVector[Projection[j_], opts2___]] :=
    If[! FreeQ[jj[opts1, opts2, Options[IsoVector]], fcsunn -> 3],
      SU3Delta[i, j], SU2Delta[i, j]];



(* The cross product: *)



(* Zero and one-elements, etc.: *)

IsoCross[0, a_] := 0;
IsoCross[a_, 0] := 0;
IsoCross[a_] /; FreeQ[{a}, patterns] := a;
IsoCross[1, UMatrix[UIdentity, opts___]] := UMatrix[UIdentity, opts];
IsoCross[UMatrix[UIdentity, opts___], 1] := UMatrix[UIdentity, opts];



(* Distributivity: *)

IsoCross[b__ + c_, d__] := IsoCross[b, d] + IsoCross[c, d];
IsoCross[a__, b__ + c_] := IsoCross[a, b] + IsoCross[a, c];



(* Commutativity of the iso-vector of matrices UGeneratorMatrixIsoVector[opts]
and any other non-matrix vector: *)

IsoCross[IsoVector[UMatrix[UGenerator[a___], optsm___], optsv___],
      b_ /; FreeQ[{b}, UMatrix, Infinity, Heads -> True]] := -IsoCross[
        Conjugate[b],
        Conjugate[IsoVector[UMatrix[UGenerator[a], optsm], optsv]]];



(* Getting numbers out: *)

IsoCross[a_*b_, c_] /; UScalarQ[a]&&FreeQ[a,allpatterns] := Conjugate[a]*IsoCross[b, c];
IsoCross[a_, b_*c_] /; UScalarQ[b]&&FreeQ[b,allpatterns] := b*IsoCross[a, c];
IsoCross[a_, b_] /; btss[a, b] := Conjugate[a]*b;



(* Squaring a non-matrix iso-vector (Added 26/2-2000): *)



(* IsoCross[a_, a_] /; nbtui[a] := 0; *)



(* Squaring the generator iso-vector (commutation relations):
Bug fixed (3 instead of 2 in SU(3)) 1/2-2000.
Commented out because of the new definition of IsoDot, IsoCross and
IsoSymmetricCross with conjugation on the first argument. *)

(*IsoCross[IsoVector[UMatrix[UGenerator, optsm___], optsv___],
          IsoVector[
            UMatrix[UGenerator, ___], ___]] /; ((fcsunn /.
                      Flatten[{opts}] /. Options[UGenerator] /.
                  Options[IsoVector]) == 2 &&
            gaugedimcheck[UMatrix, opts] == 2) :=
      2*I*IsoVector[UMatrix[UGenerator, optsm], optsv];*)
(*IsoCross[IsoVector[UMatrix[UGenerator, optsm___], optsv___],
          IsoVector[
            UMatrix[UGenerator, ___], ___]] /; ((fcsunn /.
                      Flatten[{opts}] /. Options[UGenerator] /.
                  Options[IsoVector]) == 3 &&
            gaugedimcheck[UMatrix, opts] == 3) :=
      3*I*IsoVector[UMatrix[UGenerator, optsm], optsv];*)



(* IsoCross of projection iso-vectors: *)

(*Bug fixed 21/12/1999*)
  IsoCross[IsoVector[Projection[i_], opts1___],
        IsoVector[Projection[j_], opts2___]] /; (IntegerQ[i] &&
          IntegerQ[j]) := (gg = (fcsunn /. Flatten[{opts1, opts2}] /.
            Options[IsoVector]);
      Sum[Which[gg == 2, SU2F[i, j, k], gg == 3, SU3F[i, j, k], True,
            fcsunf[i, j, k]]*IsoVector[Projection[k], opts1], {k,
          gg^2 - 1}]);



(* The symmetric cross product: *)



(* Zero and one-elements, etc.: *)

IsoSymmetricCross[0, a_] := 0;
IsoSymmetricCross[a_, 0] := 0;
IsoSymmetricCross[a_] /; FreeQ[a, patterns] := a;
IsoSymmetricCross[1, UMatrix[UIdentity, opts___]] := UMatrix[UIdentity, opts];

IsoSymmetricCross[UMatrix[UIdentity, opts___], 1] :=
    UMatrix[UIdentity, opts];



(* Distributivity: *)

IsoSymmetricCross[b__ + c_, d__] :=
    IsoSymmetricCross[b, d] + IsoSymmetricCross[c, d];
IsoSymmetricCross[a__, b__ + c_] :=
    IsoSymmetricCross[a, b] + IsoSymmetricCross[a, c];



(* Commutativity of the iso-vector of matrices UGeneratorMatrixIsoVector[opts]
and any other non-matrix iso-vector?? Are the d-symbols always symmetric in
the first two indices?? *)

IsoSymmetricCross[IsoVector[UMatrix[UGenerator[a___], optsm___], optsv___],
      b_ /; FreeQ[{b}, UMatrix, Infinity, Heads -> True]] :=
    IsoSymmetricCross[Conjugate[b],
      Conjugate[IsoVector[UMatrix[UGenerator, optsm], optsv]]];
IsoSymmetricCross[Iso[b : UMatrix[UGenerator[___], ___] ..],
      a_ /; FreeQ[a, UMatrix, Infinity, Heads -> True]] :=
    IsoSymmetricCross[Conjugate[a], Conjugate[Iso[b]]];



(* Getting numbers out: *)

IsoSymmetricCross[a_*b_, c_] /; UScalarQ[a]&&FreeQ[a,allpatterns] :=
    Conjugate[a]*IsoSymmetricCross[b, c];
IsoSymmetricCross[a_, b_*c_] /; UScalarQ[b]&&FreeQ[b,allpatterns] := b*IsoSymmetricCross[a, c];
IsoSymmetricCross[a_, b_] /; btss[a, b] := Conjugate[a]*b;



(* IsoSymmetricCross of projection iso-vectors: *)

(*Added 21/12/1999*)
  IsoSymmetricCross[IsoVector[Projection[i_], opts1___],
        IsoVector[Projection[j_], opts2___]] /; (IntegerQ[i] &&
          IntegerQ[j]) := (gg = (fcsunn /. Flatten[{opts1, opts2}] /.
            Options[IsoVector]);
      Sum[Which[gg == 2, SU2D[i, j, k], gg == 3, SU3D[i, j, k], True,
            fcsund[i, j, k]]*IsoVector[Projection[k], opts1], {k,
          gg^2 - 1}]);



(* Products of all vectors is zero in SU(2) : *)

IsoSymmetricCross[a_,
        b_] /; (FreeQ[jj[a, b, Options[IsoVector]], fcsunn -> 3]) && (!
            FreeQ[{a, b}, IsoVector | Iso]) && FreeQ[{a, b}, allpatterns] :=
    0;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Power functions *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* The non-commutative dot power: *)

UDotPower[ff_, n_] := IsoDot @@ Table[ff, {n}];
UDotPower[IsoVector[fcqf[__], ___], 0] := 1;
UDotPower[Iso[fcqf[__], ___], 0] := 1;
UDotPower[IsoVector[UMatrix[UGenerator, opts___], ___], 0] :=
    UMatrix[UIdentity, opts];
UDotPower[Iso[UMatrix[UGenerator[_], opts___] ..], 0] :=
    UMatrix[UIdentity, opts];
UDotPowerTemp[ff_, n_] := IsoDot @@ Table[ff, {n}];
UDotPowerTemp[IsoVector[UMatrix[UGenerator, opts___], ___], 0] :=
    UMatrix[UIdentity, opts];
UDotPowerTemp[Iso[UMatrix[UGenerator[_], opts___] ..], 0] :=
    UMatrix[UIdentity, opts];



(* The non-commutative power: *)

NMPower[ff_, n_Integer] /; btui[ff/.(*Change9/2-2002*)UTrace1->utra] && n>0 := NM @@ Table[ff, {n}];
NMPower[ff_, n_Integer] /; btui[ff/.UTrace1->tra] && n<0 := NM @@ Table[Inverse[ff], {-n}];
NMPower[ff_,
        n_(*Change 7/2 - 1999Integer*)] /;
        ((! btui[ff/.UTrace1->tra]) && (! MatrixQ[ff])) :=
     Power[ff, n];
NMPower[ff_, 0] /; MatrixQ[ff] := IdentityMatrix[Length[ff]];
NMPower[ff_, 0] /; btui[ff/.UTrace1->tra] :=
    UIdentityMatrix[
      Sequence @@
        OptionsSelect[UMatrix,
          List @@ Union @@ Cases[{ff}, _UMatrix, Infinity, Heads -> True]]];
NMPower[ff_, 0] /; ! btui[ff/.UTrace1->tra] := 1



(* Non-commutative matrix product and power: *)

UMatrixProduct[aa_, bb_] /; MatrixQ[{aa}[[1]]] && MatrixQ[{bb}[[1]]] :=
    Table[
      Sum[NM[aa[[i, k]], bb[[k, j]]], {k, 1, Length[aa]}], {i, 1,
        Length[aa]}, {j, 1, Length[aa]}];
UMatrixPower[m_ /; MatrixQ[m], 0] := IdentityMatrix[Length[m]];
UMatrixPower[m_ /; MatrixQ[m], n_] :=
    UMatrixProduct[UMatrixPower[m, n - 1], m];



(* The exponentiation to be used for the U fields: *)

UExp[m_, n_Integer] :=
    If[n >= 0, $UExpansionCoefficients[[1]]*
            UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, m]], 0] +
        If[n >= 1,
          Sum[$UExpansionCoefficients[[i + 1]]*NMPower[m, i], {i, 1, n}],
          0] /. If[FreeQ[m, UMatrix, Infinity, Heads -> True],
        UMatrix[UIdentity, ___] -> 1, {}];
UExp[0, m_, n : (_Integer | _List)] :=
    UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, m]] /.
      If[FreeQ[m, UMatrix, Infinity, Heads -> True],
        UMatrix[UIdentity, ___] -> 1, {}];
UExp[ii_, m_,
      n_Integer] := (ssuu = (Sum[$UExpansionCoefficients[[i]]*x^(i - 1), {i,
                    1, n + 1}] + O[x]^(n + 1))^ii;
        sumstart = Max[ssuu[[4]], 1];
        If[n >= 0,
            ssuu[[3, 1]]*
              UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, m]], 0] +
          If[n >= 1,
            Sum[ssuu[[3, i + 1]]*NMPower[m, i/ssuu[[6]]], {i, sumstart,
                ssuu[[5]] - 1}], 0]) /.
      If[FreeQ[m, UMatrix, Infinity, Heads -> True],
        UMatrix[UIdentity, ___] -> 1, {}];



(* The three definitions below added 10/3-2000. *)

UExp[ii_, m_, l : {_Integer}] := UExp[ii, m, {l}];
UExp[ii_, m_,
      l : {__List}] := (n = Flatten[l]; sumend = Max[n]; sumstart = Min[n];
        ssuu = (Sum[$UExpansionCoefficients[[i + 1]]*x^i, {i, 0, sumend}] +
                O[x]^(sumend + 1))^ii;
        If[sumend >= 1,
          Sum[SeriesCoefficient[ssuu, n[[i]]]*NMPower[m, n[[i]]], {i, 1,
              Length[n]}],
          UMatrix[UIdentity, Sequence @@ OptionsSelect[UMatrix, m]]]) /.
      If[FreeQ[m, UMatrix, Infinity, Heads -> True],
        UMatrix[UIdentity, ___] -> 1, {}];
UExp[ii_,
      m_, {fi_Integer, la_Integer}] := (n = Range[fi, la]; sumend = Max[n];
        sumstart = Min[n];
        ssuu = (Sum[$UExpansionCoefficients[[i + 1]]*x^i, {i, 0, sumend}] +
                O[x]^(sumend + 1))^ii;
        If[sumend >= 1,
          Sum[SeriesCoefficient[ssuu, n[[i]]]*NMPower[m, n[[i]]], {i, 1,
              Length[n]}],
          UMatrix[UIdentity, Sequence @@ OptionsSelect[UMatrix, m]]]) /.
      If[FreeQ[m, UMatrix, Infinity, Heads -> True],
        UMatrix[UIdentity, ___] -> 1, {}];
UExpSeries[a_*m_IsoDot, n_Integer] :=
    If[n >= 0, $UExpansionCoefficients[[1]]*
          UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, a*m]], 0] +
      If[n >= 1,
        Sum[$UExpansionCoefficients[[i + 1]]*Power[a, i]*Power[m, i], {i, 1,
            n}], 0] + O[m]^(n + 1);
UExpSeries[m_, n_Integer] :=
    If[n >= 0, $UExpansionCoefficients[[1]]*
          UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, m]], 0] +
      If[n >= 1, Sum[$UExpansionCoefficients[[i + 1]]*Power[m, i], {i, 1, n}],
         0] + O[m]^(n + 1);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Scalars *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Declaring x as a scalar simply gives x the attribute NumericFunction and adds
it to $UScalars, so that all multiplication functions etc. will bring it out
(for some cases only when UScalarsOut is applied) *)

DeclareUScalar[
      x_] := (If[MemberQ[$UScalars, x]=!=True,AppendTo[$UScalars,x]];
			(*If[AtomQ[x], SetAttributes[x, {NumericFunction, NHoldAll}],
            VerbosePrint[1, x,
              " is not a symbol and cannot be assigned attribute \
NumericFunction.  You'll have to use UScalarsOut"]];*));
UndeclareUScalar[
      x_] := ($UScalars=Complement[$UScalars,
        {x}];);
us[i_] := $UScalars[[i]];
(*Do[SetAttributes[Evaluate[us[i]], {NumericFunction, NHoldAll}];, {i,
        Length[$UScalars]}];*)
upar[i_] := {Scalar, PseudoScalar, Vector, AxialVector, Fermion}[[i]];
(*Do[SetAttributes[Evaluate[upar[i]], {NumericFunction, NHoldAll}];, {i,
        5}];*)
(*SetAttributes[RenormalizationState, {NumericFunction, NHoldAll}];*)
(*SetAttributes[RenormalizationScheme, {NumericFunction, NHoldAll}];*)
(*SetAttributes[ExpansionState, {NumericFunction, NHoldAll}];*)



(* When sums or products of the declared scalars are inside NM or IsoDot, it is
necessary to either use Expand or UScalarsOut to get them out: *)

(*SetAttributes[ppf, {NumericFunction, NHoldAll}];*)



(* Commented out 6/3-2000 to accomodate no longer relying on the NumericFunction
Attribute (got tired of the error messages from Mathematica 4 trying to
evaluate things numerically). *)

(*UScalarsOut[aa_] :=
      Block[{tmpres, f, fc, i, j, a, b, c, pp, res}, $UScalars =
          Union[$UScalars, {ppf}];
        tmpres = (((aa //. (VerbosePrint[2,
                          "Adding temporary arguments to $UScalars"];
                        Flatten[{(Rule[#,
                                    ppf[Position[$Particles, #][[1,
                                        1]]]] & /@ $Particles), (Rule[
                                    f_[a___, #, b___],
                                    f[a, #[ppf[]] /. Pattern -> pp /.
                                        pp[c_, ___] -> c, b]] & /@ $UScalars),
                             fcsuni[i_] /; FreeQ[i, ppf] :>
                              fcsuni[
                                ppf @@ ToCharacterCode[ToString[i]]]}])) /.
                  Flatten[{(Rule[
                              ppf[Position[$Particles, #][[1,
                                    1]]], #] & /@ $Particles),
                      fcsuni[ppf[j___]] :>
                        fcsuni[
                          ToExpression[
                            FromCharacterCode[{j}]]]}]) //.(*Added 25/2 -
                  2000, to allow more complex patterns in $UScalars*)(VerbosePrint[2,
                  "Pulling out composed $UScalars patterns from NM products"];
                 a_NM /; (MemberQ[a, _[ppf[___]]]) :> (NM @@
                        Select[
                          List @@ a, ! MatchQ[#, _[ppf[___]]] &])*(Times @@
                        Select[List @@ a, MatchQ[#, _[ppf[___]]] &])));
        VerbosePrint[2, "Removing temporary arguments to $UScalars"];
        VerbosePrint[3, "\nExpression with temporary arguments:\n",
          StandardForm[tmpres]];
        res = tmpres //. HoldPattern[fx_[ppf[___]]] -> fx; $UScalars =
          Complement[$UScalars, {ppf}]; res];*)



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Matrices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Getting the space-time dependence out of UVector: *)

UVector[fcqf[a__][x_], opts___] := UVector[fcqf[a], opts][x];



(* Powers of the identity matrix are the identity matrix: *)

UMatrix /: UMatrix[UIdentity, opts___]^n_ := UIdentityMatrix[opts];
UMatrixPower[UMatrix[UIdentity, opts___], n_] := UIdentityMatrix[opts];
UMatrix /: UMatrix[UIdentity, opts___]*UMatrix[UIdentity, ___] :=
    UIdentityMatrix[opts];
UMatrix /: (UMatrix[UIdentity, ___]*b_ /; btui[b]) := b;



(* The identity matrix dotted with a UVector is the UVector: *)

Unprotect[Dot];
fcdot[a___, HoldPattern[Times[b___, UMatrix[UIdentity, ___], bb___]],
        c___] /; (! FreeQ[{a}, UMatrix | UVector] || !
            FreeQ[{c}, UMatrix | UVector]) := fcdot[a, Times[b, bb], c];
fcdot[a___, HoldPattern[NM[b___, UMatrix[UIdentity, ___], bb___]],
        c___] /; (! FreeQ[{a}, UMatrix | UVector] || !
            FreeQ[{c}, UMatrix | UVector]) := fcdot[a, NM[b, bb], c];
fcdot[a___, HoldPattern[NM[b___, ba__*UMatrix[UIdentity, ___], bb___]],
        c___] /; (! FreeQ[{a}, UMatrix | UVector] || !
            FreeQ[{c}, UMatrix | UVector]) := fcdot[a, NM[b, ba, bb], c];
fcdot[a___, UMatrix[UIdentity, ___],
        c___] /; (! FreeQ[{a}, UMatrix | UVector] || !
            FreeQ[{c}, UMatrix | UVector]) := fcdot[a, c];
fcdot[UMatrix[UIdentity, ___], b_ /; btui[b]] := b;



(* Linearity: *)

(* Commented out because fcdot is Dot by default and Dot is used by FeynmanParametrize1
   and FeynmanReduce --- yakk!: *)
(*fcdot[a___, b_*c_, d___] /; UScalarQ[b] := b*fcdot[a, c, d];*)



(* Commented out on Buettiker's suggestion: *)
(*fcdot[a__, b___] /; btss[a, b] := Times[a, b];*)
(*fcdot[a___, Plus[b_, c__], d___] := Plus @@ (fcdot[a, #, d] & /@ {b, c});*)



(* Any matrix to the 0th is the identity matrix: *)

UMatrixPower[UMatrix[m_, opts___], 0] /; btui[UMatrix[m, opts]] :=
  UIdentityMatrix[opts];
UMatrixPower[Adjoint[UMatrix[m_, opts___]], 0] := UIdentityMatrix[opts];
UMatrixPower[Transpose[UMatrix[m_, opts___]], 0] := UIdentityMatrix[opts];
UMatrixPower[Conjugate[UMatrix[m_, opts___]], 0] := UIdentityMatrix[opts];



(* Projection iso-vectors are brought left in dot products: *)



(* Added Conjugate because of the new iso-products. 1/2-2000. *)

IsoDot[a_ /; FreeQ[a, Projection], ProjectionIsoVector[i_, b___]] :=
    IsoDot[ProjectionIsoVector[i, b], Conjugate[a]];
IsoDot[a_ /; FreeQ[a, Projection], Iso[b : Projection[_][_] ..]] :=
    IsoDot[Iso[b], Conjugate[a]];
IsoCross[a_ /; FreeQ[a, Projection],
      ProjectionIsoVector[i_, b___]] := -IsoCross[ProjectionIsoVector[i, b],
        Conjugate[a]];
IsoCross[a_ /; FreeQ[a, Projection],
      Iso[b : Projection[_][_] ..]] := -IsoCross[Iso[b], Conjugate[a]];
IsoSymmetricCross[a_ /; FreeQ[a, Projection], ProjectionIsoVector[i_, b___]] :=
     IsoSymmetricCross[ProjectionIsoVector[i, b], Conjugate[a]];
IsoSymmetricCross[a_ /; FreeQ[a, Projection], Iso[b : Projection[_][_] ..]] :=
     IsoSymmetricCross[Iso[b], Conjugate[a]];



(* Resolution of a diagonal matrix over the generator matrices and the identity
matrix: *)

DiagonalUMatrix[l_List,
      opts___] := (dim = (fcsunn /.
                Flatten[OptionsSelect[UMatrix, opts]] /. Options[UMatrix] /.
            Options[UGenerator]);
      gg = WriteOutUMatrices[
          Table[UGeneratorMatrix[fcsuni[i],
              Sequence @@
                Union[OptionsSelect[UGenerator, opts],
                  OptionsSelect[UMatrix, opts]]], {i, dim^2 - 1}]];
      sol = (((#[[2]]) &) /@
            Sort[Flatten[
                Solve[(Equal @@ #) & /@
                    Transpose[{Flatten[
                          Sum[gg[[i]]cc[i], {i, dim^2 - 1}] +
                            cc[0]*IdentityMatrix[
                                gaugedimcheck[UMatrix, opts]]],
                        Flatten[DiagonalMatrix[l]]}],
                  Table[cc[j], {j, 0, dim^2 - 1}]]]]);
      sol.(Join[{UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, opts]]},
            Table[UGeneratorMatrix[fcsuni[ii],
                Sequence @@
                  Union[OptionsSelect[UGenerator, opts],
                    OptionsSelect[UMatrix, opts]]], {ii, dim^2 - 1}]]));



(* The quark charge matrix: *)



(* New definitions to allow changing the basis. 2/2-2000. *)

UMatrix[UQuarkCharge[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)],
        opts1___] /; (DiagonalToU /. Flatten[{opts}] /.
            Options[UQuarkCharge]) && ((fcsunn /.
                  Flatten[OptionsSelect[UMatrix, opts1]] /.
                Options[UMatrix]) ==
            2) && (gaugedimcheck[UMatrix, opts1, opts] == 2)  && (fcexpt /. Flatten[{opts}] /.
                Options[UQuarkCharge]) :=
    DiagonalUMatrix[{2/3*fccoupl[QED[1],st,sc,qs], -1/3*
            fccoupl[QED[1],st,sc,qs]}, opts, opts1] /.
      If[(RemoveIntegerIndices /. Flatten[{opts}] /.
            Options[UQuarkCharge]), {UMatrix[UGenerator[fcsuni[i_Integer]],
              optst___] :>
            IsoDot[ProjectionIsoVector[i, ##] & @@
                OptionsSelect[IsoVector, optst],
              UGeneratorMatrixIsoVector[##] & @@
                Union[OptionsSelect[UMatrix, optst],
                  OptionsSelect[IsoVector, optst],
                  OptionsSelect[UGenerator, optst]]]}, {}];

UMatrix[UQuarkCharge[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)],
        opts1___] /; (DiagonalToU /. Flatten[{opts}] /.
            Options[UQuarkCharge]) && ((fcsunn /.
                  Flatten[OptionsSelect[UMatrix, opts1]] /.
                Options[UMatrix]) ==
            3) && (gaugedimcheck[UMatrix, opts1, opts] == 3)  && (fcexpt /. Flatten[{opts}] /.
                Options[UQuarkCharge]) :=
    DiagonalUMatrix[{2/3*fccoupl[QED[1],st,sc,qs], -1/3*
            fccoupl[QED[1],st,sc,qs], -1/3*fccoupl[QED[1],st,sc,qs]}, opts,
        opts1] /.
      If[(RemoveIntegerIndices /. Flatten[{opts}] /.
            Options[UQuarkCharge]), {UMatrix[UGenerator[fcsuni[i_Integer]],
              optst___] :>
            IsoDot[ProjectionIsoVector[i, ##] & @@
                OptionsSelect[IsoVector, optst],
              UGeneratorMatrixIsoVector[##] & @@
                Union[OptionsSelect[UMatrix, optst],
                  OptionsSelect[IsoVector, optst],
                  OptionsSelect[UGenerator, optst]]]}, {}];



(* The quark mass matrix: *)

UMatrix[UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)],
        opts1___] /; (DiagonalToU /. Flatten[{opts}] /.
            Options[UQuarkMass]) && ((fcsunn /.
                  Flatten[OptionsSelect[UMatrix, opts1]] /.
                Options[UMatrix]) == 2) && (gaugedimcheck[UMatrix, opts1] ==
            2)  && (fcexpt /. Flatten[{opts}] /.
                Options[UQuarkMass]) := ParticleMass[UpQuark, st, sc,
              qs]*(-1/2*UGeneratorMatrix[fcsuni[3], ##] & @@
                  OptionsSelect[UMatrix, opts1] +
                1/2*UIdentityMatrix[opts1]) +
          ParticleMass[DownQuark, st, sc,
              qs]*(1/2*UGeneratorMatrix[fcsuni[3], ##] & @@
                  OptionsSelect[UMatrix, opts1] +
                1/2*UIdentityMatrix[opts1]) /.
        If[(QuarkToMesonMasses /. Flatten[{opts}] /.
              Options[UQuarkMass]), $QuarkToPionMassesRules, {}] /.
      If[(RemoveIntegerIndices /. Flatten[{opts}] /.
            Options[UQuarkMass]), {UMatrix[UGenerator[fcsuni[i_Integer]],
              optst___] :>
            IsoDot[ProjectionIsoVector[i, ##] & @@
                OptionsSelect[IsoVector, optst],
              UGeneratorMatrixIsoVector[##] & @@
                Union[OptionsSelect[UMatrix, optst],
                  OptionsSelect[IsoVector, optst],
                  OptionsSelect[UGenerator, optst]]]}, {}];
UMatrix[UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)],
        opts1___] /; (((! DiagonalToU) && QuarkToMesonMasses) /.
              Flatten[{opts}] /.
            Options[UQuarkMass]) && ((fcsunn /.
                  Flatten[OptionsSelect[UMatrix, opts1]] /.
                Options[UMatrix]) == 2) && (gaugedimcheck[UMatrix, opts1] ==
            2)  && (fcexpt /. Flatten[{opts}] /.
                Options[UQuarkMass]) := ParticleMass[Pion, st, sc, qs]^2/(2*
            QuarkCondensate[st, sc, qs])*UIdentityMatrix[opts1];
UMatrix[UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)],
        opts1___] /; (DiagonalToU /. Flatten[{opts}] /.
            Options[UQuarkMass]) && ((fcsunn /.
                  Flatten[OptionsSelect[UMatrix, opts1]] /.
                Options[UMatrix]) == 3) && (gaugedimcheck[UMatrix, opts1] ==
            3)  && (fcexpt /. Flatten[{opts}] /.
                Options[UQuarkMass]) := ParticleMass[UpQuark, st, sc,
              qs]*(1/2*UGeneratorMatrix[fcsuni[3], ##] & @@
                        OptionsSelect[UMatrix, opts1] +
                      1/(2*Sqrt[3])*UGeneratorMatrix[fcsuni[8], ##] & @@
                  OptionsSelect[UMatrix, opts1] +
                1/3*UIdentityMatrix[opts1]) +
          ParticleMass[DownQuark, st, sc,
              qs]*(-1/2*UGeneratorMatrix[fcsuni[3], opts1] +
                1/(2*Sqrt[3])*UGeneratorMatrix[fcsuni[8], opts1] +
                1/3*UIdentityMatrix[opts1]) +
          ParticleMass[StrangeQuark, st, sc,
              qs]*(-1/Sqrt[3]*UGeneratorMatrix[fcsuni[8], ##] & @@
                  OptionsSelect[UMatrix, opts1] +
                1/3*UIdentityMatrix[opts1]) /.
        If[(QuarkToMesonMasses /. Flatten[{opts}] /.
              Options[UQuarkMass]), $QuarkToMesonMassesRules, {}] /.
      If[(RemoveIntegerIndices /. Flatten[{opts}] /.
            Options[UQuarkMass]), {UMatrix[UGenerator[fcsuni[i_Integer]],
              optst___] :>
            IsoDot[ProjectionIsoVector[i, ##] & @@
                OptionsSelect[IsoVector, optst],
              UGeneratorMatrixIsoVector[##] & @@
                Union[OptionsSelect[UMatrix, optst],
                  OptionsSelect[IsoVector, optst],
                  OptionsSelect[UGenerator, optst]]]}, {}];



(* The UChi matrix: *)

(*Change 6/3 - 1999*)(*Again 19/12 - 1999*)(*Again 10/1 -
    2000*)(*Again 25/2 - 2000 - added options to Particle*)(*Again 26/5 - 2001 -
    dropped options to Particle[PseudoScalar[...]]*)
  UMatrix[UChi[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)],
        opts1 : (__Rule | __List)][x_] /; (fcexpt /. Flatten[{opts}] /.
                Options[UChi]) :=
    2*(QuarkCondensate[st, sc, qs, ##] & @@
          OptionsSelect[QuarkCondensate, opts,
            opts1])*((UGeneratorMatrixIsoDotFull[
                  fcqf[Particle[Scalar[0], st, sc, qs, ##]][x], ##] +
                I*UGeneratorMatrixIsoDotFull[
                    fcqf[Particle[PseudoScalar[0], st, sc, qs]][
                      x], ##]) & @@
          Union[OptionsSelect[UChi, opts, opts1],
            OptionsSelect[UMatrix, opts, opts1]]);
            
(*Change 6/3 - 1999*)(*Again 19/12/1999*)(*Again 10/1 - 2000*)(*Again 25/2 -
    2000 - added options to Particle*)(*Again 26/5 - 2001 -
    dropped options to Particle[PseudoScalar[...]]*)
  UMatrix[UChi[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)]][x_]  /; (fcexpt /. Flatten[{opts}] /.
                Options[UChi]) :=
    2*(QuarkCondensate[st, sc, qs, ##] & @@
          OptionsSelect[QuarkCondensate,
            opts])*((UGeneratorMatrixIsoDotFull[
                  fcqf[Particle[Scalar[0], st, sc, qs, ##]][x], ##] +
                I*UGeneratorMatrixIsoDotFull[
                    fcqf[Particle[PseudoScalar[0], st, sc, qs]][
                      x], ##]) & @@
          Union[OptionsSelect[UChi, opts], OptionsSelect[UMatrix, opts]]);



(* The vector representation commonly used for external sources: *)

UGeneratorMatrixIsoDot[a_ + b_, opts___] :=
    UGeneratorMatrixIsoDot[a, opts] + UGeneratorMatrixIsoDot[b, opts];
UGeneratorMatrixIsoDot[a_*b_, opts___] /; UScalarQ[a]&&FreeQ[a,allpatterns] :=
    a*UGeneratorMatrixIsoDot[b, opts];
UGeneratorMatrixIsoDot[0, ___] := 0;
UGeneratorMatrixIsoDot[
      a_ /; (Head[a] == IsoVector || Head[a] == IsoCross ||
              Head[a] == IsoSymmetricCross) && Head[a] =!= Plus, opts___] :=
    IsoDot[a,
      UGeneratorMatrixIsoVector[##] & @@
        Union[OptionsSelect[UMatrix, opts], OptionsSelect[IsoVector, opts],
          OptionsSelect[UGenerator, opts]]];
UGeneratorMatrixIsoDot[a_ /; Head[a] =!= Plus, opts___] :=
    IsoDot[IsoVector[a, ##] & @@ OptionsSelect[IsoVector, opts],
      UGeneratorMatrixIsoVector[##] & @@
        Union[OptionsSelect[UMatrix, opts], OptionsSelect[IsoVector, opts],
          OptionsSelect[UGenerator, opts]]];
UGeneratorMatrixIsoDotFull[a_, opts___] :=
    UGeneratorMatrixIsoDot[a,
        opts] + (a /. fcqf[body__] -> fcqf[body, fcsuni[0]])*
        UIdentityMatrix[Sequence @@ OptionsSelect[UMatrix, opts]];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Iso-vectors *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Linearity: *)

(*Added 9/2-2002*)IsoVector[0] = 0;
IsoVector[a_*b_, opts___][x_] /; UScalarQ[a]&&FreeQ[a,allpatterns] := a*IsoVector[b, opts][x];
IsoVector[a_*b_, opts___] /; UScalarQ[a] &&FreeQ[a,allpatterns]:= a*IsoVector[b, opts];
IsoVector[a_ + b_, opts___][x_] :=
    IsoVector[a, opts][x] + IsoVector[b, opts][x];
IsoVector[a_ + b_, opts___] := IsoVector[a, opts] + IsoVector[b, opts];
IsoVector[0, ___][_] := 0;
Iso[a : (b_*aa_) ..] /; UScalarQ[b]&&FreeQ[b,allpatterns] := b*Iso @@ ({a}/b);
(*Changed 19/12/1999*)
  Iso[a : (aa_ + bb_) ..] :=
    Iso @@ (Transpose[{List @@ a}][[1]]) +
      Iso @@ (Transpose[{List @@ a}][[2]]);
Iso /: Plus[isovecs : ((Iso[___Integer, _, ___Integer]) ..)] :=
    pluss[isovecs] /. Iso[a__] -> {a, temp} /.
        pluss -> Plus /. {b__, _*temp} -> Iso[b];



(* Getting the space-time dependence out: *)

IsoVector[fcqf[a__][x_], opts___] := IsoVector[fcqf[a], opts][x];



(* Right- and left-handed fields: *)

IsoVector[fcqf[aa___, Particle[LeftComponent[a_, opts0___Rule], i___], bb___], opts___][
      x_] /; (fcexpt /. {opts0} /. Options[LeftComponent]) :=
       1/2*(IsoVector[fcqf[aa, Particle[Vector[a], i], bb], opts][x] +
          IsoVector[fcqf[aa, Particle[AxialVector[a], i], bb], opts][x]);
IsoVector[fcqf[aa___, Particle[RightComponent[a_, opts0___Rule], i___], bb___], opts___][
      x_] /; (fcexpt /. {opts0} /. Options[RightComponent]) :=
       1/2*(IsoVector[fcqf[aa, Particle[Vector[a], i], bb], opts][x] -
          IsoVector[fcqf[aa, Particle[AxialVector[a], i], bb], opts][x]);
fcqf[aa___, Particle[LeftComponent[a_, opts0___Rule], i___], bb___][
       x_] /; (fcexpt /. {opts0} /. Options[LeftComponent]) :=
        1/2*(fcqf[aa, Particle[Vector[a], i], bb][x] +
          fcqf[aa, Particle[AxialVector[a], i], bb][x]);
fcqf[aa___, Particle[RightComponent[a_, opts0___Rule], i___], bb___][
        x_] /; (fcexpt /. {opts0} /. Options[RightComponent]) :=
         1/2*(fcqf[aa, Particle[Vector[a], i], bb][x] -
          fcqf[aa, Particle[AxialVector[a], i], bb][x]);



(* FeynCalc heads are supplied rightaway: *)

FieldStrengthTensorFull[{der_}, b___] :=
    FieldStrengthTensorFull[
      HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[der], b];
FieldStrengthTensor[{der_}, b___] :=
    FieldStrengthTensor[
      HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex[der], b];



(* Field strength tensors: *)



(* Linearity: *)

FieldStrengthTensor[der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
      a_ + b_, r___] := FieldStrengthTensor[der, a, r] +
      FieldStrengthTensor[der, b, r];
FieldStrengthTensor[der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        a_*b_, r___] /; UScalarQ[a] &&FreeQ[a,allpatterns]:= a*FieldStrengthTensor[der, b, r];

FieldStrengthTensorFull[der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
      a_ + b_, r___] := FieldStrengthTensorFull[der, a, r] +
      FieldStrengthTensorFull[der, b, r];
FieldStrengthTensorFull[der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        a_*b_, r___] /; UScalarQ[a] &&FreeQ[a,allpatterns]:= a*FieldStrengthTensorFull[der, b, r];



(* Without the non-abelian term - the first occuring Lorentz index is used: *)

(*With a QuantumField as input*)
  FieldStrengthTensor[
      der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
      fcqf[ders1___HighEnergyPhysics`FeynCalc`PartialD`PartialD,
          p_Particle,
          lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
          lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
          iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex][x_], opts___Rule] /;
	  (fcexpt/.{opts}/.Options[FieldStrengthTensor]) :=
    fcqf[fcpd[der], ders1, p, lli, lis, iis][x] -
      fcqf[fcpd[lli], ders1, p, der, lis, iis][x];
(*With general input*)
  FieldStrengthTensor[
      der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex, obj_, x_?AtomQ, opts___Rule] /;
	  (fcexpt/.{opts}/.Options[FieldStrengthTensor])  :=
   FieldDerivative[obj, x,
        der] - (obj /. {(fff_[
                    ff_[a___ /; FreeQ[{a}, fcli],
                      b_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                      c___], rest___][x] ->
                FieldDerivative[fff[ff[a, der, c], rest][x], x,
                  b]), (ff_[a___ /; FreeQ[{a}, fcli],
                    b_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                    c___][x] -> FieldDerivative[ff[a, der, c][x], x, b])});
(*A field might have been set to zero - added 23/7-2001*)
FieldStrengthTensor[
  _HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,0,opts___Rule] :=0



(* With the non-abelian term: *)

(*With an non - matrix iso - vector of QuantumFields as input*)
  FieldStrengthTensorFull[
        der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        IsoVector[body__][x_], coup_:1, opts___Rule]  /;
	  (fcexpt/.{opts}/.Options[FieldStrengthTensorFull]) :=
    FieldStrengthTensor[der, IsoVector[body][x], x, opts] +
      coup*IsoCross[
          IsoVector[body][x] /.
            fcqf[first___, p_Particle,
                a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                rest___] :> fcqf[first, p, der, rest], IsoVector[body][x]];
(*With a matrix of quantum fields as input*)
  FieldStrengthTensorFull[
        der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex, obj_, x_,
        coup_:I, opts___Rule] /; (!FreeQ[obj,fcqf,Infinity,Heads->True] &&
	            (fcexpt/.{opts}/.Options[FieldStrengthTensorFull])) :=
    FieldStrengthTensor[der, obj, x, opts] +
      coup*(NM[obj /.
                fcqf[first___, p_Particle,
                    a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                    rest___] :> fcqf[first, p, der, rest], obj] -
            NM[obj, obj /.
                fcqf[first___, p_Particle,
                    a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                    rest___] :> fcqf[first, p, der, rest]]);
(*With general input*)
(*The object must then have only one Lorentz index...*)
  FieldStrengthTensorFull[
        der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex, obj_, x_,
        coup_?(!MatchQ[#,_Rule]&), opts___Rule] /; (FreeQ[obj,fcqf,Infinity,Heads->True] &&
	            (fcexpt/.{opts}/.Options[FieldStrengthTensorFull])) :=
    FieldStrengthTensor[der, obj, x, opts] +
      coup*(NM[obj /.
                a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex :>
		der, obj] -
            NM[obj, obj /.
	    a_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex :>
	    der]);

FieldStrengthTensorFull[
        der_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex, obj_, x_,
        opts___Rule] /; (FreeQ[obj,fcqf,Infinity,Heads->True] &&
	            (fcexpt/.{opts}/.Options[FieldStrengthTensorFull])) :=
FieldStrengthTensorFull[der,obj,x,I,opts];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Explicit objects *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Writing out iso-vectors: *)

WriteOutIsoVectors[aa_, opts___Rule] :=
    aa /. writeoutivrule1[opts] /. writeoutivrule2[opts] /.
    writeoutivrule3[opts];



(* IsoVectors - general case: *)

writeoutivrule1[
      opts1___] := (IsoVector[a_[b__, opts0___Rule], opts___Rule][
          x_] :> (Iso @@
            Table[(a[b, ##] & @@ OptionsSelect[a, opts0, opts1, opts])[
                  fcsuni[isosp]][
                x], {isosp, ((fcsunn^2 - 1) /. Flatten[{opts1}] /.
                      OptionsSelect[IsoVector, opts] /.
                    Options[IsoVector])}]));
writeoutivrule2[opts1___] :=
    IsoVector[a_[b__, opts0___Rule], opts___Rule] :>
      Iso @@ Table[(a[b, ##] & @@ OptionsSelect[a, opts0, opts1, opts])[
            fcsuni[isosp]], {isosp, ((fcsunn^2 - 1) /.
                    Flatten[{opts1}] /. OptionsSelect[IsoVector, opts] /.
                Options[IsoVector])}];
(* Added 17/6-2001 *)
writeoutivrule3[opts1___] :=
    IsoVector[a_, opts___Rule] :>
      Iso @@ Table[a[fcsuni[isosp]], {isosp, ((fcsunn^2 - 1) /.
                    Flatten[{opts1}] /. Options[IsoVector])}];

(* Dot products. Rewritten 21/12-1999.
Changed to accomodate the new definition of IsoDot. 1/2-2000. *)

IsoDot[Iso[aa__], Iso[bb__]] :=
  Sum[NM[Conjugate[{aa}[[ii]]], {bb}[[ii]]], {ii, 1, Length[{aa}]}]
(*IsoDot[Iso[aa__], Iso[bb__]] /; MatrixQ[{aa}[[1]]] || MatrixQ[{bb}[[1]]] :=
    Sum[NM[{aa}[[ii]], {bb}[[ii]]], {ii, 1, Length[{aa}]}];
IsoDot[Iso[aa__],
        Iso[bb__]] /; ((MatrixQ[{aa}[[1]]] &&
              UScalarQ[{aa}[[1, 1, 1]]]) || (MatrixQ[{bb}[[1]]] &&
              UScalarQ[{bb}[[1, 1, 1]]])) :=
    Sum[NM[{aa}[[ii]], {bb}[[ii]]], {ii, 1, Length[{aa}]}];
IsoDot[Iso[aa__], Iso[bb__]] /; MatrixQ[{aa}[[1]]] && MatrixQ[{bb}[[1]]] :=
    Sum[UMatrixProduct[{aa}[[ii]], {bb}[[ii]]], {ii, 1, Length[{aa}]}];
IsoDot[Iso[aa__], Iso[bb__]] /; ! MatrixQ[{aa}[[1]]] && ! MatrixQ[{bb}[[1]]] :=
     Sum[NM[{aa}[[ii]], {bb}[[ii]]], {ii, 1, Length[{aa}]}];
IsoDot[Iso[aa__],
        Iso[bb__]] /; (! MatrixQ[{aa}[[1]]] && !
            MatrixQ[{bb}[[1]]] && (UScalarQ[{aa}[[1]]] &&
              UScalarQ[{bb}[[1]]])) :=
    Sum[Times[{aa}[[ii]], {bb}[[ii]]], {ii, 1, Length[{aa}]}];*)



(* Cross products. Rewritten 21/12/1999. *)

IsoCross[Iso[aa__],
      Iso[bb__]] := (ng = (fcsunn /.
            Join[Flatten[
                Cases[{expr}, _[fcsunn, _], Infinity, Heads -> True]],
              Options[IsoVector]]);
      suf = Which[ng == 2, SU2F, ng == 3, SU3F, True, fcsunf];
      Iso @@ Table[
          Sum[suf[ii, jj, kk]*NM[Conjugate[{aa}[[ii]]], {bb}[[jj]]], {ii, 1,
              Length[{aa}]}, {jj, 1, Length[{aa}]}], {kk, 1, Length[{aa}]}]);
IsoSymmetricCross[Iso[aa__],
      Iso[bb__]] := (ng = (fcsunn /.
            Join[Flatten[
                Cases[{expr}, _[fcsunn, _], Infinity, Heads -> True]],
              Options[IsoVector]]);
      suf = Which[ng == 2, SU2D, ng == 3, SU3D, True, fcsund];
      Iso @@ Table[
          Sum[suf[ii, jj, kk]*NM[Conjugate[{aa}[[ii]]], {bb}[[jj]]], {ii, 1,
              Length[{aa}]}, {jj, 1, Length[{aa}]}], {kk, 1, Length[{aa}]}]);
(*IsoCross[Iso[aa__],
        Iso[bb__]] /; ((MatrixQ[{aa}[[1]]] || MatrixQ[{bb}[[1]]]) &&
          Length[{aa}] == 3 && Length[{bb}] == 3) :=
    Iso[NM[{aa}[[2]], {bb}[[3]]] - NM[{aa}[[3]], {bb}[[2]]],
      NM[{aa}[[3]], {bb}[[1]]] - NM[{aa}[[1]], {bb}[[3]]],
      NM[{aa}[[1]], {bb}[[2]]] - NM[{aa}[[2]], {bb}[[1]]]];
IsoCross[Iso[aa__],
        Iso[bb__]] /; ((MatrixQ[{aa}[[1]]] &&
              UScalarQ[{aa}[[1, 1, 1]]]) || (MatrixQ[{bb}[[1]]] &&
              UScalarQ[{bb}[[1, 1, 1]]])) :=
    Iso[NM[{aa}[[2]], {bb}[[3]]] - NM[{aa}[[3]], {bb}[[2]]],
      NM[{aa}[[3]], {bb}[[1]]] - NM[{aa}[[1]], {bb}[[3]]],
      NM[{aa}[[1]], {bb}[[2]]] - NM[{aa}[[2]], {bb}[[1]]]];
IsoCross[Iso[aa__], Iso[bb__]] /; MatrixQ[{aa}[[1]]] && MatrixQ[{bb}[[1]]] :=
    Iso[UMatrixProduct[{aa}[[2]], {bb}[[3]]] -
        UMatrixProduct[{aa}[[3]], {bb}[[2]]],
      UMatrixProduct[{aa}[[3]], {bb}[[1]]] -
        UMatrixProduct[{aa}[[1]], {bb}[[3]]],
      UMatrixProduct[{aa}[[1]], {bb}[[2]]] -
        UMatrixProduct[{aa}[[2]], {bb}[[1]]]];
IsoCross[Iso[aa__],
        Iso[bb__]] /; ! MatrixQ[{aa}[[1]]] && ! MatrixQ[{bb}[[1]]] :=
    Iso[NM[{aa}[[2]], {bb}[[3]]] - NM[{aa}[[3]], {bb}[[2]]],
      NM[{aa}[[3]], {bb}[[1]]] - NM[{aa}[[1]], {bb}[[3]]],
      NM[{aa}[[1]], {bb}[[2]]] - NM[{aa}[[2]], {bb}[[1]]]];
IsoCross[Iso[aa__],
        Iso[bb__]] /; (! MatrixQ[{aa}[[1]]] && !
            MatrixQ[{bb}[[1]]] && (UScalarQ[{aa}[[1]]] &&
(UScalarQ[{aa}[[1]]]))) :=
    Iso[Times[{aa}[[2]], {bb}[[3]]] - Times[{aa}[[3]], {bb}[[2]]],
      Times[{aa}[[3]], {bb}[[1]]] - Times[{aa}[[1]], {bb}[[3]]],
      Times[{aa}[[1]], {bb}[[2]]] - Times[{aa}[[2]], {bb}[[1]]]];*)



(* Writing out UMatrices: *)

uix = UIndex;
(*Bug fixed 1/2 - 2000*)(*Also added the tbl stuff;
  don't know why mma messes up when using Table directly*)

  WriteOutUMatrices1[aa_, (optss___Rule | optss___List)] :=
  aa(*added 13/3 - 2000*)/.
     (*Added 29/12-1999 because UTrace1 checks only for UMatrices not for
       explicit matrices when pulling out factors*)
       UTrace1 -> tr/.
            Power -> NMPower /. {UMatrix[a_[ind___, op___Rule], opts___] :>
              tbl[a[ind, Sequence @@ OptionsSelect[a, op, opts, optss]][
                  uix[tmp`i], uix[tmp`j]], {tmp`i,
                  gaugedimcheck[UMatrix,
                    UMatrix[a[ind, op], opts, optss]]}, {tmp`j,
                  gaugedimcheck[UMatrix, UMatrix[a[ind, op], opts, optss]]}],
            UVector[a_[ind___, op___Rule], opts___] :>
              tbl[a[ind, Sequence @@ OptionsSelect[a, op, opts, optss]][
                  uix[tmp`i]], {tmp`i,
                  gaugedimcheck[UVector, UVector[a[ind, op], opts],
                    optss]}]} /. {UMatrix[a_, opts___] /; AtomQ[a] :>
            tbl[a[uix[tmp`i], uix[tmp`j],
                Sequence @@ OptionsSelect[a, opts, optss]], {tmp`i,
                gaugedimcheck[UMatrix, UMatrix[a, opts], optss]}, {tmp`j,
                gaugedimcheck[UMatrix, UMatrix[a, opts], optss]}],
          UVector[a_, opts___] /; AtomQ[a] :>
            tbl[a[uix[tmp`i],
                Sequence @@ OptionsSelect[a, opts, optss]], {tmp`i,
                gaugedimcheck[UVector, UVector[a, opts], optss]}]} /.
      tbl -> Table /. tr -> UTrace1(*/. (fcsunn -> _) -> Sequence[]*);
uindxx = (uix | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex);

 (*added 28/5 - 2002*)
WriteOutUMatrices2[aa_, (optss___Rule | optss___List)] :=
  aa(*added 13/3 - 2000*)/.
     (*Added 29/12-1999 because UTrace1 checks only for UMatrices not for
       explicit matrices when pulling out factors*)
       UTrace1 -> tr/.
            Power -> NMPower /. {UMatrix[a_[ind___, op___Rule], opts___][x___] :>
              tbl[a[ind, Sequence @@ OptionsSelect[a, op, opts, optss]][
                  uix[tmp`i], uix[tmp`j]][x], {tmp`i,
                  gaugedimcheck[UMatrix,
                    UMatrix[a[ind, op], opts, optss]]}, {tmp`j,
                  gaugedimcheck[UMatrix, UMatrix[a[ind, op], opts, optss]]}],
            UVector[a_[ind___, op___Rule], opts___][x___] :>
              tbl[a[ind, Sequence @@ OptionsSelect[a, op, opts, optss]][
                  uix[tmp`i]][x], {tmp`i,
                  gaugedimcheck[UVector, UVector[a[ind, op], opts],
                    optss]}]} /. {UMatrix[a_, opts___][x___] /; AtomQ[a] :>
            tbl[a[uix[tmp`i], uix[tmp`j],
                Sequence @@ OptionsSelect[a, opts, optss]][x], {tmp`i,
                gaugedimcheck[UMatrix, UMatrix[a, opts], optss]}, {tmp`j,
                gaugedimcheck[UMatrix, UMatrix[a, opts], optss]}],
          UVector[a_, opts___][x___] /; AtomQ[a] :>
            tbl[a[uix[tmp`i],
                Sequence @@ OptionsSelect[a, opts, optss]][x], {tmp`i,
                gaugedimcheck[UVector, UVector[a, opts], optss]}]} /.
      tbl -> Table /. tr -> UTrace1(*/. (fcsunn -> _) -> Sequence[]*);
uindxx = (uix | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex);

WriteOutUMatrices[aa_, (optss___Rule | optss___List)] := 
  WriteOutUMatrices1[WriteOutUMatrices2[aa/.NM->nm,optss],optss]/.nm->NM;

UIdentity[i : uindxx[_], j : uindxx[_], (opts___Rule | opts___List)] :=
    Which[(fcsunn /. Flatten[{opts}] /. Options[UMatrix]) == 2 &&
        gaugedimcheck[UMatrix, opts] == 2,
      SU2Delta[i,
        j], (fcsunn /. Flatten[{opts}] /. Options[UMatrix]) == 3 &&
        gaugedimcheck[UMatrix, opts] == 3, SU3Delta[i, j]];



(* Change 29/11-1998: Added option dependence for the dimension -
   don't know why I didn't have that already ... *)

(*UGenerator[fcsuni[i_Integer], opts___][uindxx[j_Integer],
        uindxx[k_Integer]] := $SUNBasis[
          gaugedimcheck[UGenerator,
            opts], ((fcsunn - 1)/2) /. Flatten[{opts}] /.
            Options[UGenerator]][[i, j, k]];*)
(*UGenerator[fcsuni[i_Integer]][uindxx[j_Integer],
        uindxx[k_Integer]] := $SUNBasis[
          fcsunn /. Options[UGenerator], ((fcsunn - 1)/2) /.
            Options[UGenerator]][[i, j, k]];*)
(*Added fcexsuni, 31/1 - 2000*)
  UGenerator[(fcsuni | fcexsuni)[i_Integer], opts___][uindxx[j_Integer],
      uindxx[k_Integer]] := $SUNBasis[
        gaugedimcheck[UGenerator,
          opts], ((gaugedimcheck[UGenerator, opts] - 1)/2)][[i, j, k]];
UGenerator[(fcsuni | fcexsuni)[i_Integer]][uindxx[j_Integer],
      uindxx[k_Integer]] := $SUNBasis[
        gaugedimcheck[UGenerator,
          opts], ((gaugedimcheck[UGenerator, opts] - 1)/2)][[i, j, k]];
(*Added fcexsuni, 14/3 - 2000*)
  UGenerator[i_Integer, opts___][uindxx[j_Integer],
      uindxx[k_Integer]] := $SUNBasis[
        gaugedimcheck[UGenerator,
          opts], ((gaugedimcheck[UGenerator, opts] - 1)/2)][[i, j, k]];
UGenerator[i_Integer][uindxx[j_Integer],
      uindxx[k_Integer]] := $SUNBasis[
        gaugedimcheck[UGenerator,
          opts], ((gaugedimcheck[UGenerator, opts] - 1)/2)][[i, j, k]];



(* UQuarkChargeMatrix is treated as a special case: *)

UQuarkCharge[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)][uindxx[i_Integer],
         uindxx[j_Integer]] /; ((fcsunn /. Flatten[{opts}] /.
                Options[UQuarkCharge]) == 2 &&
          gaugedimcheck[UQuarkCharge, opts] ==
            2) := ({{2/3*fccoupl[QED[1], st, sc, qs],
            0}, {0, -1/3*fccoupl[QED[1], st, sc, qs]}})[[i, j]];
UQuarkCharge[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)][
        uindxx[ii_Integer],
        uindxx[jj_Integer]] /; ((fcsunn /. Flatten[{opts}] /.
                Options[UQuarkCharge]) == 3 &&
          gaugedimcheck[UQuarkCharge, opts] ==
            3) := ({{2/3*fccoupl[QED[1], st, sc, qs], 0,
            0}, {0, -1/3*fccoupl[QED[1], st, sc, qs], 0}, {0,
            0, -1/3*fccoupl[QED[1], st, sc, qs]}})[[ii, jj]];



(* UQuarkMassMatrix is treated as a special case: *)

UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)][uindxx[i_Integer],
         uindxx[j_Integer]] /; ((fcsunn /. Flatten[{opts}] /.
                Options[UQuarkMass]) == 2 &&
          gaugedimcheck[UQuarkMass, opts] ==
            2) := ({{ParticleMass[UpQuark, st, sc, qs], 0}, {0,
              ParticleMass[DownQuark, st, sc, qs]}} /.
          If[(QuarkToMesonMasses /. Flatten[{opts}] /.
                Options[UQuarkMass]), $QuarkToPionMassesRules, {}])[[i, j]];
UQuarkMass[st___RenormalizationState, sc___RenormalizationScheme,
          qs___ExpansionState, (opts___Rule | opts___List)][
        uindxx[ii_Integer],
        uindxx[jj_Integer]] /; ((fcsunn /. Flatten[{opts}] /.
                Options[UQuarkMass]) == 3 &&
          gaugedimcheck[UQuarkMass, opts] ==
            3) := ({{ParticleMass[UpQuark, st, sc, qs], 0, 0}, {0,
              ParticleMass[DownQuark, st, sc, qs], 0}, {0, 0,
              ParticleMass[StrangeQuark, st, sc, qs]}} /.
          If[(QuarkToMesonMasses /. Flatten[{opts}] /.
                Options[UQuarkMass]), $QuarkToMesonMassesRules, {}])[[ii,
        jj]];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Field matrices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* The U field is expanded in terms of the tuplet of meson fields which we call
PhiMesonIsoVector, assuming the exponential representation for terms of
higher order than 4: *)

(*SetAttributes[DropFactor, {NumericFunction, NHoldAll}];*)
DeclareUScalar[DropFactor];
Adjoint[DropFactor[a___]] := DropFactor[a];
Conjugate[DropFactor[a___]] ^:= DropFactor[a];
Transpose[DropFactor[a___]] ^:= DropFactor[a];
(*Change 20/12/1999; added qs___ExpansionState*)
  udrop[p_, ar___RenormalizationState, br___RenormalizationScheme,
      qs___ExpansionState, opts___Rule | opts___List] :=
    If[(DropOrder /. Flatten[{opts}] /. Options[UFieldMatrix]) =!= Infinity,
      DropFactor[p, ar, br, qs], 1];
(*Change 20/12/1999; dropped qs___ExpansionState*)
  UFieldMatrix[(f_HighEnergyPhysics`FeynCalc`QuantumField`QuantumField)[x_](*,
         qs___ExpansionState*), opts___] := UFieldMatrix[1, f[x],(*qs,*)opts];

UFieldMatrix[
      fcqf[f__][x_],(*qs___ExpansionState,*)opts___Rule | opts___List] :=
    UFieldMatrix[1, fcqf[f][x],(*qs,*)opts];

(*Change 20/12/1999;
  put qs___ExpansionState under Particle instead of under UFieldMatrix*)
  UFieldMatrix[ii_(*?NumberQ*),
      fcqf[ders___fcpd,
        Particle[p_, ar___RenormalizationState, br___RenormalizationScheme,
          qs___ExpansionState],
        lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex],
      opts___] := (
  decon = (Constant /. Flatten[{opts}] /. Options[UFieldMatrix]);
  If[decon==Automatic,decon=DecayConstant[p, ar, br, qs]];
  Clear[DropFactor];
      DropFactor /: MakeBoxes[DropFactor[___], TraditionalForm] :=
        MakeBoxes[StyleForm["\[Aleph]", FontSlant -> "Italic"]][[1]];
      DropFactor /:
        Power[DropFactor[p, ar, br, qs], i_] /;
          i > (DropOrder /. Flatten[{opts}] /. Options[UFieldMatrix]) := 0;
      expon = IsoDot[((IsoVector[
                    fcqf[ders, Particle[p, ar, br, qs], lis, iis], ##] &) @@
              OptionsSelect[IsoVector, opts]),
          UGeneratorMatrixIsoVector @@
            Union[OptionsSelect[UMatrix, opts],
              OptionsSelect[IsoVector, opts],
              OptionsSelect[UGenerator, opts]]];
      nn = ExpansionOrder /. Flatten[{opts}] /. Options[UFieldMatrix];
      Simplify[UExp[ii,
          I*udrop[p, ar, br, qs, opts]*expon/(decon),
          nn]]);

UFieldMatrix[ii_(*?NumberQ*),
      fcqf[ders___fcpd,
          Particle[p_, ar___RenormalizationState, br___RenormalizationScheme,
            qs___ExpansionState],
          lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
          iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex][x_],
      opts___] := (
       decon = (Constant /. Flatten[{opts}] /. Options[UFieldMatrix]);
  If[decon==Automatic,decon=DecayConstant[p, ar, br, qs]];
     Clear[DropFactor];
      DropFactor /: MakeBoxes[DropFactor[___], TraditionalForm] :=
        MakeBoxes[StyleForm["\[Aleph]", FontSlant -> "Italic"]][[1]];
      DropFactor /:
        Power[DropFactor[p, ar, br, qs], i_] /;
          i > (DropOrder /. Flatten[{opts}] /. Options[UFieldMatrix]) := 0;
      expon = IsoDot[((IsoVector[
                      fcqf[ders, Particle[p, ar, br, qs], lis, iis], ##] &) @@
                 OptionsSelect[IsoVector, opts])[x],
          UGeneratorMatrixIsoVector @@
            Union[OptionsSelect[UMatrix, opts],
              OptionsSelect[IsoVector, opts],
              OptionsSelect[UGenerator, opts]]];
      nn = ExpansionOrder /. Flatten[{opts}] /. Options[UFieldMatrix];
      Simplify[UExp[ii,
          I*udrop[p, ar, br, qs, opts]*expon/(decon),
          nn]]);

UFieldMatrixSeries[
      fcqf[ders___fcpd,
        Particle[p_, ar___RenormalizationState, br___RenormalizationScheme,
          qs___ExpansionState],
        lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex],
      opts___] := (
    decon = (Constant /. Flatten[{opts}] /. Options[UFieldMatrixSeries]);
  If[decon==Automatic,decon=DecayConstant[p, ar, br, qs]];
  expon = IsoDot[((IsoVector[
                    fcqf[ders, Particle[p, ar, br, qs], lis, iis], ##] &) @@
              OptionsSelect[IsoVector, opts]),
          UGeneratorMatrixIsoVector @@
            Union[OptionsSelect[UMatrix, opts],
              OptionsSelect[IsoVector, opts],
              OptionsSelect[UGenerator, opts]]];
      nn = ExpansionOrder /. Flatten[{opts}] /. Options[UFieldMatrixSeries];
      Simplify[UExpSeries[I*expon/(decon), nn]]);

UFieldMatrixSeries[
      fcqf[ders___fcpd,
          Particle[p_, ar___RenormalizationState, br___RenormalizationScheme,
            qs___ExpansionState],
          lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
          iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex][x_],
      opts___] := (
  decon = (Constant /. Flatten[{opts}] /. Options[UFieldMatrixSeries]);
  If[decon==Automatic,decon=DecayConstant[p, ar, br, qs]];
  expon =IsoDot[((IsoVector[
                      fcqf[ders, Particle[p, ar, br, qs], lis, iis], ##] &) @@
                 OptionsSelect[IsoVector, opts])[x],
          UGeneratorMatrixIsoVector @@
            Union[OptionsSelect[UMatrix, opts],
              OptionsSelect[IsoVector, opts],
              OptionsSelect[UGenerator, opts]]];
      nn = ExpansionOrder /. Flatten[{opts}] /. Options[UFieldMatrixSeries];
      Simplify[UExpSeries[I*expon/(decon), nn]]);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Derivatives *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* FeynCalc heads are put on immediately: *)

FieldDerivative[aa_, x_, {loris__}] :=
    FieldDerivative[aa,
          x, ##] & @@ (HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex /@
	  {loris});



(* If there is no x dependence, the derivative i zero: *)

FieldDerivative[aa_,
        x_, __HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex] /;
      FreeQ[aa, x, Infinity] := 0;



(* Derivative of functions that FieldDerivative distributes over: *)

distheads = Alternatives @@ $UDistributiveFunctions;

FieldDerivative[a : (distheads[__]), x_,
      lori_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex] :=
  (Head[a]) @@ Join[
        FieldDerivative[#, x, lori]& /@ Select[List@@a, (!MatchQ[#, (_ -> _) | ({(_ -> _) ...})]&)],
        Select[List@@a, (MatchQ[#, (_ -> _) | ({(_ -> _) ...})]&)]];


(* Linearity: *)

FieldDerivative[a_Plus, x_,
      lori_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex] :=
    Plus @@ (FieldDerivative[#, x, lori] & /@ (List @@ a));
multheads = Alternatives @@ $UMultiplications;
FieldDerivative[a_Times, x_,
        lori_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex] /;
	(Plus @@ (If[FreeQ[#, x], 1, 0] & /@ (List @@ a))) !=
        0 := (Times @@ Select[List @@ a, FreeQ[#, x] &])*
      FieldDerivative[Times @@ Select[List @@ a, ! FreeQ[#, x] &], x, lori];



(* The product rule: *)

FieldDerivative[aa : (multheads[__]), x_,
      lori_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex](*/; (Plus @@
(If[FreeQ[#, x], 1, 0] & /@ (List @@ aa))) == 0*):= (Head[aa])[
        FieldDerivative[(List @@ aa)[[1]], x, lori],
        Sequence @@ (Drop[List @@ aa, 1])] + (Head[aa])[(List @@ aa)[[1]],
        FieldDerivative[(Head[aa]) @@ (Drop[List @@ aa, 1]), x, lori]];



(* The power rule: *)

FieldDerivative[aa_^n_, x_,
      lori_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex] :=
    n*aa^(n - 1)*FieldDerivative[aa, x, lori];



(* Definition of the derivative on QuantumFields: *)

FieldDerivative[fcqf[body___][x_], x_,
      lori_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex] :=
    fcqf[fcpd[lori], body][x];



(* Special cases: *)

(*Changed 31/7-2001. The [x] dependence is put back on the outer function*)
FieldDerivative[((h : IsoVector | UVector | UMatrix)[fcqf[field__], opts___])[
        x_], x_, lori_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex] :=
     h[FieldDerivative[fcqf[field][x], x, lori] /. f_[x]->f, opts][x];



(* Recursive definition of multiple derivatives: *)

FieldDerivative[
      aa_(*?(MemberQ[{Times, Plus},
                Head[#]] &)*)(*Bug fixed and commented out. 9/2 - 2000*), x_,
      loris__HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
      lori1_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex] := (newfunc[
          1] = FieldDerivative[aa, x, lori1];
      Do[newfunc[rep + 1] =
          FieldDerivative[newfunc[rep], x, ##] & @@
            Take[{loris}, {-rep}], {rep, 1, Length[{loris}]}];
      newfunc[Length[{loris}] + 1]);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Adjoints and conjugates *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(*SetAttributes[Adjoint, {NumericFunction, NHoldAll}];*)
(*DeclareUScalar[Adjoint];*)



(* The adjoint of a series: *)

(*Adjoint[sd_SeriesData] :=
      Adjoint[Normal[sd]] + O[sd[[1]]]^(sd[[5]]/sd[[6]]);*)



(* Anti-particles: *)

ChargeConjugate[fcqf[p__][x_]] := ChargeConjugate[fcqf[p]][x];
ChargeConjugate[fcqf[ders___, Particle[p_, i___], c___]] :=
    fcqf[ders, Particle[ChargeConjugate[p], i], c];
ChargeConjugate[IsoVector[a_, b___][x_]] :=
    IsoVector[ChargeConjugate[a], b][x];
Adjoint[IsoVector[a_, b___][x_]] := IsoVector[Adjoint[a], b][x];
(*Added, 6/2 - 2000*)Adjoint[IsoVector[a_, b___]] := IsoVector[Adjoint[a], b];

Adjoint[Projection[i_]] = Projection[i];
Adjoint[Projection[i_][j_]] = Projection[i][j];
Projection /:
Conjugate[Projection[i_][j_]] = Projection[i][j];
Projection /:
Conjugate[Projection[i_]] = Projection[i];
(*Adjoint[UMatrix[UGenerator, opts___]] = -UMatrix[UGenerator, opts];*)
(*Changed. 6/2 - 2000*)
  Adjoint[UMatrix[UGenerator[i___], opts___]] /; $StandardSUNBasis :=
    UMatrix[UGenerator[i], opts];
Adjoint[Adjoint[a_]] := a;



(* Distributivity of adjungation: *)

Adjoint[b__ + c_] := Adjoint[Plus[b]] + Adjoint[c];
Adjoint[b__*c_] := Adjoint[Times[b]]*Adjoint[c];
Adjoint[Power[a_, b_]] := Power[Adjoint[a], b];
(*Adjoint[IsoDot[b__, c_]] := IsoDot[Adjoint[b], Adjoint[c]];
Adjoint[IsoCross[b__, c_]] := -IsoCross[Adjoint[b], Adjoint[c]];
Adjoint[IsoSymmetricCross[b__, c_]] :=
    IsoSymmetricCross[Adjoint[b], Adjoint[c]];*)
(*Changed 17/2 - 2000*)
  Adjoint[IsoDot[b_, c_]] :=
    IsoDot[Conjugate[Adjoint[c]], Conjugate[Adjoint[b]]];
Adjoint[IsoCross[b_, c_]] /; $StandardSUNBasis := -IsoCross[
        Conjugate[Adjoint[c]], Conjugate[Adjoint[b]]];
Adjoint[IsoSymmetricCross[b_, c_]] /; $StandardSUNBasis :=
    IsoSymmetricCross[Conjugate[Adjoint[c]], Adjoint[b]];



(* Operators and matrices are interchanged when adjoined: *)

Adjoint[NM[b__, c_]] := NM[Adjoint[c], Adjoint[NM[b]]];



(* Distributivity of conjugation: *)

Unprotect[Conjugate];
Conjugate[b__ + c_] := Conjugate[Plus[b]] + Conjugate[c];
Conjugate[b__*c_] := Conjugate[Times[b]]*Conjugate[c];
Conjugate[Power[a_, b_]] := Power[Conjugate[a], b];
Conjugate[IsoDot[b_, c_]] := IsoDot[Conjugate[b], Conjugate[c]];
Conjugate[IsoCross[b_, c_]] := IsoCross[Conjugate[b], Conjugate[c]];
Conjugate[IsoSymmetricCross[b_, c_]] :=
    IsoSymmetricCross[Conjugate[b], Conjugate[c]];
Conjugate[NM[b__, c_]] := NM[Conjugate[b], Conjugate[c]];

(* Cheap hack to deal with issue with WriteOutIsoVectors. Added 17/6-2001*)
Conjugate[Conjugate[a_][fcsuni[i_]]] := a[fcsuni[i]];


(* The generator matrices are self-adjoined
   (in the standad representation only). *)

Adjoint[UMatrix[UGenerator[i_], opts___]] /; $StandardSUNBasis :=
    UMatrix[UGenerator[i], opts];
Adjoint[IsoVector[UMatrix[UGenerator, optsm___],
          optsv___]] /; $StandardSUNBasis :=
    IsoVector[UMatrix[UGenerator, optsm], optsv];
(*Adjoint[Iso[a : UMatrix[UGenerator[_], ___]]] /; $StandardSUNBasis :=
      Iso[a];*)
Adjoint[UMatrix[UIdentity, opts___]] := UIdentityMatrix[opts];



(* Commented out the two defs below as they are covered by the more general one
   further down. 7/3-2000. *)

(*Adjoint[
          a : (SU2F | SU3F | SU3D | fcsunf |
                  fcsund)[__]] /; $StandardSUNBasis := a;*)
(*Adjoint[a : (SU2Delta | SU3Delta | fcsundel)[__]] := a;*)
Conjugate[
        a : (SU2F | SU3F | SU3D | fcsunf |
                fcsund)[__]] /; $StandardSUNBasis := a;
Conjugate[a : (SU2Delta | SU3Delta | fcsundel)[__]] := a;
Conjugate[UMatrix[UIdentity, opts___]] := UIdentityMatrix[opts];
(*Added, 7/2 - 2000*)
  Conjugate[IsoVector[a_, b___]] := IsoVector[Conjugate[a], b];
(*Added, 7/2 - 2000*)
  Conjugate[IsoVector[a_, b___][x_]] := IsoVector[Conjugate[a], b][x];
Protect[Conjugate];


Unprotect[Transpose];

(* Scalars are brought out of Transpose: *)

Transpose[a__Times] /; btss @@ a :=
    Times[##] & @@ Select[{a}, ((UScalarQ[#]&&FreeQ[#,allpatterns])&)]*Transpose[Times[##]] & @@
      Select[{a}, nbts];



(* The transpose of the diagonal matrices: *)

Transpose[UMatrix[UIdentity, opts___]] := UIdentityMatrix[opts];
(*Commented out 1/2 -
    2000 because of the generalization to arbitrary basis*)(*Transpose[
        UMatrix[UGenerator[3], opts___]] := UMatrix[UGenerator[3], opts];*)
(*Transpose[UMatrix[UGenerator[8], opts___]] := UMatrix[UGenerator[8], opts];*)

Protect[Transpose];



(* Scalars are simply conjugated: *)

(*Adjoint[a_] /; UScalarQ[a] && FreeQ[{Head[a]}, $UAllParticleHeads] :=
      Conjugate[a];*)
(*Added and above commented out 7/3 - 2000*)

Adjoint[a_?((UScalarQ[#]&&FreeQ[#,allpatterns])&)] := Conjugate[a];
Adjoint[Conjugate[a_?((UScalarQ[#]&&FreeQ[#,allpatterns])&)]] := a;
Adjoint[IsoVector[Projection[i_], opts___]] :=
    IsoVector[Projection[i], opts];



(* Matrices are transposed and conjugated: *)

Adjoint[a_?MatrixQ] := Conjugate[Transpose[a]];
Adjoint[Conjugate[Transpose[a_]]] := a;



(* The Dirac bar: *)

DiracBar[fcqf[p__][x_]] := DiracBar[fcqf[p]][x];
DiracBar[fcqf[ders___, Particle[p_, i___], c___]] :=
    fcqf[ders, DiracBar[Particle[p, i]], c];
DiracBar[p_][ui_uix] := DiracBar[p[ui]];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Traces *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* The trace is defined.  It should be noted that the trace is performed on
terms proportional to  UChiMatrix[opts] only if QuarkMatrixSimplify is true
and on terms proportional to UIdentityMatrix[opts], GeneratorIsoVector[opts]
or Generator[i,opts], which matrices have traces UDimension and 0
respectively: *)



(* Tracing a series: *)

(*UTrace1[sd_SeriesData, opts___] :=
      UTrace1[Normal[sd], opts] + O[sd[[1]]]^(sd[[5]]/sd[[6]]);*)



(* Distributivity: *)

UTrace1[a_ + b_, opts___] := UTrace1[a, opts] + UTrace1[b, opts];
UTrace2[a_ + b_, opts___] := UTrace2[a, opts] + UTrace2[b, opts];



(* Getting scalars out: *)

(* Changed to avoid infinite loop on e.g. UTrace1[UMatrix[a]*UMatrix[b]].
Of course, such expressions do not make sense - it would perhaps be better to
issue a warning. 25/2-2000. *)

UTrace1[a_Times,
        opts___] /; (btui[a] && (Union[btui /@ (List @@ a)] =!= {True})) :=
    Times @@ Select[List @@ a, nbtui]*
      UTrace1[Times @@ Select[List @@ a, btui], opts];



(* Getting scalars out: *)

UTrace2[a_Times,
        opts___] /; (btui[a] && (Union[btui /@ (List @@ a)] =!= {True})) :=
    Times @@ Select[List @@ a, nbtui]*
      UTrace2[Times @@ Select[List @@ a, btui], opts];



(* Traces known by Phi: *)

UTrace1[0, ___] := 0;
UTrace1[a_?NumberQ, ___] := a;
UTrace1[IsoVector[UMatrix[UGenerator[___Rule], ___], ___], ___] := 0;
UTrace1[UMatrix[UIdentity, opts1___], opts___] :=
    gaugedimcheck[UTrace, opts, opts1];
UTrace1[UMatrix[UGenerator[_, ___Rule], ___], ___] := 0;
UTrace1[b_, opts___Rule] /; MatrixQ[b] :=
    Sum[b[[i, i]], {i, 1, Length[b[[1]]]}];



(* Products where UIdentityMatrix is the only matrix: *)

UTrace1[NM[aa__],
        opts___Rule] /; (FreeQ[NM[aa] /. UMatrix[UIdentity, ___] -> tempm,
            UMatrix |(*Added 12/3 - 2000*)$UMatrices,
            Infinity] && (! FreeQ[NM[aa], UIdentity, Infinity]) &&
          gaugedimcheck[UMatrix, opts, aa] == 2) :=
    2*NM[aa] /. UMatrix[UIdentity, ___] -> 1;
UTrace1[NM[aa__], opts___Rule] /;
      FreeQ[NM[aa] /. UMatrix[UIdentity, ___] -> tempm,
          UMatrix |(*Added 12/3 - 2000*)$UMatrices,
          Infinity] && (!
            FreeQ[NM[aa], UIdentity, Infinity]) && (gaugedimcheck[UMatrix,
              opts, aa] == 3) := 3*NM[aa] /. UMatrix[UIdentity, ___] -> 1;



(* Products with one generator and no other matrices: *)



(* Change 15/12/1999: Added &&FreeQ[NM[aa],$UMatrices,Infinity], etc. *)
(* Change 28/5-2002: Added MatrixQ stuff *)

HoldPattern[UTrace1[NM[aa__],
        opts___Rule]] /; ((TraceSimplify /. Flatten[{opts}] /.
              Options[UTrace]) && (Count[NM[aa] /. Power -> NMPower,
                UGenerator, Infinity, Heads -> True] == 1) &&
          FreeQ[NM[aa], UMatrix[a___ /; FreeQ[{a}, UGenerator]], Infinity] &&
          FreeQ[NM[aa], $UMatrices, Infinity] && ((Or@@(MatrixQ/@{aa}))=!=True)) := 0;
UTrace1[IsoDot[aa__],
        opts___Rule] /; ((TraceSimplify /. Flatten[{opts}] /.
              Options[UTrace]) && (Count[IsoDot[aa] /. Power -> NMPower,
                UGenerator, Infinity, Heads -> True] == 1) &&
          FreeQ[IsoDot[aa], UMatrix[a___ /; FreeQ[{a}, UGenerator]],
            Infinity] && FreeQ[IsoDot[aa], $UMatrices, Infinity]) := 0;
UTrace1[IsoCross[aa__],
        opts___Rule] /; ((TraceSimplify /. Flatten[{opts}] /.
              Options[UTrace]) && (Count[IsoCross[aa] /. Power -> NMPower,
                UGenerator, Infinity, Heads -> True] == 1) &&
          FreeQ[IsoCross[aa], UMatrix[a___ /; FreeQ[{a}, UGenerator]],
            Infinity] && FreeQ[IsoCross[aa], $UMatrices, Infinity]) := 0;
UTrace1[IsoSymmetricCross[aa__],
        opts___Rule] /; ((TraceSimplify /. Flatten[{opts}] /.
              Options[UTrace]) && (Count[
                IsoSymmetricCross[aa] /. Power -> NMPower, UGenerator,
                Infinity, Heads -> True] == 1) &&
          FreeQ[IsoSymmetricCross[aa],
            UMatrix[a___ /; FreeQ[{a}, UGenerator]], Infinity] &&
          FreeQ[IsoSymmetricCross[aa], $UMatrices, Infinity]) := 0;



(* The final trace function (UTrace of matrices without arguments should not be
evaluated): *)

UTrace[a_,
        opts___Rule] /; (! HoldUTrace /. Flatten[{opts}] /. Options[UTrace]) :=
     UTrace2[a, opts] /. UTrace2 -> UTrace1;



(* Invoking the trace function SUNTrace of FeynCalc: *)

UTraceToFCTrace[a_,
        opts___Rule] /; (gaugedimcheck[UTraceToFCTrace, opts, a] ==
            3 && (fcsunn /. Flatten[{opts}] /. Options[UTraceToFCTrace]) ==
             3) := (trtemp = (a /. {UTrace1 -> utemp,
                UTrace -> utemp}) /. {fcqf[b___fcpd, field_,
                lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                ii__HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] ->
              fieldtemp[pdrs[b], qf[field], pdrs[lis]][ii]};
      trtemp1 = trtemp /. utemp[aa_, b___] -> utemp[aa, fcexpt -> True];
      trtemp2 = trtemp1 /. utemp -> fctr;
      trtemp2 /. {fieldtemp[pdrs[b___], qf[field_], pdrs[lis___]][ii_] ->
              fcqf[b, field, lis, ii]} /. fcsunn -> 3);
UTraceToFCTrace[a_,
        opts___Rule] /; (gaugedimcheck[UTraceToFCTrace, opts, a] ==
            3 && (fcsunn /. Flatten[{opts}] /. Options[UTraceToFCTrace]) ==
             3) := (trtemp = (a /. {UTrace1 -> utemp,
                UTrace -> utemp}) /. {fcqf[b___fcpd, field_,
                lis___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
                ii__HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] ->
              fieldtemp[pdrs[b], qf[field], pdrs[lis]][ii]};
      trtemp1 = trtemp /. utemp[aa_, b___] -> utemp[aa, fcexpt -> True];
      trtemp2 = trtemp1 /. utemp -> fctr;
      trtemp2 /. {fieldtemp[pdrs[b___], qf[field_], pdrs[lis___]][ii_] ->
              fcqf[b, field, lis, ii]} /. fcsunn -> 2);



(* Cyclicity of the trace: *)

CycleUTraces[expr_, sf___] :=
    Block[{a, tmplist, sortlist, smallest},
      expr /. UTrace1[a : (Dot | NM)[__]] :> (tmplist = List @@ a;
            sortlist = Sort[tmplist, sf];
            smallest = sortlist[[1]];

            Do[If[Count[sortlist, sortlist[[i]]] === 1,
                smallest = sortlist[[i]]; Break[]], {i, 1,
                Length[sortlist]}];

            While[! (tmplist[[1]] === smallest && tmplist[[-1]] =!= smallest ||
                     tmplist[[1]] === smallest && Length[tmplist] <= 2 ||
                    Length[Union[tmplist]] === 1),
              tmplist = RotateLeft[tmplist]];
            UTrace1[a[[0]] @@ tmplist])];


(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* SU(2) and SU(3) structure constants *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* The Kronecker delta function: *)

SetAttributes[SU2Delta, Orderless];
uindxx = (UIndex | HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex);
SU2Delta[a___, uindxx[b_Integer], c___] := SU2Delta[a, b, c];
SU2Delta[i_Integer, i_Integer] := 1;
SU2Delta[i_Integer, j_Integer] := 0;
SU2Delta[i_Symbol, i_Symbol] /; MemberQ[$ConstantIsoIndices, i] := 1;
SU2Delta[fcsuni[i_Symbol], fcsuni[i_Symbol]] /;
    MemberQ[$ConstantIsoIndices, i] := 1;



(* The totally antisymmetric structure constants of SU(2) (J. F. Donoghue, E.
Golowich and B. R. Holstein, Dynamics of the Standard Model): *)



(* We may as well take these directly from the matrices so it is easier to
change basis (changed 31/1-2000). *)



(* This definition is covered by the one below, but I hope its faster... *)

SU2F[a : (uindxx[_Integer]) ..] := SU2F[Sequence @@ (((#[[1]]) &) /@ {a})];
SU2F[a___, uindxx[b_Integer], c___] := SU2F[a, b, c];
SU2F[a__] /; (OrderedQ[{a}] != True && $StandardSUNBasis) :=
    Signature[{a}]*SU2F @@ Sort[{a}];
SU2F[a_, b_, c_] /; (! OrderedQ[{a, b}]) :=
    Signature[{a, b}]*SU2F[##, c] & @@ Sort[{a, b}];
SU2F[a__Integer] /; $StandardSUNBasis := 0;
SU2F[a_, a_, b_] /; $StandardSUNBasis := 0;
SU2F[a_, b_, b_] /; $StandardSUNBasis := 0;
SU2F[a_, b_, a_] /; $StandardSUNBasis := 0;



(* The Kronecker delta function: *)

SetAttributes[SU3Delta, Orderless];
SU3Delta[a___, uindxx[b_Integer], c___] := SU3Delta[a, b, c];
SU3Delta[i_Integer, i_Integer] := 1;
SU3Delta[i_Integer, j_Integer] := 0;
SU3Delta[i_Symbol, i_Symbol
] /; MemberQ[$ConstantIsoIndices, i] := 1;
SU3Delta[uindxx[i_Symbol], uindxx[i_Symbol]] /;
    MemberQ[$ConstantIsoIndices, i] := 1



(* The totally antisymmetric structure constants and symmetric coefficients of
SU(3) (J. F. Donoghue, E. Golowich and B. R. Holstein, Dynamics of the
Standard Model, (II.2.10)): *)



(* This definition is covered by the one below, but I hope its faster... *)

SU3F[a : (uindxx[_Integer]) ..] := SU3F[Sequence @@ (((#[[1]]) &) /@ {a})];
SU3F[a___, uindxx[b_Integer], c___] := SU3F[a, b, c];



(* The totally antisymmetric structure constants of SU(3) (J. F. Donoghue, E.
Golowich and B. R. Holstein, Dynamics of the Standard Model): *)

pairs[a_] :=
    Union[Sort /@ Flatten[Outer[List, a, a], 1]] /. {b_, b_} -> Sequence[];
pairsd[a_] := Union[Sort /@ Flatten[Outer[List, a, a], 1]];
SU3F[a__] /; (! OrderedQ[{a}] && $StandardSUNBasis) :=
    Signature[{a}]*SU3F @@ Sort[{a}];
SU3F[a_, b_, c_] /; (! OrderedQ[{a, b}]) :=
    Signature[{a, b}]*SU3F[##, c] & @@ Sort[{a, b}];
(*SU3F[a__Integer] := 0;*)
SU3F[a_Integer, b_Integer,
        c_] /; (MemberQ[fzeropairlist, Sort[{a, b}]] && $StandardSUNBasis) :=
    0;
SU3F[a_, a_, b_] /; $StandardSUNBasis := 0;
SU3F[a_, b_, b_] /; $StandardSUNBasis := 0;
SU3F[a_, b_, a_] /; $StandardSUNBasis := 0;
pairsfunc[a_] :=
    Rule[HoldPattern[SU3F[##]] & @@
            Join[#, {i_}], (SU3F[Sequence @@ #, Complement[a, #][[1]]])*
            SU3Delta[i, Complement[a, #][[1]]]] & /@ pairs[a];



(* This definition is covered by the one below, but I hope its faster... *)

SU3D[a : (uindxx[_Integer]) ..] := SU3D[Sequence @@ (((#[[1]]) &) /@ {a})];
SU3D[a___, uindxx[b_Integer], c___] := SU3D[a, b, c];
SetAttributes[SU3D, Orderless];



(* The totally symmetric coefficients of SU(3) (J. F. Donoghue, E. Golowich and
B. R. Holstein, Dynamics of the Standard Model): *)



(* We may as well take these directly from the matrices so it is easier to
change basis (changed 31/1-2000). *)

SU3D[a_Integer, b_Integer,
        c_] /; ($StandardSUNBasis && MemberQ[dzeropairlist, Sort[{a, b}]]) :=
    0;
(*Added 7/1 - 2000*)(*Added check for $ConstantIsoIndices, 24/1 - 2000*)
  SU3D[___,
        a : fcsuni[_?(((! IntegerQ[#]) &&
                      FreeQ[$ConstantIsoIndices, #]) &)], ___,
        a_, ___] /; $StandardSUNBasis := 0;



(* This function drops only as many elements from a as b has - starting from the
left: *)

ComplementAll[a_List, b_List] := (listf[0] = a;
      Do[listf[l] = listf[l - 1] /. {i___, b[[l]], k___} -> {i, k}, {l,
          Length[b]}]; listf[Length[b]]);
pairsall[a_] :=
    Union[Join[Take[#, 2] & /@ Permutations[a]]];(*Changed 16/12/1999*)
pairsfuncd[a_] :=
    Union[Rule[
            HoldPattern[SU3D[##]] & @@ Join[#, {i_}], (SU3D @@ a)*
              SU3Delta[i, ComplementAll[a, #][[1]]]] & /@ pairsall[a]];



(* This is to allow changing basis matrices spanning SU(N) and have the change
propagate to the structure constants. Works only when staying in the usual
dimensional representations (2 for SU(2) and 3 for SU(3)). *)



(* fnlist: List of ordered triplets yielding non-zero f. fnonzeropairlist:list
of all pairs of elements which will give f!=0 regardless of the third
argument.. fzeropairlist:list of all pairs of elements which will give f=0
regardless of the third argument. *)

FixSUN := (

      If[HighEnergyPhysics`Phi`$Phi && (Length[$SUNBasis[2, 1/2]] =!= 3 ||
              Union[MatrixQ /@ $SUNBasis[2, 1/2]] =!= {True} ||
              Union[Flatten[Dimensions /@ $SUNBasis[2, 1/2]]] =!= {2}),
        Message[FixSUN::badmatr2]; Return[];];

      If[HighEnergyPhysics`Phi`$Phi && (Length[$SUNBasis[3, 1]] =!= 8 ||
              Union[MatrixQ /@ $SUNBasis[3, 1]] =!= {True} ||
              Union[Flatten[Dimensions /@ $SUNBasis[3, 1]]] =!= {3}),
        Message[FixSUN::badmatr2]; Return[];];

      VerbosePrint[2, "$StandardSUNBasis is ", $StandardSUNBasis];
      VerbosePrint[2, "Setting new values of SU2F"];

      tt2 =
        Table[WriteOutUMatrices[
            UGeneratorMatrix[fcsuni[i], fcsunn -> 2]], {i, 3}];

(*List of (ordered) triplets :*)

      trip2f = If[$StandardSUNBasis,
          Flatten[Table[
                Table[Table[hh[k, j, i], {i, j + 1, 3}], {j, k + 1, 3}], {k,
                  3}]] /. hh -> List,
          Flatten[Table[
                Table[Table[hh[k, j, i], {i, 1, 3}], {j, 1, 3}], {k, 1,
                  3}]] /. hh -> List];

      Do[SU2F[trip2f[[i, 1]], trip2f[[i, 2]], trip2f[[i, 3]]] = -I/4*
            UTrace[Adjoint[
                  tt2[[trip2f[[i,
                          3]]]]].(tt2[[trip2f[[i, 1]]]].tt2[[trip2f[[i,
                              2]]]] -
                    tt2[[trip2f[[i, 2]]]].tt2[[trip2f[[i, 1]]]])], {i,
          Length[trip2f]}];

      VerbosePrint[2, "Setting new values of SU3F and SU3D"];

      ClearAttributes[SU3D, Orderless];
      If[$StandardSUNBasis, SetAttributes[SU3D, Orderless]];

      fnlist = {}; dnlist = {};

      (*List of (ordered) triplets :*)

      tripf = If[$StandardSUNBasis,
          Flatten[Table[
                Table[Table[hh[k, j, i], {i, j + 1, 8}], {j, k + 1, 8}], {k,
                  8}]] /. hh -> List,
          Flatten[Table[
                Table[Table[hh[k, j, i], {i, 1, 8}], {j, 1, 8}], {k, 8}]] /.
            hh -> List];

      tripd =
        If[$StandardSUNBasis,
          Flatten[Table[
                Table[Table[hh[k, j, i], {i, j, 8}], {j, k, 8}], {k, 8}]] /.
            hh -> List,
          Flatten[Table[
                Table[Table[hh[k, j, i], {i, 1, 8}], {j, 1, 8}], {k, 8}]] /.
            hh -> List];

      tt3 =
        Table[WriteOutUMatrices[
            UGeneratorMatrix[fcsuni[i], fcsunn -> 3]], {i, 8}];

      Do[If[(SU3F[tripf[[i, 1]], tripf[[i, 2]], tripf[[i, 3]]] =
                Evaluate[-I/4*
                    UTrace[Adjoint[
                          tt3[[tripf[[i,
                                  3]]]]].(tt3[[tripf[[i, 1]]]].tt3[[tripf[[i,
                                      2]]]] -
                            tt3[[tripf[[i, 2]]]].tt3[[tripf[[i, 1]]]])]]) =!=
            0, fnlist =
            Append[fnlist, {tripf[[i, 1]], tripf[[i, 2]],
                tripf[[i, 3]]}]], {i, Length[tripf]}];

      Do[If[(SU3D[tripd[[i, 1]], tripd[[i, 2]], tripd[[i, 3]]] =
                Evaluate[
                  1/4*UTrace[
                      Adjoint[
                          tt3[[tripd[[i,
                                  3]]]]].(tt3[[tripd[[i, 1]]]].tt3[[tripd[[i,
                                      2]]]] +
                            tt3[[tripd[[i, 2]]]].tt3[[tripd[[i, 1]]]])]]) =!=
            0, dnlist =
            Append[dnlist, {tripd[[i, 1]], tripd[[i, 2]],
                tripd[[i, 3]]}]], {i, Length[tripd]}];

      VerbosePrint[2,
        "Building table of reduction rules for SU(2) and SU(3)"];

      fnonzeropairlist =
        Union[FlattenAt[pairs /@ fnlist, Table[{i}, {i, Length[fnlist]}]]];

      fzeropairlist =
        Complement[
          Union[Sort /@
              Flatten[Outer[
                  List, {1, 2, 3, 4, 5, 6, 7, 8}, {1, 2, 3, 4, 5, 6, 7, 8}],
                1]], fnonzeropairlist];

      dnonzeropairlist =
        Union[FlattenAt[pairsd /@ dnlist, Table[{i}, {i, Length[dnlist]}]]];

dzeropairlist =
        Complement[
          Union[Sort /@
              Flatten[Outer[
                  List, {1, 2, 3, 4, 5, 6, 7, 8}, {1, 2, 3, 4, 5, 6, 7, 8}],
                1]], dnonzeropairlist];

      (*When two indices are integers,
        only some values of the third will give a non -
          zero result.$SU3FReduceList is the corresponding list of rules,
        substituting SU3F with SU3Delta*)

      $SU3FReduceList =
        If[$StandardSUNBasis,
          Flatten[Sort[
                Union[FlattenAt[Evaluate[pairsfunc /@ fnlist],
                    Table[{i}, {i, Length[fnlist]}]]]] //. {a___, b_ -> c_,
                  b_ -> d_, e___} -> {a, b -> c + d, e}], {}];

      $SU3DReduceList =
        If[$StandardSUNBasis,
          Flatten[Sort[
                Union[FlattenAt[Evaluate[pairsfuncd /@ dnlist],
                    Table[{i}, {i, Length[dnlist]}]]]] //. {a___, b_ -> c_,
                  b_ -> d_, e___} -> {a, b -> c + d, e}], {}];

      If[HighEnergyPhysics`Phi`$Phi, $SUNRules =
          Join[$SUNDeltaRules,
            If[$StandardSUNBasis, $SUNDFRules, {}], $SU3FReduceList,
	    $SU3DReduceList]];

      VerbosePrint[3, "New reduction tables read:\n", $SUNRules];

      );
FixSUN;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Supplying iso-indices *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Support functions for IsoIndicesSupply.  Index numerators, increasing by one
each time applied: *)

id[x_, (opts___Rule | opts___List)] := x;
$IsoIndicesCounter = 0;
iin[opts___] := (++$IsoIndicesCounter;
      ToExpression[(IsoIndicesString /. Flatten[{opts}] /.
              Options[IsoIndicesSupply]) <> ToString[$IsoIndicesCounter]]);
iinintern := (++iicintern;
      ToExpression["internisoindstr" <> ToString[iicintern]]);
iinfree[opts___] := (++$IsoIndicesCounter;
      ToExpression[(FreeIsoIndicesString /. Flatten[{opts}] /.
              Options[IsoIndicesSupply]) <> ToString[$IsoIndicesCounter]]);



(* Every object obj ocurring as an argument to IsoDot, IsoCross or
IsoSymmetricCross is substituted with obj[i], where i is an isospin index
generated anew each time.  Here follow how the different objects interpret
this additional isospin dependence *)

IsoVector[a_[ii : uindxx[_], jj : uindxx[_], op___Rule | op___List], opts___][
      i__HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] := (a[i, ##] & @@
          OptionsSelect[a, opts, op])[ii, jj];
IsoVector[a_[b__, (op___Rule | op___List)], opts___][x_][
      i__HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] := (a[b, ##] & @@
            OptionsSelect[a, op, opts])[i][x];
IsoVector[a_[b__, (op___Rule | op___List)], opts___][
      i__HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] := (a[b, ##] & @@
          OptionsSelect[a, op, opts])[i];



(* IsoVectors - fixing special cases: *)

fcqf[ders___HighEnergyPhysics`FeynCalc`PartialD`PartialD, a__,
        lors___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex][
      isosp_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] :=
    fcqf[ders, a, lors, isosp, iis];
(*Changed 7/2 - 2000*)
  UMatrix[UGenerator[op___], opts___][
      i_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] :=
    UMatrix[UGenerator[i, op], opts];
(*Added 17/6 - 2001*)
Unprotect[Conjugate];
  Conjugate[UMatrix[UGenerator[op___], opts___]][
      i_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] :=
    Conjugate[UMatrix[UGenerator[i, op], opts]];
Protect[Conjugate];
(*Redundant - gives double option rules*)(*UGeneratorMatrixIsoVector[opts___][
        i_HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex] := (UGenerator[
              i, ##] & @@ OptionsSelect[UGenerator, opts]);*)



(* Support function for IsoIndicesSupply.  supptemp is a temporary item wrapping
the arguments of IsoCross and IsoSymmetricCross.  wrap is a temporary wrapper
for the temporarily free index: *)

supptemp[a_][sunitemp[wrap[in_]]][in1_] :=
    supptemp[a] /. sunitemp[wrap[in]] -> sunitemp[in1];



(* Step two in the supplial of the extra isospin dependence: *)



(* Added Conjugate because of the new iso-products. 2/2-2000. *)

indsuppdot[a_, b_, i_] := NM[Conjugate[a[sunitemp[i]]], b[sunitemp[i]]];
indsuppcross[a_, b_, i1_, i2_, i3_] :=
    supptemp[NM[Conjugate[a[sunitemp[i2]]], b[sunitemp[i3]]]*
          fcsunf @@ sunitemp /@ {i1, i2, i3}][sunitemp[i1]];
indsuppsymmcross[a_, b_, i1_, i2_, i3_] :=
    supptemp[NM[Conjugate[a[sunitemp[i2]]], b[sunitemp[i3]]]*
          fcsund @@ sunitemp /@ {i1, i2, i3}][sunitemp[i1]];



(* Step one in the supplial of the extra isospin dependence: *)

indicesdotrule[
      optss___] := (IsoDot[a_, b_] /;
          FreeQ[{a, b}, (_IsoDot | _IsoCross | _IsoSymmetricCross)] :>
        indsuppdot[a, b, iin[optss]]);
indicescrossrule[
      optss___] := (IsoCross[a_, b_] /;
          FreeQ[{a, b}, (_IsoDot | _IsoCross | _IsoSymmetricCross)] :>
        indsuppcross[a, b, wrap[iinintern], iin[optss], iin[optss]]);
indicessymmcrossrule[
      optss___] := (isctemp[a_, b_] /;
          FreeQ[{a, b}, (_IsoDot | _IsoCross | _isctemp)] :>
        indsuppsymmcross[a, b, wrap[iinintern], iin[optss], iin[optss]]);



(* Catching free indices: *)

freeindicesrules[
      opts___] := (fi =
        ToExpression[
          FreeIsoIndexString /. Flatten[{opts}] /.
            Options[IsoIndicesSupply]]; {f_[fcsuni[wrap[_]]] -> f,
        IsoVector[a__][x_] :> IsoVector[a][x][fcsuni[fi]],
        IsoVector[a__] :> IsoVector[a][fcsuni[fi]]});
freeindicesrules1[opts___] := {f_[fcsuni[wrap[_]]] -> f,
      IsoVector[a__][x_] :> IsoVector[a][x][fcsuni[iinfree[opts]]],
      IsoVector[a__] :> IsoVector[a][fcsuni[iinfree[opts]]]};



(* The function that supplies indices to expressions involving IsoDots,
IsoCrosses and IsoSymmetricCrosses of iso-spin vectors: *)


(*Added 26/9-2000. Should make life a lot easier for IndicesCleanup*)
IsoIndicesSupply[x_Plus] := Block[{tmpic=$IsoIndicesCounter},
                    ($IsoIndicesCounter=tmpic;IsoIndicesSupply[#])& /@ x];


IsoIndicesSupply[
      aa_, (optss___Rule | optss___List)] := (VerbosePrint[2,
        "Starting with number ", $IsoIndicesCounter]; iicintern = 0;
      aa //.(*Changed 27/2 - 2000*)(*IsoDot[a_, b_]^n_ :>
                        times1 @@
                          Table[IsoDot[a, b], {rep,
                              n}]*)(c_?(!
                                  FreeQ[{#}, IsoDot[_, _], Infinity] &))^
                        n_ :> (VerbosePrint[2, "Fixing powers"];
                        times1 @@ Table[c, {n}]) /.
                  IsoSymmetricCross -> isctemp //. (VerbosePrint[2,
                    "Recursively resolving iso-vector products"];
          {indicesdotrule[optss], indicescrossrule[optss],
                    indicessymmcrossrule[optss]}) /. {sunitemp -> fcsuni,
                supptemp -> id, isctemp -> IsoSymmetricCross} /.
            If[NumerateFree /. Flatten[{optss}] /. Options[IsoIndicesSupply],
              VerbosePrint[2, "Non-contracted indices will be numerated"];
              freeindicesrules1[optss],
              VerbosePrint[2, "Non-contracted indices will not be numerated"];
               freeindicesrules[optss]] /.
          wrap[___] ->
            ToExpression[
              FreeIsoIndexString /. Flatten[{optss}] /.
                Options[IsoIndicesSupply]] /. times1 -> Times);



(* Support functions for UIndicesSupply: *)

UIndicesCounter = 0;
nnn[opts___] :=
    uix[(++UIndicesCounter;
        ToExpression[(UIndicesString /. Flatten[{opts}] /.
                Options[UIndicesSupply]) <> ToString[UIndicesCounter]])];
nnm[opts___] :=
    uix[(ToExpression[(UIndicesString /. Flatten[{opts}] /.
                Options[UIndicesSupply]) <> ToString[UIndicesCounter + 1]])];



(* UIndicesSupply: *)

UIndicesSupply[a_, opts___] :=
    UIndicesSupply1[a,
              opts] /. {UMatrix[m_[ind_, op___], i_uix, j_uix, opt___] :>
                m[ind, Sequence @@ OptionsSelect[m, opts, op, opt]][i, j],
              UMatrix[m_, i_uix, j_uix, opt___] :> m[i, j, opt]} /.
          If[(UIndexToSUNIndex /. Flatten[{opts}] /. Options[UIndicesSupply]),
             uix -> fcsuni, {}] /. fcdot -> NM /. nnmm -> NM;



(* Linearity: *)

UIndicesSupply[a_ + b_, opts___] :=
    UIndicesSupply[a, opts] + UIndicesSupply[b, opts];
UIndicesSupply[a_*b_, opts___] /; FreeQ[a, UMatrix | UVector] :=
    a*UIndicesSupply[b, opts];
UIndicesSupply[a_ /; FreeQ[a, UMatrix | UVector], ___] := a;
UIndicesSupply1[a_ + b_, opts___] :=
    UIndicesSupply[a, opts] + UIndicesSupply[b, opts];
UIndicesSupply1[a_*b_, opts___] /; FreeQ[a, UMatrix | UVector] :=
    a*UIndicesSupply[b, opts];
UIndicesSupply1[a_ /; FreeQ[a, UMatrix | UVector], ___] := a;



(* Supplying matrix indices: *)



(* Unnested NMs: *)

UIndicesSupply1[aa_NM, optss1___] /;
      FreeQ[List @@ aa, NM | HighEnergyPhysics`FeynCalc`DOT`DOT] := (ui1 =
        nnmm @@ Table[(If[! FreeQ[aa[[rep]], UMatrix],
                  indexpair = Sequence[nnn[optss1], nnm[optss1]]];
                ReplacePart[aa,
                    aa[[rep]] /.
                      UMatrix[a_, opts___] :> UMatrix[a, indexpair, opts],
                    rep][[rep]]), {rep, Length[aa]}] /. nnmm -> NM;
      UIndicesCounter++; ui1);



(* Nested NMs are NMExpanded: *)

UIndicesSupply1[a_NM,
        optss1___] /; (!
          FreeQ[List @@ a, NM | HighEnergyPhysics`FeynCalc`DOT`DOT]) :=
    UIndicesSupply1[NMExpand[a], optss1];



(* A single UMatrix or UVector: *)

UIndicesSupply1[aa_,
        optss1___] /; (FreeQ[aa,
            NM | HighEnergyPhysics`FeynCalc`DOT`DOT] && !
            FreeQ[aa, UVector | UMatrix]) := (indexpair =
        Sequence[nnn[optss1], nnm[optss1]]; UIndicesCounter++;
      aa /. {UMatrix[a_, opts___] :> UMatrix[a, indexpair, opts],
          UVector[a_, opts___] :> UVector[a, indexpair[[1]], opts]});
UIndicesSupply1[] := Sequence[];
UIndicesSupply[] := Sequence[];



(* When supplying indices to a dot product, the enclosed NM product is first
supplied with indices, then the enclosing vectors are supplied with indices: *)

UIndicesSupply1[(HighEnergyPhysics`FeynCalc`DOT`DOT | Dot)[aa1_, aa2___,
        aa3_], optss1___] :=
    tempdot[aa1, UIndicesSupply1[NM[aa2]],
              aa3] //. {(*vbar.m.v*)
                tempdot[a___, b_, c__, d_,
                    e___] /; (FreeQ[{c}, UVector] && !
                        FreeQ[{b}, UVector] && ! FreeQ[{d}, UVector] && !
                        FreeQ[{c}, UMatrix]) :>
                tempdot[a,
                  b /. UVector[p_] ->
                      p[Flatten[
                            Cases[{c}, _uix, Infinity, Heads -> True]][[1]]],
                  c, d /.
                    UVector[p1_] ->
                      p1[Flatten[
                            Cases[{c}, _uix, Infinity, Heads -> True]][[-1]]],
                   e],(*vbar.v*)
                tempdot[a___, b_, c___, d_,
                    e___] /; (FreeQ[{c}, UVector] && !
                        FreeQ[{b}, UVector] && ! FreeQ[{d}, UVector] &&
                      FreeQ[{c}, UMatrix]) :> (index = nnn[optss1];
                  tempdot[a, b /. UVector[p2_] -> p2[index], c,
                    d /. UVector[p3_] -> p3[index], e])} //. {(*m.v*)
              tempdot[a___, c__, d_,
                  e___] /; (FreeQ[{c}, UVector] && ! FreeQ[{d}, UVector] && !
                      FreeQ[{c}, UMatrix]) :>
              tempdot[a, c,
                d /. UVector[p1_] ->
                    p1[Flatten[
                          Cases[{c}, _uix, Infinity, Heads -> True]][[-1]]],
                e],(*v.m*)
              tempdot[a___, d_, c__,
                  e___] /; (FreeQ[{c}, UVector] && ! FreeQ[{d}, UVector] && !
                      FreeQ[{c}, UMatrix]) :>
              tempdot[a, c,
                d /. UVector[p1_] ->
                    p1[Flatten[
                          Cases[{c}, _uix, Infinity, Heads -> True]][[1]]],
                e]} /. nnmm -> NM /. tempdot -> fcdot;

                
UIndicesSupply1[UTrace1[aa_], opts___] :=
    UIndicesSupply1[
            aa /.(*tracing single matrices*){UTrace[
                    UMatrix[a_, ss_uix, sss_uix, op___]] ->
                  UMatrix[a, ss, ss, op],
                UTrace1[UMatrix[a_, ss_uix, sss_uix, op___]] ->
                  UMatrix[a, ss, ss,
                    op]}] /.(*replacing the last index with the first*)NM ->
            nmtemp /. {nmtemp[a___, b_, c___, d_,
                e___] /; ((! FreeQ[{b, d}, uix]) && (FreeQ[{a, e},
                      uix])) :> (uinds =
                Cases[{a, b, c, d, e}, _uix, Infinity, Heads -> True];
              nmtemp[a, b, c, d /. uinds[[-1]] -> uinds[[1]], e])} /.
      nmtemp -> NM;
fcqf[ders___HighEnergyPhysics`FeynCalc`PartialD`PartialD, a__, uis_uix,
        lors___HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        iis___HighEnergyPhysics`FeynCalc`SUNIndex`SUNIndex][ui_uix] :=
    fcqf[ders, a, ui, uis, lors, isosp, iis];




(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Transformation to FC notation *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



iinint[opts___] := (++$IsoIndicesCounter;
      ToExpression[(FreeIsoIndicesString /. Flatten[{opts}] /.
              Options[PhiToFC]) <> ToString[$IsoIndicesCounter]]);
intindicesrules[
      opts___] := (fi =
        ToExpression[
          FreeIsoIndexString /. Flatten[{opts}] /.
            Options[IsoIndicesSupply]]; {fcsundel[a___, b_Integer, c___] :>
          Projection[b][a, c],
        fcsund[a___, b_Integer, c___] :>
          fcsund[a, fcsuni[fi], c]*Projection[b][fcsuni[fi]],
        fcsunf[a___, b_Integer, c___] :>
          fcsunf[a, fcsuni[fi], c]*Projection[b][fcsuni[fi]]});
intindicesrules1[
      opts___] := {fcsundel[a___, b_Integer, c___] :> Projection[b][a, c],
      fcsund[a___, b_Integer, c___] :> (fi = fcsuni[iinint[opts]];
          fcsund[a, fi, c]*Projection[b][fi]),
      fcsunf[a___, b_Integer, c___] :> (fi = fcsuni[iinint[opts]];
          fcsunf[a, fi, c]*Projection[b][fi])};
intindicesruleslast[
      opts___] := (fi =
        ToExpression[
          FreeIsoIndexString /. Flatten[{opts}] /. Options[IsoIndicesSupply]];
       f_ /; (Count[f, fcsuni[_Integer], Infinity] == 1 &&
              FreeQ[{f},
                NMPower | Power | NM | Times | Dot | fcdot | IsoDot |
                  IsoCross | IsoSymmetricCross]) :> (ni =
            Cases[f, fcsuni[_Integer], Infinity][[1]]; (f /.
                ni -> fcsuni[fi])*Projection[ni[[1]]][fcsuni[fi]]));
intindicesruleslast1[opts___] :=
    f_ /; (Count[f, fcsuni[_Integer], Infinity] == 1 &&
            FreeQ[{f},
              NMPower | Power | NM | Times | Dot | fcdot | IsoDot | IsoCross |
                 IsoSymmetricCross]) :> (fi = fcsuni[iinint[opts]];
        ni = Cases[f, fcsuni[_Integer], Infinity][[1]]; (f /. ni -> fi)*
          Projection[ni[[1]]][fi]);

PhiToFC[aa_, (opts___Rule | opts___List)] :=
    aa /. {(*UGenerator[fcsuni[i_Symbol /; FreeQ[$ConstantIsoIndices, i]],
                      opts___Rule | opts___List] -> 2*fcsunt[fcsuni[i]],*)NM ->
                   fcdot, fcqf[pp__][_] :> fcqf[pp], uix -> fcsuni,
                SU2Delta -> fcsundel, SU3Delta -> fcsundel, SU2F -> fcsunf,
                SU3F -> fcsunf, SU3D -> fcsund} /. (Power | NMPower)[
                p_?(! FreeQ[#, fcsuni, Infinity, Heads -> True] &), n_] :>
              Times @@ MapIndexed[pp, Table[p, {n}]] /.
          If[(RemoveIntegerIndices /. Flatten[{opts}] /. Options[PhiToFC]),
            If[(NumerateFree /. Flatten[{opts}] /. Options[PhiToFC]),
              intindicesrules1[opts], intindicesrules[opts]], {}] /.
        If[(RemoveIntegerIndices /. Flatten[{opts}] /. Options[PhiToFC]),
          If[(NumerateFree /. Flatten[{opts}] /. Options[PhiToFC]),
            intindicesruleslast1[opts], intindicesruleslast[opts]], {}] /.
      pp[p_, _] -> p;



(* The (n-dimensional) isospin indices from 1 to IsoIndicesNumber, used by
IsoIndicesSupply: *)

IsoIndicesList[(opts___Rule | opts___List)] := (Table[
        ToExpression[(IsoIndicesString /. Flatten[{opts}] /.
                Options[IsoIndicesList]) <> ToString[i]], {i, 1,
          IsoIndicesNumber /. Flatten[{opts}] /. Options[IsoIndicesList]}]);



(* The momenta of the incoming/outgoing particles: *)

MomentumVariables[(opts___Rule | opts___List)] :=
    Table[ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
              Options[MomentumVariables]) <> ToString[i]], {i,
        ParticlesNumber /. Flatten[{opts}] /. Options[MomentumVariables]}];



(* The fields to be declared to FeynCalc (all are incoming): *)

FieldsSet[body___fcqf, (opts___Rule | opts___List)] /;
      VectorQ[ParticlesNumber /. Flatten[{opts}] /.
          Options[FieldsSet]] := (pnr[
          x_] := (ParticlesNumber /. Flatten[{opts}] /.
              Options[FieldsSet])[[x]];
      Table[(fcqf[##] & @@
                Join[List @@ body,
                  If[(LorentzIndicesString /. Flatten[{opts}] /.
                          Options[FieldsSet]) ===
                      None, {seq[]}, {fcli[
                        ToExpression[(LorentzIndicesString /.
                                  Flatten[{opts}] /. Options[FieldsSet]) <>
                            ToString[pnr[j]]]]}],
                  If[(IsoIndicesString /. Flatten[{opts}] /.
                          Options[FieldsSet]) ===
                      None, {seq[]}, {fcsuni[
                        ToExpression[(IsoIndicesString /. Flatten[{opts}] /.
                                Options[FieldsSet]) <> ToString[pnr[j]]]]}]])[
            ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                    Options[FieldsSet]) <> ToString[pnr[j]]]], {j,
            Length[ParticlesNumber /. Flatten[{opts}] /.
                Options[FieldsSet]]}] /. seq -> Sequence);
FieldsSet[body___fcqf, (opts___Rule | opts___List)] /;
      IntegerQ[ParticlesNumber /. Flatten[{opts}] /. Options[FieldsSet]] :=
    Table[(fcqf1[##] & @@
                Join[List @@
                    body, {If[(LorentzIndicesString /. Flatten[{opts}] /.
                            Options[FieldsSet]) ===
                        None, {seq[]}, {fcli[
                          ToExpression[(LorentzIndicesString /.
                                    Flatten[{opts}] /. Options[FieldsSet]) <>
                              ToString[j]]]}],
                    If[(IsoIndicesString /. Flatten[{opts}] /.
                            Options[FieldsSet]) ===
                        None, {seq[]}, {fcsuni[
                          ToExpression[(IsoIndicesString /.
                                    Flatten[{opts}] /. Options[FieldsSet]) <>
                              ToString[j]]]}]}])[
            ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                    Options[FieldsSet]) <> ToString[j]]], {j,
            ParticlesNumber /. Flatten[{opts}] /. Options[FieldsSet]}] /.
        seq -> Sequence /. fcqf1 -> fcqf;



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Discarding terms *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)



(* Terms of order different from ExpansionOrder are dropped.  For this is used
that terms of order ExpansionOrder in the fields have a factor
tempfac[]^ExpansionOrder.  Blank may be used only as e.g. Retain->{_->2},
that is, alone: *)

DiscardTerms[l_, opts___Rule] :=
    Block[{i, mme, res},(*Do[
            ClearAttributes[Evaluate[upar[i]], NumericFunction];, {i, 5}];*)
	(*Added 8/3-2000 to have DiscardTerms work when DOT products are involved*)
        SetAttributes[#, {NumericFunction, NHoldAll}]& /@ $ParticleTypes;
        mme = (Method /. Flatten[{opts}] /. Options[DiscardTerms]);
      res = Which[mme === Expand, VerbosePrint[1, "Using Method->Expand"];
          DiscardTerms1[l, opts], mme === Coefficient,
          VerbosePrint[1, "Using Method->Coefficient"];
          DiscardTerms2[l, opts], True,
          Message[DiscardTerms::nomethod, mme]];
	  ClearAttributes[#, {NumericFunction, NHoldAll}]& /@ $ParticleTypes;
	  (*Do[SetAttributes[Evaluate[upar[i]], NumericFunction];, {i, 5}];*)
        res];

untugrules={
     (a___.(tempfac[p___]*b_).c___) :> (tempfac[p]*(a.b.c)),
     HoldPattern[CovariantFieldDerivative[tempfac[p___]*a_, b__]] ->
        tempfac[p]*CovariantFieldDerivative[a, b],
     HoldPattern[CovariantNabla[tempfac[p___]*a_, b__]] ->
        tempfac[p]*CovariantNabla[a, b],
     HoldPattern[CovariantFieldDerivative[tempfac[p___]^n_*a_, b__]] ->
        tempfac[p]^n*CovariantFieldDerivative[a, b],
     HoldPattern[CovariantNabla[tempfac[p___]^n_*a_, b__]] ->
        tempfac[p]^n*CovariantNabla[a, b],
     IsoVector[ders___, tempfac[aa__]*pp_, cc___] :>
             tempfac[aa]*IsoVector[ders, pp, cc],
     UVector[ders___,
             tempfac[aa__]*pp_, cc___] :> tempfac[aa]*UVector[ders, pp, cc],
     (tempfac[aa__]*bb_[cc__])[x_] -> tempfac[aa]*bb[cc][x]};


DiscardTerms1[l_, opts___Rule] :=
    Block[{nodrop, tempfac, tempfacts, retord, rf, ro, ddt1, arg, p, x, ders,
        ar, br, qs, ttf, rest, pp, cc, tt, bb, aa, ddt2, ddt3, ddt, tr},
      nodrop = Alternatives @@ (NoDrop /. Flatten[{opts}] /.
              Options[DiscardTerms]);
      Clear[tempfac, tempfacts, retord, rf,ro];
      SetAttributes[
            tempfac, {NumericFunction, NHoldAll}];
      $UScalars = Union[$UScalars, {tempfac, ppf}];
      Adjoint[tempfac[a___]] := tempfac[a];
      Conjugate[tempfac[a___]] ^:= tempfac[a];
      Transpose[tempfac[a___]] ^:= tempfac[a]; tempfacts = 1;
      retord = (Retain /. Flatten[{opts}] /. Options[DiscardTerms]);
      Do[rf = retord[[rep, 1]]; ro = retord[[rep, 2]];
        tempfacts = tempfacts*tempfac @@ rf(*;
          tempfac /: (tempfac @@ rf)^(ro + 2) := 1*), {rep, Length[retord]}];
      VerbosePrint[3, "Putting on overall factor ", tempfacts^2];
      VerbosePrint[2,
        "Expanding NM series products and putting on dummy factors"];
      ddt1 = NMSeriesExpand[
          tempfacts^2*l /.
    fcqf[arg__][x_] -> argrec[fcqf[arg], x] /.
	  {fcqf[ders___fcpd, Particle[p_, ar___RenormalizationState,
           br___RenormalizationScheme, qs___ExpansionState], rest___] :>
           tempfac[p, ar, br, qs]* fcqf[ders, Particle[p, ar, br, qs], rest],
           fcqf[ders___fcpd, DiracBar[ Particle[p_, ar___RenormalizationState,
           br___RenormalizationScheme, qs___ExpansionState]],rest___] :>
           tempfac[p, ar, br, qs]*fcqf[ders, DiracBar[Particle[p, ar, br, qs]],rest]} /.
           argrec[tempfac[ttf__]*fcqf[arg__], x_] -> tempfac[ttf]*
           fcqf[arg][x] //. untugrules /.
            If[Length[retord] === 1 && rf[[1]] === _,
              tempfac[tt___] /; FreeQ[{tt}, nodrop] -> tempfac[ppf[]], {}]];
      VerbosePrint[2, "Expanding NM products"];
      ddt2 = NMExpand[ddt1];
      (*Commented out 4/3 -  2000.
      Don't know why it's here and its really slow on large expressions*)
      (*3/8-2000: Well, it's here because of the UVector stuff.
      See DiscardTerms.nb. Uncommented and changed fcdots to DotExpand*)
      VerbosePrint[2, "Expanding DOT products"];
      VerbosePrint[3, ddt2];
     ddt3 = DotExpand[ddt2];
     VerbosePrint[2, "Expanding"];
     VerbosePrint[3, ddt3];
     ddt =(*Change 20/12/1999;
          added qs*)(*20/3 - 2000 : Dropped Collect in favour of Expand*)
          Expand[ddt3 /. UTrace1 -> tr /. tr -> UTrace1] /. (VerbosePrint[2,
                  "Discarding terms"];
                Flatten[
                  Table[rf = retord[[rep, 1]];
                    ro = retord[[rep, 2]]; {(tempfac @@ rf)^(ro + 2) -> 1,
                      tempfac[ppf[]]^(ro + 2) -> 1}, {rep,
                      Length[retord]}]]) /.
            tempfac[tt___] /; FreeQ[{tt}, nodrop] -> 0 /.
          DropFactor[___] -> 1; $UScalars =
        Complement[$UScalars, {tempfac, ppf}];
      If[(CommutatorReduce /. Flatten[{opts}] /. Options[DiscardTerms]),
        ddt // (VerbosePrint[2,
                  "Applying CommutatorReduce"]; CommutatorReduce[#,opts])&, ddt]];



(* (1998) According to Buettiker there is a problem with having only one field.
This alternative form may solve it?? *)

lpat[i_Integer] := _?((# > i) &);

DiscardTerms2[l_, opts___Rule] :=
    Block[{nodrop, tempfac, tempfacts, retord, rf, ro, ddt1, arg, p, x, ders,
        ar, br, qs, ttf, rest, pp, cc, tt, bb, aa, ddtt, ddt, ddt0},
      Clear[tempfac, tempfacts, tempfactcoeff, retord, rf, ro];
      SetAttributes[tempfac, {NumericFunction, NHoldAll}];
      $UScalars = Union[$UScalars, {tempfac, ppf}];
      Adjoint[tempfac[a___]] := tempfac[a];
      Conjugate[tempfac[a___]] ^:= tempfac[a];
      Transpose[tempfac[a___]] ^:= tempfac[a]; tempfacts = 1;
      tempfactcoeff = 1;
      retord = (Retain /. Flatten[{opts}] /. Options[DiscardTerms]);
      Do[rf = retord[[rep, 1]]; ro := retord[[rep, 2]];
        tempfacts = tempfacts*tempfac @@ rf;
        tempfactcoeff = tempfactcoeff*(tempfac @@ rf)^ro;
        VerbosePrint[3, "Setting ", (tempfac @@ rf)^lpat[ro + 2], ":=0"];
        tempfac /: (tempfac @@ rf)^lpat[ro + 2] := 0, {rep, Length[retord]}];
      VerbosePrint[2, "Expanding NM series products and putting on dummy factors"];
      ddt0 = NMSeriesExpand[
          tempfacts^2*l /. fcqf[arg__][x_] -> argrec[fcqf[arg], x] /.
	  {fcqf[ders___fcpd, Particle[p_, ar___RenormalizationState,
           br___RenormalizationScheme, qs___ExpansionState],rest___] :>
           tempfac[p, ar, br, qs]*fcqf[ders, Particle[p, ar, br, qs], rest],
           fcqf[ders___fcpd, DiracBar[ Particle[p_, ar___RenormalizationState,
           br___RenormalizationScheme, qs___ExpansionState]],rest___] :>
           tempfac[p, ar, br, qs]*fcqf[ders, DiracBar[Particle[p, ar, br, qs]],rest]} /.
           argrec[tempfac[ttf__]*fcqf[arg__], x_] -> tempfac[ttf]*fcqf[arg][x] //.
	   untugrules /.
            If[Length[retord] === 1 && rf[[1]] === _,
              tempfac[tt___] /; FreeQ[{tt}, nodrop] -> tempfac[ppf[]], {}]];
      VerbosePrint[2, "Expanding NM products"];
      ddt1 = NMExpand[ddt0];
      VerbosePrint[2, "Expand DOT products"];
         ddt0 = DotExpand[ddt1];
      VerbosePrint[2, "Expanding"];
      ddt = ExpandAll[ddt0];
      VerbosePrint[2, "Finding the coefficient"];
      VerbosePrint[3, "of ",
        tempfac[ppf[]]^(2*Length[retord] + Plus @@ ((#[[2]]) & /@ retord)),
        " in ", ddt /. UTrace1 -> tr /. tr -> UTrace1];
      VerbosePrint[3, " and of ", tempfacts^2*tempfactcoeff, " in ", ddt];
      ddtt = Coefficient[ddt /. UTrace1 -> tr /. tr -> UTrace1,
                tempfac[
                    ppf[]]^(2*Length[retord] +
                      Plus @@ ((#[[2]]) & /@ retord))] +
              Coefficient[ddt, tempfacts^2*tempfactcoeff] /.
            tempfac[___] -> 1 /. DropFactor[___] -> 1; $UScalars =
        Complement[$UScalars, {tempfac, ppf}];
      If[(CommutatorReduce /. Flatten[{opts}] /. Options[DiscardTerms]),
        ddtt // (VerbosePrint[2,
                  "Applying CommutatorReduce"]; CommutatorReduce[#,opts])&, ddtt]];

SetCommutators := ($CommutatorRules /. Rule -> SetDelayed;);



(* The substitution rule to eliminate one of the momentum variables: *)

MomentaSumRule[(opts___Rule | opts___List)] :=
    Which[(MomentaSumLeft /. Flatten[{opts}] /. Options[MomentaSumRule]) ===
        FirstHalf,
      ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MomentaSumRule]) <>
            ToString[
              ParticlesNumber /. Flatten[{opts}] /.
                Options[MomentaSumRule]]] ->
        Sum[ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                    Options[MomentaSumRule]) <> ToString[i]], {i,
              1, ((ParticlesNumber /. Flatten[{opts}] /.
                      Options[MomentaSumRule])/2)}] +
          Sum[(-ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                        Options[MomentaSumRule]) <>
                    ToString[i]]), {i, (ParticlesNumber /. Flatten[{opts}] /.
                      Options[MomentaSumRule])/2 +
                1, (ParticlesNumber /. Flatten[{opts}] /.
                    Options[MomentaSumRule]) - 1}], (MomentaSumLeft /.
              Flatten[{opts}] /. Options[MomentaSumRule]) === All,
      ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MomentaSumRule]) <>
            ToString[
              ParticlesNumber /. Flatten[{opts}] /.
                Options[MomentaSumRule]]] ->
        Sum[(-ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                      Options[MomentaSumRule]) <> ToString[i]]), {i,
            1, (ParticlesNumber /. Flatten[{opts}] /.
                  Options[MomentaSumRule]) - 1}], (MomentaSumLeft /.
              Flatten[{opts}] /. Options[MomentaSumRule]) === Odd,
      ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                Options[MomentaSumRule]) <>
            ToString[
              ParticlesNumber /. Flatten[{opts}] /.
                Options[MomentaSumRule]]] ->
        Sum[ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                    Options[MomentaSumRule]) <> ToString[i]], {i,
              1, (ParticlesNumber /. Flatten[{opts}] /.
                  Options[MomentaSumRule]), 2}] -
          Sum[ToExpression[(MomentumVariablesString /. Flatten[{opts}] /.
                    Options[MomentaSumRule]) <> ToString[i]], {i,
              2, (ParticlesNumber /. Flatten[{opts}] /.
                    Options[MomentaSumRule]) - 2, 2}]];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Expanding composed objects used in chiral lagrangians *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* The u_mu field *)

$Substitutions = Append[$Substitutions,
USmall[mu_,ar___RenormalizationState,br___RenormalizationScheme,
cr___ExpansionState,(opts___Rule|opts___List)][x_] :>
I*NM[Adjoint[SMM[x,ar,br,cr,opts]],
     CDr[MM[x,ar,br,cr,opts],x,{mu}],
     Adjoint[SMM[x,ar,br,cr,opts]]]];

$Substitutions = Append[$Substitutions, USmall[mu_] :>

I*NM[Adjoint[SMM],CDr[MM,{mu}],Adjoint[SMM]]];

(* ************************************************************** *)

(* The Gamma_mu field *)

$Substitutions = Append[$Substitutions,
UGamma[mu_,ar___RenormalizationState,br___RenormalizationScheme,
cr___ExpansionState,(opts___Rule|opts___List)][x_] :>

1/2*(

NM[Adjoint[SMM[x,ar,br,cr,opts]],
FDr[SMM[x,ar,br,cr,opts],x,{mu}]]+

I*NM[Adjoint[SMM[x,ar,br,cr,opts]],
UGeneratorMatrixIsoDot[fcqf[Particle[
LeftComponent[0],ar,br,cr],{mu}][x]],
SMM[x,ar,br,cr,opts]]+

NM[SMM[x,ar,br,cr,opts],
FDr[Adjoint[SMM[x,ar,br,cr,opts]],x,{mu}]]+

I*NM[SMM[x,ar,br,cr,opts],
UGeneratorMatrixIsoDot[fcqf[Particle[
RightComponent[0],ar,br,cr],{mu}][x]],
Adjoint[SMM[x,ar,br,cr,opts]]]

)];

$Substitutions = Append[$Substitutions, UGamma[mu_] :>

1/2*(

NM[Adjoint[SMM],FDr[SMM,{mu}]]+

I*NM[Adjoint[SMM],
UGeneratorMatrixIsoDot[fcqf[Particle[
LeftComponent[0]],{mu}]],SMM]+

NM[SMM,FDr[Adjoint[SMM],{mu}]]+

I*NM[SMM,
UGeneratorMatrixIsoDot[fcqf[Particle[
RightComponent[0]],{mu}]],Adjoint[SMM]]

)];

(* ************************************************************** *)

(* The Chi_plus/Chi_minus fields *)

$Substitutions = Append[$Substitutions,
UChiPlus[x_,ar___RenormalizationState,br___RenormalizationScheme,
cr___ExpansionState,(opts___Rule|opts___List)] :>

NM[Adjoint[SMM[x,ar,br,cr,opts]],UChiMatrix[x,ar,br,cr,opts],
Adjoint[SMM[x,ar,br,cr,opts]]]+
NM[SMM[x,ar,br,cr,opts],Adjoint[UChiMatrix[x,ar,br,cr,opts]],
SMM[x,ar,br,cr,opts]]];

$Substitutions = Append[$Substitutions,
UChiMinus[x_,ar___RenormalizationState,br___RenormalizationScheme,
cr___ExpansionState,(opts___Rule|opts___List)] :>

NM[Adjoint[SMM[x,ar,br,cr,opts]],UChiMatrix[x,ar,br,cr,opts],
Adjoint[SMM[x,ar,br,cr,opts]]]-
NM[SMM[x,ar,br,cr,opts],Adjoint[UChiMatrix[x,ar,br,cr,opts]],
SMM[x,ar,br,cr,opts]]];

$Substitutions = Append[$Substitutions, UChiPlus :>

NM[Adjoint[SMM],UChiMatrix,Adjoint[SMM]]+NM[SMM,Adjoint[UChiMatrix],SMM]];

$Substitutions = Append[$Substitutions, UChiMinus :>

NM[Adjoint[SMM],UChiMatrix,Adjoint[SMM]]-NM[SMM,Adjoint[UChiMatrix],SMM]];

(* ************************************************************** *)

(* The f_plus/f_minus fields.
   The fields Vector[0] and AxialVector[0] are used *)

$Substitutions = Append[$Substitutions,
UFPlus[mu_,nu_,ar___RenormalizationState,br___RenormalizationScheme,
cr___ExpansionState,(opts___Rule|opts___List)][x_] :>

NM[SMM[x,ar,br,cr,opts],
  FieldStrengthTensorFull[{mu}, 
   UGeneratorMatrixIsoDot[
    fcqf[Particle[
         LeftComponent[0,Sequence@@OptionsSelect[LeftComponent,opts]],ar,br,cr],
	    {nu}][x]], x, -I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]],
      Adjoint[SMM[x,ar,br,cr,opts]]]+

NM[Adjoint[SMM[x,ar,br,cr,opts]],
  FieldStrengthTensorFull[{mu}, 
   UGeneratorMatrixIsoDot[
    fcqf[Particle[
         RightComponent[0,Sequence@@OptionsSelect[RightComponent,opts]],ar,br,cr],
	    {nu}][x]], x, -I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]],
  SMM[x,ar,br,cr,opts]]];

$Substitutions = Append[$Substitutions, UFPlus[mu_,nu_] :>

NM[SMM,FST[LeftComponent[0],{mu},{nu}],Adjoint[SMM]]+

NM[Adjoint[SMM],FST[RightComponent[0],{mu},{nu}],SMM]];

$Substitutions = Append[$Substitutions,
UFMinus[mu_,nu_,ar___RenormalizationState,br___RenormalizationScheme,
cr___ExpansionState,(opts___Rule|opts___List)][x_] :>

NM[SMM[x,ar,br,cr,opts],
  FieldStrengthTensorFull[{mu}, 
   UGeneratorMatrixIsoDot[
    fcqf[Particle[
         LeftComponent[0,Sequence@@OptionsSelect[LeftComponent,opts]],ar,br,cr],
	    {nu}][x]], x, -I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]],
      Adjoint[SMM[x,ar,br,cr,opts]]]-

NM[Adjoint[SMM[x,ar,br,cr,opts]],
  FieldStrengthTensorFull[{mu}, 
   UGeneratorMatrixIsoDot[
    fcqf[Particle[
         RightComponent[0,Sequence@@OptionsSelect[RightComponent,opts]],ar,br,cr],
	    {nu}][x]], x, -I,Sequence@@OptionsSelect[FieldStrengthTensorFull,opts]],
  SMM[x,ar,br,cr,opts]]];

$Substitutions = Append[$Substitutions, UFMinus[mu_,nu_] :>

NM[SMM,FST[LeftComponent[0],{mu},{nu}],Adjoint[SMM]]-

NM[Adjoint[SMM],FST[RightComponent[0],{mu},{nu}],SMM]];



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Easy entering of lagrangians *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


MM[x_?(! NumberQ[#] &), h___, opts___Rule] /; (fcexpt/.Flatten[{opts}]/.Options[MM]) :=

    UFieldMatrix[fcqf[(Particle[Pion, h])][x], ##] & @@
      Union[OptionsSelect[UFieldMatrix, opts], OptionsSelect[UMatrix, opts]];
MM[ii_?NumberQ, x_, h___, opts___Rule] /; (fcexpt/.Flatten[{opts}]/.Options[MM]) :=
    UFieldMatrix[ii, fcqf[(Particle[Pion, h])][x], ##] & @@
      Union[OptionsSelect[UFieldMatrix, opts], OptionsSelect[UMatrix, opts]];
SMM[x_, h___, opts___Rule] /; (fcexpt/.Flatten[{opts}]/.Options[SMM]) := MM[1/2, x, h, opts];
MMS[x_, h___, opts___Rule] /; (fcexpt/.Flatten[{opts}]/.Options[MMS]) :=
    UFieldMatrixSeries[fcqf[(Particle[Pion, h])][x], ##] & @@
       Union[OptionsSelect[UFieldMatrix, opts], OptionsSelect[UMatrix, opts]];

FST[p_, {mu_}, {nu_}, x_, a___RenormalizationState, b___RenormalizationScheme,
       c___ExpansionState, (opts___Rule | opts___List)] :=
    FST[p, fcli[mu], fcli[nu], x, a, b, c, opts];
(*FST[p_, mu_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
        nu_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex, x_,
        a___RenormalizationState, b___RenormalizationScheme,
        c___ExpansionState, (opts___Rule | opts___List)] :=
      FieldStrengthTensorFull[mu,
UGeneratorMatrixIsoDotFull[fcqf[Particle[p, a, b, c], nu][x], opts], x];*)



(* Tentative simpler form: *)

FST[p_, mu_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
      nu_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex, x_,
      a___RenormalizationState, b___RenormalizationScheme,
      c___ExpansionState, (opts___Rule | opts___List)] :=
    FieldStrengthTensor[mu,
UGeneratorMatrixIsoDotFull[fcqf[Particle[p, a, b, c], nu][x], opts], x, opts] +
      (*Think this was a bug - changed 14/11-2001*)(*2*I*UGeneratorMatrixIsoDotFull[*)
      2*I*UGeneratorMatrixIsoDot[
          IsoCross[(IsoVector[fcqf[Particle[p, a, b, c], mu], ##] & @@
                  OptionsSelect[IsoVector, opts])[
              x], (IsoVector[fcqf[Particle[p, a, b, c], nu], ##] & @@
                  OptionsSelect[IsoVector, opts])[x]], opts];

(*Why this?? ?*)
  fccoupl[a_, b_, rest__][i_] :=
    fccoupl[a, b, RenormalizationState[i], rest];
fccoupl[a_, b_][i_] :=
    fccoupl[a, b, RenormalizationState[i]];



(* Splitting products of MMs and SMMs into sums of expanded factors. 11/3-2000. *)



(* The help function fdr knows how to do multiple partial derivations and the
   product rule: *)

(*Commented out 18/4 - 2000*)
(*fdr[aa_, {loris__HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
          lori1_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex}] :=
(newfuncc[1] = fdr[aa, lori1];
        Do[
          newfuncc[rep + 1] =
            fdr[newfuncc[rep], ##] & @@ Take[{loris}, {-rep}], {rep, 1,
            Length[{loris}]}]; newfuncc[Length[{loris}] + 1]);*)
(*fdr[(tim : NM | Times | Dot)[a_, b_], \[Nu]_] :=
      tim[a, fdr[b, \[Nu]]] + tim[fdr[a, \[Nu]], b];*)

(*Changed 18/4 - 2000*)
fdr[(tim : NM | Times | Dot)[a__, b_], {\[Nu]_}] :=
    tim[a, fdr[b, {\[Nu]}]] + tim[fdr[tim[a], {\[Nu]}], b];
fdr[((ad : (Adjoint | Conjugate | Transpose))[a_]), {\[Mu]__}] :=
    ad[fdr[a, {\[Mu]}]];
(*Added 18/4 - 2000*)
fdr[fdr[a_, {\[Mu]__}], {\[Nu]__}] := fdr[a, Reverse[{\[Mu], \[Nu]}]];
(*Added 18/4 - 2000*)
fdr[a_Plus, lori : {__}] :=
   Plus @@ (fdr[#,
   (*Hmm, why is this x here, seems like a typo. Commented out 14/6 - 2000*)(*x,*)
                lori] & /@ (List @@ a));



(* Help function to get combinations of expansion orders that contribute to the
   expansion of a product: *)

combtab[dummys : List[__], order_Integer] := (List @@ #) & /@
      Flatten[Table[
          dumfunc @@
            Append[dummys,
              order - Sum[dummys[[repp]], {repp, 1, Length[dummys]}]],
          Evaluate[
            Sequence @@
              Reverse[Table[{Reverse[dummys][[rep]], 0,
                    order -
                      Sum[dummys[[repp]], {repp, 1,
                          Length[dummys] - rep}]}, {rep, Length[dummys]}]]]]];
combtab[{}, n_Integer] := {{n}};

USplit[exp_NM, x_, ar___RenormalizationState, br___RenormalizationScheme,
      cr___ExpansionState,
      opts___Rule] :=(*Block[{mms, pos, splits, parts, ct, i, ord, ord1,
          ad},*)(ord1 = ExpansionOrder /. Flatten[{opts}];
      If[Head[ord1] === List, ord = ord1[[1]], ord = ord1];
      mms = Select[List @@ exp, ((! FreeQ[{#}, MM | SMM, Infinity]) &)];
      pos = Position[exp, _?((! FreeQ[{#}, MM | SMM, Infinity]) &), {1}];
      splits = Evaluate[(dum @@ #) & /@ pos];
      parts = (ct = combtab[Drop[splits, -1], ord]);
      VerbosePrint[3, "Splitting\n", parts];
      Plus @@ Table[
          ReplacePart[
            exp, (USplit[#[[1]], x,
	    (*RenormalizationState etc. are no longer in front after Selecting, fixed. 28/9-2000*)
            Sequence @@ Join[
	    {ar, br, cr},
	    Select[{opts}, (MatchQ[#, ExpansionOrder -> _] =!= True) &]],
	    ExpansionOrder -> {#[[2]]}]) & /@ Transpose[{mms, ct[[i]]}],
            pos, ({#}) & /@ Range[Length[pos]]], {i, Length[ct]}](*]*));

USplit[fdr[SMM | MM, {_}], ___, ExpansionOrder -> {0}, ___] := 0;

USplit1[exp_, x_, ar___RenormalizationState, br___RenormalizationScheme,
      cr___ExpansionState, opts___Rule] :=(*Block[{i, n, xx, res, aa, a},*)(res =
        exp //. {CovariantFieldDerivative[
                  mm_, {\[Mu]_}] :>(*No x dependence in mm,
                  so we get the extra part only*)
                  CovariantFieldDerivative[mm, x, {\[Mu]},
                    Sequence @@
                      OptionsSelect[CovariantFieldDerivative, opts]] +
                  fdr[mm, {\[Mu]}],
              CovariantNabla[mm_, {\[Mu]_}] :>(*No x dependence in mm,
                  so we get the extra part only*)(*Added x - dependence,
                  bugfix?, 12/6 - 2000*)
                  CovariantNabla[mm, x, Sequence @@ (fcli /@ {\[Mu]}),
                    Sequence @@ OptionsSelect[CovariantNabla, opts]] +
                  fdr[mm, {\[Mu]}], FieldDerivative -> fdr,
              HoldPattern[USplit[UTrace1[xx_], n__]] :>
                UTrace1[USplit[xx, n]],
              HoldPattern[
                  USplit[(ad : (Adjoint | Conjugate | Transpose))[xx_],
                    n__]] :> ad[USplit[xx, n]],
              USplit[a_ + b_, i__] :> USplit[a, i] + USplit[b, i],
              USplit[(Times | NM)[a__],
                  i__] :> (Times @@ Select[{a}, ((UScalarQ[#]&&FreeQ[#,allpatterns])&)])*
                  USplit[NM @@ Select[{a}, ((!(UScalarQ[#]&&FreeQ[#,allpatterns]))&)],
                    i],(*Added 19/4 - 2000*)
                USplit[a_, i___, ExpansionOrder -> n_, j___] /; UScalarQ[a] &&FreeQ[a,allpatterns]:>
                 If[n === {0}, a, 0]} /.
          aa_NM /; (! FreeQ[aa, MM | SMM] &&
                  FreeQ[{aa}, USplit, Infinity, Heads -> True]) -> (USplit[aa,
                 x,
                Sequence @@ Join[
                  {ar, br, cr},Select[{opts}, (MatchQ[#, ExpansionOrder -> _] =!= True) &]],
		  ExpansionOrder -> (DropOrder /. Flatten[{opts}] /. Options[UNMSplit])]);
      VerbosePrint[3, "Expanding NM products in ",res]; NMExpand[res](*]*));

UNMSplit[exp_, x_, ar___RenormalizationState, br___RenormalizationScheme,
      cr___ExpansionState,
      opts___Rule] :=(*Block[{res, errs},*)(res =
         FixedPoint[USplit1[#, x, ar, br, cr, opts] &,
				 (*Added $PreSubstitutions and $PostSubstitutions 8/2-2002 for configurability
				 through configuration files.*)
				 exp//.$PreSubstitutions[x,ar,br,cr,opts]//.
				 $Substitutions//.$PostSubstitutions[x,ar,br,cr,opts]];
      res /.
         fdr -> FieldDerivative /.
         {USplit[mm : (SMM | MM), n__] :> ArgumentsSupply[mm, n],
	 USplit[mm :FieldDerivative[
         SMM | MM, {__(*_ replaced with __, 18/4 - 2000*)}],
                  n__] :> ArgumentsSupply[mm, n]} /.
         Times -> NM /.(*Added 19/4 - 2000*)
         USplit[a_, i___, ExpansionOrder -> n_, j___] /; UScalarQ[a]&&FreeQ[a,allpatterns] :>
           If[n === {0}, a, 0](*;
      If[(errs = Union[Cases[res, _USplit, Infinity]]) === {}, res,
          Message[UNMSplit::nores, errs]]*)(*]*));



(* Hmm -all is very non-general... *)

ArgumentsSupply1[expr_, x_, ar___RenormalizationState,
      br___RenormalizationScheme, cr___ExpansionState,
      opts___Rule] := (VerbosePrint[3,"Using options ", InputForm[{opts}]];

      (*Union does not preserve the order. Fixed 28/9-2000*)
  o1 = Join[{ar, br, cr},Union[OptionsSelect[UQuarkMass, opts], OptionsSelect[UMatrix, opts]]];
  o2 = Join[{ar, br, cr},Union[OptionsSelect[UFieldMatrix, opts], OptionsSelect[UMatrix, opts]]];
  o22 = Join[{ar, br, cr},Union[OptionsSelect[UFieldMatrixSeries, opts], OptionsSelect[UMatrix, opts]]];
  o3 = OptionsSelect[IsoVector, opts];
  o4 = Join[{ar, br, cr},Union[OptionsSelect[UQuarkCharge, opts], OptionsSelect[UMatrix, opts]]];
  o5 = OptionsSelect[UMatrix, opts];
  o6 = OptionsSelect[UGenerator, opts];

      Block[{(*Added Nabla 4/1 - 2000*)CovariantNabla,
                                   CovariantFieldDerivative,
                                  CovariantNucleonFieldDerivative, FST,
                                  FieldStrengthTensorFull,
                                  FieldStrengthTensor, MM, MMS, SMM,
                                  IsoVector, PhiMeson, PhiMesonIsoVector,
                                  NM,(*Changed UTrace to UTrace1,
                                    12 - 6 - 2000*)UTrace1, FieldDerivative,
                                  IsoDot, IsoCross, IsoSymmetricCross,
                                  UQuarkMassMatrix, UChiMatrix,UChiralSpurionMatrix,
                                  UChiralSpurionRightMatrix,UChiralSpurionLeftMatrix,
                                  UQuarkChargeMatrix, UIdentityMatrix, QCM,
				  (*Added 9/9-2000 to avoid conflicts with
				  patterns from $UScalars*)
				  a,b,pa,mu,nu,p,i,som,pp,lli,bbb,aa,lisxx},

   NM := NM5;
   CovariantNabla := CNb2;
   CovariantFieldDerivative := CDr2;
   CovariantNucleonFieldDerivative := CNDr2;(*Change 20/12/1999; added cr*)
   FST[p_[i_], mu_, nu_] := FST2[p[i], mu, nu, x, ar, br, cr, ##] & @@ o5;
   FST[p_, mu_, nu_] := FST2[p[0], mu, nu, x, ar, br, cr, ##] & @@ o5;
   FieldStrengthTensorFull[fcqf[pp_],lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
     opt___Rule] :=
      fstf[fcqf[pp][x], lli, opt];
   FieldStrengthTensor[fcqf[pp_],lli_HighEnergyPhysics`FeynCalc`LorentzIndex`LorentzIndex,
     opt___Rule] :=
      fst[fcqf[pp][x], lli, opt];
   UTrace1 := utr2;
   (*Change 11/2-2002*)
   FieldDerivative[aa_, {lis__}] := FDr2[aa, {lis}];
   (*FieldDerivative := FDr2;*)
   IsoDot := IsoDot2;
   IsoCross := IsoCross2;
   IsoSymmetricCross := IsoSymmetricCross2;
   PhiMesonIsoVector := (PV2[x, ##] & @@ o3);
   MM[som__] := MM6[som];
   MM := (MM2[x, ##] & @@ o2);
   SMM[som__] := SMM6[som];
   SMM := (SMM2[x, ##] & @@ o2);
   MMS[som__] := MMS6[som];
   MMS := (MMS2[x, ##] & @@ o22);
   UIdentityMatrix := UIdentityMatrix2[##] & @@ o5;
   UQuarkMassMatrix := QuarkMassMatrix2[##] & @@ o1;
   UQuarkChargeMatrix := UQuarkChargeMatrix2[##] & @@ o4;
   UChiMatrix := (Chi2[x, ##] & @@ o1);
   UChiralSpurionMatrix := (UChiSp[x, ##] & @@ o1);
   UChiralSpurionRightMatrix := (UChiSpR[x, ##] & @@ o1);
   UChiralSpurionLeftMatrix := (UChiSpL[x, ##] & @@ o1); expr]
   ) /.

   {DropFactor[___] -> 1,
   FST2 -> FST,
   fstf -> FieldStrengthTensorFull,
   fst -> FieldStrengthTensor, MM2 -> MM,
   SMM2 -> SMM,
   MMS2 ->MMS,
   (*utr2 -> UTrace,*)(*moved trace replacement down to avoid non - intentional zeros*)
   IsoDot2 -> IsoDot,
   IsoCross2 -> IsoCross,
   IsoSymmetricCross2 -> IsoSymmetricCross,
   PV2 -> PhiMesonIsoVector,
   QuarkMassMatrix2 -> UQuarkMassMatrix,
   UQuarkChargeMatrix2 -> UQuarkChargeMatrix,
   Chi2 -> UChiMatrix,
   (*Added spurions 27/5-2002*)
   UChiSp -> UChiralSpurionMatrix,
   UChiSpR -> UChiralSpurionRightMatrix,
   UChiSpL -> UChiralSpurionLeftMatrix,
   UIdentityMatrix2 -> UIdentityMatrix,
   utr2 -> UTrace} //.

   {CNb2[aa_, {lis__}] -> CNb3[aa, x, {lis}, ##] & @@ OptionsSelect[CovariantNabla, opts],
   CDr2[aa_, {lis__}] -> CDr3[aa, x, {lis}, ##] & @@ OptionsSelect[CovariantFieldDerivative, opts],
   CNDr2[aa_, {lis__}] -> CNDr3[aa, x, {lis}, ##] & @@ OptionsSelect[CovariantNucleonFieldDerivative,
       opts]} /.

    FDr2[aa_, {lis__}] -> FDr3[aa, x, {lis}] /.

    {fcqf[bbb__][xx_] -> qftemp[bbb][xx],
    IsoVector[fcqf[bbb__], left___][xx_] ->
    IsoVector[qftemp[bbb], left][xx],
       IsoVector[ffr_[fcqf[bbb__]], left___][xx_] ->
       IsoVector[ffr[qftemp[bbb]], left][xx]} /.

    fcqf[bbb__] -> fcqf[bbb][x] /.

    qftemp -> fcqf /.

    {FDr3 -> FieldDerivative, NM5 -> NM} /.

    {CDr3 -> CovariantFieldDerivative,
    CNDr3 -> CovariantNucleonFieldDerivative,
    CNb3 -> CovariantNabla} /.

    {Particle[pa_] -> Particle[pa, ar, br, cr],
    ParticleMass[pa_] -> ParticleMass[pa, ar, br, cr],
    DecayConstant[pa_] -> DecayConstant[pa, ar, br, cr],
    fccoupl[pa_] -> fccoupl[pa, ar, br, cr]} /.

    (*Commented out 9/9-2000; don't think its necessary; we don't rely on NumericFunction anymore;*)
    (*(Rule[f_[a___, #, b___] /; FreeQ[{f, a, b}, _Particle], f[a, #[], b]] & /@ $UScalars) /.*)

    {MM6 -> MM, SMM6 -> SMM, MMS6 -> MMS} /.

    (*Added 25/2 - 2000*)UMatrix[UGenerator[i_]] :>
     UMatrix[UGenerator[fcsuni[i], Sequence @@ o6], Sequence @@ o5]/;FreeQ[i,fcsuni];



ArgumentsSupply[expr_, x_, ar___RenormalizationState,
   br___RenormalizationScheme,cr___ExpansionState, (opts___Rule | opts___List)] :=
   (If[ MemberQ[{Rule, RenormalizationState, RenormalizationScheme,
        ExpansionState}, Head[x]], Message[ArgumentsSupply::noarg]; Return[];];
    If[!FreeQ[expr, x, Infinity, Heads -> True], Message[ArgumentsSupply::argxpr, x]];
        ArgumentsSupply1[expr//.$PreSubstitutions[x,ar,br,cr,opts]//.
				$Substitutions//.$PostSubstitutions[x,ar,br,cr,opts],
				x, ar, br, cr, opts]);



(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)
(********************************************************************************)
(* Commutation rules *)
(********************************************************************************)
(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)


(* CommutatorReduce: *)

CommutatorReduce[expr_, (opts___Rule | opts___List)]  /; (FullReduce /. Flatten[{opts}] /.
   Options[CommutatorReduce]) =!= True := expr //. $CommutatorRules;

nsort[{a_, b_}] :=
    Block[{s}, s = Sort[{a, b}]; If[s === {a, b}, {a, b}, Conjugate /@ s]];

CommutatorReduce[expr_, (opts___Rule | opts___List)] /; (FullReduce /. Flatten[{opts}] /.
   Options[CommutatorReduce]) === True :=
  Block[{a,b,exp,scq}, scq=!UScalarQ[UTrace1];DeclareUScalar[UTrace1];
	exp=expr /.patternCommRule//. $CommutatorRules /.{(NM | NonCommutativeMultiply)[a__] :>
      Times[a] /; FreeQ[{a}, Alternatives @@ $NonCommute],
      (p:(IsoDot|IsoCross|IsoSymmetricCross))[a_,b_] :>
      p@@nsort[{a,b}] /; (FreeQ[a, Alternatives @@ $NonCommute] ||
                         FreeQ[b, Alternatives @@ $NonCommute])};
			If[scq,UndeclareUScalar[UTrace1]];
			exp];

(* Patterns are considered non-scalars by NM, but here we force taking out UTrace1 of patterns *)
  patternCommRule = HoldPattern[NM[a__]]  :>  (Times @@ Select[{a}, ((UScalarQ[#])&)])*(NM[##] & @@
          Select[{a}, (! UScalarQ[#] &)]) /; btss1[a];



(* These non-commuting objects will not be taken out of NM: *)

$NonCommute := Union[List @@ $UMatrices, {UMatrix, UVector, DiracBar,
      HighEnergyPhysics`FeynCalc`DiracBasis`DiracBasis,
      HighEnergyPhysics`FeynCalc`DiracGamma`DiracGamma,
      HighEnergyPhysics`FeynCalc`DiracGammaT`DiracGammaT,
      HighEnergyPhysics`FeynCalc`DiracMatrix`DiracMatrix,
      HighEnergyPhysics`FeynCalc`DiracSlash`DiracSlash,
      (*Added 12/1-2000*)FST
      (*Commented out PartialD 3/8-2000, don't know why it's here*)(*,
      HighEnergyPhysics`FeynCalc`PartialD`PartialD*)(*,
        HighEnergyPhysics`FeynCalc`LeptonSpinor`LeptonSpinor,
        HighEnergyPhysics`FeynCalc`QuarkSpinor`QuarkSpinor,
        HighEnergyPhysics`FeynCalc`Spinor`Spinor,
        HighEnergyPhysics`FeynCalc`SpinorU`SpinorU,
        HighEnergyPhysics`FeynCalc`SpinorUBar`SpinorUBar, SpinorV`SpinorV,
        HighEnergyPhysics`FeynCalc`SpinorVBar`SpinorVBar*)}];

(*Changed 13/3 - 2000*)
noncommpatt :=
  Alternatives @@ Union[$NonCommute, List @@ $UMatrices]
(*noncommpatt := Alternatives @@ $NonCommute;*)



(* Some rules are "inverse" of each other and so may leave the object untouched,
but also after the application of one of them something may cancel.  Repeated
applications may then cancel something more: *)

$CommutatorRules1 = {

   (*Different field components commute*)
   IsoCross[a_, a_] /; FreeQ[a, UMatrix] -> 0,
   IsoCross[IsoVector[fcqf[ders__fcpd, Particle[f__]], body___][x_],
      IsoVector[fcqf[Particle[ff__]], bodyy___][x_]] ->
      (*Changed to accomodate the new IsoDot, 1/2 - 2000*)
      -IsoCross[Conjugate[IsoVector[fcqf[Particle[ff]], bodyy][x]],
            Conjugate[IsoVector[fcqf[ders, Particle[f]], body][x]]],

  (*Different field components commute*)
   IsoDot[IsoCross[a_, b_],a_] /; (FreeQ[a, UMatrix] || FreeQ[b, UMatrix]) -> 0,

   (*Different field components commute*)
   IsoDot[IsoCross[a_, b_], b_] /; (FreeQ[a, UMatrix] || FreeQ[b, UMatrix]) -> 0,

   (*Always valid for cross products*)
   IsoDot[a_ /; FreeQ[a, IsoCross], IsoCross[b_, c_]] ->
      (*Changed to accomodate the new IsoDot, 7/3 - 2000*)
      IsoDot[IsoCross[Conjugate[a], b], c],

   (*Always valid for cross products*)
    IsoDot[IsoCross[a_IsoCross, b_], c_] ->
       (*Changed to accomodate the new IsoDot, 7/3 - 2000*)
       IsoDot[Conjugate[a], IsoCross[b, c]],

   (*Absolutely identical fields commute*)
   (*NM[a___, fcqf[ders___fcpd, Particle[p_, r___], rest___][x_],
      fcqf[ders___fcpd, Particle[p_, r___], rest___][x_], b___] ->
      NM[a, fcqf[ders, Particle[p, r], rest][x]*fcqf[ders, Particle[p, r], rest][x], b],*)

   (*Absolutely identical objects commute*)
   NM[a___, c_, c_, b___](*Added condition 19/3 - 2000*)/;
      FreeQ[c, noncommpatt] && MatrixQ[c] =!= True ->
      NM[a, c*c, b],

   (*Different field components commute*)
   (*Changed to accomodate the new IsoDot, 1/2 - 2000*)
   IsoDot[IsoVector[fcqf[der___, Particle[f_, r___], o___], opts___][x_],
      IsoVector[fcqf[derr___, Particle[ff_, rr___], oo___], optss___][xx_]] /; f =!= ff :>
      (res =  Sort[IsoDot[IsoVector[fcqf[der, Particle[f, r], o], opts][x],
              IsoVector[fcqf[derr, Particle[ff, rr], oo], optss][xx]]];
       If[FreeQ[res[[1]], f], res, Conjugate /@ res])};



(* If [the fields of b are different modulo derivatives etc. from the fields of
a and c] or [the fields of b are a subset of the intersection of the fields
of a and c including derivatives etc.] then b is assumed to commute with a
and c.  Is this always true?? *)



(* Added Conjugates. 1/2-2000. *)

(* Changed from using Intersection to using checksub to avoid e.g.
   NM[IsoDot[a,b],IsoDot[b,a]] being replaced with Times[...] because
   Intersect order the result *)

checksub[x_List, y_List] :=
    Block[{xx, yy}, xx = StringJoin @@ (ToString /@ x);
      yy = StringJoin @@ (ToString /@ y);
      StringMatchQ[xx, "*" <> yy <> "*"] ||
        StringMatchQ[yy, "*" <> xx <> "*"]];

$CommutatorRules2 =
(*Changed to accomodate the new IsoDot, 1/2 - 2000*)
{
   (*Added 5/2-2001*)
   (*Commented out stuff 17/8-2001, no need for all this*)
   NM[a___, b_, c___] /; ((FreeQ[b, noncommpatt] || FreeQ[{a, c}, noncommpatt]) (*&&
     Length[tmpfield =
       Union[Cases[b, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
       Infinity, Heads -> True]]] === 1 &&
        Intersection[
            Cases[{b}, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
              Infinity, Heads -> True],
            Cases[{a, c}, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
              Infinity, Heads -> True]] ===
        Cases[{b}, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
            Infinity, Heads -> True]*)) :> b*NM[a, c],

   NM[a___, b_, c___] /;
   ((FreeQ[b, noncommpatt] || FreeQ[{a, c}, noncommpatt]) &&
   (Intersection[(#[[1]] &) /@ Cases[b, _Particle, Infinity, Heads -> True],
   (#[[1]] &) /@ Cases[{a, c}, _Particle, Infinity, Heads -> True]] === {})) :>
   b*NM[a, c],

   NM[a__, b__] /;
   ((FreeQ[{b}, noncommpatt] || FreeQ[{a}, noncommpatt]) &&
   (checksub[ Join[
      Cases[{b},_HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
         Infinity, Heads -> True],
      Cases[{a}, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
      Infinity, Heads -> True]] ])) :>
   NM[b]*NM[a],

   NM[b__,a__] /;
   ((FreeQ[{b}, noncommpatt] || FreeQ[{a}, noncommpatt]) &&
   (checksub[ Join[
      Cases[{b},_HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
         Infinity, Heads -> True],
      Cases[{a}, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
      Infinity, Heads -> True]] ])) :>
   NM[b]*NM[a],

   NM[a___, Times[b_, bb_], c___] /;
   ((FreeQ[b, noncommpatt] || FreeQ[{a, c}, noncommpatt]) &&
   (Intersection[(#[[1]] &) /@ Cases[b, _Particle, Infinity, Heads -> True],
   (#[[1]] &) /@ Cases[{a, bb, c}, _Particle, Infinity, Heads -> True]] === {})) :>
   b*NM[a, bb, c],

   NM[a___, Times[b_, bb_],c___] /;
   ((FreeQ[b, noncommpatt] || FreeQ[{a,c}, noncommpatt]) &&
   (*Changed 5/1-2001*)(checksub[
   Join[
     Cases[{a},_HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
       Infinity,Heads -> True],
       Cases[{c}, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField,
       Infinity, Heads -> True]],
     Cases[{b},_HighEnergyPhysics`FeynCalc`QuantumField`QuantumField, Infinity,
       Heads -> True]])) :>
   b*NM[a, bb, c],

    (cr:(IsoDot|IsoCross|IsoSymmetricCross))[a_, b_*(bb : (IsoVector[__][_] | IsoVector[__] | IsoCross[__] |
    IsoSymmetricCross[__]))] /;
    (FreeQ[Head[b], IsoVector] && (FreeQ[b, noncommpatt] ||
    FreeQ[a,noncommpatt]) && (Intersection[(#[[1]] &) /@ Cases[b, _Particle, Infinity,
    Heads -> True], (#[[1]] &) /@ Cases[{a, bb}, _Particle, Infinity,
    Heads -> True]] === {} )) ->
    b*cr[a, bb],

    (cr:(IsoDot|IsoCross|IsoSymmetricCross))[b_*(bb : (IsoVector[__][_] | IsoVector[__] | IsoCross[__] |
    IsoSymmetricCross[__])),a_] /;
    (FreeQ[Head[b], IsoVector] && (FreeQ[b, noncommpatt] ||
    FreeQ[a,noncommpatt]) && (Intersection[(#[[1]] &) /@ Cases[b, _Particle, Infinity,
    Heads -> True], (#[[1]] &) /@ Cases[{a, bb}, _Particle, Infinity,
    Heads -> True]] === {} )) ->
    Conjugate[b]*cr[bb, a],

    (cr:(IsoDot|IsoCross|IsoSymmetricCross))[a_, b_*(bb : (IsoVector[__][_] | IsoVector[__] | IsoCross[__] |
    IsoSymmetricCross[__]))] /;
    (FreeQ[Head[b], IsoVector] && (FreeQ[b, noncommpatt] ||
    FreeQ[a,noncommpatt]) && (
    checksub[Cases[a, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField, Infinity,
                    Heads -> True],
                 Cases[b, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField, Infinity,
		    Heads -> True]] (*Commented out 5/2-2001*)(*&&
    checksub[Cases[b, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField, Infinity,
                    Heads -> True],
                 Cases[bb, _HighEnergyPhysics`FeynCalc`QuantumField`QuantumField, Infinity,
		    Heads -> True]]*))) ->
    b*cr[a, bb]

};



(* The product of a matrix of scalars and non-matrices (added 19/3-2000 - turned
out to be redundant): *)

(*$CommutatorRules3 = {NM[a___, UMatrix[b_, ___Rule],
              c___] /; (Union[MatrixQ /@ {a, c}] === {False} &&
                FreeQ[{a, c}, UMatrix, Infinity, Heads -> True] &&
                UScalarQ[b]) :> Times[a, UMatrix[b], c]};*)
$CommutatorRules =
    Join[$CommutatorRules1, $CommutatorRules2(*, $CommutatorRules3*)];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

End[];

(*XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX*)

EndPackage[];

If[$Verboseness > 0,WriteString["stdout", "Objects | \n "]];
